;;; ok-audio-transcription.el --- Voice transcription using OpenAI's Realtime API -*- lexical-binding: t -*-

;;; Author: Alain M. Lafon <alain@200ok.ch>
;;; Version: 0.1.0
;;; Package-Requires: ((emacs "27.1"))
;;; Keywords: multimedia, convenience
;;; URL: https://github.com/200ok/ok-audio-transcription
;;; Commentary:
;;;; Inspired by https://simonsafar.com/2024/whisper/

;;; Code:

(require 'json)

(defgroup ok-audio-transcription nil
  "Voice transcription using OpenAI's Realtime API."
  :group 'multimedia)

(defcustom ok-audio-transcription--openai-token nil
  "OpenAI API token for authentication.
Must be set before using transcription functions.
Get your token from https://platform.openai.com/api-keys"
  :group 'ok-audio-transcription
  :type 'string)

(defcustom ok-audio-transcription--max-recording-seconds 180
  "Maximum recording length in seconds."
  :group 'ok-audio-transcription
  :type 'integer)

(defcustom ok-audio-transcription--model "gpt-4o-transcribe"
  "The model to use for transcription. Default is 'gpt-4o-transcribe'."
  :group 'ok-audio-transcription
  :type 'string)

(defvar ok-audio-transcription--ffmpeg-process nil
  "Process object for the current ffmpeg recording process.")
(defvar ok-audio-transcription--temp-file nil
  "Temporary file path for storing the current recording.")

(defun ok-audio-transcription--record-dwim ()
  "Toggle voice recording for transcription.
When called first time, starts recording audio using the system
microphone. When called again, stops the recording and sends it to
the selected transcription model. The transcription result is inserted at
point."
  (interactive)
  (if (not ok-audio-transcription--ffmpeg-process)
      (ok-audio-transcription--start-recording)
    (ok-audio-transcription--stop-recording)))

(defun ok-audio-transcription--start-recording ()
  "Start recording audio from the default microphone.
Creates a temporary MP3 file and starts ffmpeg process to record audio.
Maximum recording length is specified by 'ok-audio-transcription--max-recording-seconds'."
  (setq ok-audio-transcription--temp-file (make-temp-file "voice_temp" nil ".mp3"))
  (message "Temporary file created at: %s" ok-audio-transcription--temp-file)
  (let* ((proc (start-process-shell-command
                "voice ffmpeg" "*ffmpeg*"
                (format "ffmpeg -y -t %s -f pulse -i default %s"
                        ok-audio-transcription--max-recording-seconds
                        ok-audio-transcription--temp-file))))
    (setq ok-audio-transcription--ffmpeg-process proc)
    (message "Recording...")
    (set-process-sentinel proc 'ok-audio-transcription--recording-sentinel)))

(defun ok-audio-transcription--stop-recording ()
  "Stop the current audio recording process.
Interrupts the ffmpeg process and prepares for transcription."
  (interrupt-process ok-audio-transcription--ffmpeg-process)
  (message "Stopped recording"))

(defun ok-audio-transcription--recording-sentinel (proc _event)
  "Monitor the recording process and handle its completion.
Triggers transcription when recording finishes successfully.

PROC is the process object for ffmpeg.
EVENT is the process status change event."
  (setq ok-audio-transcription--ffmpeg-process nil)
  (if (memq (process-exit-status proc) '(2 255))
      (progn
        (message "Finished recording; sending to OpenAI Realtime API...")
        (ok-audio-transcription--call-openai-api ok-audio-transcription--temp-file))
    (message "Something went wrong with the process")))

(defun ok-audio-transcription--call-openai-api (audio-path)
  "Send audio file to OpenAI's Realtime API for transcription.
Insert the transcription result at point.

AUDIO-PATH is the path to the audio file to transcribe.
Requires `ok-audio-transcription--openai-token' and `ok-audio-transcription--model' to be set."
  (unless ok-audio-transcription--openai-token
    (error "No API token available"))
  (let* ((command (format "curl -s https://api.openai.com/v1/audio/transcriptions \
-H \"Authorization: Bearer %s\" \
-H \"Content-Type: multipart/form-data\" \
-F file=\"@%s\" \
-F model=\"%s\""
                          ok-audio-transcription--openai-token
                          audio-path
                          ok-audio-transcription--model))
         (response (shell-command-to-string command))
         (transcription (cdr (assoc 'text (json-read-from-string response)))))
    (message "Transcription result: %s" transcription)
    (insert transcription)
    (delete-file audio-path)))

(defun ok-audio-transcription--transcribe-file ()
  "Transcribe an existing audio file using OpenAI's Realtime API.
Prompts for an audio file selection and sends it to the API for transcription.
The transcription result is inserted at point.
Supports common audio formats like MP3, WAV, and M4A."
  (interactive)
  (let ((audio-file (read-file-name "Select audio file to transcribe: ")))
    (if (file-exists-p audio-file)
        (ok-audio-transcription--call-openai-api audio-file)
      (message "Selected file does not exist."))))

(provide 'ok-audio-transcription)
;;; ok-audio-transcription.el ends here
