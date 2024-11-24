;;; ok-whisper.el --- Voice transcription using OpenAI's Whisper API -*- lexical-binding: t -*-

;;; Author: Alain M. Lafon <alain@200ok.ch)
;;; Version: 0.1.0
;;; Package-Requires: ((emacs "27.1"))
;;; Keywords: multimedia, convenience
;;; URL: https://github.com/200ok/ok-whisper
;;; Commentary:
;;;; Inspired by https://simonsafar.com/2024/whisper/

;;; Code:

(require 'json)

(defgroup ok-whisper nil
  "Voice transcription using OpenAI's Whisper API."
  :group 'multimedia)

(defcustom ok-whisper--openai-token nil
  "OpenAI API token for authentication.
Must be set before using transcription functions.
Get your token from https://platform.openai.com/api-keys"
  :group 'ok-whisper
  :type 'string)

(defcustom ok-whisper--max-recording-seconds 180
  "Maximum recording length in seconds."
  :group 'ok-whisper
  :type 'integer)

(defvar ok-whisper--ffmpeg-process nil
  "Process object for the current ffmpeg recording process.")
(defvar ok-whisper--temp-file nil
  "Temporary file path for storing the current recording.")

(defun ok-whisper--record-dwim ()
  "Toggle voice recording for transcription.
When called first time, starts recording audio using the system
microphone. When called again, stops the recording and sends it to
Whisper API for transcription. The transcription result is inserted at
point."
  (interactive)
  (if (not ok-whisper--ffmpeg-process)
      (ok-whisper--start-recording)
    (ok-whisper--stop-recording)))

(defun ok-whisper--start-recording ()
  "Start recording audio from the default microphone.
Creates a temporary MP3 file and starts ffmpeg process to record audio.
Maximum recording length is 180 seconds."
  (setq ok-whisper--temp-file (make-temp-file "voice_temp" nil ".mp3"))
  (message "Temporary file created at: %s" ok-whisper--temp-file)
  (let* ((proc (start-process-shell-command
                "voice ffmpeg" "*ffmpeg*"
                (format "ffmpeg -y -t %s -f pulse -i default %s"
                        ok-whisper--max-recording-seconds
                        ok-whisper--temp-file))))
    (setq ok-whisper--ffmpeg-process proc)
    (message "Recording...")
    (set-process-sentinel proc 'ok-whisper--recording-sentinel)))



(defun ok-whisper--stop-recording ()
  "Stop the current audio recording process.
Interrupts the ffmpeg process and prepares for transcription."
  (interrupt-process ok-whisper--ffmpeg-process)
  (message "Stopped recording"))

(defun ok-whisper--recording-sentinel (proc _event)
  "Monitor the recording process and handle its completion.
Triggers transcription when recording finishes successfully.

PROC is the process object for ffmpeg.
EVENT is the process status change event."
  (setq ok-whisper--ffmpeg-process nil)
  (if (memq (process-exit-status proc) '(2 255))
      (progn
        (message "Finished recording; sending to Whisper...")
        (ok-whisper--call-openai-whisper ok-whisper--temp-file))
    (message "Something went wrong with the process")))

(defun ok-whisper--call-openai-whisper (audio-path)
  "Send audio file to OpenAI's Whisper API for transcription.
Insert the transcription result at point.

AUDIO-PATH is the path to the audio file to transcribe.
Requires `ok-whisper--openai-token' to be set."
  (unless ok-whisper--openai-token
    (error "No API token available"))
  (let* ((command (format "curl -s https://api.openai.com/v1/audio/transcriptions \
-H \"Authorization: Bearer %s\" \
-H \"Content-Type: multipart/form-data\" \
-F file=\"@%s\" \
-F model=\"whisper-1\""
                          ok-whisper--openai-token
                          audio-path))
         (response (shell-command-to-string command))
         (transcription (cdr (assoc 'text (json-read-from-string response)))))
    (message "Transcription result: %s" transcription)
    (insert transcription)
    (delete-file audio-path)))

(defun ok-whisper--transcribe-file ()
  "Transcribe an existing audio file using OpenAI's Whisper API.
Prompts for an audio file selection and sends it to Whisper for transcription.
The transcription result is inserted at point.
Supports common audio formats like MP3, WAV, and M4A."
  (interactive)
  (let ((audio-file (read-file-name "Select audio file to transcribe: ")))
    (if (file-exists-p audio-file)
        (ok-whisper--call-openai-whisper audio-file)
      (message "Selected file does not exist."))))


(provide 'ok-whisper)
;;; ok-whisper.el ends here
