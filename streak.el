;;; streak.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Colin Woodbury
;;
;; Author: Colin Woodbury <https://www.fosskers.ca>
;; Maintainer: Colin Woodbury <colin@fosskers.ca>
;; Created: June 18, 2021
;; Modified: June 18, 2021
;; Version: 1.0.0
;; Keywords: streak, tracking, record
;; Homepage: https://github.com/fosskers/streak
;; Package-Requires: ((emacs "27.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; `streak-mode' is a minor mode for tracking some daily streak.
;; Exercising? Learning a language? Track your success in your mode line!
;;
;;; Code:

(define-minor-mode streak-mode
  "Display the number of days of a successful streak in the mode line."
  :init-value nil
  :global t
  :group 'streak
  :lighter " Streak")

(defvar streak--seconds-per-day 86400
  "The number of seconds in a day.")

(defvar streak--streak-message nil
  "String representation of the current streak.")

(defcustom streak-file (substitute-in-file-name "$HOME/.cache/streak")
  "The location to save the start of the current streak."
  :group 'streak
  :type 'file)

;;;###autoload
(defun streak--seconds-since-unix-epoch ()
  "The number of seconds since the Unix Epoch."
  (time-convert nil 'integer))

;;;###autoload
(defun streak--current ()
  "Read the streak file for the current streak."
  (streak--render (streak--current-int)))

;;;###autoload
(defun streak--current-int ()
  "Read the streak file for the current streak."
  (if (not (file-exists-p streak-file))
      (streak--init)
    (when-let ((buffer (find-file-noselect streak-file)))
      (string-to-number (streak--buffer-first-line buffer)))))

;;;###autoload
(defun streak--render (start)
  "Give a human-friendly presentation of the streak, given its START."
  (let* ((now (streak--seconds-since-unix-epoch))
         (delta (/ (- now start) streak--seconds-per-day)))
    (cond ((< delta 0) " Streak Starts Tomorrow ")
          ((= 1 delta) " 1 Day ")
          (t (format " %d Days " delta)))))

;;;###autoload
(defun streak--buffer-first-line (buffer)
  "Yield the first line of a BUFFER as a string."
  (with-current-buffer buffer
    (goto-char (point-min))
    (buffer-substring-no-properties (line-beginning-position) (line-end-position))))

;;;###autoload
(defun streak--init ()
  "Initialize the streak file but don't set the mode line.
Returns the time that was set."
  (let ((now (streak--seconds-since-unix-epoch)))
    (message "Streak: Setting your streak start to the current time.")
    (streak-set now)
    now))

;;;###autoload
(defun streak-reset ()
  "Mark the current time as the start of a new streak."
  (interactive)
  (let ((now (streak--init)))
    (setq streak--streak-message (streak--render now))))

;;;###autoload
(defun streak-set (seconds)
  "Set the streak in the streak file, given SECONDS since the Unix epoch."
  (interactive "nSeconds since the Unix Epoch: ")
  (when-let ((buffer (find-file-noselect streak-file))
             (date (format-time-string "%Y-%m-%d %H:%M" seconds (current-time-zone))))
    (with-current-buffer buffer
      (goto-char (point-min))
      (insert (format "%d ;; %s\n" seconds date))
      (save-buffer))))

;;;###autoload
(defun streak-increment ()
  "Move the streak start back by 1 day, increasing the overall streak days."
  (interactive)
  (let* ((start (streak--current-int))
         (new (- start streak--seconds-per-day)))
    (streak-set new)
    (setq streak--streak-message (streak--render new))))

;;;###autoload
(defun streak-decrement ()
  "Move the streak start up by 1 day, decreasing the overall streak days."
  (interactive)
  (let* ((start (streak--current-int))
         (new (+ start streak--seconds-per-day)))
    (streak-set new)
    (setq streak--streak-message (streak--render new))))

;;;###autoload
(defun streak-update ()
  "Reread the streak file and update the mode line."
  (interactive)
  (setq streak--streak-message (streak--current)))

;;;###autoload
(defun streak--tomorrow ()
  "A time tomorrow, as seconds since the Unix epoch."
  (+ streak--seconds-per-day (streak--seconds-since-unix-epoch)))

;;;###autoload
(defun streak--show-streak-in-modeline ()
  "Show the current streak count in days in the mode line."
  (unless streak--streak-message
    (streak-update))
  (add-to-list 'global-mode-string '(t streak--streak-message)))

(add-hook 'streak-mode-on-hook #'streak--show-streak-in-modeline)

(provide 'streak)
;;; streak.el ends here
