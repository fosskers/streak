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
;; Package-Requires: ((emacs "24.3"))
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
(defun streak--days-since-unix-epoch ()
  "The number of days since the Unix Epoch."
  (/ (time-convert nil 'integer)
     streak--seconds-per-day))

;;;###autoload
(defun streak-current ()
  "Read the streak file for the current streak."
  (unless (file-exists-p streak-file)
    (streak-init))
  (when-let ((buffer (find-file-noselect streak-file))
             (start (string-to-number (streak--buffer-first-line buffer))))
    (streak--render start)))

;;;###autoload
(defun streak--render (start)
  "Give a human-friendly presentation of the streak, given its START."
  (let* ((today (streak--days-since-unix-epoch))
         (delta (- today start)))
    (cond ((< delta 0) "Streak Starts Tomorrow")
          ((= 1 delta) "1 Day")
          (t (format "%d Days" delta)))))

;;;###autoload
(defun streak--buffer-first-line (buffer)
  "Yield the first line of a BUFFER as a string."
  (with-current-buffer buffer
    (goto-char (point-min))
    (buffer-substring-no-properties (line-beginning-position) (line-end-position))))

;;;###autoload
(defun streak-init ()
  "Mark today as the start of a new streak."
  (interactive)
  (when-let ((buffer (find-file-noselect streak-file))
             (today (streak--days-since-unix-epoch)))
    (with-current-buffer buffer
      (message "Streak: Setting your first streak!")
      (delete-region (point-min) (point-max))
      (insert (format "%d\n" today))
      (save-buffer))))

;;;###autoload
(defun streak-reset ()
  "Reset the streak back to 0."
  (interactive)
  (when-let ((buffer (find-file-noselect streak-file))
             (tomorrow (streak--tomorrow)))
    (with-current-buffer buffer
      (message "Streak: Resetting your streak. Restart Emacs to see the effect.")
      (delete-region (point-min) (point-max))
      (insert (format "%d\n" tomorrow))
      (save-buffer)
      (setq streak--streak-message (streak--render tomorrow)))))

;;;###autoload
(defun streak--tomorrow ()
  "Tomorrow's date, as days since the Unix epoch."
  (1+ (streak--days-since-unix-epoch)))

;;;###autoload
(defun streak--show-streak-in-modeline ()
  "Show the current streak count in days in the mode line."
  (unless streak--streak-message
    (setq streak--streak-message (streak-current)))
  (add-to-list 'global-mode-string '(t streak--streak-message)))

(add-hook 'streak-mode-on-hook #'streak--show-streak-in-modeline)

(provide 'streak)
;;; streak.el ends here
