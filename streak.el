;;; streak.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Colin Woodbury
;;
;; Author: Colin Woodbury <https://github.com/fosskers>
;; Maintainer: Colin Woodbury <colin@fosskers.ca>
;; Created: June 18, 2021
;; Modified: June 18, 2021
;; Version: 0.0.1
;; Keywords: Symbol’s value as variable is void: finder-known-keywords
;; Homepage: https://github.com/fosskers/streak
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
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

(defcustom streak-file (substitute-in-file-name "$HOME/.streak")
  "The location to save the start of the current streak."
  :group 'streak
  :type 'file)

;;;###autoload
(defun streak--days-since-unix-epoch ()
  "The number of days since the Unix Epoch."
  (/ (time-convert nil 'integer)
     streak--seconds-per-day))

;;;###autoload
(defun streak--read-streak-start ()
  "Read the starting date of the current streak."
  (unless (file-exists-p streak-file)
    (streak-init))
  (when-let ((buffer (find-file-noselect streak-file)))
    (with-current-buffer buffer
      (let* ((raw (progn
                    (copy-region-as-kill (line-beginning-position) (line-end-position))
                    (current-kill 0)))
             (start (string-to-number raw))
             (today (streak--days-since-unix-epoch))
             (delta (- today start)))
        (if (= 1 delta)
            "1 Day"
          (format "%d Days" delta))))))

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
  (when-let ((buffer (find-file-noselect streak-file)))
    (with-current-buffer buffer
      (message "Streak: Resetting your streak. Restart Emacs to see the effect.")
      (delete-region (point-min) (point-max))
      (insert (streak--tomorrow))
      (save-buffer))))

;;;###autoload
(defun streak--tomorrow ()
  "Tomorrow's date, as days since the Unix epoch."
  (1+ (streak--days-since-unix-epoch)))

;;;###autoload
(defun streak--show-streak-in-modeline ()
  "Show the current streak count in days in the mode line."
  (add-to-list 'global-mode-string '(t (:eval (streak--read-streak-start)))))

(add-hook 'streak-mode-on-hook #'streak--show-streak-in-modeline)

(provide 'streak)
;;; streak.el ends here
