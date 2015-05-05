;;; taskwarrior.el --- Taskwarrior client for Emacs

;; Copyright (C) 2015  Andrey Lisin <andrey.lisin@gmail.com>

;; Author: Andrey Lisin <andrey.lisin@gmail.com>
;; Keywords: taskwarrior
;; Version: 0.0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Rule taskwarrior from Emacs
;;
;; Have a nice day!

;;; Code:

(defvar taskwarrior-mode-hook nil)

(defvar taskwarrior-buffer-name "*taskwarrior*"
  "Name of the taskwarrior buffer.")

(defvar taskwarrior-map (make-sparse-keymap)
  "The keymap to use with taskwarrior")

(if taskwarrior-map
    (progn
      (define-key taskwarrior-map (kbd "g") 'taskwarrior)
      (define-key taskwarrior-map (kbd "a") 'taskwarrior-add-task)
      (define-key taskwarrior-map (kbd "d") 'taskwarrior-done-task)))

(defun display-tasks ()
  (let ((shell-output (shell-command-to-string "task"))
        (taskwarrior-buffer (get-buffer-create taskwarrior-buffer-name)))
    (switch-to-buffer taskwarrior-buffer)
    (erase-buffer)
    (insert shell-output)))

(defun taskwarrior-add-task (task-description)
  "Add taskwarrior task."
  (interactive "sTask description: ")
  (shell-command (format "task add %s" task-description))
  (display-tasks))

(defun filter-spaces (seq)
  "Filter spaces from seq of strings"
  (delq nil (mapcar (lambda (x) (if (string-equal x " ") nil x)) seq)))

(defun get-task-number (line)
  (let ((tokens (split-string line)))
    (nth 0 (filter-spaces tokens))))

(defun delete-task ()
  (interactive)
  (let* ((current-line (thing-at-point 'line))
         (task-number (get-task-number current-line)))
      (message (format "task %i delete" (string-to-number task-number)))))

(defun taskwarrior-done-task ()
  "Mark taskwarrior task as done."
  (interactive)
  (let* ((current-line (thing-at-point 'line))
         (task-number (get-task-number current-line)))
    (shell-command (format "task %i done" (string-to-number task-number)))
  (display-tasks)))

(defun taskwarrior ()
  "The entry point for taskwarrior client."
  (interactive)
  (display-tasks)
  (use-local-map taskwarrior-map)
  )

(define-minor-mode taskwarrior-mode
  "Taskwarrior mode."
  :keymap '(((kbd "g") . display-tasks)
            ((kbd "a") . add-task)
            ((kbd "d") . done-task)))

(provide 'taskwarrior)
;;; taskwarrior.el ends here
