(defvar taskwarrior-mode-hook nil)

(defvar taskwarrior-buffer-name "*taskwarrior*"
  "Name of the taskwarrior buffer.")

(defun display-tasks ()
  (interactive)
  (let ((shell-output (shell-command-to-string "task"))
        (taskwarrior-buffer (get-buffer-create taskwarrior-buffer-name)))
    (switch-to-buffer taskwarrior-buffer)
    (erase-buffer)
    (insert shell-output)))

(defun add-task (task-description)
  (interactive "sTask description: ")
  (shell-command (format "task add %s" task-description)))

(defun filter-spaces (seq)
  "Filter spaces from seq of strings"
  (delq nil (mapcar (lambda (x) (if (string-equal x " ") nil x)) seq)))

(defun get-task-number (line)
  (let ((tokens (split-string line)))
    (nth 0 (filter-spaces tokens))))

(defun delete-task ()
  (interactive)
  (let ((current-line (thing-at-point 'line)))
    (let ((task-number (get-task-number current-line)))
      (message (format "task %i delete" (string-to-number task-number))))))

(define-minor-mode taskwarrior-mode
  "Taskwarrior mode."
  :keymap '(((kbd "g") . display-tasks)
            ((kbd "a") . add-task)
            ((kbd "d") . delete-task)))

(provide 'taskwarrior-mode)
;;; taskwarrior-mode.el ends here
