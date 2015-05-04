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

(defun delete-task (task-number)
  (shell-command (format "task %i delete" task-number)))

(define-minor-mode taskwarrior-mode
  "Taskwarrior mode."
  :keymap '(((kbd "g") . display-tasks)
            ((kbd "a") . add-task)))

(provide 'taskwarrior-mode)
;;; taskwarrior-mode.el ends here
