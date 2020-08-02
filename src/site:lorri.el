;; -*- lexical-binding: t; -*-

(defvar site:lorri-command
  "lorri")

(defvar site:lorri-buffer
  "*Lorri*")

;;;###autoload
(cl-defun site:lorri-start ()
  "Start the lorri daemon."
  (interactive)
  (with-current-buffer (get-buffer-create site:lorri-buffer)
    (setq buffer-read-only t)
    (start-process
     "lorri"
     (current-buffer) site:lorri-command "daemon")))

;;;###autoload
(cl-defun site:lorri-init ()
  "Initialize the current project using lorri."
  (interactive)
  (when (and (fboundp 'project-current) (fboundp 'direnv-allow))
    (let
	((default-directory (cdr (project-current))))
      (start-process "lorri" nil site:lorri-command "init")
      (direnv-allow))))

(provide 'site:lorri)
