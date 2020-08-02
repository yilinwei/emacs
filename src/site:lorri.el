;; -*- lexical-binding: t; -*-

(defvar site:lorri-buffer
  "*Lorri*")

;;;###autoload
(cl-defun site:lorri-start ()
  "Start the `lorri' daemon."
  (interactive)
  (with-current-buffer (get-buffer-create site:lorri-buffer)
    (setq buffer-read-only t)
    (start-process
     "lorri"
     (current-buffer) "lorri" "daemon")))

(provide 'site:lorri)
