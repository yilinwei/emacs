;;; project+.el --- Additional support -*- lexical-binding: t; -*-
;; Package-Requires: (widget project+)
;;; Code:

(require 'widget)
(require 'project+)

;; (require 'use-package)

;; (use-package widget
;;   :functions (widget-create widget-insert))

;; (use-package project+
;;   :functions (project+-known-projects project+-switch-project))

(cl-defun dashboard--widget-projects ()
  "Create a widget for the projects."
  (widget-insert "Projects: \n\n")
  (cl-flet
      ((open (widget &rest ignore)
	     (project-switch-project (widget-value widget))))
      (cl-loop
       for project in (project-known-projects)
       do
       (widget-create 'push-button :notify #'open project)
       (widget-insert "\n"))))

;;;###autoload
(cl-defun dashboard ()
  (interactive)
  (with-current-buffer
      (get-buffer-create "*dashboard*")
    (setq buffer-read-only t)
    (let ((inhibit-read-only t))
      (erase-buffer))
    (dashboard--widget-projects)
    (current-buffer)))

(provide 'dashboard)
