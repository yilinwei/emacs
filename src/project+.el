;;; -*- lexical-binding: t; -*-

(require 'nix-shell)

(defvar project-bookmarks
  (concat user-emacs-directory ".project-bookmarks.el"))

(cl-defun project-known-projects ()
  "Get known projects from `project-bookmarks'."
  (with-temp-buffer
    (insert-file-contents project-bookmarks)
    (goto-char (point-min))
    (read (current-buffer))))

(cl-defun project-switch-project (dir)
  "Switch to the project with the root DIR."
  (interactive
   (list
    (completing-read
     "Project: "
     (project-known-projects))))
  (let
      ((project (project--find-in-directory dir)))
    (project-find-file-in nil (project-roots project) project)))

(cl-defun project--current-root ()
  (car (project-roots (project-current t))))

(cl-defun project-start-nix-shell (prefix)
  "Start an `eshell' in the current project. 

Using it with a PREFIX argument starts the shell in the `default-directory'"
  (interactive "P")
  (let*
      ((root (project--current-root))
       (default-directory
	 (or
	  (and prefix
	       default-directory)
	   root)))
    (nix-eshell
     (expand-file-name
      (concat root "shell.nix")))))

(cl-defun project--bookmarks-write (bookmarks)
  (with-temp-buffer
    (prin1 bookmarks (current-buffer))
    (write-file project-bookmarks)))

(cl-defun project-bookmarks-add ()
  "Add the current project to bookmarks."
  (interactive)
  (let
      ((ps (project-known-projects))
       (p (project--current-root)))
    (unless (member p ps)
      (project--bookmarks-write
       (cons p ps)))))

(cl-defun project-bookmarks-remove ()
  "Remove the current project from bookmarks."
  (interactive)
  (let
      ((ps (project-known-projects))
       (p (project--current-root)))
    (when (member p (project-known-projects))
      (project--bookmarks-write
       (remove p ps)))))

;;;###autoload
(defvar project-command-map
  (let
      ((map (make-sparse-keymap)))
    (define-key map (kbd "p") #'project-switch-project)
    (define-key map (kbd "f") #'project-find-file)
    (define-key map (kbd "s") #'project-start-nix-shell)
    map)
  "Useful map")

;;;###autoload
(define-minor-mode
  project+-mode
  "Extra functions build on top of project.el."
  :global t)

(provide 'project+)
