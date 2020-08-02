;;; -*- lexical-binding: t; -*-

(require 'ivy)
(require 'nix-shell)

(defvar site:project-bookmarks
  (concat user-emacs-directory ".project-bookmarks.el"))

(cl-defun site:project-known-projects ()
  "Get known projects from `site:project-bookmarks'."
  (with-temp-buffer
    (insert-file-contents site:project-bookmarks)
    (goto-char (point-min))
    (read (current-buffer))))

(cl-defun site:project-find-file-in (filename dirs project)
  "See `project-find-file-in'."
  (cl-letf
      (((symbol-function 'completing-read) #'ivy-completing-read))
    (project-find-file-in filename dirs project)))

(cl-defun site:project-switch-project (dir)
  "Switch to the project with the root DIR."
  (interactive
   (list
    (ivy-completing-read
     "Project: "
     (site:project-known-projects))))
  (let
      ((project (project--find-in-directory dir)))
    (project+-find-file-in nil (project-roots project) project)))

(cl-defun site:project-find-file ()
  "See `project-find-file'."
  (interactive)
  (cl-letf
      (((symbol-function 'completing-read) #'ivy-completing-read))
    (project-find-file)))

(cl-defun site:project-start-nix-shell ()
  "Start an `eshell' in the current project."
  (interactive)
  (let
      ((dir (car (project-roots (project-current t)))))
    (nix-eshell (expand-file-name (concat dir "shell.nix")))))

;;;###autoload
(defvar site:project-command-map
  (let
      ((map (make-sparse-keymap)))
    (define-key map (kbd "p") #'site:project-switch-project)
    (define-key map (kbd "f") #'site:project-find-file)
    (define-key map (kbd "s") #'site:project-start-nix-shell)
    map)
  "Useful map")

;;;###autoload
(define-minor-mode
  site:project-mode
  "Extra functions build on top of project.el."
  :global t)

(provide 'site:project)
