;;; -*- lexical-binding: t; -*-
;;; melpa-mirror-packages.el --- Entry point
;;; Commentary:
;;; Code:

(require 'use-package)

(use-package dash
  :functions (-repeat -reject))

;; TODO: as

(defun lispy-brackets-switch ()
  (interactive)
  (save-excursion
    (thing-at-point--beginning-of-sexp)
    (let*
	((lb (char-before))
	 (is-round (char-equal ?\( lb)))
      (delete-char -1)
      (insert (if is-round ?\[ ?\())
      (thing-at-point--end-of-sexp)
      (delete-char 1)
      (insert (if is-round ?\] ?\))))))

(defgroup site
  nil
  "Group for site customization.")

(defface site:font-lock-todo-face
  '((default . (:inherit font-lock-comment-face
			 :underline t)))
  "TODO face."
  :group 'site)

(defconst site:lisp-todo-keyword
  '(";; \\(TODO\\):" 1 'site:font-lock-todo-face prepend))

(font-lock-add-keywords 'emacs-lisp-mode
			    `(,site:lisp-todo-keyword))

(setq use-package-verbose t)

(use-package diminish
  :commands (diminish))

(use-package undo-tree
  :diminish (undo-tree-mode)
  :commands (undo-tree))

(use-package evil
  :commands
  (evil-define-minor-key evil-insert evil-delay evil-define-key)
  :init (setq
	 evil-want-C-u-scroll t
	 evil-want-integration t
	 evil-want-keybinding nil))

(use-package evil-collection
  :after evil
  :commands
  (evil-collection-dired-setup
   evil-collection-eshell-setup
   evil-collection-magit-setup
   evil-collection-buff-menu-setup
   evil-collection-proced-setup))

(use-package ivy
  :diminish ivy-mode
  :commands (ivy-completing-read)
  :config
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-count-format "(%d/%d) ")
  :bind
  ("C-c C-r" . 'ivy-resume)
  :hook
  ((magit-mode . ivy-mode)))

(use-package counsel
  :after ivy
  :bind
  (("\C-s" . 'counsel-grep-or-swiper)
   ("C-x C-f" . 'counsel-find-file)
   ("C-h f" . 'counsel-describe-function)
   ("C-h v" . 'counsel-describe-variable)
   ("C-h l" . 'counsel-find-library)
   ("C-x d" . 'counsel-dired)
   ("M-i" . 'counsel-imenu)
   ("M-x" . 'counsel-M-x)
   ("C-x b" . 'counsel-switch-buffer)))

(use-package swiper
  :after ivy)

(use-package magit
    :bind (("C-c g s" . magit-status)
	   ("C-c g b" . magit-blame))
    :config
    (evil-collection-magit-setup))

(use-package evil-magit
  :after (magit evil))

(use-package direnv)

;;; Helper functions
(defun comment-or-uncomment-line ()
  "Call `comment-or-uncomment-region' with `line-beginning-position' 
and `line-end-position'."
  (interactive)
  (comment-or-uncomment-region
   (line-beginning-position) (line-end-position)))

(define-minor-mode code-mode
  "Common bindings for coding.
\\{code-mode-map}"
  :keymap (make-sparse-keymap)
  (display-line-numbers-mode))

(evil-define-key 'normal code-mode-map
  (kbd "C-c c c") 'comment-or-uncomment-line)

(evil-define-key 'visual code-mode-map
  (kbd "C-c c c") 'comment-or-uncomment-region)

;;; Customize emacs-lisp
(add-hook 'emacs-lisp-mode-hook 'electric-pair-mode)
(add-hook 'emacs-lisp-mode-hook 'code-mode)
(add-hook 'emacs-lisp-mode-hook 'show-paren-mode)

(use-package dired
  :config
  (evil-collection-dired-setup)
  (setq dired-dwim-target t))

;; since buff-menu doesn't provide anything 
(eval-after-load 'buff-menu
  (evil-collection-buff-menu-setup))

(use-package proced
  :config
  (evil-collection-proced-setup))

;; Needed for nix mode
(use-package irony
  :defer t)

(use-package ffap
  :commands (ffap)
  :defines ffap-c-path)

(use-package woman
  :commands (woman)
  :defines woman-manpath)

(use-package nix-shell
  :commands (nix-eshell nix-eshell-with-packages)
  :config
  (progn
    (require 'woman)
    (require 'irony)))

(use-package nix-mode
  :after nix-shell
  :commands (nix-repl nix-shell)
  :mode "\\.nix\\'")

(use-package rackunit
  :commands (rackunit-test))

(use-package racket-mode
  :interpreter "racket"
  :commands racket-run
  :mode "\\.rkt\\'"
  :bind (("C-c c t" . rackunit-test))
  :config
  (progn
    (evil-define-key 'normal racket-describe-mode-map
      (kbd "q") 'quit-window)
    (font-lock-add-keywords 'racket-mode
			    `(,site:lisp-todo-keyword))
    (evil-define-key 'normal racket-mode-map
      (kbd "C-c c r") 'racket-run)
    (add-hook 'racket-mode-hook 'code-mode)
    (add-hook 'racket-mode-hook 'show-paren-mode)
    (use-package racket-xp
      :commands (racket-xp-mode)
      :hook ((racket-mode . racket-xp-mode))
      :bind (("C-c r r" . 'racket-send-region)
	     ("C-c r d" . 'racket-send-definition)
	     ("C-c r s" . 'racket-send-last-sexp)))))

(use-package lispyville
  :diminish lispyville-mode
  :commands lispyville-set-key-theme
  :config (lispyville-set-key-theme
	   '(operators
	     c-w
	     wrap
	     mark
	     additional
	     text-objects
	     atom-motions
	     slurp/barf-lispy))
  :hook
  ((emacs-lisp-mode . lispyville-mode)
   (racket-mode . lispyville-mode))
  :config
  (progn
    (evil-define-key 'visual lispyville-mode-map
      (kbd "C-c c c") 'lispyville-comment-or-uncomment)
    (evil-define-key 'normal lispyville-mode-map
      (kbd "C-c c c") 'lispyville-comment-or-uncomment-line)))

(use-package avy
  :bind
  ("C-:" . avy-goto-char)
  ("C-'" . avy-goto-char-2))

(use-package org-agenda
  :bind
  (("C-c o a" . org-agenda)
   :map org-agenda-mode-map
   ("j" . org-agenda-previous-line)
   ("k" . org-agenda-next-line)))

(use-package yasnippet
  :commands (yas-minor-mode yas-reload-all)
  :hook
  ((python-mode . yas-minor-mode)
   (racket-mode . yas-minor-mode))
  :bind
  :config
  (progn
   (yas-reload-all)
   (use-package ivy-yasnippet
     :after (ivy yasnippet)
     :commands (ivy-yasnippet)
     :bind
     (:map yas-minor-mode-map
	   ("C-M-j" . ivy-yasnippet)))))

;; (use-package undo-tree
;;   :diminish undo-tree-mode
;;   :commands (undo-tree-mode))

(use-package haskell-mode
  :mode ("\\.hs\\'" . haskell-mode)
  :bind
  (:map haskell-mode-map
	("C-c c b" . haskell-compile)))

(use-package org
  :bind
  ("C-c o l" . org-store-link)
  ("C-c o t" . org-clock-goto)
  :config
  (progn
    (setq org-todo-keywords
	  '((sequence "TODO" "FEEDBACK" "|" "DONE" "DELEGATED")
	    (sequence "CANCELED"))
	  org-pretty-entities t)
    (use-package org-variable-pitch
      :diminish (org-variable-pitch-minor-mode)
      :hook ((org-mode . org-variable-pitch-minor-mode))
      :config
      (progn
	(setq org-hide-emphasis-markers t
	      org-hide-leading-stars t
	      org-variable-pitch-fixed-faces
	      (append
	       '(org-superstar-leading)
	       (-reject
		(lambda (%) (memq %
				  '(org-special-keyword org-todo org-done org-indent)))
		(default-value 'org-variable-pitch-fixed-faces))))))
    (use-package org-superstar
      :hook ((org-mode . org-superstar-mode))
      :config
      (progn
	(setq org-superstar-headline-bullets-list
	      (-repeat 5
		       ;; Zero-width space
		       65279))))))

(defun company--set-mode-backends (mode-hook backends)
  "Set company BACKENDS for MODE-HOOK."
  (let
      ((cb (lambda ()
	     (setq-local company-backends backends))))
    (add-hook mode-hook cb)))

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :config
  (add-hook 'python-mode-hook 'code-mode))

(use-package anaconda-mode
  :hook python-mode
  :commands (anaconda-mode-find-definitions)
  :config
  (evil-define-key 'normal anaconda-mode-map
    (kbd "gd") 'anaconda-mode-find-definitions))

(use-package company-anaconda
  :commands (company-anaconda)
  :after (anaconda-mode company-mode))

(use-package scala-mode
  :mode "\\.scala\\'"
  :config
  (add-hook 'scala-mode-hook 'code-mode))

(use-package company
  :defines (company-backends)
  :diminish company-mode
  :bind
  ((:map code-mode-map
	 ("C-M-i" . company-complete)))
  :init
  (progn
    (company--set-mode-backends 'emacs-lisp-mode-hook '(company-capf company-files))
    (company--set-mode-backends 'anaconda-mode-hook '(company-anaconda)))
  :hook
  ((emacs-lisp-mode . company-mode)
   (anaconda-mode . company-mode)
   (racket-xp-mode . company-mode)))

(use-package typescript-mode
  :mode "\\.ts\\'")

;;TODO: get this building better
(use-package mu4e
  :commands (mu4e)
  :config
  (progn
    (evil-collection-mu4e-setup)
    (setq mu4e-completing-read-function 'ivy-completing-read)
    (setq mail-user-agent 'mu4e-user-agent)))

(use-package rainbow-delimiters
  :hook
  ((emacs-lisp-mode . rainbow-delimiters-mode)
   (racket-mode . rainbow-delimiters-mode)))

(provide 'melpa-mirror-packages)
