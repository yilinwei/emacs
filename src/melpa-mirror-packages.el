;;; -*- lexical-binding: t; -*-
;;; melpa-mirror-packages.el --- Entry point
;;; Commentary:
;;; Code:

(require 'use-package)

(use-package dash
  :functions (-repeat -reject --mapcat -split-at))

(use-package s
  :functions (s-concat))

(defun comment-or-uncomment-line ()
  "Call `comment-or-uncomment-region' with `line-beginning-position' 
and `line-end-position'."
  (interactive)
  (comment-or-uncomment-region
   (line-beginning-position) (line-end-position)))

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

(defface font-lock-todo-face
  '((default . (:inherit font-lock-comment-face
			 :underline t)))
  "TODO face.")

(defconst lisp-todo-keyword
  '(";; \\(TODO\\):" 1 'font-lock-todo-face prepend))

(font-lock-add-keywords 'emacs-lisp-mode
			`(,lisp-todo-keyword))

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
	 evil-want-keybinding nil)
  :bind
  ("C-SPC" . universal-argument))

(use-package evil-collection
  :after evil
  :commands
  (evil-collection-dired-setup
   evil-collection-eshell-setup
   evil-collection-magit-setup
   evil-collection-buff-menu-setup
   evil-collection-proced-setup))

(defmacro evil-define-key/prefix
    (state keymap prefix key def &rest bindings)
  (let* ((step 2)
	 (pairs (cons
		 (list key def)
		 (-partition-all-in-steps step step bindings)))
	 (body
	 (--mapcat
	  `((kbd ,(s-concat prefix " " (car it))) ,(cadr it))
	  pairs)))
    `(evil-define-key ,state ,keymap
       ,@body)))

(use-package evil-surround
  :commands global-evil-surround-mode
  :after evil
  :config
  (global-evil-surround-mode 1))

(define-minor-mode code-mode
  "Common bindings for coding.
\\{code-mode-map}"
  :keymap (make-sparse-keymap)
  (display-line-numbers-mode))

(evil-define-key/prefix 'normal code-mode-map "C-c c"
			"c" 'comment-or-uncomment-line)

(evil-define-key/prefix 'visual code-mode-map "C-c c"
			"c" 'comment-or-uncomment-region)



(use-package ivy
  :diminish ivy-mode
  :commands (ivy-completing-read)
  :config
  (setq ivy-use-virtual-buffers t
	enable-recursive-minibuffers t
	ivy-count-format "(%d/%d) "
	completing-read-function #'ivy-completing-read)
  :bind
  ("C-c C-r" . 'ivy-resume)
  :hook
  ((magit-mode . ivy-mode)))

(use-package counsel
  :after ivy
  :config
  (progn
    (evil-define-key/prefix 'normal 'global "C-h"
			    "f" 'counsel-describe-function
			    "v" 'counsel-describe-variable
			    "l" 'counsel-find-library)
    (evil-define-key/prefix 'normal 'global "C-x"
			    "C-f" 'counsel-find-file
			    "d" 'counsel-dired
			    "b" 'counsel-switch-buffer)
    (evil-define-key 'normal 'global
      (kbd "C-M-i") 'counsel-imenu)
    (evil-define-key 'normal 'global
      (kbd "C-s") 'counsel-grep-or-swiper)
    (evil-define-key 'normal 'global
      (kbd "M-x") 'counsel-M-x)))

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
  :bind
  ("C-x C-f" . ffap))

(use-package woman
   :commands (woman)
   :defines woman-manpath)

(use-package nix-shell
  :commands (nix-eshell
	     nix-eshell-with-packages)
  :config
  (progn
    (require 'woman)
    (require 'irony)
    (require 'ffap)))

(use-package nix-mode
  :after nix-shell
  :commands (nix-repl nix-shell)
  :mode "\\.nix\\'")

(use-package yaml-mode
  :mode "\\.yml\\'")

(use-package racket-mode
  :interpreter "racket"
  :commands racket-run
  :mode "\\.rkt\\'"
  :bind (("C-c c t" . racket-test))
  :config
  (progn
    (evil-define-key 'normal racket-describe-mode-map
      (kbd "q") 'quit-window)
    (font-lock-add-keywords 'racket-mode
			    `(,lisp-todo-keyword))
    (evil-define-key 'normal racket-mode-map
      (kbd "C-c c r") 'racket-run)
    (add-hook 'racket-mode-hook 'code-mode)
    (add-hook 'racket-mode-hook 'show-paren-mode)
    (add-hook 'racket-mode-hook 'whitespace-cleanup)
    (use-package racket-xp
      :commands (racket-xp-mode)
      :hook ((racket-mode . racket-xp-mode))
      :bind (("C-c r r" . 'racket-send-region)
	     ("C-c r d" . 'racket-send-definition)
	     ("C-c r s" . 'racket-send-last-sexp))
      :config
      (progn
	(evil-define-key 'normal racket-xp-mode-map
	  (kbd "gd") 'racket-xp-visit-definition)))))

(use-package lispy
  :defer t
  :config
  (progn
    (add-to-list
     'lispy-colon-no-space-regex
     '((racket-mode . "."))))
  :hook
  ((emacs-lisp-mode . lispy-mode)
   (racket-mode . lispy-mode)))

(use-package lispyville
  :diminish lispyville-mode
  :commands lispyville-set-key-theme
  :config (progn
	    (lispyville-set-key-theme
	     '(operators
	       c-w
	       escape
	       mark-toggle
	       c-u
	       prettify
	       wrap
	       additional
	       additional-insert
	       additional-motions
	       text-objects
	       atom-motions
	       slurp/barf-lispy))
	    (setq lispyville-motions-put-into-special t)
	    (lispy-define-key lispy-mode-map "v" #'lispyville-toggle-mark-type))
  :hook
  ((lispy-mode . lispyville-mode))
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
   ("j" . org-agenda-next-line)
   ("k" . org-agenda-previous-line)))

(use-package yasnippet
  :commands (yas-minor-mode yas-reload-all yas-expand)
  :hook
  ((python-mode . yas-minor-mode)
   (racket-mode . yas-minor-mode))
  :bind
  (:map yas-minor-mode-map
	("C-M-j" . yas-expand/evil)
	("TAB" . nil))
  :config
  (yas-reload-all))

(defun yas-expand/evil (&rest args)
  "Expand in insert-mode. See `yas-expand'."
  (interactive)
  (cond
   ((or (evil-visual-state-p)
	(lispyville--lispy-keybindings-active-p))
    (progn
      (evil-insert 0)
      (apply #'yas-insert-snippet args)))
   ((evil-insert-state-p)
    (apply #'yas-expand args))))

;; (use-package undo-tree
;;   :diminish undo-tree-mode
;;   :commands (undo-tree-mode))

(use-package haskell-mode
  :mode ("\\.hs\\'" . haskell-mode)
  :bind
  (:map haskell-mode-map
	("C-c c b" . haskell-compile)))

(remove-hook 'kill-emacs-hook #'org-clock-out)

(use-package org
  :commands (org-clock-in-last
	     org-clock-out)
  :bind
  ("C-c o l" . org-store-link)
  ("C-c o t" . org-clock-goto)
  :hook ((kill-emacs . (lambda ()
			 (org-clock-out nil t))))
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

(use-package sbt-mode
  :commands sbt-start sbt-command)

(use-package scala-mode
  :mode "\\.scala\\'"
  :config
  (add-hook 'scala-mode-hook 'code-mode))

(use-package company
  :defines (company-backends)
  :commands (company-complete) 
  :diminish company-mode
  :config
  (progn
    (evil-define-key 'insert code-mode-map
      (kbd "C-M-i") . 'company-complete))
  :init
  (progn
    (company--set-mode-backends 'emacs-lisp-mode-hook '(company-capf company-files))
    (company--set-mode-backends 'anaconda-mode-hook '(company-anaconda)))
  :hook
  ((emacs-lisp-mode . company-mode)
   (anaconda-mode . company-mode)
   (racket-xp-mode . company-mode)))

(use-package rust-mode
  :mode "\\.rs\\'"
  :config
  (progn
    (add-hook 'rust-mode-hook 'code-mode)
    (add-hook 'rust-mode-hook 'electric-pair-mode)))

(use-package cargo
  :commands (cargo-process-check)
  :hook
  ((rust-mode . cargo-minor-mode))
  :config
  (progn
    (evil-define-key/prefix 'normal cargo-minor-mode-map "C-c c"
			    "b" 'cargo-process-check
			    "t" 'cargo-process-test)))

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

(use-package elpher)

(use-package rainbow-delimiters
  :hook
  ((emacs-lisp-mode . rainbow-delimiters-mode)
   (racket-mode . rainbow-delimiters-mode)))

(provide 'melpa-mirror-packages)
