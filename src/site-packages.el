;;; -*- lexical-binding: t; -*-
;;; melpa-mirror-packages.el --- Entry point
;;; Commentary:
;;; Code:

(require 'use-package)

(use-package dash
  :functions (--map))

(use-package s :defer t)

(use-package diminish :defer t)

(defun add-hooks (hook function &rest functions)
  "Add to the value of HOOK the function FUNCTION and each function in FUNCTIONS."
  (--map
   (add-hook hook it)
   (cons function functions)))

(defface font-lock-todo-face
  '((default . (:inherit font-lock-comment-face
			 :underline t)))
  "Face for TODO comments.")

(defconst lisp-todo-keyword
  '(";; \\(TODO\\):" 1 'font-lock-todo-face prepend))

(use-package fandango :defer t)

(use-package evil
  :commands
  (evil-define-minor-key evil-insert evil-delay evil-define-key)
  :init (setq
	 evil-want-C-u-scroll t
	 evil-want-integration t
	 evil-want-keybinding nil)
  :bind
  ("C-SPC" . universal-argument))

(use-package evil-surround
  :commands global-evil-surround-mode
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-collection
  :after evil
  :commands
  (evil-collection-dired-setup
   evil-collection-eshell-setup
   evil-collection-magit-setup
   evil-collection-buff-menu-setup
   evil-collection-proced-setup))

(use-package dired
  :config
  (progn
    (evil-collection-dired-setup)
    (setq dired-dwim-target t)))

;; since buff-menu doesn't provide anything 
(eval-after-load 'buff-menu
  (evil-collection-buff-menu-setup))

(use-package proced
  :config
  (evil-collection-proced-setup))

(use-package ivy
  :diminish ivy-mode
  :commands ivy-completing-read
  :config
  (setq ivy-use-virtual-buffers t
	enable-recursive-minibuffers t
	ivy-count-format "(%d/%d) "
	completing-read-function #'ivy-completing-read)
  :bind ("C-c C-r" . 'ivy-resume)
  :hook
  ((magit-mode . ivy-mode)))

(use-package counsel
  :after ivy
  :config
  (fandango global
	     :prefix counsel
	     :normal
	     ("C-h" . (("f" . -describe-function)
		       ("v" . -describe-variable)
		       ("l" . -find-library)))
	     ("C-x" . (("f" . -find-file)
		       ("d" . -dired)
		       ("b" . -switch-buffer)))
	     ("C-M-i" . -imenu)
	     ("C-s" . -grep-or-swiper)
	     ("M-x" . -M-x)))

(use-package swiper
  :after ivy)

(use-package display-line-numbers
  :hook
  ((display-line-numbers-mode . emacs-lisp-mode)))

(use-package flymake
  :config
  (fandango flymake-diagnostics-buffer-mode
	    :prefix flymake
	    :normal
	    ("q" . quit-window)
	    ("RET" . -goto-diagnostic))
  (fandango flymake
	    :prefix flymake-goto
	    ("C-c e" . (("n" . -next-error)
		      ("p" . -prev-error))))
  :hook
  ((flymake-mode . emacs-lisp-mode)
   (flymake-mode . python-mode)))

(use-package avy
  :config
  (fandango global
	    :prefix avy
	    :normal
	    ("C-:" . -goto-char)
	    ("C-'" . -goto-char-2)))

(use-package lispyville
   :diminish lispyville-mode
   :commands lispyville-set-key-theme
   :config 
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
   (lispy-define-key lispy-mode-map "v" #'lispyville-toggle-mark-type)
   :hook
   ((lispy-mode . lispyville-mode))
   :config
   (fandango lispyville-mode
	     :prefix lispyville
	     :normal
	     ("C-c c" . -comment-or-uncomment-line)
	     :visual
	     ("C-c c" . -comment-or-uncomment)))

(use-package magit
    :bind (("C-c g s" . magit-status)
	   ("C-c g b" . magit-blame))
    :config
    (evil-collection-magit-setup))

(use-package lispy
  :defer t
  :hook
  ((emacs-lisp-mode . lispy-mode)))

(use-package rainbow-delimiters
  :defer t
  :config
  (set-face-attribute 'rainbow-delimiters-base-face
		      nil :weight 'semi-light)
  :hook
  ((emacs-lisp-mode . rainbow-delimiters-mode)))

(use-package hideshow
  :functions hs-hide-all hs-show-all hs-toggle-hiding
  :config
  (fandango hs-minor-mode
	    :prefix hs
	    :normal
	    ("C-c h" . (("e" . -toggle-hiding)
		      ("a" . -hide-or-show-all)))
	    :all
	    ("C-c @" . nil)))

(defun hs-hide-or-show-all (hidep)
  (interactive "P")
  (if hidep
      (hs-hide-all)
    (hs-show-all)))

(use-package direnv
  :commands (direnv-mode))


(use-package elpher :defer t)
(use-package irony :defer t)

(use-package ffap
  :commands (ffap)
  :bind
  ("C-x C-f" . ffap))

(use-package woman
   :commands woman
   :defines woman-manpath)

(use-package nix-shell
  :commands nix-eshell nix-eshell-with-packages
  :config
  (progn
    (require 'woman)
    (require 'irony)
    (require 'ffap)))

(use-package nix-mode
  :commands (nix-repl)
  :config
  (fandango nix-mode
	    :prefix comment-or-uncomment
	    :normal
	    ("c c" . -line)
	    :visual
	    ("c c" . -region))
  :mode "\\.nix\\'")

(use-package elec-pair
  :commands electric-pair-local-mode
  :hook
  ((electric-pair-local-mode . python-mode)
   (electric-pair-local-mode . scala-mode)))

(use-package yaml-mode
  :mode "\\.yml\\'"
  :config
  (fandango yaml-mode
	    :prefix comment-or-uncomment
	    :normal
	    ("C-c c" . -line)
	    :visual
	    ("C-c c" . -region)))

(defun comment-or-uncomment-line ()
  "Call `comment-or-uncomment-region' with `line-beginning-position'
and `line-end-position'."
  (interactive)
  (comment-or-uncomment-region
   (line-beginning-position) (line-end-position)))

(use-package company
  :defines company-backends
  :commands (company-complete) 
  :diminish company-mode
  :config
  (fandango company-mode
	    :prefix company
	    :insert
	    ("C-M-i" . -complete))
  :hook
  ((emacs-lisp-mode . company-mode)))

(use-package yasnippet
  :commands yas-minor-mode yas-reload-all yas-expand
  :hook
  ((emacs-lisp-mode . yas-minor-mode))
  :config
  (fandango yas-minor-mode
	    :prefix yas
	    :all
	    ("C-M-j" . -expand/evil)
	    ("TAB" . nil)))

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

(use-package racket-mode
  :interpreter "racket"
  :commands racket-run
  :mode "\\.rkt\\'")

(use-package python
   :mode ("\\.py\\'" . python-mode)
   :commands run-python
   :interpreter ("python" . python-mode))

(use-package undo-tree
  :diminish undo-tree-mode
  :commands undo-tree-mode)

(use-package proced
  :config
  (evil-collection-proced-setup))


(use-package sbt-mode
  :commands sbt-start sbt-command run-scala)

(use-package scala-mode
  :mode "\\.scala'")


(setq-mode-local 'emacs-lisp-mode
		 company-backends '(company-capf company-files))

(add-hook 'before-save-hook #'whitespace-cleanup)


;; (use-package scala-mode
;;   :mode "\\.scala\\'"
;;   :config
;;   (add-hook 'scala-mode-hook 'code-mode))

;; (use-package racket-mode
;;   :interpreter "racket"
;;   :commands racket-run
;;   :mode "\\.rkt\\'"
;;   :bind (("C-c c t" . racket-test))
;;   :config
;;   (progn
;;     (evil-define-key 'normal racket-describe-mode-map
;;       (kbd "q") 'quit-window)
;;     (font-lock-add-keywords 'racket-mode
;; 			    `(,lisp-todo-keyword))
;;     (evil-define-key 'normal racket-mode-map
;;       (kbd "C-c c r") 'racket-run)
;;     (add-hook 'racket-mode-hook 'code-mode)
;;     (add-hook 'racket-mode-hook 'show-paren-mode)
;;     (add-hook 'racket-mode-hook 'whitespace-cleanup)
;;     (use-package racket-xp
;;       :commands (racket-xp-mode)
;;       :hook ((racket-mode . racket-xp-mode))
;;       :bind (("C-c r r" . 'racket-send-region)
;; 	     ("C-c r d" . 'racket-send-definition)
;; 	     ("C-c r s" . 'racket-send-last-sexp))
;;       :config
;;       (progn
;; 	(evil-define-key 'normal racket-xp-mode-map
;; 	  (kbd "gd") 'racket-xp-visit-definition)))))


;; (use-package org-agenda
;;   :bind
;;   (("C-c o a" . org-agenda)
;;    :map org-agenda-mode-map
;;    ("j" . org-agenda-next-line)
;;    ("k" . org-agenda-previous-line)))


;; (use-package haskell-mode
;;   :mode ("\\.hs\\'" . haskell-mode)
;;   :bind
;;   (:map haskell-mode-map
;; 	("C-c c b" . haskell-compile)))

;; (remove-hook 'kill-emacs-hook #'org-clock-out)

;; (use-package org
;;   :commands (org-clock-in-last
;; 	     org-clock-out)
;;   :bind
;;   ("C-c o l" . org-store-link)
;;   ("C-c o t" . org-clock-goto)
;;   :hook ((kill-emacs . (lambda ()
;; 			 (org-clock-out nil t))))
;;   :config
;;   (progn
;;     (setq org-todo-keywords
;; 	  '((sequence "TODO" "FEEDBACK" "|" "DONE" "DELEGATED")
;; 	    (sequence "CANCELED"))
;; 	  org-pretty-entities t)
;;     (use-package org-variable-pitch
;;       :diminish (org-variable-pitch-minor-mode)
;;       :hook ((org-mode . org-variable-pitch-minor-mode))
;;       :config
;;       (progn
;; 	(setq org-hide-emphasis-markers t
;; 	      org-hide-leading-stars t
;; 	      org-variable-pitch-fixed-faces
;; 	      (append
;; 	       '(org-superstar-leading)
;; 	       (-reject
;; 		(lambda (%) (memq %
;; 				  '(org-special-keyword org-todo org-done org-indent)))
;; 		(default-value 'org-variable-pitch-fixed-faces))))))
;;     (use-package org-superstar
;;       :hook ((org-mode . org-superstar-mode))
;;       :config
;;       (progn
;; 	(setq org-superstar-headline-bullets-list
;; 	      (-repeat 5
;; 		       ;; Zero-width space
;; 		       65279))))))

;; (defun company--set-mode-backends (mode-hook backends)
;;   "Set company BACKENDS for MODE-HOOK."
;;   (let
;;       ((cb (lambda ()
;; 	     (setq-local company-backends backends))))
;;     (add-hook mode-hook cb)))

;; (use-package python
;;   :mode ("\\.py\\'" . python-mode)
;;   :interpreter ("python" . python-mode)
;;   :config
;;   (add-hook 'python-mode-hook 'code-mode))

;; (use-package anaconda-mode
;;   :hook python-mode
;;   :commands (anaconda-mode-find-definitions)
;;   :config
;;   (evil-define-key 'normal anaconda-mode-map
;;     (kbd "gd") 'anaconda-mode-find-definitions))

;; (use-package company-anaconda
;;   :commands (company-anaconda)
;;   :after (anaconda-mode company-mode))


;; (use-package rust-mode
;;   :mode "\\.rs\\'"
;;   :config
;;   (progn
;;     (add-hook 'rust-mode-hook 'code-mode)
;;     (add-hook 'rust-mode-hook 'electric-pair-mode)))

;; (use-package cargo
;;   :commands (cargo-process-check)
;;   :hook
;;   ((rust-mode . cargo-minor-mode))
;;   :config
;;   (progn
;;     (evil-define-key/prefix 'normal cargo-minor-mode-map "C-c c"
;; 			    "b" 'cargo-process-check
;; 			    "t" 'cargo-process-test)))

;; (use-package typescript-mode
;;   :mode "\\.ts\\'")

(provide 'remote-packages)
