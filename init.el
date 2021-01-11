
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(add-to-list 'custom-theme-load-path "~/.my-emacs.d/themes")

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Ben Smith"
      user-mail-address "bensmithmath@gmail.com")

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))

(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(defun highlight-selected-window ()
  "Highlight selected window with a different background color."
  (walk-windows (lambda (w)
                  (unless (eq w (selected-window))
                    (with-current-buffer (window-buffer w)
                      (buffer-face-set '(:background "#373737"))))))
  (buffer-face-set 'default))
(add-hook 'buffer-list-update-hook 'highlight-selected-window)

(use-package diminish) ;gets rid of lighters with use-package :diminish

(use-package delight
  :config
  (delight `((buffer-face-mode nil "face-remap"))))


(use-package magit)

(use-package evil
  :ensure t
  :diminish
  :init
  (setq evil-search-module "evil-search")
  (setq evil-cross-lines t)
  (setq evil-respect-visual-line-mode t)
  (setq evil-want-fine-undo t)
  (setq evil-undo-system 'undo-tree)
  :config
  (evil-mode 1)
  (global-set-key (kbd "C-<tab>") 'evil-window-next)
  (global-set-key (kbd "<C-iso-lefttab>") 'evil-window-prev))

(use-package evil-magit
  :after magit evil)
(use-package evil-surround
  :config
  (global-evil-surround-mode 1)
  (setq evil-surround-pairs-alist (delete  '(40 . ("( " . " )")) evil-surround-pairs-alist))
  (setq evil-surround-pairs-alist (delete  '(91 . ("[ " . " ]")) evil-surround-pairs-alist))
  (setq evil-surround-pairs-alist (delete  '(123 . ("{ " . " }")) evil-surround-pairs-alist))
  (push  '(40 . ("(" . ")")) evil-surround-pairs-alist)
  (push  '(91 . ("[" . "]")) evil-surround-pairs-alist)
  (push  '(123 . ("{" . "}")) evil-surround-pairs-alist))

(use-package undo-tree
  :ensure t
  :diminish
  :config
  (global-undo-tree-mode))

(use-package emacs
  :config 
  (load-theme 'zenburnt t)
  (set-frame-font "Fira Code 12" nil t)
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (global-visual-line-mode)
  (setq visible-bell t)
  (setq inhibit-splash-screen t)
;  (setq meta-prefix-char nil) ;stops ESC from being weird ; breaks ALT completely!
  (show-paren-mode)
  (global-set-key (kbd "s-s") 'save-buffer)
  (global-set-key (kbd "C-s-f") 'find-file-other-window)
  (global-set-key (kbd "s-e") 'eval-last-sexp)
  (global-set-key (kbd "s-d") 'kill-buffer)
  (global-set-key (kbd "s-0") 'delete-window)
  (global-set-key (kbd "s-1") 'delete-other-windows)
  (global-set-key (kbd "s-2") 'split-window-below)
  (global-set-key (kbd "s-3") 'split-window-right)
  (global-set-key (kbd "s-4") 'ctl-x-4-prefix)
  ;(set-scroll-bar-mode 'right) ; use this after getting theme/face right
)

(use-package avy
  :ensure t
  :config
  (setq avy-all-windows t)
  (evil-global-set-key 'normal (kbd "u") 'avy-goto-char-timer) ;reserved for avy
  (evil-global-set-key 'motion (kbd "u") 'avy-goto-char-timer)
  (evil-global-set-key 'visual (kbd "u") 'avy-goto-char-timer)
  (evil-global-set-key 'operator (kbd "u") 'avy-goto-char-timer))

(use-package expand-region
  :ensure t
  :config
  (global-set-key (kbd "M-e") 'er/expand-region) 
  (global-set-key (kbd "C-M-e") 'er/contract-region)) 

(use-package openwith
  :ensure t
  :config
  (openwith-mode t)
  (setq openwith-associations '(("\\.pdf\\'" "zathura" (file))
                                ("\\.xlsx\\'" "soffice" (file))
                                ("\\.docx\\'" "soffice" (file))
                                ("\\.pptx\\'" "soffice" (file))
                                ("\\.csv\\'" "soffice" (file))
                                ("\\.ods\\'" "soffice" (file)))))

(use-package rg
  :config
  (rg-enable-default-bindings))

(use-package ivy
  :ensure t
  :diminish
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "s-f") 'counsel-find-file)
  (global-set-key (kbd "s-r") 'ivy-resume)
  (global-set-key (kbd "s-o") 'ivy-occur)
  (global-set-key (kbd "s-f") 'counsel-find-file)
  (global-set-key (kbd "s-b") 'counsel-switch-buffer)
  (global-set-key (kbd "s-g") 'counsel-ag)
  (global-set-key (kbd "C-s-b") 'counsel-switch-buffer-other-window)
  (global-set-key (kbd "C-h f") 'counsel-describe-function)
  (global-set-key (kbd "C-h v") 'counsel-describe-variable)
  (define-key ivy-minibuffer-map (kbd "<C-return>") 'ivy-call)
  (define-key ivy-minibuffer-map (kbd "<M-return>") 'ivy-dispatching-done)
  (define-key ivy-minibuffer-map (kbd "<C-M-return>") 'ivy-dispatching-call)
  (define-key ivy-minibuffer-map (kbd "C-M-j") 'ivy-next-line-and-call)
  (define-key ivy-minibuffer-map (kbd "C-M-k") 'ivy-previous-line-and-call)
  (define-key ivy-minibuffer-map (kbd "C-c") 'ivy-immediate-done)
  (define-key ivy-switch-buffer-map (kbd "C-k") 'ivy-switch-buffer-kill)
  (setq ivy-height 15)
  (setq ivy-fixed-height-minibuffer t)
  (setq ivy-magic-slash-non-match-action nil)
  (setq ivy-virtual-abbreviate 'full)
;  (setf (alist-get 't ivy-format-functions-alist)
        ;#'ivy-format-function-arrow)
  (setq ivy-count-format "(%d/%d) "))

(use-package counsel
  :after openwith
  :ensure t
  :config

  (defun ben/file-fzf (dir)
    (let ((counsel-fzf-cmd "fd --hidden --type f --color never | fzf -f \"%s\"")
	  (ben/fzf-dir-or-not t)) ;result is a dir and needs a / at end
      (counsel-fzf nil dir)))

  (defun ben/dir-fzf (dir)
    (let ((counsel-fzf-cmd "fd --hidden --type d --color never | fzf -f \"%s\"")
	  (ben/fzf-dir-or-not nil)) ;result is a file and needs the file name stripped
    (counsel-fzf nil dir)))

  (global-set-key (kbd "s-C-,") #'(lambda () (interactive)
				    (ben/dir-fzf "~/")
				    (setq ben/base-dir "~/")))
  (global-set-key (kbd "s-C-.") #'(lambda () (interactive)
				    (ben/dir-fzf default-directory)
				    (setq ben/base-dir default-directory)))
  (global-set-key (kbd "s-C-/") #'(lambda () (interactive)
				    (ben/dir-fzf "/")
				    (setq ben/base-dir "/")))
  (global-set-key (kbd "s-,") #'(lambda () (interactive)
				  (ben/file-fzf "~/")
				  (setq ben/base-dir "~/")))
  (global-set-key (kbd "s-.") #'(lambda () (interactive)
				  (ben/file-fzf default-directory)
				  (setq ben/base-dir default-directory)))
  (global-set-key (kbd "s-/") #'(lambda () (interactive)
				  (ben/file-fzf "/")
				  (setq ben/base-dir "/")))
  (defun ben/xournalpp (arg)
    "Open file in xournalpp"
    (openwith-open-unix "xournalpp" (list (if (file-name-absolute-p arg)
                                            arg
                                          (expand-file-name arg ben/base-dir)))))

  (defun ben/zathura (arg)
    "open file in zathura"
    (openwith-open-unix "zathura" (list (if (file-name-absolute-p arg)
					    arg
					  (expand-file-name arg ben/base-dir)))))

  (defun ben/dropbox-link (arg)
      "Get public dropbox link to file, and copy it to clipboard"
    (with-ivy-window
      (shell-command (concat "dropbox-cli sharelink \""
                                   (if (file-name-absolute-p arg)
                                       arg
                                     (expand-file-name arg ben/base-dir))
                                   "\" | xclip -selection clipboard &> /dev/null"))))

  ;; Pass functions as appropriate Ivy actions (accessed via M-o)
  (ivy-add-actions
   'counsel-fzf
   '(("b" ben/dropbox-link "copy public dropbox link")
     ("a" ben/xournalpp "xournal++")
     ("z" ben/zathura "zathura")
     ("g" ben/ag "grep this directory")))

  (ivy-add-actions
   'counsel-find-file
   '(("b" ben/dropbox-link "copy public dropbox link")
     ("a" ben/xournalpp "xournal++")
     ("z" ben/zathura "zathura")
     ("g" ben/ag "grep this directory"))))

(use-package ivy-rich
  :config
  (ivy-rich-mode 1))

(use-package prescient
  :after (counsel))

(use-package ivy-prescient
  :after (counsel)
  :config
  (ivy-prescient-mode))

(use-package which-key
  :ensure t
  :diminish
  :config
  (which-key-mode))

(use-package eldoc
  :diminish)

(use-package doct
  :commands (doct))

(use-package org
  :config
  (global-set-key (kbd "C-c a") 'org-agenda)
  (global-set-key (kbd "C-c l") 'org-store-link)
  (global-set-key (kbd "C-c c") 'org-capture)
  (define-key org-mode-map (kbd "s-j") 'org-next-visible-heading)
  (define-key org-mode-map (kbd "s-k") 'org-previous-visibile-heading)
  (define-key org-mode-map (kbd "s-h") 'outline-up-heading)
  (define-key org-mode-map (kbd "<s-tab>") 'org-cycle)
  (setq org-startup-indented t)
  (setq org-cycle-emulate-tab nil)
  (setq org-src-tab-acts-natively t)
  (setq org-directory "~/org/")
  (setq org-agenda-skip-scheduled-if-done t
        org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "SOMEDAY(s)" "CANCELLED(c)"))
        org-agenda-files '("~/org/inbox.org" "~/org/gtd.org" "~/org/tickler.org"))
  ;(setq org-capture-templates
  ;      (doct `(("Binding" :keys "b"
  ;               :type entry
  ;               :file "~/org/bindings.org"
  ;               :function ,(defun +org-capture-heading-from-major-mode ()
  ;                            (let* ((buffer (org-capture-get :original-buffer))
  ;                                   (mm (with-current-buffer buffer (symbol-name major-mode))))
  ;                              (if-let ((marker (org-find-exact-headline-in-buffer mm)))
  ;                                  (goto-char marker)
  ;                                (goto-char (point-max))
  ;                                (insert "* " mm))))
  ;               :template "* %?")
  ;              ("Todo" :keys "i"
  ;               :type entry
  ;               :file "~/org/inbox.org"
  ;               :headline "Tasks"
  ;               :template "* %i%?")
  ;              ("Tickler" :keys "t"
  ;               :type entry
  ;               :file "~/org/tickler.org"
  ;               :headline "Tickler"
  ;               :template "* %i%? \n %U"))))
  (setq org-refile-targets '(("~/org/gtd.org" :maxlevel . 3)
                           ("~/org/someday.org" :level . 1)
                           ("~/org/tickler.org" :maxlevel . 2)))
  (setq process-connection-type nil))

(use-package tex
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (add-hook 'LaTeX-mode-hook (lambda ()
			       (flyspell-mode)
			       (TeX-source-correlate-mode)
			       (setq TeX-source-correlate-start-server t)
			       (add-to-list 'TeX-view-program-selection
                                            '(output-pdf "Zathura"))
			       (setq TeX-electric-sub-and-superscript nil)
			       (setq +latex-indent-level-item-continuation 2))))

(use-package evil-tex
  :config
  (add-hook `LaTeX-mode-hook #'evil-tex-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(evil-magit evil-tex delight auctex magit rg rainbow-delimiters diminish persp-mode ivy-rich ivy-prescient prescient undo-tree which-key counsel openwith ivy expand-region avy use-package hc-zenburn-theme evil-surround)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
