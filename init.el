
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
  :ensure t
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

(use-package diminish
  :ensure t) ;gets rid of lighters with use-package :diminish

(use-package delight
  :ensure t
  :config
  (delight `((buffer-face-mode nil "face-remap"))))

(use-package yascroll
  :ensure t
  :config
  (global-yascroll-bar-mode 1)
  (setq yascroll:delay-to-hide nil))

(use-package magit
  :ensure t)

(use-package evil
  :ensure t
  :diminish
  :init
  (setq evil-search-module "evil-search")
  (setq evil-cross-lines t)
  (setq evil-want-integration t) ;; recommended for evil-collection
  (setq evil-want-keybinding nil) ;; required for evil-collection
  (setq evil-want-fine-undo t)
  (setq evil-undo-system 'undo-fu)
  :config
  (evil-mode 1)
  (global-set-key (kbd "C-<tab>") 'evil-window-next)
  (global-set-key (kbd "<C-iso-lefttab>") 'evil-window-prev))

;; subsumed by evil-collection
;; (use-package evil-magit
;;   :after magit evil)

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1)
  (setq evil-surround-pairs-alist (delete  '(40 . ("( " . " )")) evil-surround-pairs-alist))
  (setq evil-surround-pairs-alist (delete  '(91 . ("[ " . " ]")) evil-surround-pairs-alist))
  (setq evil-surround-pairs-alist (delete  '(123 . ("{ " . " }")) evil-surround-pairs-alist))
  (push  '(40 . ("(" . ")")) evil-surround-pairs-alist)
  (push  '(91 . ("[" . "]")) evil-surround-pairs-alist)
  (push  '(123 . ("{" . "}")) evil-surround-pairs-alist))

(use-package undo-fu
  :ensure t
  :config
  ;(global-undo-tree-mode -1)
  (define-key evil-normal-state-map (kbd "C-/") 'undo-fu-only-undo)
  (define-key evil-normal-state-map (kbd "C-?") 'undo-fu-only-redo))

(use-package emacs
  :config 
  (load-theme 'zenburnt t)
  (set-frame-font "DejaVu Sans Mono 11" nil t)
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (setq mouse-wheel-scroll-amount '(1))
  ;;(fringe-mode nil)
  ;; (global-visual-line-mode)
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
  (setq dired-listing-switches "-alh")
)

(use-package dired-du
  :ensure t)

(use-package vterm
  :ensure t)

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
                                ("\\.ods\\'" "soffice" (file))
				("\\.xopp\\'" "xournalpp" (file)))))

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
  (global-set-key (kbd "s-b") 'ivy-switch-buffer)
  (global-set-key (kbd "s-g") 'counsel-ag)
  (global-set-key (kbd "C-s-b") 'ivy-switch-buffer-other-window)
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
  (setq ivy-count-format "(%d/%d) ")
  (add-to-list 'ivy-re-builders-alist '(counsel-org-goto . ivy--regex-fuzzy)))

(use-package counsel
  :after openwith
  :ensure t
  :config
  (setq counsel-switch-buffer-preview-virtual-buffers nil)
  ;; (advice-add 'openwith-file-handler :around #'ben/ignore-errors)

  ;; (defun ben/ignore-errors (&rest r)
  ;;   (ignore-errors (apply (car r) (cdr r))))
  
  (defun ben/file-fzf (dir)
    (let ((counsel-fzf-cmd "fd --hidden --type f --color never | fzf -f \"%s\"")
	  (ben/fzf-dir-or-not nil)) ;result is a dir and needs a / at end
      (counsel-fzf nil dir)))

  (defun ben/dir-fzf (dir)
    (let ((counsel-fzf-cmd "fd --hidden --type d --color never | fzf -f \"%s\"")
	  (ben/fzf-dir-or-not t)) ;result is a file and needs the file name stripped
    (counsel-fzf nil dir)))

  (global-set-key (kbd "s-C-,") #'(lambda () (interactive)
				    (setq ben/base-dir "~/")
				    (ben/dir-fzf "~/")))
  (global-set-key (kbd "s-C-.") #'(lambda () (interactive)
				    (setq ben/base-dir default-directory)
				    (ben/dir-fzf default-directory)))
  (global-set-key (kbd "s-C-/") #'(lambda () (interactive)
				    (setq ben/base-dir "/")
				    (ben/dir-fzf "/")))
  (global-set-key (kbd "s-,") #'(lambda () (interactive)
				  (setq ben/base-dir "~/")
				  (ben/file-fzf "~/")))
  (global-set-key (kbd "s-.") #'(lambda () (interactive)
				  (setq ben/base-dir default-directory)
				  (ben/file-fzf default-directory)))
  (global-set-key (kbd "s-/") #'(lambda () (interactive)
				  (setq ben/base-dir "/")
				  (ben/file-fzf "/")))

  (defun ben/ag (arg)
    "Grep in this directory"
    (let* ((ben/root (if (file-name-absolute-p arg)
			arg
		       (expand-file-name arg ben/base-dir)))
	   (ben/dir (if ben/fzf-dir-or-not
			 ben/root
		       (expand-file-name (concat (file-name-as-directory ben/root) "..")))))
      (counsel-ag nil ben/dir nil (concat "grep in " ben/dir ":"))));initial input, directory, prompt
  
  ;; This did not work, and was much slower than fzf.
  ;; (defun ben/file-jump (dir)
  ;;   (let ((ben/fzf-dir-or-not nil))
  ;;     (counsel-file-jump nil dir)))
  
  ;; (defun ben/dir-jump (dir)
  ;;   (let ((ben/fzf-dir-or-not t))
  ;;     (counsel-dired-jump nil dir)))
  
  ;; (global-set-key (kbd "s-C-,") #'(lambda () (interactive)
  ;; 				    (ben/dir-jump "~/")
  ;; 				    (setq ben/base-dir "~/")))
  ;; (global-set-key (kbd "s-C-.") #'(lambda () (interactive)
  ;; 				    (ben/dir-jump default-directory)
  ;; 				    (setq ben/base-dir default-directory)))
  ;; (global-set-key (kbd "s-C-/") #'(lambda () (interactive)
  ;; 				    (ben/dir-jump "/")
  ;; 				    (setq ben/base-dir "/")))
  ;; (global-set-key (kbd "s-,") #'(lambda () (interactive)
  ;; 				  (ben/file-jump "~/")
  ;; 				  (setq ben/base-dir "~/")))
  ;; (global-set-key (kbd "s-.") #'(lambda () (interactive)
  ;; 				  (ben/file-jump default-directory)
  ;; 				  (setq ben/base-dir default-directory)))
  ;; (global-set-key (kbd "s-/") #'(lambda () (interactive)
  ;; 				  (ben/file-jump "/")
  ;; 				  (setq ben/base-dir "/")))
  
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
  :ensure t
  :config
  (ivy-rich-mode 1))

(use-package prescient
  :ensure t
  :after (counsel))

(use-package ivy-prescient
  :ensure t
  :after (counsel)
  :config
  (ivy-prescient-mode))

(use-package which-key
  :ensure t
  :diminish
  :config
  (which-key-mode))

(use-package eldoc
  :ensure t
  :diminish)

(use-package doct
  :ensure t
  :commands (doct))

(use-package org
  :config
  (global-set-key (kbd "C-c a") 'org-agenda)
  (global-set-key (kbd "C-c l") 'org-store-link)
  (global-set-key (kbd "C-c c") 'org-capture)
  (setq org-goto-interface 'outline-path-completion)
  (setq org-outline-path-complete-in-steps nil)
  (define-key org-mode-map (kbd "s-j") 'org-next-visible-heading)
  (define-key org-mode-map (kbd "s-k") 'org-previous-visible-heading)
  (define-key org-mode-map (kbd "s-h") 'outline-up-heading)
  (define-key org-mode-map (kbd "s-m") 'counsel-outline)
  (define-key org-mode-map (kbd "M-j") 'org-metadown)
  (define-key org-mode-map (kbd "M-k") 'org-metaup)
  (define-key org-mode-map (kbd "M-l") 'org-metaright)
  (define-key org-mode-map (kbd "M-h") 'org-metaleft)
  (define-key org-mode-map (kbd "<s-tab>") 'org-cycle)
  (define-key org-mode-map (kbd "<C-tab>") nil)
  (define-key org-mode-map (kbd "<M-return>") `sbr-org-insert-dwim)
  (setq org-startup-indented t)
  (setq org-startup-truncated nil)
  (setq org-cycle-emulate-tab nil)
  (setq org-M-RET-may-split-line nil)
  (setq org-src-tab-acts-natively t)
  (setq org-directory "~/org")
  (setq org-default-notes-file (concat org-directory "/gtd.org"))
  (defun sbr-org-insert-dwim (&optional arg)
  "Insert another entry of the same type as the current
entry. For example, if the point is on a list item, then add
another list item of the same type, and if the point is on a
checkbox list item, then add an empty checkbox item. If instead
the point is in a heading, then add another heading. If the point 
is in a TODO heading, then add another TODO heading (set to the 
TODO state). 

By default, the new entry is inserted below the current
subtree/item. With a 'C-u' prefix, insert the entry above the
current heading/item instead. Taken from https://www.reddit.com/r/orgmode/comments/boyu8r/function_for_dwim_insertion_of_new_entries/"
  (interactive "P")
  (when (eq major-mode 'org-mode)
    (let ((org-special-ctrl-a/e t)
	  (below? (unless  (equal arg '(4)) '(4))))
      ;; hack to ensure that the point is not after ellipses because
      ;; that would mess up org-at-item-p etc.
      (org-beginning-of-line)
      (cond ((org-at-item-p) ;; at list item or checkbox
	     (let ((org-M-RET-may-split-line nil)
		   (org-enable-sort-checkbox nil))
	       ;; hack to make item be inserted after the current one
	       ;; doesn't work if we are on an empty item line
	       (when below?
		 (org-end-of-line))                     
	       (org-insert-item (org-at-item-checkbox-p))))
	    ((org-before-first-heading-p) ;; above first heading
	     (org-insert-heading))
	    (t ;; in some kind of heading
	     (org-back-to-heading)
	     (if (org-get-todo-state)
		 ;; at TODO heading
		 (org-insert-todo-heading t below?)
	       ;; at non-TODO heading 
	       (org-insert-heading below?)))))))

  (defun sbr-org-shift-return (&optional arg)
    "If point is at a table, copy the table cell downward (i.e.,
the usual effect of typing S-RET). Otherwise,  insert the same
kind of heading or item as the current entry containing the
point. "
    (interactive "P")
    (if (org-at-table-p)
	(org-table-copy-down (prefix-numeric-value arg))
      (sbr-org-insert-dwim arg)))
  
  (setq org-agenda-skip-scheduled-if-done t
        org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "SOMEDAY(s)" "CANCELLED(c)"))
        org-agenda-files '("~/org/inbox.org" "~/org/gtd.org"))
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
                ;("Tickler" :keys "t"
                ; :type entry
                ; :file "~/org/tickler.org"
                ; :headline "Tickler"
                ; :template "* %i%? \n %U"))))
  (setq org-refile-targets '(("~/org/gtd.org" :maxlevel . 4)))
;                           ("~/org/someday.org" :level . 1)
 ;                          ("~/org/tickler.org" :maxlevel . 2)))
  (setq process-connection-type t))

(use-package ess-r-mode
  :mode ("\\.r\\'" . ess-r-mode))

(use-package latex
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (add-hook 'LaTeX-mode-hook (lambda ()
			       (yas-minor-mode 1)
			       (flyspell-mode)
			       (TeX-source-correlate-mode)
			       (setq-default TeX-engine 'xetex
					     TeX-PDF-mode t)
			       (setq TeX-source-correlate-start-server t)
			       (add-to-list 'TeX-view-program-selection
                                            '(output-pdf "Zathura"))
			       (setq TeX-electric-sub-and-superscript nil)
			       (setq +latex-indent-level-item-continuation 2))))

(use-package evil-tex
  :ensure t
  :config
  (add-hook `LaTeX-mode-hook #'evil-tex-mode))

(use-package yasnippet
  :ensure t
  :config
  (setq yas-snippet-dirs '("~/.my-emacs.d/snippets"))
  (yas-reload-all)
  (setq yas-triggers-in-field t))

(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" . gfm-mode)
  :commands (markdown-mode gfm-mode)
  :config
  (setq markdown-command "pandoc -t html5"))

;; TODO learn how to configure lsp-mode properly!
(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deffered)
  :hook (lsp-mode . lsp-enable-which-key-integration))

(use-package lsp-ivy
  :ensure t
  :after lsp-mode)

(use-package lsp-pyright
  :ensure t
  :hook
  (python-mode . (lambda ()
                   (require 'lsp-pyright)
                   (lsp-deferred))))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(evil-collection lsp-pyright lsp-ivy lsp-mode vterm dired-du yascroll good-scroll markdown-mode yasnippet ess which-key use-package undo-fu rg rainbow-delimiters persp-mode openwith ivy-rich ivy-prescient hc-zenburn-theme expand-region evil-tex evil-surround evil-magit diminish delight counsel avy))
 '(safe-local-variable-values
   '((eval when
	   (require 'rainbow-mode nil t)
	   (rainbow-mode 1)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
