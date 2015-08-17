(require 'package)
(require 'cl)
(setq package-enable-at-startup nil)

;; packages
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa-stable" . "http://melpa-stable.milkbox.net/packages/")
                         ("melpa" . "http://melpa.org/packages/")
                         ))
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'diminish)

;;General config
(scroll-bar-mode -1)
(setq ring-bell-function 'ignore)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq make-backup-files nil)
(setq auto-save-default nil)
(require 'recentf)

(recentf-mode 1)
(setq recentf-max-menu-items 25)
(tool-bar-mode -1)
(electric-pair-mode t)
(show-paren-mode t)
(setq org-src-fontify-natively t)
(setq org-startup-indented t)

(add-hook 'prog-mode-hook 'linum-mode)
;; CSS mode is not a prog mode
(add-hook 'css-mode-hook 'linum-mode)

(set-face-attribute 'default nil
                    :family "Consolas" :height 140 :weight 'normal)
(blink-cursor-mode 0)
(setq exec-path (append exec-path '("/usr/local/bin/")))
(setq ns-use-native-fullscreen nil) ;; Disables OSX bullshit fullscreen
(setq inhibit-startup-message t)

(require 'whitespace)
(setq whitespace-line-column 80) ;; limit line length
(setq whitespace-style '(face lines-tail))
(add-hook 'prog-mode-hook 'whitespace-mode)
(add-hook 'term-mode-hook (lambda () (yas-minor-mode -1)))
(setq ruby-insert-encoding-magic-comment nil)
(setq css-indent-offset 2)

(use-package ace-jump-mode
  :ensure t
  )

(use-package diff-hl
  :ensure t
  :config

  (add-hook 'prog-mode-hook 'diff-hl-mode)
  (add-hook 'css-mode-hook 'diff-hl-mode)
  )

(use-package scss-mode
  :ensure t

  :config
  (progn
    (add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode)))
  )

(use-package cider
  :ensure t
  )

(use-package web-mode
  :ensure t
  :config
  (progn
    (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))

    (setq web-mode-markup-indent-offset 2)
    )
  )

(use-package rspec-mode
  :ensure t
  :config
  (defadvice rspec-compile (around rspec-compile-around)
    "Use BASH shell for running the specs because of ZSH issues."
    (let ((shell-file-name "/bin/bash"))
      ad-do-it))
  (ad-activate 'rspec-compile)
  (add-hook 'after-init-hook 'inf-ruby-switch-setup)
  )

(use-package rvm
  :ensure t
  )

(use-package zenburn-theme
  :ensure t
  :config
  (load-theme 'zenburn t)
  )

(use-package inf-ruby
  :ensure t
  )

(use-package restclient
  :ensure t
  )

(use-package coffee-mode
  :ensure t
  )

(use-package evil
  :ensure t
  )

(use-package helm-ag
  :ensure t
  )

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :config
  (yas-global-mode t)
  )

(use-package magit
  :ensure t
  :init
  (setq magit-last-seen-setup-instructions "1.4.0")
  :config
  (define-key magit-mode-map (kbd "N") 'magit-section-backward)
  )

(use-package eyebrowse
  :ensure t
  :config
  (progn
    (eyebrowse-mode t)
    )
  )

(use-package helm
  :ensure t
  :config
  (progn
    (require 'helm-config)
    (setq helm-split-window-in-side-p t)
    (setq helm-recentf-fuzzy-match t)
    (setq helm-buffers-fuzzy-matching t)
    (setq helm-M-x-fuzzy-match t)
    (setq helm-semantic-fuzzy-match t)
    (setq helm-apropos-fuzzy-match t)
    (setq helm-lisp-fuzzy-completion t)
    (define-key helm-map (kbd "C-z") 'helm-select-action)
    (define-key helm-map (kbd "C-c x") 'my-helm-split)
    (define-key helm-map (kbd "C-c v") 'my-helm-vsplit)
    (define-key helm-map (kbd "C-j") 'helm-next-line)
    (define-key helm-map (kbd "C-k") 'helm-previous-line)
    )
  )

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :init
  (setq projectile-completion-system 'helm)
  :config
  (progn
    (projectile-global-mode))
  )

(use-package helm-projectile
  :ensure t
  :init
  (setq helm-projectile-fuzzy-match t)
  :config
  (progn
    (helm-projectile-on)
    )
  )

(use-package company
  :ensure t
  :diminish company-mode
  :config
  (defvar company-mode/enable-yas t
    "Enable yasnippet for all backends.")

  (defun company-mode/backend-with-yas (backend)
    (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
        backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))

  (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))

  (add-hook 'after-init-hook 'global-company-mode)
  (define-key company-active-map (kbd "C-j") 'company-select-next)
  (define-key company-active-map (kbd "C-k") 'company-select-previous)

  (setq company-idle-delay 0)
  )

;; HELM
(defun my-helm-split ()
  (interactive)
  (with-helm-alive-p
    (helm-quit-and-execute-action '(lambda(file)
                                     (split-window)
                                     (find-file file)
                                     ))))
(defun my-helm-vsplit ()
  (interactive)
  (with-helm-alive-p
    (helm-quit-and-execute-action '(lambda(file)
                                     (split-window-horizontally)
                                     (find-file file)
                                     ))))

(use-package evil
  :init
  (progn
    (use-package evil-leader
      :ensure t
      :init (global-evil-leader-mode)
      :config
      (progn
        (evil-leader/set-leader "<SPC>")
        (evil-leader/set-key ":" 'helm-M-x)
        (evil-leader/set-key
          "a" 'ace-jump-mode
          "w w" 'other-frame
          "f f" 'helm-find-files
          "f n" 'my-notes-find
          "p f" 'helm-projectile-find-file
          "p p" 'projectile-switch-project
          "p n" 'my-new-project-file
          "f r" 'helm-recentf
          "b b" 'helm-buffers-list
          "b n" 'evil-next-buffer
          "b p" 'evil-prev-buffer
          "s v" 'split-window-horizontally
          "s h" 'split-window-vertically
          "g" 'magit-status
          "/" 'helm-projectile-ag

          "q" 'quit-window

          "1" 'eyebrowse-switch-to-window-config-1
          "2" 'eyebrowse-switch-to-window-config-2
          "3" 'eyebrowse-switch-to-window-config-3
          "4" 'eyebrowse-switch-to-window-config-4
          "5" 'eyebrowse-switch-to-window-config-5
          "6" 'eyebrowse-switch-to-window-config-6
          "7" 'eyebrowse-switch-to-window-config-7
          "8" 'eyebrowse-switch-to-window-config-8
          "9" 'eyebrowse-switch-to-window-config-9
          )
        (evil-leader/set-key-for-mode 'ruby-mode
          "r r" 'ruby-send-region
          "t f" 'rspec-verify
          "t l" 'rspec-verify-single
          "t t" 'rspec-verify-all
          )
        )
      )
    (evil-mode 1))
  :config
  (progn
    (evil-set-initial-state 'help-mode 'normal)
    (define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
    (define-key evil-visual-state-map (kbd "C-u") 'evil-scroll-up)

    ;; Changing windows
    (define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
    (define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
    (define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
    (define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)

    (define-key evil-normal-state-map (kbd "M-k") 'org-metaup)
    (define-key evil-normal-state-map (kbd "M-j") 'org-metadown)
    (define-key evil-normal-state-map (kbd "M-h") 'org-metaleft)
    (define-key evil-normal-state-map (kbd "M-l") 'org-metaright)

    (define-key evil-normal-state-map (kbd "C-c y") 'company-yasnippet)
    (define-key evil-insert-state-map (kbd "C-c y") 'company-yasnippet)
    (use-package evil-surround
      :ensure t
      :config
      (global-evil-surround-mode t)
      )
    )
  )
;; FIXME: Learn why this has to be on bottom
(toggle-frame-fullscreen)

;; Gheto
(defun my-new-project-file (new_file)
  (interactive
   (list (read-file-name "New file: " (projectile-project-root))))
  (find-file new_file)
  )

(defun my-notes-find ()
  (interactive)
  (let ((default-directory "~/Dropbox/org-notes/"))
    (helm-find-files nil)
    )
  )
