(require 'package)
(require 'cl)
(setq package-enable-at-startup nil)

;; packages
(add-to-list
 'package-archives
 '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list
 'package-archives
 '("melpa" . "https://melpa.org/packages/") t)
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
(menu-bar-mode -1)
(electric-pair-mode t)
(show-paren-mode t)
(setq org-src-fontify-natively t)
(setq org-startup-indented t)

(add-hook 'prog-mode-hook 'linum-mode)
;; CSS mode is not a prog mode
(add-hook 'css-mode-hook 'linum-mode)

(set-face-attribute 'default nil
                    :family "Consolas" :height 120 :weight 'normal)
(blink-cursor-mode 0)

(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'exec-path "~/bin")
(setenv "PATH" (concat (getenv "PATH") ":~/bin"))
(setq ns-use-native-fullscreen nil) ;; Disables OSX bullshit fullscreen
(setq inhibit-startup-message t)
(setq whitespace-line-column 80) ;; limit line length
(setq whitespace-style '(face lines-tail))
(add-hook 'prog-mode-hook 'whitespace-mode)
(add-hook 'term-mode-hook (lambda () (yas-minor-mode -1)))
(setq ruby-insert-encoding-magic-comment nil)
(setq css-indent-offset 2)
(global-hl-line-mode t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq-default fill-column 100)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((sh . t)
   (ruby .t)))

(use-package clj-refactor
  :ensure t
  )

(use-package nyan-mode
  :ensure t
  )

(use-package tex-mode
  :ensure t
  :config
  (add-hook 'LaTeX-mode-hook
            (lambda ()
              (auto-fill-mode)
              (flyspell-mode)
              (turn-on-reftex)
              (TeX-add-style-hook
               "abntex2cite"
               (lambda ()
                 (TeX-add-symbols
                  '("citeonline" TeX-arg-cite)
                  )
                 ))
              ))
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  (setq reftex-plug-into-AUCTeX t)
  (setq font-latex-user-keyword-classes
        '(
          ("autoref" (("autoref" "{")) font-lock-constant-face command)
          ("citeonline" (("citeonline" "{")) font-lock-constant-face command)
          ))
  (setq reftex-external-file-finders
        '(("tex" . "kpsewhich -format=.tex %f")
          ("bib" . "kpsewhich -format=.bib %f"))))

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda() (org-bullets-mode 1))))

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
  :config
  (setq cider-cljs-lein-repl
        "(do (require 'figwheel-sidecar.repl-api)
           (figwheel-sidecar.repl-api/start-figwheel!)
           (figwheel-sidecar.repl-api/cljs-repl))")
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
    (add-to-list 'auto-mode-alist '("\\.hbs\\'" . web-mode))

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

(use-package solarized-theme
  :ensure t
  :config
  (load-theme 'solarized-light t))

(use-package inf-ruby
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
  (setq magit-diff-refine-hunk 'all)
  :config
  (define-key magit-mode-map (kbd "N") 'magit-section-backward)
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

  (setq company-idle-delay 0.4)
  )

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
          "a w" `evil-ace-jump-word-mode
          "a a" 'evil-ace-jump-char-mode
          "w w" 'other-frame
          "w m" 'toggle-maximize-buffer
          "w c" 'delete-other-windows
          "f f" 'helm-find-files
          "f n" 'my-notes-find
          "p f" 'helm-projectile-find-file
          "p p" 'projectile-switch-project
          "p n" 'my-new-project-file
          "p r" 'projectile-run-async-shell-command-in-root
          "f r" 'helm-recentf
          "b b" 'helm-buffers-list
          "b n" 'evil-next-buffer
          "b p" 'evil-prev-buffer
          "s v" 'split-window-horizontally
          "s h" 'split-window-vertically
          "g" 'magit-status
          "/" 'helm-projectile-ag
          "s f" 'helm-imenu

          "q" 'quit-window
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

    (evil-define-key 'normal org-mode-map (kbd "M-k") 'org-metaup)
    (evil-define-key 'normal org-mode-map (kbd "M-j") 'org-metadown)
    (evil-define-key 'normal org-mode-map (kbd "M-h") 'org-metaleft)
    (evil-define-key 'normal org-mode-map (kbd "M-l") 'org-metaright)
    (evil-define-key 'normal global-map (kbd "M-k") 'my-move-lines-up)
    (evil-define-key 'normal global-map (kbd "M-j") 'my-move-lines-down)

    (use-package evil-surround
      :ensure t
      :config
      (global-evil-surround-mode t)
      )
    )
  )

;; Hides white vertical line on splits
(set-face-attribute 'vertical-border
                    nil
                    :foreground (face-attribute 'default :background))

;; Gheto
(defun my-new-project-file (new-file)
  (interactive
   (list (read-file-name "New file: " (projectile-project-root))))
  (find-file new-file)
  )

(defun my-notes-find ()
  (interactive)
  (let ((default-directory "~/Dropbox/org-notes/"))
    (helm-find-files nil)
    )
  )

(defun toggle-maximize-buffer () "Maximize buffer"
       (interactive)
       (if (= 1 (length (window-list)))
           (jump-to-register '_)
         (progn
           (window-configuration-to-register '_)
           (delete-other-windows))))

(defun my-move-lines-up ()
  (interactive)
  (transpose-lines 1)
  (previous-line 2)
  )

(defun my-move-lines-down ()
  (interactive)
  (next-line)
  (transpose-lines 1)
  (previous-line)
  )

(defun align-assignment ()
  (interactive)
  (align-regexp "=")
  )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" default)))
 '(org-agenda-files (quote ("~/Dropbox/org-notes/trustvox.org")))
 '(org-file-apps
   (quote
    ((auto-mode . emacs)
     ("\\.mm\\'" . default)
     ("\\.x?html?\\'" . default)
     ("\\.pdf\\'" . "evince %s"))))
 '(reftex-ref-style-default-list (quote ("Default" "Hyperref")))
 '(safe-local-variable-values (quote ((ispell-dictionary . "pt_BR")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
