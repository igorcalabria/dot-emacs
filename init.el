(require 'package)
(require 'cl)
(setq package-enable-at-startup nil)

;; packages
(add-to-list
 'package-archives
 '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

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

(setq org-agenda-files '("~/org-notes/projects.org.gpg"
                        "~/org-notes/study.org.gpg"))
(setq org-refile-targets '((org-agenda-files :maxlevel . 2)
                           ("~/org-notes/snippets.org.gpg" :maxlevel . 1)))
(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-allow-creating-parent-nodes 'confirm)
(setq org-edit-src-content-indentation 0
      org-src-tab-acts-natively t
      org-src-fontify-natively t
      org-confirm-babel-evaluate nil)
(org-babel-do-load-languages 'org-babel-load-languages
    '((shell . t)))

(add-hook 'prog-mode-hook 'linum-mode)
;; CSS mode is not a prog mode
(add-hook 'css-mode-hook 'linum-mode)

(set-face-attribute 'default nil
                    :family "DejaVuSansMono" :height 110 :weight 'normal)
(blink-cursor-mode 0)

(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'exec-path "~/bin")
(setenv "PATH" (concat (getenv "PATH") ":~/bin"))
(setq ns-use-native-fullscreen nil) ;; Disables OSX bullshit fullscreen
(setq inhibit-startup-message t)
(setq whitespace-line-column 80) ;; limit line length
(setq whitespace-style '(face lines-tail))
(add-hook 'prog-mode-hook 'whitespace-mode)
(add-hook 'term-mode-hook (lambda ()
                            (yas-minor-mode -1)
                            (setq fill-column 0)
                            (define-key term-raw-map (kbd "C-c C-k") 'toggle-term-mode)
                            (define-key term-mode-map (kbd "C-r") 'my-term-search)
                            (define-key term-raw-map (kbd "C-r") 'my-term-search)
                            (define-key term-mode-map (kbd "C-c C-k") 'toggle-term-mode)))
(setq ruby-insert-encoding-magic-comment nil)
(setq css-indent-offset 2)
(global-hl-line-mode t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq-default fill-column 100)

(add-hook 'org-mode-hook (lambda ()
                           (auto-fill-mode)
                           (org-indent-mode)))

(setq-default ispell-dictionary "american")
(add-hook 'text-mode-hook 'flyspell-mode)

(use-package org-download
  :ensure t
  :config
  (setq-default org-download-image-dir "~/org-notes/pictures"))

(use-package diminish
  :ensure t)

(use-package markdown-mode
  :ensure t
  :config
  (add-hook 'markdown-mode 'auto-fill-mode))

(use-package neotree
  :ensure t)

(use-package anaconda-mode
  :ensure t
  :config
  (add-hook 'python-mode-hook 'anaconda-mode))

(use-package paredit
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook 'paredit-mode)
  (add-hook 'clojure-mode-hook 'paredit-mode))

(use-package helm-dash
  :ensure t
  :config
  (setq helm-dash-common-docsets '("Clojure"
                                   "Ruby on Rails"
                                   "Ruby"
                                   "Python 2"
                                   "Pandas")))

(use-package multi-term
  :ensure t
  :config
  (setq-default multi-term-dedicated-select-after-open-p t))

(use-package dash
  :ensure t)

(use-package nyan-mode
  :ensure t)

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
                  '("citeonline" TeX-arg-cite))))))

  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  (setq reftex-plug-into-AUCTeX t)
  (setq font-latex-user-keyword-classes
        '(("autoref" (("autoref" "{")) font-lock-constant-face command)
          ("citeonline" (("citeonline" "{")) font-lock-constant-face command)))

  (setq reftex-external-file-finders
        '(("tex" . "kpsewhich -format=.tex %f")
          ("bib" . "kpsewhich -format=.bib %f"))))

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda() (org-bullets-mode 1))))

(use-package ace-jump-mode
  :ensure t)

(use-package diff-hl
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'diff-hl-mode)
  (add-hook 'css-mode-hook 'diff-hl-mode))

(use-package scss-mode
  :ensure t
  :config
  (progn
    (add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))))

(use-package cider
  :ensure t)

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

    (setq web-mode-markup-indent-offset 2)))

(use-package rspec-mode
  :ensure t
  :config
  (defadvice rspec-compile (around rspec-compile-around)
    "Use BASH shell for running the specs because of ZSH issues."
    (let ((shell-file-name "/bin/bash"))
      ad-do-it))
  (ad-activate 'rspec-compile)
  (add-hook 'after-init-hook 'inf-ruby-switch-setup))

(use-package rvm
  :ensure t)

(use-package solarized-theme
  :ensure t
  :config
  (setq solarized-use-variable-pitch nil)
  (setq solarized-scale-org-headlines nil))

(use-package zenburn-theme
  :ensure t
  :config
  (load-theme 'zenburn t))

(use-package inf-ruby
  :ensure t)

(use-package coffee-mode
  :ensure t)

(use-package evil
  :ensure t)

(use-package helm-ag
  :ensure t)

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :config
  (yas-global-mode t))

(use-package magit
  :ensure t
  :init
  (setq magit-last-seen-setup-instructions "1.4.0")
  (setq magit-diff-refine-hunk 'all)
  :config
  (define-key magit-mode-map (kbd "N") 'magit-section-backward))

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
    (define-key helm-map (kbd "C-k") 'helm-previous-line)))

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :init
  (setq projectile-completion-system 'helm)
  :config
  (projectile-global-mode))

(use-package helm-projectile
  :ensure t
  :init
  (setq helm-projectile-fuzzy-match t)
  :config
  (progn
    (helm-projectile-on)))

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
  (use-package company-anaconda
    :ensure t
    :config
    (add-to-list 'company-backends 'company-anaconda)))

(use-package evil
  :init
  (evil-mode 1)
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
    (evil-define-key 'normal helm-ag-mode-map (kbd "RET") 'helm-ag-mode-jump-other-window)
    (evil-set-initial-state 'term-mode 'emacs)
    (evil-set-initial-state 'neotree-mode 'emacs)))

(use-package evil-surround
  :ensure t
  :config
  :after evil
  (global-evil-surround-mode t))

(use-package key-chord
  :ensure t
  :after evil
  :config
  (key-chord-mode 1)
  (key-chord-define evil-insert-state-map "jj" 'evil-normal-state))

(use-package evil-leader
  :ensure t
  :init (global-evil-leader-mode)
  :after evil
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
      "f t" 'neotree-dir
      "b b" 'helm-buffers-list
      "b n" 'evil-next-buffer
      "b p" 'evil-prev-buffer
      "s v" 'split-window-horizontally
      "s h" 'split-window-vertically
      "g" 'magit-status
      "/" 'helm-projectile-ag
      "s f" 'helm-imenu
      "m t" 'what-year-is-it?
      "q q" 'multi-term-dedicated-toggle)
    (evil-leader/set-key-for-mode 'ruby-mode
      "r r" 'ruby-send-region
      "t f" 'rspec-verify
      "t l" 'rspec-verify-single
      "t t" 'rspec-verify-all)))

;; Hides white vertical line on splits
(set-face-attribute 'vertical-border
                    nil
                    :foreground (face-attribute 'default :background))

;; Gheto
(defun my-new-project-file (new-file)
  (interactive
   (list (read-file-name "New file: " (projectile-project-root))))
  (find-file new-file))

(defun my-notes-find ()
  (interactive)
  (let ((default-directory "~/Dropbox/org-notes/"))
    (helm-find-files nil)))

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
  (previous-line 2))

(defun my-move-lines-down ()
  (interactive)
  (next-line)
  (transpose-lines 1)
  (previous-line))

(defun align-assignment ()
  (interactive)
  (align-regexp "="))

(defun what-year-is-it? ()
  (interactive)
  (message (current-time-string)))

(defun toggle-term-normal-mode ()
  (interactive)
  (term-line-mode)
  (evil-normal-state)
  (previous-line))

(defun toggle-term-char-mode ()
  (interactive)
  (evil-emacs-state)
  (term-char-mode))

(defun toggle-term-mode ()
  (interactive)
  (if (term-in-char-mode)
      (toggle-term-normal-mode)
    (toggle-term-char-mode)))

(defun my-term-search ()
  (interactive)
  (term-send-raw-string "\C-r"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#3F3F3F" "#CC9393" "#7F9F7F" "#F0DFAF" "#8CD0D3" "#DC8CC3" "#93E0E3" "#DCDCCC"])
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#657b83")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-safe-themes
   (quote
    ("d8f76414f8f2dcb045a37eb155bfaa2e1d17b6573ed43fb1d18b936febc7bbc2" "0e219d63550634bc5b0c214aced55eb9528640377daf486e13fb18a32bf39856" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" default)))
 '(fci-rule-color "#383838")
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#fdf6e3" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#586e75")
 '(highlight-tail-colors
   (quote
    (("#eee8d5" . 0)
     ("#B4C342" . 20)
     ("#69CABF" . 30)
     ("#69B7F0" . 50)
     ("#DEB542" . 60)
     ("#F2804F" . 70)
     ("#F771AC" . 85)
     ("#eee8d5" . 100))))
 '(hl-bg-colors
   (quote
    ("#DEB542" "#F2804F" "#FF6E64" "#F771AC" "#9EA0E5" "#69B7F0" "#69CABF" "#B4C342")))
 '(hl-fg-colors
   (quote
    ("#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3")))
 '(magit-diff-use-overlays nil)
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(org-download-screenshot-method "xclip -selection clipboard -t image/png -o > %s")
 '(org-file-apps
   (quote
    ((auto-mode . emacs)
     ("\\.mm\\'" . default)
     ("\\.x?html?\\'" . default)
     ("\\.pdf\\'" . "evince %s"))))
 '(package-selected-packages
   (quote
    (org-download request evil-collection restclient neotree groovy-mode indent-guide rbenv docker dockerfile-mode jsx-mode oauth2 yaml-mode gist ox-pandoc spacegray-theme zenburn-theme company-anaconda anaconda-mode solarized-theme nginx-mode gradle-mode git-link web-mode use-package scss-mode rvm rspec-mode org-bullets nyan-mode multi-term magit inf-ruby helm-projectile helm-dash helm-ag evil-surround evil-leader diff-hl company color-theme-sanityinc-tomorrow coffee-mode clj-refactor ace-jump-mode)))
 '(pdf-view-midnight-colors (quote ("#DCDCCC" . "#383838")))
 '(pos-tip-background-color "#eee8d5")
 '(pos-tip-foreground-color "#586e75")
 '(reftex-ref-style-default-list (quote ("Default" "Hyperref")))
 '(safe-local-variable-values (quote ((ispell-dictionary . "pt_BR"))))
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#eee8d5" 0.2))
 '(term-default-bg-color "#fdf6e3")
 '(term-default-fg-color "#657b83")
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3")
 '(weechat-color-list
   (quote
    (unspecified "#fdf6e3" "#eee8d5" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#657b83" "#839496")))
 '(xterm-color-names
   ["#eee8d5" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#073642"])
 '(xterm-color-names-bright
   ["#fdf6e3" "#cb4b16" "#93a1a1" "#839496" "#657b83" "#6c71c4" "#586e75" "#002b36"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
