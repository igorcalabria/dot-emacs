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

(defvar prelude-packages
  '(fringe-helper
    coffee-mode
    evil
    helm-ag
    yasnippet
    magit
    eyebrowse
    helm
    projectile
    company
    evil-leader
    evil-surround
    inf-ruby
    zenburn-theme
    restclient
    git-gutter-fringe
    web-mode
    rspec-mode
    )
  "A list of packages to ensure are installed at launch.")

(defun prelude-packages-installed-p ()
  (loop for p in prelude-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

(unless (prelude-packages-installed-p)
  ;; check for new packages (package versions)
  (message "%s" "Emacs Prelude is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; install the missing packages
  (dolist (p prelude-packages)
    (when (not (package-installed-p p))
      (package-install p))))

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
(load-theme 'zenburn t)
(electric-pair-mode t)
(show-paren-mode t)
(setq org-src-fontify-natively t)
(setq org-startup-indented t)
(add-hook 'prog-mode-hook (lambda() (linum-mode)))
(set-face-attribute 'default nil
                    :family "Consolas" :height 140 :weight 'normal)
(blink-cursor-mode 0)

(require 'whitespace)
(setq whitespace-line-column 80) ;; limit line length
(setq whitespace-style '(face lines-tail))
(add-hook 'prog-mode-hook 'whitespace-mode)

(use-package web-mode
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
  :config
  (defadvice rspec-compile (around rspec-compile-around)
    "Use BASH shell for running the specs because of ZSH issues."
    (let ((shell-file-name "/bin/bash"))
      ad-do-it))
  (ad-activate 'rspec-compile)
  (add-hook 'after-init-hook 'inf-ruby-switch-setup)
  )

(use-package fringe-helper)
(use-package git-gutter-fringe
  :config
  (set-face-foreground 'git-gutter-fr:added "forest green")
  (set-face-foreground 'git-gutter-fr:deleted "dark red")
  (set-face-foreground 'git-gutter-fr:modified "goldenrod")
  (global-git-gutter-mode t)
  )
(use-package restclient)
(use-package coffee-mode)
(use-package evil)
(use-package helm-ag)

(use-package yasnippet
  :config
  (yas-global-mode t)
  )

(use-package magit
  :init
  (setq magit-last-seen-setup-instructions "1.4.0")
  :config
  (define-key magit-mode-map (kbd "N") 'magit-section-backward)
  )

(use-package eyebrowse
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
;; Gheto
(defun my-js-run-test ()
  (interactive)
  (process-send-string "*js-test*" "npm test \n")
  )

(defun my-new-project-file (new_file)
  (interactive
   (list (read-file-name "New file: " (projectile-project-root))))
  (find-file new_file)
  )

(defun my-rspec-file ()
  (interactive)
  (process-send-string "*rspec*" (concat "rspec " (buffer-file-name) "\n"))
  )

(use-package evil
  :init
  (progn
    (use-package evil-leader
      :init (global-evil-leader-mode)
      :config
      (progn
        (evil-leader/set-leader "<SPC>")
        (evil-leader/set-key ":" 'helm-M-x)
        (evil-leader/set-key
          "f f" 'helm-find-files
          "p f" 'helm-projectile-find-file
          "p p" 'projectile-switch-project
          "r" 'helm-recentf
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
        (evil-leader/set-key-for-mode 'js-mode
          "t" 'my-js-run-test
          )
        (evil-leader/set-key-for-mode 'ruby-mode
          "t" 'my-rspec-file
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

    (define-key evil-normal-state-map (kbd "C-c y") 'company-yasnippet)
    (define-key evil-insert-state-map (kbd "C-c y") 'company-yasnippet)
    (use-package evil-surround
      :config
      (global-evil-surround-mode t)
      )
    (use-package git-timemachine
      :config
      (progn
        (evil-set-initial-state 'git-timemachine-mode 'normal)
        (evil-define-key 'normal git-timemachine-mode-map
          (kbd "n") 'git-timemachine-show-next-revision
          (kbd "N") 'git-timemachine-show-previous-revision
          (kbd "q") 'git-timemachine-quit
          )
        )
      )
    )
  )
