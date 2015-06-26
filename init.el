(require 'package)
(setq package-enable-at-startup nil)

;; packages
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa-stable" . "http://melpa-stable.milkbox.net/packages/")))
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
(setq make-backup-file nil)
(setq auto-save-default nil)
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(tool-bar-mode -1)
(load-theme 'solarized-light t)
(electric-pair-mode t)
(show-paren-mode t)
(setq org-src-fontify-natively t)
(setq org-startup-indented t)

(use-package coffee-mode)
(use-package evil)
(use-package helm-ag)
(use-package magit
  :init
  (setq magit-last-seen-setup-instructions "1.4.0")
  )

(use-package helm
  :ensure t
  :diminish helm-mode
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
    (define-key helm-map (kbd "C-k") 'helm-previous-line))
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
(use-package auto-complete
  :ensure t
  :config
  (progn
    (ac-config-default)
    (ac-set-trigger-key "TAB")
    (ac-set-trigger-key "<tab>")
    )
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
  (process-send-string "*js*" "npm test \n")
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
          "w v" 'split-window-horizontally
          "w h" 'split-window-vertically
          "g" 'magit-status
          )
        (evil-leader/set-key-for-mode 'js-mode
          "t" 'my-js-run-test
          )
        )
      )
    (evil-mode 1))
  :config
  (progn
    (define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
    (define-key evil-visual-state-map (kbd "C-u") 'evil-scroll-up)

    ;; Changing windows
    (define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
    (define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
    (define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
    (define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)
    )
  )
