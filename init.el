;; packages
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                        ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa-stable" . "http://melpa-stable.milkbox.net/packages/")))
(package-initialize)

(defun require-package (package)
  (setq-default highlight-tabs t)
  "Install given PACKAGE."
  (unless (package-installed-p package)
    (unless (assoc package package-archive-contents)
      (package-refresh-contents))
    (package-install package)))

(require-package 'helm-ag)
(require-package 'evil)
(require-package 'helm)
(require-package 'evil-leader)
(require-package 'auto-complete)
(require-package 'projectile)
(require-package 'coffee-mode)
(setq helm-projectile-fuzzy-match t)
(require-package 'helm-projectile) 
(require-package 'yasnippet)
(require-package 'color-theme-solarized)

;;General config
(projectile-global-mode)
(scroll-bar-mode -1)
(setq ring-bell-function 'ignore)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq make-backup-file nil)
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(tool-bar-mode -1)
(load-theme 'solarized-light t)
(electric-pair-mode t)
(show-paren-mode t)
(setq org-src-fontify-natively t)
(setq org-startup-indented t)

;; Yas
;; Autocomplete
(ac-config-default)
(ac-set-trigger-key "TAB")
(ac-set-trigger-key "<tab>")

;; Projectile
(setq projectile-completion-system 'helm)
(helm-projectile-on)

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
(require 'helm-config)
(setq helm-recentf-fuzzy-match t)
(setq helm-buffers-fuzzy-matching t)
(setq helm-M-x-fuzzy-match t)
(setq helm-semantic-fuzzy-match t)
(setq helm-apropos-fuzzy-match t)
(setq helm-lisp-fuzzy-completion t)
(define-key helm-map (kbd "C-z") 'helm-select-action)
(define-key helm-map (kbd "C-h") 'my-helm-split)
(define-key helm-map (kbd "C-v") 'my-helm-vsplit)
(define-key helm-map (kbd "C-j") 'helm-next-line)
(define-key helm-map (kbd "C-k") 'helm-previous-line)

;; Needs to be on bottom
(global-evil-leader-mode)
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
  "w s" 'split-window-vertically
  )
(evil-define-key 'normal org-mode-map
  (kbd "M-j") 'org-metadown
  (kbd "M-k") 'org-metaup
  )
(define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
(define-key evil-visual-state-map (kbd "C-u") 'evil-scroll-up)
;; Changing windows
(define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
(define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
(define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
(define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)
(evil-mode 1)
