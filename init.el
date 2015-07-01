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

(require 'whitespace)
(setq whitespace-line-column 80) ;; limit line length
(setq whitespace-style '(face lines-tail))
(add-hook 'prog-mode-hook 'whitespace-mode)

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
  (define-key magit-mode-map (kbd "N") 'magit-goto-previous-section)
  )

(use-package eyebrowse
  :config
  (progn
    (eyebrowse-mode t)
    )
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

(use-package company
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (define-key company-active-map (kbd "C-j") 'company-select-next)
  (define-key company-active-map (kbd "C-k") 'company-select-previous)
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

          "q" 'eyebrowse-close-window-config

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
