(defvar pkg-refreshed nil)
(defvar package-list `(
		       color-theme-sanityinc-tomorrow
		       flycheck
		       helm
		       magit
		       which-key
		       whitespace
		       ))

(require 'package)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")))
(package-initialize)

(with-demoted-errors
    (when (file-exists-p package-user-dir)
      (dolist (package package-list)
	(unless (package-installed-p package)
	  (unless pkg-refreshed
	    (package-refresh-contents)
	    (setq pkg-refreshed t))
	  (package-install package)))))

(ido-mode t)
(global-set-key (kbd "C-x C-f") 'ido-find-file)

;; Windmove is built into Emacs. It lets you move point from window to window using
;; Meta and the arrow keys. This is easier to type than ‘C-x o’ when there are multiple
;; windows open.
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings 'meta))

(show-paren-mode 1)  ; Highlight parentheses macth
(column-number-mode)  ; Show column number
(menu-bar-mode -1) ; Disable menu bar
(tool-bar-mode -1) ; Do not display GUI Toolbar
(scroll-bar-mode -1) ; Disable scroll bars
(setq-default indent-tabs-mode nil) ; Use spaces for tab

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(global-auto-revert-mode t) ; Automatically reload files
(setq compilation-scroll-output 'first-error)

(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)

;; Display available keybindings in popup
(require 'which-key)
(which-key-mode 1)

;; Helm config
(require 'helm)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x b") 'helm-mini)

(require 'projectile)
(projectile-mode 1)
(global-set-key (kbd "<f4>") 'projectile-compile-project)

;; Register bash completion for the shell buffer and shell command line.
(require 'bash-completion)
(bash-completion-setup)

(require 'whitespace)
;; Highlight exceeding line length (80 chars) and trailing spaces
(setq whitespace-style '(face lines-tail trailing tab-mark))
(global-whitespace-mode t)

(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)

;; Setting t be cheked from Andrea
(setq line-move-visual 1)
;; Disable startup message
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)



;; EVS mode stub
(define-derived-mode evs-mode json-mode "EVS Mode"
  "EVS Mode"
  (erase-buffer)
  (shell-command
   (concat "/work/univent/usr/bin/u3cat -i" (buffer-file-name))
   (current-buffer))
  (set-buffer-modified-p nil)
  )


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(c-basic-offset 2)
 '(custom-enabled-themes (quote (sanityinc-tomorrow-eighties)))
 '(custom-safe-themes
   (quote
    ("628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" default)))
 '(debug-on-error t)
 '(package-selected-packages
   (quote
    (which-key flycheck helm-projectile bash-completion projectile color-theme-sanityinc-tomorrow json-mode company helm magit)))
 '(projectile-mode t nil (projectile))
 '(safe-local-variable-values
   (quote
    ((projectile-project-compilation-cmd . "scons -C /work/univent/build -Y.. -j8 install")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
