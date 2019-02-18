
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(require 'package)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")))
(package-initialize)

;; Mandatory magit
(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)

(ido-mode t)
(global-set-key (kbd "C-x C-f") 'ido-find-file)

;; Use meta-arrow to move among windows
(global-set-key (kbd "M-<left>") 'windmove-left)
(global-set-key (kbd "M-<right>") 'windmove-right)
(global-set-key (kbd "M-<up>") 'windmove-up)
(global-set-key (kbd "M-<down>") 'windmove-down)

(show-paren-mode 1) 

;; Setting t be cheked from Andrea
(setq line-move-visual 1)
(require 'helm)

;; EVS mode stub
(define-derived-mode evs-mode json-mode "EVS Mode"
  "EVS Mode"
  (erase-buffer)
  (shell-command
   (concat "/work/univent/usr/bin/u3cat -i" (buffer-file-name))     
   (current-buffer))
  (not-modified)
  )



;; Windmove is built into Emacs. It lets you move point from window to window using
;; Shift and the arrow keys. This is easier to type than ‘C-x o’ when there are multiple
;; windows open.
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(custom-enabled-themes (quote (misterioso)))
 '(debug-on-error t)
 '(package-selected-packages (quote (json-mode company helm magit))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
