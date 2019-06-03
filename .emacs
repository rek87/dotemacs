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

;; Ibuffer conf
(defalias 'list-buffers 'ibuffer)
(add-hook 'ibuffer-mode-hook (lambda () (ibuffer-auto-mode 1)))

(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("exwm" (mode . exwm-mode))
               ("lisp" (or
                        (mode . lisp-mode)
                        (mode . slime-repl-mode)
                        (mode . slime-inspector-mode)
                        (name . "^\\*slime-\\(description\\|compilation\\|xref\\)\\*$")
                        (name . "^\\*sldb .*\\*$")
                        (filename . "^/usr/local/doc/HyperSpec/")))
               ("python" (or
                          (mode . python-mode)
                          (mode . inferior-python-mode)
                          (name . "^\\*Python \\(Check\\|Doc\\)\\*$")))
               ("shell" (or
                         (mode . shell-mode)
                         (mode . term-mode)
                         (mode . sh-mode)
                         (mode . conf-unix-mode)
                         (mode . eshell-mode)
                         (name . "^\\*Shell Command Output\\*$")))
               ("C" (or
                     (derived-mode . c-mode)
                     (mode . c++-mode)))
               ("asm" (mode . asm-mode))
               ("yaml" (mode . yaml-mode))
               ("dired" (or
                         (mode . dired-mode)
                         (mode . wdired-mode)
                         (mode . archive-mode)
                         (mode . proced-mode)))
               ("man" (or
                       (mode . Man-mode)
                       (mode . woman-mode)))
               ("data" (or
                        (filename . ".*\\.\\([ct]sv\\|dat\\)$")))
               ("LaTeX" (or
                         (mode . latex-mode)
                         (mode . tex-shell)
                         (mode . TeX-output-mode)
                         (name . "^\\*\\(Latex Preview Pane \\(Welcome\\|Errors\\)\\|pdflatex-buffer\\)\\*$")))
               ("text" (mode . text-mode))
               ("pdf" (or
                       (mode . doc-view-mode)
                       (mode . pdf-view-mode)))
               ("web" (or
                       (mode . w3m-mode)
                       (mode . eww-mode)))
               ("org" (or (derived-mode . org-mode)
                          (mode . org-agenda-mode)))
               ("planner" (or
                           (name . "^\\*Calendar\\*$")
                           (name . "^diary$")
                           (mode . muse-mode)))
               ("org" (or (mode . org-mode)
                          (filename . "OrgMode")))
               ("git" (or (derived-mode . magit-mode)
                          (filename . "\\.git\\(ignore\\|attributes\\)$")))
               ("diff" (or
                        (mode . diff-mode)
                        (mode . ediff-mode)
                        (name . "^\\*[Ee]?[Dd]iff.*\\*$")))
               ("mail" (or
                        (mode . message-mode)
                        (mode . bbdb-mode)
                        (mode . mail-mode)
                        (mode . gnus-group-mode)
                        (mode . gnus-summary-mode)
                        (mode . gnus-article-mode)
                        (mode . mu4e-compose-mode)
                        (name . "*mu4e*")
                        (name . "^\\.bbdb$")
                        (name . "^\\.newsrc-dribble")))
               ("emacs" (or
                         (mode . emacs-lisp-mode)
                         (mode . lisp-interaction-mode)
                         (mode . help-mode)
                         (mode . Info-mode)
                         (mode . package-menu-mode)
                         (mode . finder-mode)
                         (mode . Custom-mode)
                         (mode . apropos-mode)
                         (mode . ioccur-mode)
                         (mode . occur-mode)
                         (mode . reb-mode)
                         (mode . calc-mode)
                         (mode . calc-trail-mode)
                         (mode . messages-buffer-mode)))
               ("misc" (name . "^\\*[0-9A-Za-z_]+\\*$"))))))

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")))

(setq ibuffer-show-empty-filter-groups nil)
(setq ibuffer-jump-offer-only-visible-buffers t)

(c-add-style "univent-c-style"
            '("gnu"
              (c-basic-offset . 4)     ; Guessed value
              (c-offsets-alist
               (access-label . -2)      ; Guessed value
               (arglist-intro . 4)     ; Guessed value
               (class-close . 0)       ; Guessed value
               (cpp-define-intro . 0)  ; Guessed value
               (defun-block-intro . *) ; Guessed value
               (defun-close . 0)       ; Guessed value
               (defun-open . 0)        ; Guessed value
               (func-decl-cont . *)    ; Guessed value
               (inclass . +)           ; Guessed value
               (inline-close . 0)      ; Guessed value
               (innamespace . 0)       ; Guessed value
               (member-init-intro . 2) ; Guessed value
               (namespace-close . 0)   ; Guessed value
               (statement . 0)         ; Guessed value
               (statement-cont . 2)    ; Guessed value
               (topmost-intro . 0)     ; Guessed value
               (topmost-intro-cont . 0) ; Guessed value
               (annotation-top-cont . 0)
               (annotation-var-cont . +)
               (arglist-close . c-lineup-close-paren)
               (arglist-cont c-lineup-gcc-asm-reg 0)
               (arglist-cont-nonempty . 4)
               (block-close . 0)
               (block-open . 0)
               (brace-entry-open . 0)
               (brace-list-close . 0)
               (brace-list-entry . 0)
               (brace-list-intro . +)
               (brace-list-open . +)
               (c . c-lineup-C-comments)
               (case-label . 0)
               (catch-clause . 0)
               (class-open . 0)
               (comment-intro . c-lineup-comment)
               (composition-close . 0)
               (composition-open . 0)
               (cpp-macro . -1000)
               (cpp-macro-cont . +)
               (do-while-closure . 0)
               (else-clause . 0)
               (extern-lang-close . 0)
               (extern-lang-open . 0)
               (friend . 0)
               (incomposition . +)
               (inexpr-class . +)
               (inexpr-statement . +)
               (inextern-lang . +)
               (inher-cont . 2)
               (inher-intro . 2)
               (inlambda . c-lineup-inexpr-block)
               (inline-open . 0)
               (inmodule . +)
               (knr-argdecl . 0)
               (knr-argdecl-intro . 5)
               (label . 0)
               (lambda-intro-cont . +)
               (member-init-cont . [6])
               (module-close . 0)
               (module-open . 0)
               (namespace-open . 0)
               (objc-method-args-cont . c-lineup-ObjC-method-args)
               (objc-method-call-cont c-lineup-ObjC-method-call-colons c-lineup-ObjC-method-call +)
               (objc-method-intro .
                                  [0])
               (statement-block-intro . 2)
               (statement-case-intro . +)
               (statement-case-open . +)
               (stream-op . c-lineup-streamop)
               (string . -1000)
               (substatement . 2)
               (substatement-label . 0)
               (substatement-open . +)
               (template-args-cont c-lineup-template-args +))))

(defun univent-c-mode-hook ()
   (c-set-style "univent-c-style"))

(add-hook 'c-mode-hook 'univent-c-mode-hook)
(add-hook 'c++-mode-hook 'univent-c-mode-hook)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(c-basic-offset 2)
 '(c-default-style
   (quote
    ((java-mode . "java")
     (awk-mode . "awk")
     (other . "gnu"))))
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
