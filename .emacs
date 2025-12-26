(defvar pkg-refreshed nil)
(defconst package-list `(
                       color-theme-sanityinc-tomorrow
                       flycheck
                       helm
                       helm-projectile
                       helm-swoop
                       magit
                       which-key
                       whitespace
                       projectile
                       bash-completion
                       json-mode
                       quelpa
                       beacon
                       paredit
                       company
                       elpy
                       undo-tree
                       pdf-tools
                       ))

(require 'package)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")))
(package-initialize)

(defun check-packages (packages)
  (with-demoted-errors
      (dolist (package packages)
        (unless (package-installed-p package)
          (unless pkg-refreshed
            (package-refresh-contents)
            (setq pkg-refreshed t))
          (package-install package)))))

(check-packages package-list)

;;(quelpa '(smime
;;         :fetcher git
;;         :url "ssh://fedrec01@eu-gerrit-2.euhpc.arm.com:29418/cpu/tools/midas.git"
;;         :files ("etc/SMIME/smime.el"))
;;    :upgrade t)
;;(require 'smime)

;; Disable package signature check!!
;; Up to 26.2 is bugged https://debbugs.gnu.org/cgi/bugreport.cgi?bug=33825
(unless (or (> emacs-major-version 26)
            (and (= emacs-major-version 26)
                 (> emacs-minor-version 2)))
  (setq package-check-signature nil))

(when (file-directory-p "~/my_emacs/cc-mode/")
  (add-to-list 'load-path "~/my_emacs/cc-mode/"))

(when (file-directory-p "~/my_emacs/tarmac-mode/")
  (check-packages `(cl-lib popup))
  (add-to-list 'load-path "~/my_emacs/tarmac-mode/")
  (require 'tarmac-mode))

;; Fabien's armasm mode, interested only in doc for now
;; https://git.research.arm.com/fabric01/armasm-mode/
(when (file-directory-p "~/my_emacs/armasm-mode/rel")
  (add-to-list 'load-path "~/my_emacs/armasm-mode/rel")
  (require 'armdoc))

(when (file-directory-p "~/my_emacs/module-mode/")
  (add-to-list 'load-path "~/my_emacs/module-mode/")
  (require 'module-mode))

;; https://github.com/daimrod/highlight-sexp.git
(when (file-directory-p "~/my_emacs/highlight-sexp/")
  (load "~/my_emacs/highlight-sexp/highlight-sexp.el")
  (require 'highlight-sexp)
  (add-hook 'lisp-mode-hook 'highlight-sexp-mode)
  (add-hook 'emacs-lisp-mode-hook 'highlight-sexp-mode))

(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")
(require 'mu4e)
(setq user-mail-address "federico.recanati@arm.com"
      user-full-name "Federico Recanati")
(setq  mu4e-sent-folder "/Sent"
       mu4e-drafts-folder "/Drafts"
       mu4e-trash-folder "/Trash"
       mu4e-refile-folder "/Archive"
       mu4e-get-mail-command "offlineimap -o"
       mu4e-update-interval 600

       mu4e-headers-date-format "%a  %d/%m/%y  %H:%M"
       mu4e-headers-fields '((:date    . 23)
                             (:flags   . 6)
                             (:from    . 30)
                             (:subject . nil))
       mu4e-headers-visible-lines 14

       mu4e-maildir-shortcuts '((:maildir "/INBOX" :key ?i))
       )
;; How to send an email.
;; Use `C-x m' to write an email, then `C-c C-c' to send it
(setq mu4e-compose-format-flowed t
      message-sendmail-envelope-from 'header
      message-send-mail-function 'smtpmail-send-it
      message-kill-buffer-on-exit t
      smtpmail-smtp-server "localhost"
      smtpmail-smtp-service 1025)

;; Useful for reformatting paragraphs at 80 cols w/ M-q
(setq fill-column 80)
(setq sentence-end-double-space nil)

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
(add-hook 'git-commit-setup-hook 'git-commit-turn-on-flyspell)

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
(setq whitespace-global-modes '(c-mode c++-mode python-mode lisp-mode))
(global-whitespace-mode t)

(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)

(require 'helm-swoop)
(global-set-key (kbd "M-i") 'helm-swoop)
(global-set-key (kbd "M-I") 'helm-swoop-back-to-last-point)
(global-set-key (kbd "C-c M-i") 'helm-multi-swoop)
(global-set-key (kbd "C-x M-i") 'helm-multi-swoop-all)

(require 'beacon)
(beacon-mode 1)

(require 'paredit)
(add-hook 'lisp-mode-hook 'paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)

(require 'company)
(add-hook 'lisp-mode-hook 'company-mode)
(add-hook 'emacs-lisp-mode-hook 'company-mode)

;; Disable startup message
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

;; Ibuffer conf
(defalias 'list-buffers 'ibuffer)
(add-hook 'ibuffer-mode-hook (lambda () (ibuffer-auto-mode 1)))

;; Ask y/n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

(global-undo-tree-mode)

;; Python config
;; Make sure to have `venv` package installed
(setq python-shell-interpreter "python3")
(elpy-enable)
(define-key elpy-mode-map (kbd "<M-left>") nil)
(define-key elpy-mode-map (kbd "<M-right>") nil)
(define-key elpy-mode-map (kbd "<S-left>") 'elpy-nav-indent-shift-left)
(define-key elpy-mode-map (kbd "<S-right>") 'elpy-nav-indent-shift-right)
;; Workaround for elpy complaining "Overlapping strings detected"
;; Got from stackoverflow
(setq elpy-eldoc-show-current-function nil)

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

;; Allow python execution in org-babel
(org-babel-do-load-languages 'org-babel-load-languages '((python . t)))

;; pdf-tools
(pdf-tools-install)
(setq doc-view-cache-directory "~/doc-view-cache")

(c-add-style "univent"
             '("stroustrup"
               (c-basic-offset . 2)
               (c-backslash-column . 79)
               (c-backslash-max-column . 79)
               (c-offsets-alist . ((innamespace . 0)
                                   (access-label . -)
                                   (case-label . +)
                                   (arglist-intro . ++)
                                   (statement-cont . ++)
                                   (inline-open . 0)
                                   (member-init-intro . ++)
                                   ))))

(add-hook 'c++-mode-hook (lambda ()
                           (c-set-style "univent")))

(defun print-c-alist ()
  "Print c-offsets-alist in a buffer."
  (interactive)
  (let ((cfg c-offsets-alist))
    (switch-to-buffer-other-window "*c-offsets-alist*")
    (goto-char (point-min))
    (cl-prettyprint cfg)
    )
  )

(defun c-offset (&optional n)
  "Set the C basic offset (default 4)."
  (interactive)
  (setq c-basic-offset (if n n 4)))

;; Set Univent in emacs shell env
(defun univent-env (path)
  "Set Univent environment"
  (interactive "DInstall Path: ")
  (setenv "PATH"
          (concat
           path "/bin" ":"
           (getenv "PATH")))
  (setenv "LD_LIBRARY_PATH"
          (concat
           path "/lib" ":"
           (getenv "LD_LIBRARY_PATH")))
  (setenv "UNIVENT_PLUGIN_PATH"
          (concat
           path "/share/univent/plugins" ":"
           (getenv "UNIVENT_PLUGINPATH")))
  (setenv "PYTHONPATH"
          (concat
           path "/lib/python" ":"
           (getenv "PYTHONPATH")))
  (setenv "SBPATH"
          (concat
           path "/share/sb" ":"
           (getenv "SBPATH")))
  (setenv "ARMLMD_LICENSE_FILE"
          "/arm/tools/arm/license/validation/license.dat"))


;; Set unicode mode when run in terminal
(if (not (display-graphic-p))
    (set-terminal-coding-system 'utf-8))

;; Set python-mode for SCons files
(add-to-list 'auto-mode-alist '("SCons" . python-mode))

(when (equal (system-name) "e121080-lin")
  ;; Set LD_LIBRARY_PATH for local GCC
  (setenv "LD_LIBRARY_PATH"
          (concat "/usr/local/lib64/:" (getenv "LD_LIBRARY_PATH"))))

(defun shell-name-default-directory ()
  "Open a shell from current buffer's `default-directory`."
  (concat "*shell@" default-directory "*"))
(defun shell-at-default-directory ()
  "Open a shell from current buffer's `default-directory`."
  (interactive)
  (shell (shell-name-default-directory)))
(defun rename-buffer-shell (arg)
  ""
  (rename-buffer (shell-name-default-directory))
  (message "CIAO"))
(global-set-key (kbd "<f2>") 'shell-at-default-directory)
;;(advice-add #'shell-process-cd :after #'rename-buffer-shell)
;;(advice-add #'shell-rocess-cd :before #'bar)

;; TODO: define
;; Color for sexp in light theme: #cabbca
(defun light ()
  "Set light theme"
  (interactive)
  (ef-themes-select 'ef-day))

(defun dark ()
  "Set dark theme"
  (interactive)
  (color-theme-sanityinc-tomorrow-eighties))

(defun insert-change-id ()
  (interactive)
  (insert "Change-Id: I")
  ;; The hash is computed by Gerrit hook like so:
  ;; (whoami ; hostname ; date; cat $1 ; echo $RANDOM) | git hash-object --stdin
  ;; `git hash-object' computes a SHA1 sum of the input.
  ;; Compute a checksum of the same random data.
  (insert (secure-hash 'sha1 (format "%s%s%s%s"
                                     (user-full-name)
                                     (system-name)
                                     (current-time)
                                     (random)))))

(defun jira-issue-md-at-point ()
  (interactive)
  (let ((st (make-syntax-table (syntax-table))))
    (modify-syntax-entry ?- "w")
    (with-syntax-table st
      (let ((jira-issue (word-at-point)))
        (backward-word)
        (insert "[")
        (forward-word)
        (insert "|https://jira.arm.com/browse/" jira-issue "]")
        ))))


(setq custom-file "~/.config/emacs-custom.el")
(load custom-file)
