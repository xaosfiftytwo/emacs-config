;;; init-minus-starter-kit.el
;; plain old el and no starter-kit activation

;; This sets up the load path so that we can override it
;; (package-initialize nil)
;; Override the packages with the git version of Org
;; (add-to-list 'load-path "~/elisp/org-mode/lisp")
;; (add-to-list 'load-path "~/elisp/org-mode/contrib/lisp")
;; Load the rest of the packages
;; (package-initialize t)
;; (require 'org)
;; (require 'ob-tangle)
;; (org-babel-load-file (expand-file-name "~/.emacs.d/Sacha.org"))

(let ((minver 24))
  (unless (>= emacs-major-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'init-benchmarking) ;; Measure startup time

;;----------------------------------------------------------------------------
;; Bootstrap config (purcell)
;;----------------------------------------------------------------------------
(require 'init-compat)
(require 'init-utils)
(require 'init-site-lisp) ;; Must come before elpa, as it may provide package.el
(require 'init-elpa)      ;; Machinery for installing required packages
;; init-exec-path is mac- or windows-specific
;; (require 'init-exec-path) ;; Set up $PATH

;;----------------------------------------------------------------------------
;; Better emacs defaults (Phil Hagelberg, alias technomancy)
;;----------------------------------------------------------------------------
(require 'init-better-defaults)

;;----------------------------------------------------------------------------
;; Settings  (Bastien Guerry
;;----------------------------------------------------------------------------
(require 'init-bzg)

;;----------------------------------------------------------------------------
;; Has leim been disabled  (xaos52)
;;----------------------------------------------------------------------------
(unless (file-exists-p "/usr/local/share/emacs/24.3.90/lisp/leim/leim-list.el")
  (progn
    (message "**********")
    (message "WARNING:")
    (message "Registering leim methods has been disabled by renaming")
    (message "/usr/local/share/emacs/24.3.90/lisp/leim/leim-list.el to leim-list.el.orig.")
    (message "**********")))

(require 'dired-x)
(require 'find-dired)

;;; See init-elpa.el
;; This is sufficient to be able to install packages from melpa
;; package-initialize is run automatically after init.el
;; (setq package-archives
;;       '(("gnu"         . "http://elpa.gnu.org/packages/")
;;         ;; using org mode from source
;;         ;; ("org"         . "http://orgmode.org/elpa/")
;;         ("melpa"       . "http://melpa.milkbox.net/packages/")))
;;         ;; ("marmalade"   . "http://marmalade-repo.org/packages/")))

;;;_ key bindings

(global-set-key (kbd "C-M-h") 'backward-kill-word)

(global-set-key (kbd "C-x \\") 'align-regexp)

(global-set-key [f1] 'menu-bar-mode)

(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)

(global-set-key (kbd "C-x M-f") 'ido-find-file-other-window)
(global-set-key (kbd "C-x C-p") 'find-file-at-point)
(global-set-key (kbd "C-c y") 'bury-buffer)
(global-set-key (kbd "C-c r") 'revert-buffer)
(global-set-key (kbd "M-`") 'file-cache-minibuffer-complete)
(global-set-key (kbd "C-x f") 'recentf-ido-find-file)

(windmove-default-keybindings) ;; Shift+direction
(global-set-key (kbd "C-x O") (lambda () (interactive) (other-window -1))) ;; back one
(global-set-key (kbd "C-x C-o") (lambda () (interactive) (other-window 2))) ;; forward two

(global-set-key (kbd "C-x ^") 'join-line)

(global-set-key (kbd "C-x C-m") 'execute-extended-command)

(global-set-key (kbd "C-h a") 'apropos)

(global-set-key "\M-?" 'etags-select-find-tag-at-point)

(define-key isearch-mode-map (kbd "C-o")
  (lambda () (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp
                 isearch-string
               (regexp-quote isearch-string))))))

(define-key global-map "\C-ca" 'org-agenda)

(define-key global-map "\C-cl" 'org-store-link)

(define-key global-map "\C-x\C-r" 'rgrep)

(defun x52-screen-layout ()
  (interactive)
  ;; Load users preferred theme
  ;; package-initialize must have run for emacs to find the theme
  ;; and package-initialize runs _after_ init.el
  ;; FIXME: (load-theme 'planet t) - causes severe flickering when activated here
  (let ((buffer (if buffer-file-truename (current-buffer) nil)))
    (if (< (window-width) 300)
        (split-window-right)
      (progn (split-window-right +100)
             (switch-to-buffer "*Messages*")
             (other-window +1)
             (split-window-right -100)
             (if buffer
                 (switch-to-buffer buffer)
               (jump-to-register ?t))
             (other-window +1)
             (switch-to-buffer "*scratch*" t t)
             (other-window -1)))))
(global-set-key (kbd "C-x 3") 'x52-screen-layout)
(add-hook 'after-init-hook 'turn-on-smart-cursor-color)

;; rename rgrep, grep lgrep buffer names to make them more unique
;; Picked idea from https://blog.mozilla.org/nfroyd/2014/05/06/my-code-search-engine/
(defun x52-compilation-buffer-name (major-mode-name)
  (cond
   ;; grep.el invokes compile, we might as well take advantage of that.
   ((string-equal major-mode-name "grep")
    (if (boundp 'regexp)
        (concat "*grep for " regexp "*")
      "*grep*"))
   ;; We have no idea, just use the default.
   (t
    "*compilation*")))

(setq compilation-buffer-name-function 'x52-compilation-buffer-name)

;; FIXME: is 1024 way too much?
;; (setq-default history-length 1024)
(savehist-mode)
(setq history-length t)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history t)
;; (setq savehist-additional-variables
;;        '(kill-ring
;;        :bind (("C-c h" . helm-mini)))
;;      (ido-mode -1) ;; Turn off ido mode in case I enabled it accidentally


;;;_ miscellaneous settings
(setq visible-bell t
      echo-keystrokes 0.1
      font-lock-maximum-decoration t
      inhibit-startup-message t
      transient-mark-mode t
      color-theme-is-global t
      delete-by-moving-to-trash t
      shift-select-mode nil
      truncate-partial-width-windows nil
      whitespace-style '(trailing lines space-before-tab
                                  indentation space-after-tab)
      whitespace-line-column 100
      ediff-window-setup-function 'ediff-setup-windows-plain
      oddmuse-directory (concat user-emacs-directory "oddmuse")
      xterm-mouse-mode t
      initial-scratch-message ""
      dired-recursive-deletes 'always
      dired-recursive-copies 'always
      temporary-file-directory "~/tmp/emacs")

;; Configure auto-saves
;; For remote files
(setq auto-save-file-name-transforms
      '(("\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'" "/tmp/\\2" t)))
;; For local files
(add-to-list 'auto-save-file-name-transforms
             (list "\\`/.*/\\(.*\\)\\'" (concat temporary-file-directory "/auto-saves" "/\\1") t)
             t)
;; (setq auto-save-file-name-transforms nil)

;; this is the default setting
;; (add-to-list 'backup-directory-alist
;;              (cons "." "~/.emacs.d/backups/"))

;; Don't want this to show up every  time emacs starts
;; (customize-option 'inhibit-startup-echo-area-message)

;; Specify a dedicated window for other-window in Emacs

(winner-mode 1)
(defvar window-locked-p nil)

;;;_ More control on emacs windows?

(defun lockon-window ()
  "Set a window for other-window.  To set the target window,
        move point to the window, then call this.  Technically this sets
        all window dedicated but the target window.  Call again to free
        windows."
  (interactive)
  (walk-windows
   (lambda (win)
     (set-window-dedicated-p win (not window-locked-p))))
  (if window-locked-p
      (message "Windows are free")
    (set-window-dedicated-p (selected-window) nil)
    (message "Window is locked on"))
  (setq window-locked-p (not window-locked-p)))

(defun freeze-window ()
  "This freezes or locks a window where point is.  Call again to unfreeze.
        Technically this toggles window-dedicate-p property of a window."
  (interactive)
  (set-window-dedicated-p
   (selected-window)
   (not (window-dedicated-p (selected-window))))
  (if (window-dedicated-p (selected-window))
      (message (format "%s is freezed" (selected-window)))
    (message (format "%s is free" (selected-window)))))

(defun slide-window ()
  "This slides a buffer in a window where point is to
        next-window.  Technically this recalls previous windows-set by
        winnder-undo then shows the buffer to next-window"
  (interactive)
  (let ((buf (buffer-name)))
    (winner-undo)freeze-window
    (set-window-buffer (next-window) buf)
    ;; (switch-to-buffer buf)
    (message "Window is slided")
    (other-window 1)))

;;; Send e-mail via gmail.com (C-x m)
(setq
 user-full-name "xaos52"
 user-mail-address "xaos52@gmail.com")

(setq
 smtpmail-auth-credentials '(("smtp.gmail.com" 587 "xaos52@gmail.com" nil))
 smtpmail-smtp-server "smtp.gmail.com"
 smtpmail-smtp-service 587)
(require 'smtpmail)

;; No limit to number of lines in *Messages*
(setq message-log-max t)

;; loading of the preferred color-theme signals end of init
;; Loads theme for the initial frame
(load-theme 'wombat t)

;; switch to fullscreen after loading the color-theme
;; load a dark theme here if you plan on using another dark theme
;; to prevent excessive flickering when changing theme.
(set-frame-parameter nil 'fullscreen 'fullboth)

;;; auto-dim-other-buffers

;; Does not work well with more than 2 windows visible

;; (add-hook 'after-init-hook (lambda ()
;;   (when (fboundp 'auto-dim-other-buffers-mode)
;;     (auto-dim-other-buffers-mode t))))

;;; Fast load a file (C-x r j char)
;; contrib by SachaChua

(dolist
    (r `((?h (file . "~/"))             ;home
         (?i (file . ,(concat user-emacs-directory "init.el")))
         (?I (file . ,(let* ((user user-login-name)
                             (org (expand-file-name (concat user ".org") user-emacs-directory))
                             (el  (expand-file-name (concat user ".el") user-emacs-directory))
                             (dir (expand-file-name user user-emacs-directory)))
                        (cond
                         ((file-exists-p org) org)
                         ((file-exists-p el)  el)
                         (t dir)))))
         (?r (file . ,(concat user-emacs-directory "starter-kit-registers.org")))
         (?s (file . ,(concat user-emacs-directory "starter-kit.org")))
         ;; (?t (file . "~/tmp/today"))
         (?t (file . "~/"))
         (?x (file . ,(concat user-emacs-directory "xaos52.org")))))
  (set-register (car r) (cadr r)))

;; Push total init time to my-elapse-log and report it
(run-with-idle-timer
 0 nil
 ;; Run these after initialisation
 (lambda ()
   ;; Preferred theme and screen layout
   (load-theme 'planet t)
   (x52-screen-layout)))

(defun get-previous-indentation ()
  "Get the column of the previous indented line"
  (interactive)
  (save-excursion
    (progn
      (move-beginning-of-line nil)
          (skip-chars-backward "\n \t")
      (back-to-indentation))
    (current-column)))

(defun get-current-indentation ()
  "Return column at current indentation"
  (interactive)
  (save-excursion
    (progn
      (back-to-indentation)
      (current-column))))

(defun point-at-current-indentation ()
  "Return point at current indentation"
  (interactive)
  (save-excursion
    (progn
          (move-to-column (get-current-indentation))
      (point))))

(defun point-at-column-on-line (col)
  "Returns the point at `col` on the current line"
  (interactive)
  (save-excursion
    (progn
      (move-to-column col)
      (point))))

(defun ig-move-line-to-column (col)
  "Move the line to col; fill with all spaces if moveing forward"
 (interactive "p")
  (let ((point-at-cur-indent (point-at-current-indentation))
                (col-at-cur-indent (get-current-indentation)))
    (cond (
                   (= col 0)
                   ;; delete to beginning of line or do nothing
                   (if (= col-at-cur-indent 0)
                           nil
                         (delete-region point-at-cur-indent (point-at-column-on-line 0))))
                  (
                          (< col col-at-cur-indent)
                          ;; delete from our current point BACK to col
                          (delete-region (point-at-column-on-line col) point-at-cur-indent))
                  (
                   (> col col-at-cur-indent)
                   ;; delete all text from indent to beginning of line
                   (progn
                         (delete-region point-at-cur-indent (point-at-column-on-line 0))
                         (move-beginning-of-line nil)
                         ;; add spaces forward
                         (insert-string (make-string col ?\s)))))))

(defun ig-indent-sql ()
  "Indent by `tab-width` at most 1 time greater than the previously indented line otherwise go to the beginning of the line indent forward by `tab-width`"
  (let ((previous (get-previous-indentation))
        (current (get-current-indentation)))
    (cond ( ;; exactly at previous line's indentation
           (= previous current)
                   (ig-move-line-to-column (+ current tab-width)))

          ( ;; current is greater than previous
           (> current previous)
            ;; exactly at one indentation forward from previous lines indent
           (if (= tab-width (- current previous))
                ;; move line to beginning
               (ig-move-line-to-column 0)
             ;; go back to previous indentation level
             (ig-move-line-to-column previous)))

          (t
           (ig-move-line-to-column (+ current tab-width))))))

(defun transpose-windows (arg)
   "Transpose the buffers shown in two windows."
   (interactive "p")
   (let ((selector (if (>= arg 0) 'next-window 'previous-window)))
     (while (/= arg 0)
       (let ((this-win (window-buffer))
             (next-win (window-buffer (funcall selector))))
         (set-window-buffer (selected-window) next-win)
         (set-window-buffer (funcall selector) this-win)
         (select-window (funcall selector)))
       (setq arg (if (plusp arg) (1- arg) (1+ arg))))))

(define-key ctl-x-4-map (kbd "t") 'transpose-windows)

(add-hook 'sql-mode-hook
          (function (lambda ()
                      (make-local-variable 'indent-line-function)
                      (setq indent-line-function 'ig-indent-sql))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("0744f61189c62ed6d1f8fa69f6883d5772fe8577310b09e623c62c040f208cd4" default)))
 '(dired-listing-switches "-al --group-directories-first")
 '(epg-debug t)
 '(etags-select-go-if-unambiguous t)
 '(etags-select-use-short-name-completion t)
 '(inhibit-startup-echo-area-message "xaos52")
 '(send-mail-function (quote smtpmail-send-it))
 '(session-use-package t nil (session)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(diff-file-header ((t (:background "grey60" :foreground "white smoke" :weight bold))))
 '(diff-function ((t (:inherit diff-header :foreground "white"))))
 '(diff-header ((t (:background "grey45" :foreground "gold"))))
 '(ediff-current-diff-C ((t (:background "white"))))
 '(ediff-fine-diff-B ((t (:background "#22aa22" :foreground "light gray"))))
 '(ediff-fine-diff-C ((t (:background "gainsboro")))))

(message "Loading init.el...done")
