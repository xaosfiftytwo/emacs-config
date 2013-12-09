;;; init.el --- Where all the magic begins
;;
;; Part of the Emacs Starter Kit
;;
;; This is the first thing to get loaded.
;;

(setq dotfiles-dir (file-name-directory (or load-file-name (buffer-file-name))))

;; Common Lisp compatability
(require 'cl-lib)

;; do not call package-initialize while processing the startup.el file
;; when the variable is non-nil package-initialize is called at startup.el
;; and ~/.emacs.d/elpa subdirs are added to load-path
(setq package-enable-at-startup nil)

;; Emacs default font specified in $HOME/.Xresources

;; load the starter kit from the `after-init-hook'
(add-hook 'after-init-hook
          `(lambda ()
            (setq starter-kit-dir
                  ,(file-name-directory (or load-file-name (buffer-file-name))))
            (require 'org)
            (org-babel-load-file (expand-file-name "starter-kit.org" starter-kit-dir))))
;; end of init.el

;; enable commands that are disabled by default
(put 'upcase-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
