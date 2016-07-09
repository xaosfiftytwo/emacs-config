
(add-to-list 'load-path
                 (expand-file-name  "yasnippet"
                                    (expand-file-name "elpa"
                                                      dotfiles-dir)))
  (require 'yasnippet)
;;  (yas/set-ac-modes)
;;  (yas/enable-emacs-lisp-paren-hack)
  (yas/global-mode 1)

(yas/load-directory (expand-file-name "snippets" dotfiles-dir))

(require 'auto-complete)
  (require 'auto-complete-config)
  (global-auto-complete-mode t)
  (add-to-list 'ac-dictionary-directories (expand-file-name "auto-complete" dotfiles-dir))
  (add-to-list 'ac-dictionary-directories "~/.emacs.d/local-autocomplete")
  (setq ac-modes (append ac-modes '(org-mode))) 
  (ac-config-default)
;;  (define-key ac-complete-mode-map [tab] 'ac-expand)
;;  (require 'ac-R) ;; in ~/.emacs.d/local-autocomplete
;; (setq ac-auto-start 4)
;; (ac-flyspell-workaround)
;; (define-key ac-mode-map (kbd "M-TAB") 'auto-complete)
;; (setq ac-auto-show-menu 0.8)
;; ;; 
;; (set-face-background 'ac-candidate-face "#366060")
;; (set-face-foreground 'ac-selection-face "#1f1f1f")
;; (set-face-background 'ac-selection-face "#8cd0d3")
;; (set-face-foreground 'ac-selection-face "#1f1f1f")
;;
