
;;  (autoload 'word-count-mode "word-count"
;;  "Minor mode to count words." t nil)
;;  (dolist (hook '(org-mode-hook
;;  markdown-mode-hook
;;  TeX-mode-hook
;;  text-mode-hook))
;;  (add-hook hook (lambda () (word-count-mode 1))))
;;  
;; (require 'wc)

(global-visual-line-mode t)
;;; prefer auto-fill to visual line wrap in ESS mode
(add-hook 'ess-mode-hook 'turn-on-auto-fill)
(add-hook 'inferior-ess-mode-hook 'turn-on-auto-fill) 

;;; but turn off auto-fill in tex and markdown
(add-hook 'markdown-mode-hook 'turn-off-auto-fill)
(add-hook 'latex-mode-hook 'turn-off-auto-fill)

;;; unfill paragraph
(defun unfill-paragraph ()
(interactive)
(let ((fill-column (point-max)))
(fill-paragraph nil)))
(global-set-key (kbd "<f6>") 'unfill-paragraph)

;; smooth-scrolling 
(require 'smooth-scrolling)

;; more smooth efforts.
(setq-default 
scroll-conservatively 0
scroll-up-aggressively 0.01
scroll-down-aggressively 0.01)

;; centered-cursor package in src/
;; (and
;;  (require 'centered-cursor-mode)
;;  (global-centered-cursor-mode +1))

(require 'autopair)
(autopair-global-mode) ;; enable autopair in all buffers 
(setq autopair-autowrap t)

(autoload 'markdown-mode "markdown-mode.el"
"Major mode for editing Markdown files" t)
(setq auto-mode-alist
(cons '("\\.Markdown" . markdown-mode) auto-mode-alist)
)
(setq auto-mode-alist
(cons '("\\.MarkDown" . markdown-mode) auto-mode-alist)
)
(setq auto-mode-alist
(cons '("\\.markdown" . markdown-mode) auto-mode-alist)
)
(setq auto-mode-alist
(cons '("\\.md" . markdown-mode) auto-mode-alist)
)
