
(defvar starter-kit-packages (list 
                              ;; 'ack-and-a-half
                              'auto-complete
                              'autopair
                              'browse-kill-ring
                              ;; 'cdlatex
                              ;; 'css-mode
                              'exec-path-from-shell
                              'expand-region
                              ;; 'gist
                              'hl-line+
                              ;; 'idle-highlight
                              ;; 'inf-ruby
                              ;; 'latex-pretty-symbols
                              ;; 'mac-key-mode
                              ;; 'magit
                              ;; 'markdown-mode
                              'maxframe
                              ;; 'multiple-cursors
                              'paredit
                              ;; 'python
                              'redo+
                              ;; 'ruby-mode
                              's
                              'smex
                              ;; 'solarized-theme
                              ;; 'tango-2-theme
                              ;; 'textmate
                              ;; 'typopunct
                              ;; 'yaml-mode
                              'yasnippet
                              ;; 'r-autoyas
                              ;; 'zenburn-theme
                              ;; 'auctex
                              ;; 'ess
                              )
  "Libraries that should be installed by default.")

(defun starter-kit-elpa-install ()
  "Install all starter-kit packages that aren't installed."
  (interactive)
  (dolist (package starter-kit-packages)
    (unless (or (member package package-activated-list)
                (functionp package))
      (message "Installing %s" (symbol-name package))
      (package-install package))))

(defun esk-online? ()
  "See if we're online.

Windows does not have the network-interface-list function, so we
just have to assume it's online."
  ;; TODO how could this work on Windows?
  (if (and (functionp 'network-interface-list)
           (network-interface-list))
      (some (lambda (iface) (unless (equal "lo" (car iface))
                         (member 'up (first (last (network-interface-info
                                                   (car iface)))))))
            (network-interface-list))
    t))

(when (esk-online?)
  (unless package-archive-contents (package-refresh-contents))
  (starter-kit-elpa-install))

(when (memq window-system '(mac ns))
(exec-path-from-shell-initialize))
