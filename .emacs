(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell 
      (replace-regexp-in-string "[[:space:]\n]*$" "" 
        (shell-command-to-string "$SHELL -l -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))
(when (equal system-type 'darwin) (set-exec-path-from-shell-PATH))

(require 'package)
(add-to-list 'package-archives 
	     '("marmalade" .
	       "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;; Set default load path
(add-to-list 'load-path "~/.emacs.d")

(require 'cl)

(defvar prelude-packages
  '(coffee-mode haml-mode inf-ruby auto-complete color-theme-sanityinc-tomorrow
                magit markdown-mode python flymake-ruby ruby-end
                sass-mode solarized-theme
                flymake-haml flymake-coffee
                volatile-highlights yaml-mode yari zenburn-theme)
  "A list of packages to ensure are installed at launch.")

(defun prelude-packages-installed-p ()
  (loop for p in prelude-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

(unless (prelude-packages-installed-p)
  ;; check for new packages (package versions)
  (message "%s" "Emacs Prelude is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; install the missing packages
  (dolist (p prelude-packages)
    (when (not (package-installed-p p))
      (package-install p))))

(provide 'prelude-packages)
;;; prelude-packages.el ends here

;; set language
;;(set-language-environment 'utf-8)
;;(prefer-coding-system 'utf-8)
(if (eq system-type 'darwin)
    (create-fontset-from-fontset-spec
     "-apple-bitstream vera sans mono-medium-r-normal--12-*-*-*-*-*-fontset-mymonaco,
ascii:-apple-Monaco-medium-normal-normal-*-12-*-*-*-m-0-iso10646-1,
chinese-gb2312:-apple-STHeiti-medium-normal-normal-12-*-*-*-*-p-0-iso10646-1,
latin-iso8859-1:-apple-Monaco-medium-normal-normal-*-12-*-*-*-m-0-iso10646-1,
mule-unicode-0100-24ff:-apple-Monaco-medium-normal-normal-*-12-*-*-*-m-0-iso10646-1")

  (setq default-frame-alist (append '((font . "fontset-mymonaco")) default-frame-alist))
  (set-default-font "fontset-mymonaco")

  )


(setq make-backup-files nil)
(setq auto-save-default nil)
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)
(setq inhibit-startup-message t)
(fset 'yes-or-no-p 'y-or-n-p)
(delete-selection-mode t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(blink-cursor-mode t)
(show-paren-mode t)
(column-number-mode t)
(set-fringe-style -1)
(tooltip-mode -1)

(require 'ido)
(ido-mode t)

;; color-theme
;(require 'color-theme)
;(require 'color-theme-sanityinc-tomorrow)
;(color-theme-initialize)
;(color-theme-tty-dark)
;(color-theme-github)
;(load-theme 'solarized-dark t)
;(load-theme 'sunny-day)

;; hot key
(global-set-key (kbd "C-c C-g") 'goto-line)
(define-key global-map (kbd "RET") 'newline-and-indent)
(define-key global-map (kbd "C-c C-c") 'comment-region)
(define-key global-map (kbd "C-2") 'set-mark-command)
(define-key global-map (kbd "C-x C-j") 'dired-jump)

;; auto-complete
(require 'auto-complete-config)
(ac-config-default)

;; org-mode
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(add-to-list 'auto-mode-alist '("\TODO$" . org-mode))

;; auto-complete
(require 'auto-complete-config)
(ac-config-default)

;; eshell
(defun m-eshell-hook ()
  ; define control p, control n and the up/down arrow
  (define-key eshell-mode-map [(control p)] 'eshell-previous-matching-input-from-input) 				
  (define-key eshell-mode-map [(control n)] 'eshell-next-matching-input-from-input)
  
  (define-key eshell-mode-map [up] 'previous-line)
  (define-key eshell-mode-map [down] 'next-line)
  )

(add-hook 'eshell-mode-hook 'm-eshell-hook)

;;(require 'pearl-mode)
;;(add-to-list 'auto-mode-alist '("\\.pearl\\'" . pearl-mode))

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.sls$" . yaml-mode))


;; Ruby
(require 'ruby-mode)
(require 'flymake-ruby)
(add-hook 'ruby-mode-hook 'flymake-ruby-load)

(add-to-list 'auto-mode-alist '("\\.\\(rb\\|ru\\|builder\\|rake\\|thor\\|gemspec\\)\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\(rake\\|thor\\|guard\\|gem\\|cap\\|vagrant\\)file\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.fevoi$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rules$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.specs$" . ruby-mode))

;; (require 'ruby-electric)
;; (add-hook 'ruby-mode-hook (lambda () (ruby-electric-mode t)))

					; support newline-and-indent
(add-hook 'ruby-mode-hook (lambda () (local-set-key "\r" 'newline-and-indent)))

;; all customized lisps
(add-to-list 'load-path "~/.emacs.d/lisp")
;(require 'pac-mode)


;; (defun ruby-mode-hook ()
;;   (autoload 'ruby-mode "ruby-mode" nil t)
;;   (add-to-list 'auto-mode-alist '("Capfile" . ruby-mode))
;;   (add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
;;   (add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))
;;   (add-to-list 'auto-mode-alist '("\\.rake\\'" . ruby-mode))
;;   (add-to-list 'auto-mode-alist '("\\.rb\\'" . ruby-mode))
;;   (add-to-list 'auto-mode-alist '("\\.ru\\'" . ruby-mode))
;;   (add-hook 'ruby-mode-hook '(lambda ()
;; 			       (setq ruby-deep-arglist t)
;; 			       (setq ruby-deep-indent-paren nil)
;; 			       (setq c-tab-always-indent nil)
;; 			       (require 'inf-ruby)
;; 			       (require 'ruby-compilation))))
;; (defun rhtml-mode-hook ()
;;   (autoload 'rhtml-mode "rhtml-mode" nil t)
;;   (add-to-list 'auto-mode-alist '("\\.erb\\'" . rhtml-mode))
;;   (add-to-list 'auto-mode-alist '("\\.rjs\\'" . rhtml-mode))
;;   (add-hook 'rhtml-mode '(lambda ()
;; 			   (define-key rhtml-mode-map (kbd "M-s") 'save-buffer))))
;; (defun yaml-mode-hook ()
;;   (autoload 'yaml-mode "yaml-mode" nil t)
;;   (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
;;   (add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode)))
;; (defun css-mode-hook ()
;;   (autoload 'css-mode "css-mode" nil t)
;;   (add-hook 'css-mode-hook '(lambda ()
;; 			      (setq css-indent-level 2)
;; 			      (setq css-indent-offset 2))))
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(ansi-color-faces-vector [default bold shadow italic underline bold bold-italic bold])
;;  '(ansi-color-names-vector (vector "#4d4d4c" "#c82829" "#718c00" "#eab700" "#4271ae" "#8959a8" "#3e999f" "#ffffff"))
;;  '(custom-enabled-themes (quote (sanityinc-tomorrow-blue)))
;;  '(custom-safe-themes (quote ("82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "3eaa11afcc41357f889ba76f9f985c07a557536dafe0554c1f226b7a71848118" default)))
;;  '(fci-rule-color "#efefef")
;;  '(vc-annotate-background nil)
;;  '(vc-annotate-color-map (quote ((20 . "#c82829") (40 . "#f5871f") (60 . "#eab700") (80 . "#718c00") (100 . "#3e999f") (120 . "#4271ae") (140 . "#8959a8") (160 . "#c82829") (180 . "#f5871f") (200 . "#eab700") (220 . "#718c00") (240 . "#3e999f") (260 . "#4271ae") (280 . "#8959a8") (300 . "#c82829") (320 . "#f5871f") (340 . "#eab700") (360 . "#718c00"))))
;;  '(vc-annotate-very-old-color nil))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  )
;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(ansi-color-faces-vector [default bold shadow italic underline bold bold-italic bold])
;;  '(ansi-color-names-vector (vector "#4d4d4c" "#c82829" "#718c00" "#eab700" "#4271ae" "#8959a8" "#3e999f" "#ffffff"))
;;  '(custom-enabled-themes (quote (sanityinc-tomorrow-night)))
;;  '(custom-safe-themes (quote ("06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" default)))
;;  '(fci-rule-color "#efefef")
;;  '(vc-annotate-background nil)
;;  '(vc-annotate-color-map (quote ((20 . "#c82829") (40 . "#f5871f") (60 . "#eab700") (80 . "#718c00") (100 . "#3e999f") (120 . "#4271ae") (140 . "#8959a8") (160 . "#c82829") (180 . "#f5871f") (200 . "#eab700") (220 . "#718c00") (240 . "#3e999f") (260 . "#4271ae") (280 . "#8959a8") (300 . "#c82829") (320 . "#f5871f") (340 . "#eab700") (360 . "#718c00"))))
;;  '(vc-annotate-very-old-color nil))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  )
(add-hook 'haml-mode-hook
          (lambda ()
            (set (make-local-variable 'electric-indent-functions)
                 (list (lambda (arg) 'no-indent)))))

(server-start)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (sanityinc-tomorrow-day)))
 '(custom-safe-themes (quote ("bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
