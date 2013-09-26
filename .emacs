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

(server-start)

;; set language
;;(set-language-environment 'utf-8)
;;(prefer-coding-system 'utf-8)

(create-fontset-from-fontset-spec
 "-apple-bitstream vera sans mono-medium-r-normal--12-*-*-*-*-*-fontset-mymonaco,
ascii:-apple-Monaco-medium-normal-normal-*-12-*-*-*-m-0-iso10646-1,
chinese-gb2312:-apple-STHeiti-medium-normal-normal-12-*-*-*-*-p-0-iso10646-1,
latin-iso8859-1:-apple-Monaco-medium-normal-normal-*-12-*-*-*-m-0-iso10646-1,
mule-unicode-0100-24ff:-apple-Monaco-medium-normal-normal-*-12-*-*-*-m-0-iso10646-1")

(setq default-frame-alist (append '((font . "fontset-mymonaco")) default-frame-alist))
(set-default-font "fontset-mymonaco")


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
(require 'color-theme)
(color-theme-initialize)
(color-theme-tty-dark)
;(color-theme-github)

;; hot key
(global-set-key (kbd "C-c C-g") 'goto-line)
(define-key global-map (kbd "RET") 'newline-and-indent)
(define-key global-map (kbd "C-c C-c") 'comment-region)
(define-key global-map (kbd "C-2") 'set-mark-command)

;; auto-complete
(require 'auto-complete-config)
(ac-config-default)

;; org-mode
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(add-to-list 'auto-mode-alist '("\TODO$" . org-mode))

;; eshell
(defun m-eshell-hook ()
  ; define control p, control n and the up/down arrow
  (define-key eshell-mode-map [(control p)] 'eshell-previous-matching-input-from-input) 				
  (define-key eshell-mode-map [(control n)] 'eshell-next-matching-input-from-input)
  
  (define-key eshell-mode-map [up] 'previous-line)
  (define-key eshell-mode-map [down] 'next-line)
  )

(add-hook 'eshell-mode-hook 'm-eshell-hook)

(require 'pearl-mode)
(add-to-list 'auto-mode-alist '("\\.pearl\\'" . pearl-mode))

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))


;; Ruby
(require 'ruby-mode)
(require 'flymake-ruby)
(add-hook 'ruby-mode-hook 'flymake-ruby-load)

(add-to-list 'auto-mode-alist '("\\.\\(rb\\|ru\\|builder\\|rake\\|thor\\|gemspec\\)\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\(rake\\|thor\\|guard\\|gem\\|cap\\|vagrant\\)file\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.fevoi$" . ruby-mode))

(require 'ruby-electric)
(add-hook 'ruby-mode-hook (lambda () (ruby-electric-mode t)))

					; support newline-and-indent
(add-hook 'ruby-mode-hook (lambda () (local-set-key "\r" 'newline-and-indent)))


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
