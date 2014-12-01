;; -*- mode: emacs-lisp -*-
;; Simple .emacs configuration

;; ---------------------
;; -- Global Settings --
;; ---------------------
(add-to-list 'load-path "~/.emacs.d")
(require 'cl)
(require 'ido)
(require 'ffap)
(require 'uniquify)
(require 'ansi-color)
(require 'recentf)
(require 'linum)
(require 'smooth-scrolling)
(require 'whitespace)
(require 'dired-x)
(require 'compile)

;; melpa packages
(require 'package)
(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; auto-complete : package-install -> auto-complete
(require 'auto-complete-config)
(ac-config-default)

;; package-install -> expand-region
(require 'expand-region)

;; package-install -> projectile
(require 'projectile)

;; package-install -> helm-projectile
(require 'helm-projectile)

;; package-install -> undo-tree
(require 'undo-tree)
(global-undo-tree-mode 1)


(menu-bar-mode -1)
(normal-erase-is-backspace-mode 0)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(setq column-number-mode t)
(setq inhibit-startup-message t)
(setq save-abbrevs nil)
(setq show-trailing-whitespace t)
(setq suggest-key-bindings t)
(setq vc-follow-symlinks t)

;; flx-ido mode   package-install -> flx-ido
(ido-mode t)
(ido-everywhere 1)
(flx-ido-mode 1)
(setq ido-use-face nil)

;; projectile
(projectile-global-mode)

;; --------------------------
;; -- indentation settings --
;; --------------------------
(electric-indent-mode 1)

;;; Indentation for python

;; Ignoring electric indentation
(defun electric-indent-ignore-python (char)
  "Ignore electric indentation for python-mode"
  (if (equal major-mode 'python-mode)
      `no-indent'
    nil))
(add-hook 'electric-indent-functions 'electric-indent-ignore-python)

;; Enter key executes newline-and-indent
(defun set-newline-and-indent ()
  "Map the return key with `newline-and-indent'"
  (local-set-key (kbd "RET") 'newline-and-indent))
(add-hook 'python-mode-hook 'set-newline-and-indent)





;; -------------------------
;; -- theme customization --
;; -------------------------
(when (not (window-system))
  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(default ((t (:inherit autoface-default :strike-through nil :underline nil :slant normal :weight normal :height 120 :width normal :family "monaco"))))
   '(column-marker-1 ((t (:background "red"))) t)
   '(diff-added ((t (:foreground "cyan"))) t)
   '(flymake-errline ((((class color) (background light)) (:background "Red"))) t)
   '(font-lock-comment-face ((((class color) (min-colors 8) (background light)) (:foreground "red"))))
   '(fundamental-mode-default ((t (:inherit default))) t)
   '(highlight ((((class color) (min-colors 8)) (:background "white" :foreground "magenta"))))
   '(isearch ((((class color) (min-colors 8)) (:background "yellow" :foreground "black"))))
   '(linum ((t (:foreground "black" :weight bold))))
   '(region ((((class color) (min-colors 8)) (:background "white" :foreground "magenta"))))
   '(secondary-selection ((((class color) (min-colors 8)) (:background "gray" :foreground "cyan"))))
   '(show-paren-match ((((class color) (background light)) (:background "black"))) t)
   '(vertical-border ((t nil))))

  (set-face-foreground 'font-lock-comment-face "red"))


;; package-install -> color-theme-sanityinc-tomorrow
(when (window-system)
  (custom-set-variables
   ;; custom-set-variables was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(ansi-color-faces-vector [default bold shadow italic underline bold bold-italic bold])
   '(ansi-color-names-vector (vector "#cccccc" "#f2777a" "#99cc99" "#ffcc66" "#6699cc" "#cc99cc" "#66cccc" "#2d2d2d"))
   '(custom-enabled-themes (quote (sanityinc-tomorrow-bright)))
   '(custom-safe-themes (quote ("1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" default)))
   '(fci-rule-color "#515151")
   '(vc-annotate-background nil)
   '(vc-annotate-color-map (quote ((20 . "#f2777a") (40 . "#f99157") (60 . "#ffcc66") (80 . "#99cc99") (100 . "#66cccc") (120 . "#6699cc") (140 . "#cc99cc") (160 . "#f2777a") (180 . "#f99157") (200 . "#ffcc66") (220 . "#99cc99") (240 . "#66cccc") (260 . "#6699cc") (280 . "#cc99cc") (300 . "#f2777a") (320 . "#f99157") (340 . "#ffcc66") (360 . "#99cc99"))))
   '(vc-annotate-very-old-color nil))
  (show-paren-mode t)
  (set-frame-font "DejaVu Sans Mono-10")
  (scroll-bar-mode -1)
  (tool-bar-mode -1))






;; ------------
;; -- Macros --
;; ------------
(load "defuns-config.el")
(fset 'align-equals "\C-[xalign-regex\C-m=\C-m")
(global-set-key "\M-=" 'align-equals)
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c;" 'comment-or-uncomment-region)
(global-set-key "\M-n" 'next5)
(global-set-key "\M-p" 'prev5)
(global-set-key "\M-o" 'other-window)
(global-set-key "\M-i" 'back-window)
(global-set-key "\C-z" 'zap-to-char)
(global-set-key "\C-h" 'backward-delete-char)
(global-set-key "\M-d" 'delete-word)
(global-set-key "\M-h" 'backward-delete-word)
(global-set-key "\M-u" 'zap-to-char)

(global-set-key (kbd "C-=") 'er/expand-region) 
(global-set-key (kbd "C-;") 'iedit-mode)
(global-set-key (kbd "C-c SPC") 'ace-jump-mode)  ;; package-install -> ace-jump-mode
(global-set-key (kbd "M-l") 'select-current-line) ;; defined in defun-config.el
(global-set-key (kbd "C-S-y") 'duplicate-current-line-or-region) ;; defined in defun-config.el

;; ---------------------------
;; -- JS Mode configuration --
;; ---------------------------
(load "js-config.el")
(add-to-list 'load-path "~/.emacs.d/jade-mode") ;; github.com/brianc/jade-mode
(require 'sws-mode)
(require 'jade-mode)
(require 'js2-mode) ;; package-install -> js2-mode
(add-to-list 'auto-mode-alist '("\\.styl$" . sws-mode))
(add-to-list 'auto-mode-alist '("\\.jade$" . jade-mode))

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode)) 


(defun beautify-json ()
  (interactive)
  (let ((b (if mark-active (min (point) (mark)) (point-min)))
        (e (if mark-active (max (point) (mark)) (point-max))))
    (shell-command-on-region b e
     "python -mjson.tool" (current-buffer) t)))




;; -------------------------------
;; -- Python Mode configuration --
;; -------------------------------
(require 'python)
(setq
 python-shell-interpreter "ipython"
 python-shell-interpreter-args ""
 python-shell-prompt-regexp "In \\[[0-9]+\\]: "
 python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
 python-shell-completion-setup-code
   "from IPython.core.completerlib import module_completion"
 python-shell-completion-module-string-code
   "';'.join(module_completion('''%s'''))\n"
 python-shell-completion-string-code
   "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")


