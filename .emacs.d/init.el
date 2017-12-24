;; -*- mode: emacs-lisp -*-
;; Simple .emacs configuration

;; ---------------------
;; -- Global Settings --
;; ---------------------
(add-to-list 'load-path "~/.emacs.d/lisp")
(setenv "LANG" "en_US.UTF-8")

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



(require 'cask "~/.cask/cask.el")
(cask-initialize)
;; (require 'package) ;; melpa packages
;; (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
;; (package-initialize)


(require 'multiple-cursors)
(require 'web-mode)


(ac-config-default) ;; package: auto-complete
(global-undo-tree-mode 1) ;;package: undo-tree

;; disable vc-git
(setq vc-handled-backends ())


;; set $PATH for shell
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))


;; (menu-bar-mode -1)
(normal-erase-is-backspace-mode 0)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(setq column-number-mode t)
(setq inhibit-startup-message t)
(setq save-abbrevs nil)
(setq show-trailing-whitespace t)
(setq suggest-key-bindings t)
(setq vc-follow-symlinks t)

;; flx-ido mode
(ido-mode t)
(ido-everywhere 1)
(flx-ido-mode 1)
(setq ido-use-face nil)

;; projectile
(projectile-global-mode)



;; Switch option and command in mac
(when (eq system-type 'darwin) ;; mac specific settings
  (setq mac-option-modifier 'super)
  (setq mac-command-modifier 'meta)
  (global-set-key [kp-delete] 'delete-char) ;; sets fn-delete to be right-delete
  )



;; --------------------------
;; -- indentation settings --
;; --------------------------
(setq-default tab-width 4)
(electric-indent-mode 1)




;; ----------------------------
;; -- unicode configurations --
;; ----------------------------
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))




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

  (set-face-foreground 'font-lock-comment-face "red")
  (global-set-key (kbd "C-c =") 'er/expand-region)
  (global-set-key (kbd "C-c i") 'iedit-mode)
  (global-set-key (kbd "C-c y") 'duplicate-current-line-or-region)
  (global-set-key (kbd "C-c >") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-c <") 'mc/mark-previous-like-this) )



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
  (set-face-attribute 'default nil
                    :family "Monaco"
                    :height 160)
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
(global-set-key (kbd "C-S-e") 'eval-and-replace) ;; defined in defun-config.el

;; multiple-cursors
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-unset-key (kbd "M-<down-mouse-1>"))
(global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click)


;; ---------------------------
;; -- JS Mode configuration --
;; ---------------------------
(load "js-config.el")
(add-to-list 'load-path "~/.emacs.d/jade-mode") ;; github.com/brianc/jade-mode
(require 'sws-mode)
(require 'jade-mode)
(require 'js2-mode) ;; package-install -> js2-mode
(require 'js-comint) ;; package-install -> js-comint


(add-to-list 'auto-mode-alist '("\\.styl$" . sws-mode))
(add-to-list 'auto-mode-alist '("\\.jade$" . jade-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode)) 


(defun beautify-json ()
  (interactive)
  (let ((b (if mark-active (min (point) (mark)) (point-min)))
        (e (if mark-active (max (point) (mark)) (point-max))))
    (shell-command-on-region b e
     "python -mjson.tool" (current-buffer) t)))

(setq inferior-js-program-command "node")
(setq inferior-js-program-arguments '("--interactive"))


(add-hook 'js2-mode-hook '(lambda ()
			    (local-set-key "\C-x\C-e" 'js-send-last-sexp)			    
			    (local-set-key "\C-x\C-r" 'js-send-region-and-go)			    
			    (local-set-key "\C-\M-x" 'js-send-last-sexp-and-go)
			    (local-set-key "\C-cb" 'js-send-buffer)
			    (local-set-key "\C-c\C-b" 'js-send-buffer-and-go)
			    (local-set-key "\C-cl" 'js-load-file-and-go)
			    ))




;; ------------------------------
;; -- Typescript configuration --
;; ------------------------------


(defun indent-or-complete ()
    (interactive)
    (if (looking-at "\\_>")
        (company-complete-common)
      (if (use-region-p)
		  (indent-region (region-beginning) (region-end))
		(indent-according-to-mode))))

(add-hook 'typescript-mode-hook
          (lambda ()
            (tide-setup)
            (flycheck-mode +1)
            (setq flycheck-check-syntax-automatically '(save idle-change mode-enabled))
            (eldoc-mode +1)
			(company-mode)
			(setq company-minimum-prefix-length 2)
			(setq company-idle-delay 0)
			(setq company-dabbrev-downcase nil)
			(local-set-key [tab] 'indent-or-complete)))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; to use 2.0+ ts compiler
(setq tide-tsserver-executable "/usr/local/lib/node_modules/typescript/bin/tsserver")



;; Tide can be used along with web-mode to edit tsx files
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (tide-setup)
              (flycheck-mode +1)
              (setq flycheck-check-syntax-automatically '(save idle-change mode-enabled))
              (eldoc-mode +1)
			  (company-mode)
			  (setq company-minimum-prefix-length 2)
			  (setq company-idle-delay 0)
			  (setq company-dabbrev-downcase nil)
			  (local-set-key [tab] 'indent-or-complete))))

(add-hook
 'web-mode-hook
 '(lambda ()
	(local-set-key "\M-j" 'comment-indent-new-line)))



;; -------------------------------
;; -- Python Mode configuration --
;; ------------------------------

(elpy-enable)
;;(elpy-use-ipython)
(elpy-use-ipython "ipython3") ;; Do 'M-x elpy-use-ipython' when python 2 used

(setq elpy-rpc-backend "jedi")

(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

(defun python-settings-hook ()
  (setq
   python-shell-interpreter "ipython3"
   python-shell-interpreter-args "--simple-prompt -i"
   python-shell-prompt-regexp "In \\[[0-9]+\\]: "
   python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
   python-shell-completion-setup-code
   "from IPython.core.completerlib import module_completion"
   python-shell-completion-module-string-code
   "';'.join(module_completion('''%s'''))\n"
   python-shell-completion-string-code
   "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")
  (setq tab-width 4)
  (setq python-indent 4)
  (setq indent-tabs-mode nil))

(add-hook 'elpy-mode-hook 'python-settings-hook)





;; -----------------------------
;; -- HTML Mode configuration --
;; -----------------------------
(add-hook 'sgml-mode-hook
  (lambda ()
    ;; Default indentation is usually 2 spaces, changing to 4.
    (set (make-local-variable 'sgml-basic-offset) 4)))




;; -----------------------------
;; -- SCSS Mode configuration --
;; -----------------------------
(defun configure-auto-complete-for-scss ()
  (add-to-list 'ac-sources 'ac-source-css-property))
(add-hook 'scss-mode-hook 'configure-auto-complete-for-scss)
(add-to-list 'ac-modes 'scss-mode)
(autoload 'scss-mode "scss-mode")
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))


;; inline comment as default
(add-hook 'scss-mode-hook
          (lambda ()
            (set (make-local-variable 'comment-start) "//")
            (set (make-local-variable 'comment-end) "")))




;; hook emmet mode in sgml and css

(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.
(add-hook 'scss-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.
(add-hook 'web-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.



;; -----------------------------
;; -- Web Mode configuration --
;; -----------------------------
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ejs\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))


(setq web-mode-engines-alist
      '(("php"    . "\\.phtml\\'")
        ("blade"  . "\\.blade\\."))
)



(setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'"))) ;; jsx mode on js files

(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 4)
  (setq web-mode-code-indent-offset 4)
  (setq web-mode-enable-auto-quoting nil))
(add-hook 'web-mode-hook  'my-web-mode-hook)

(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
      (let ((web-mode-enable-part-face nil))
        ad-do-it)
    ad-do-it))

;; (flycheck-define-checker jsxhint-checker
;;   "A JSX syntax and style checker based on JSXHint."

;;   :command ("jsxhint" source)
;;   :error-patterns
;;   ((error line-start (1+ nonl) ": line " line ", col " column ", " (message) line-end))
;;   :modes (web-mode))
;; (add-hook 'web-mode-hook
;;           (lambda ()
;;             (when (equal web-mode-content-type "jsx")
;;               ;; enable flycheck
;;               (flycheck-select-checker 'jsxhint-checker)
;;               (flycheck-mode))))



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#cccccc" "#f2777a" "#99cc99" "#ffcc66" "#6699cc" "#cc99cc" "#66cccc" "#2d2d2d"))
 '(custom-enabled-themes (quote (sanityinc-tomorrow-bright)))
 '(custom-safe-themes
   (quote
	("1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" default)))
 '(fci-rule-color "#515151")
 '(package-selected-packages
   (quote
	(web-mode undo-tree tide scss-mode neotree multiple-cursors js2-mode js-comint iedit highlight-indent-guides helm-projectile flx-ido expand-region emmet-mode company color-theme-sanityinc-tomorrow cask bind-key auto-complete ace-jump-mode)))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
	((20 . "#f2777a")
	 (40 . "#f99157")
	 (60 . "#ffcc66")
	 (80 . "#99cc99")
	 (100 . "#66cccc")
	 (120 . "#6699cc")
	 (140 . "#cc99cc")
	 (160 . "#f2777a")
	 (180 . "#f99157")
	 (200 . "#ffcc66")
	 (220 . "#99cc99")
	 (240 . "#66cccc")
	 (260 . "#6699cc")
	 (280 . "#cc99cc")
	 (300 . "#f2777a")
	 (320 . "#f99157")
	 (340 . "#ffcc66")
	 (360 . "#99cc99"))))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
