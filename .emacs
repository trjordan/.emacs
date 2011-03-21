;;;
;;; TR's .emacs file
;;;

;; Bind some keys for me
(global-set-key "\C-x\C-b" 'ibuffer)
(global-set-key "\C-cs" 'shell)
(global-set-key "\C-cg" 'goto-line)
(global-set-key "\C-ca" 'align-regexp)
(global-set-key "\C-cf" 'vc-git-grep)
(global-set-key "\C-cq" 'query-replace-regexp)
(global-set-key "\C-ci" 'indent-region)
(global-set-key "\C-cn" 'flymake-goto-next-error)
(global-set-key "\C-cp" 'flymake-goto-prev-error)
(global-set-key "\C-cc" 'comment-region)
(global-set-key "\C-cu" 'uncomment-region)
(global-set-key "\C-c\C-o" 'c-set-offset)
(global-set-key "\C-cr" (lambda () (interactive) (revert-buffer t t)))

;; Set up my path
(setq load-path (append (list "~/.emacs.d") load-path))

;; Turn of the menu bars
(menu-bar-mode nil)
;(tool-bar-mode nil)

(set-face-attribute 'default nil :height 90)

;; Open the main projects dir
(find-file "~/repos")

;; Spaces, not tabs!
(setq c-basic-offset 4)

;; Load in any other modes I use frequently
(autoload 'php-mode "php-mode" "Major mode for editing php code." t)
(load "dired-x")
(require 'ibuffer-git)
(require 'flymake-jslint)
;(remove-hook 'js-mode-hook
;          (lambda () (if (< (buffer-size) (* 250 1024))
;                         (flymake-mode nil))))



(load "flymake-cursor.el")
(load "~/.emacs.d/nxhtml/autostart.el")
(require 'ourcomments-widgets)

;; Change some file associations
(add-to-list 'auto-mode-alist '("\\.tpl\\'" . html-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'" . php-mode))
(add-to-list 'auto-mode-alist '("\\.inc\\'" . php-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . django-nxhtml-mumamo-mode))

;; Enable backup files to a specific hidden directory, keeping the
;; default number of versions
(setq make-backup-files nil)
;(setq version-control t)
;(setq backup-directory-alist (quote ((".*" . "~/.emacs_backups/"))))

; Set my indentation rules

(defun my-offsets ()
  (c-set-offset 'arglist-intro '+)
  (c-set-offset 'arglist-close 0)
  (c-set-offset 'case-label '+))
(add-hook 'java-mode-hook 'my-offsets)
(add-hook 'php-mode-hook 'my-offsets)

;; End of file.
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(browse-url-browser-function (quote browse-url-default-windows-browser))
 '(dired-omit-files "^\\.?#\\|^\\.$\\|^\\.\\.$\\|\\(^\\..*\\)")
 '(dired-recursive-copies (quote always))
 '(dired-recursive-deletes (quote always))
 '(dvc-tips-enabled nil)
 '(fill-column 80)
 '(flymake-gui-warnings-enabled nil)
 '(gdb-use-separate-io-buffer t)
 '(grep-find-command "find . -wholename '*.min.js' -prune -o -type f -print0 | xargs -0 -e grep -nH -e ")
 '(ibuffer-deletion-char 68)
 '(ibuffer-expert t)
 '(ibuffer-formats (quote ((mark modified read-only " " (name 18 18 :left :elide) " " (size 9 -1 :right) " " (mode 16 16 :left :elide) " " (git-status 8 8 :left) " " filename-and-process) (mark " " (name 16 -1) " " filename))))
 '(ibuffer-git-column-length 8)
 '(indent-tabs-mode nil)
 '(js-expr-indent-offset 4)
 '(kill-whole-line t)
 '(matlab-indent-level 4)
 '(matlab-keyword-list (quote ("global" "persistent" "for" "while" "if" "elseif" "else" "endfunction" "return" "break" "continue" "switch" "case" "otherwise" "try" "catch" "tic" "toc" "Warning" "classdef" "properties" "methods")))
 '(matlab-mode-install-path (quote ("/usr/matlab/bin/toolbox/")))
 '(matlab-shell-command "/usr/matlab/bin/matlab")
 '(matlab-shell-command-switches (quote ("-nodesktop" "-nosplash")))
 '(matlab-shell-enable-gud-flag t)
 '(matlab-shell-input-ring-size 128)
 '(matlab-shell-logo "/usr/share/emacs/22.1/etc/matlab.xpm")
 '(matlab-shell-mode-hook nil)
 '(mumamo-major-modes (quote ((asp-js-mode js-mode javascript-mode espresso-mode ecmascript-mode) (asp-vb-mode visual-basic-mode) (javascript-mode js-mode javascript-mode espresso-mode ecmascript-mode) (java-mode jde-mode java-mode) (groovy-mode groovy-mode) (nxhtml-mode nxhtml-mode html-mode))))
 '(org-level-color-stars-only t)
 '(python-default-interpreter (quote cpython))
 '(python-guess-indent t)
 '(python-python-command "python")
 '(safe-local-variable-values (quote ((c-hanging-comment-ender-p))))
 '(scroll-bar-mode nil)
 '(sort-fold-case t t)
 '(transient-mark-mode t)
 '(x-select-enable-clipboard t))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(mumamo-background-chunk-submode1 ((((class color) (min-colors 8)) nil)))
 '(mumamo-background-chunk-submode2 ((((class color) (min-colors 8)) nil))))
 

(put 'upcase-region 'disabled nil)

(put 'scroll-left 'disabled nil)

(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
