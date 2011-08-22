;;;
;;; TR's .emacs file
;;;

(require 'desktop)
(desktop-save-mode 1)
(setq desktop-dirname "~/.emacs.d/")
(defun my-desktop-save ()
  (interactive)
  ;; Don't call desktop-save-in-desktop-dir, as it prints a message.
  (if (eq (desktop-owner) (emacs-pid))
      (desktop-save desktop-dirname)))
(add-hook 'auto-save-hook 'my-desktop-save)

;; Set up my path
(setq load-path (append (list "~/.emacs.d") load-path))

;; Turn of the menu bars
(menu-bar-mode nil)
(if (boundp 'tool-bar-mode) (tool-bar-mode nil))

(set-face-attribute 'default nil :height 75)

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

;; Add some places to the path
(if (< emacs-major-version 23)
    (add-to-list 'load-path "~/.emacs.d/nxml/"))

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

; Load my keyboard macros
(load "kbd-macros.el")

(defun my-offsets ()
  (c-set-offset 'arglist-intro '+)
  (c-set-offset 'arglist-close 0)
  (c-set-offset 'case-label '+))
(add-hook 'java-mode-hook 'my-offsets)
(add-hook 'php-mode-hook 'my-offsets)

;; Set up starting terms with programs in them
(defun djcb-term-start-or-switch (prg &optional use-existing name cmd)
  "* run program PRG in a terminal buffer. If USE-EXISTING is non-nil "
  " and PRG is already running, switch to that buffer instead of starting"
  " a new instance."
  (interactive)
  (let ((bufname (if name (concat "*" name "*") (concat "*" prg "*"))))
    (when (not (and use-existing
                 (let ((buf (get-buffer bufname)))
                   (and buf (buffer-name (switch-to-buffer bufname))))))
      (ansi-term prg (or name prg))
      (if cmd (process-send-string bufname (concat cmd "\n"))))))

(defmacro djcb-program-shortcut (key &optional fullname cmd)
  "* macro to create a key binding KEY to start some terminal program PRG; 
    if USE-EXISTING is true, try to switch to an existing buffer"
  `(global-set-key ,key 
     '(lambda()
        (interactive)
        (djcb-term-start-or-switch "bash" t ,fullname ,cmd))))

;; http://emacs-journey.blogspot.com/2011/02/proper-ansi-term-yankpaste.html
(defun my-term-paste (&optional string)
  (interactive)
  (process-send-string
   (get-buffer-process (current-buffer))
   (if string string (current-kill 0))))

(djcb-program-shortcut (kbd "<S-f8>") "shell" "tl")
(djcb-program-shortcut (kbd "<S-f2>") "paster-shell" "tl && paster shell development.ini")
(djcb-program-shortcut (kbd "<S-f3>") "paster-serve" "tl && paster serve development.ini --reload ")
(djcb-program-shortcut (kbd "<S-f4>") "mysql" "mysql tracelytics_mysql2test")
(djcb-program-shortcut (kbd "<S-f5>") "cassandra" "cassandra-cli --host 127.0.0.1" )
(djcb-program-shortcut (kbd "<S-f6>") "tf" "cd ~/repos/tracelons/transformer/etl && runtf")
(djcb-program-shortcut (kbd "<S-f7>") "etl" "cd ~/repos/tracelons/transformer/etl && runetl -B")
(djcb-program-shortcut (kbd "\C-cs") "shell" "cd ~/repos/tracelons/transformer/etl && runetl -B")
(djcb-program-shortcut (kbd "\C-cs") "shell" "tl")
(global-set-key "\C-cy" 'my-term-paste)

;; Fix the color in terminals
(setq term-default-bg-color nil)
(setq term-default-fg-color "#FFFFFF")
(load "colortheme.el")
(if (boundp 'color-theme-install) (color-theme-tr))

(load "sql-transform.el")
(add-hook 'sql-mode-hook
	  (function (lambda ()
		      (local-set-key "\M-q" 'sql-to-select))))

;; Don't be intelligent about how to split the window
(setq split-height-threshold nil)

;; Set me up a python IDE!
;;
;; Installations require the following:
;; ropemacs: pip install http://bitbucket.org/agr/ropemacs/get/tip.tar.gz 
;; pymacs: apt-get install pymacs
;; ropemode: pip install ropemode
;; rope: apt-get install rope
(require 'auto-complete)
(global-auto-complete-mode nil)
(define-key ac-complete-mode-map "\C-n" 'ac-next)
(define-key ac-complete-mode-map "\C-p" 'ac-previous)
(defun load-ropemacs ()
  "Load pymacs and ropemacs"
  (interactive)
  (require 'pymacs)
  (pymacs-load "ropemacs" "rope-")
  ;; Automatically save project python buffers before refactorings
  (setq ropemacs-confirm-saving 'nil)

  (define-key ropemacs-local-keymap "\M-/" 'rope-code-assist)
  (define-key ropemacs-local-keymap "\C-co" 'rope-goto-definition)
  (define-key ropemacs-local-keymap "\C-cu" 'rope-pop-mark)
  (define-key ropemacs-local-keymap "\C-cd" 'rope-show-doc)
  (define-key ropemacs-local-keymap "\C-cF" 'rope-find-occurrences)
  (define-key ropemacs-local-keymap "\M-?" 'rope-lucky-assist))

(global-set-key "\C-xpl" 'load-ropemacs)

;; Bind some keys for me
(global-set-key "\C-cg" 'goto-line)
(global-set-key "\C-ca" 'align-regexp)
(global-set-key "\C-cf" 'vc-git-grep)
(global-set-key "\C-cq" 'query-replace-regexp)
(global-set-key "\C-ci" 'indent-region)
(global-set-key "\C-cn" 'flymake-goto-next-error)
(global-set-key "\C-cp" 'flymake-goto-prev-error)
(global-set-key "\C-cc" 'comment-region)
(global-set-key "\C-cu" 'uncomment-region)
(global-set-key "\C-ct" 'global-auto-complete-mode)
(global-set-key "\C-cm" 'make-pprint-from-print)
(global-set-key "\C-c\C-o" 'c-set-offset)
(global-set-key "\C-cr" (lambda () (interactive) (revert-buffer t t)))

;; God these defaults are annoying
(global-unset-key "\C-x\C-b")
(global-unset-key "\C-x\C-n")
(delete-selection-mode 1)

;; Let me define and bind keyboard macros on the fly easily
;; These should be a defmacro + one-liners. Oh well. 
(defun setf5 ()
  (interactive)
  (name-last-kbd-macro 'f5)
  (global-set-key (kbd "<f5>") 'f5)
  (message "F5 set to last macro."))
(defun setf6 ()
  (interactive)
  (name-last-kbd-macro 'f6)
  (global-set-key (kbd "<f6>") 'f6)
  (message "F6 set to last macro."))
(defun setf7 ()
  (interactive)
  (name-last-kbd-macro 'f7)
  (global-set-key (kbd "<f7>") 'f7)
  (message "F7 set to last macro."))
(defun setf8 ()
  (interactive)
  (name-last-kbd-macro 'f8)
  (global-set-key (kbd "<f8>") 'f8)
  (message "F8 set to last macro."))
(global-set-key (kbd "<C-f5>") 'setf5)
(global-set-key (kbd "<C-f6>") 'setf6)
(global-set-key (kbd "<C-f7>") 'setf7)
(global-set-key (kbd "<C-f8>") 'setf8)

;; End of file.
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(browse-url-browser-function (quote browse-url-default-windows-browser))
 '(case-fold-search t)
 '(dired-omit-files "^\\.?#\\|^\\.$\\|^\\.\\.$\\|\\(^\\..*\\)")
 '(dired-recursive-copies (quote always))
 '(dired-recursive-deletes (quote always))
 '(dvc-tips-enabled nil)
 '(fill-column 80)
 '(flymake-gui-warnings-enabled nil)
 '(gdb-use-separate-io-buffer t)
 '(global-auto-revert-mode t)
 '(grep-find-command "find . -wholename '*.min.js' -prune -o -type f -print0 | xargs -0 -e grep -nH -e ")
 '(ibuffer-deletion-char 68)
 '(ibuffer-expert t)
 '(ibuffer-formats (quote ((mark modified read-only " " (name 18 18 :left :elide) " " (size 9 -1 :right) " " (mode 16 16 :left :elide) " " (git-status 8 8 :left) " " filename-and-process) (mark " " (name 16 -1) " " filename))))
 '(ibuffer-git-column-length 8)
 '(ido-mode (quote both) nil (ido))
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
 '(python-python-command "ipython")
 '(revert-without-query (quote (".*")))
 '(ropemacs-enable-autoimport nil)
 '(ropemacs-enable-shortcuts nil)
 '(safe-local-variable-values (quote ((eval add-hook (quote write-file-hooks) (quote time-stamp)) (c-hanging-comment-ender-p))))
 '(scroll-bar-mode nil)
 '(sort-fold-case t t)
 '(transient-mark-mode t)
 '(user-mail-address "terral.jordan@gmail.com")
 '(warning-suppress-types nil)
 '(x-select-enable-clipboard t))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(mumamo-background-chunk-major ((((class color) (min-colors 8)) nil)))
 '(mumamo-background-chunk-submode1 ((((class color) (min-colors 8)) nil)))
 '(mumamo-background-chunk-submode2 ((((class color) (min-colors 8)) nil))))
 

(put 'upcase-region 'disabled nil)

(put 'scroll-left 'disabled nil)

(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
