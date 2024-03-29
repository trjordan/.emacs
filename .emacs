;;;
;;; TR's .emacs file
;;;

;;; Don't steal files

(set 'lexical-binding :t)

;;; Code:
(setq create-lockfiles nil)

(require 'desktop)
(setq desktop-file-version (format "%s" desktop-file-version))
(desktop-save-mode 1)
(setq desktop-dirname "~/.emacs.d/")
(defun my-desktop-save ()
  (interactive)
  ;; Don't call desktop-save-in-desktop-dir, as it prints a message.
  (if (eq (desktop-owner) (emacs-pid))
      (desktop-save desktop-dirname)))
(add-hook 'auto-save-hook 'my-desktop-save)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Set up my path, in emacs and out
(setq load-path (append (list "~/.emacs.d/lisp") load-path))
(setenv "PATH" (concat (expand-file-name "~/") "bin" ":" (getenv "PATH")))
(setenv "PATH" (concat "/usr/local/bin" ":" (getenv "PATH")))
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin/"))

(setq exec-path (append exec-path '("/usr/local/bin")))
;; Turn of the menu bars
(menu-bar-mode 0)
(if (boundp 'tool-bar-mode) (tool-bar-mode 0))

;; Load my scratch buffer
;;(require 'persistent-scratch)
;;(load-persistent-scratch)

;; Spaces, not tabs!
(setq c-basic-offset 2)
(setq nxml-child-indent 2)

;; Let's do this with packages!
(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

;; activate installed packages
(package-initialize)

(defun ensure-package-installed (&rest packages)
  "Assure every package is installed, ask for installation if it’s not.
   Return a list of installed packages or nil for every skipped package."
  (mapcar
   (lambda (package)
     ;; (package-installed-p 'evil)
     (if (package-installed-p package)
         nil
       (if (y-or-n-p (format "Package %s is missing.  Install it? " package))
           (package-install package)
         package)))
   packages))

;; make sure to have downloaded archive description.
;; Or use package-archive-contents as suggested by Nicolas Dudebout
(or (file-exists-p package-user-dir)
    (package-refresh-contents))

(ensure-package-installed
 ;; Language modes
 'web-mode 'php-mode 'markdown-mode 'python-mode 'haskell-mode
 'less-css-mode 'thrift 'yaml-mode 'sass-mode 'scss-mode
 'jinja2-mode 'json-mode
 ;; Inline checking
 ;; 'pymacs
 'flycheck
 ;; Git
 'ibuffer-git
 ;; Other stuff
 ;; 'yasnippet-bundle
 'persistent-scratch
 ;; 'hexrgb
    'auto-complete
 'exec-path-from-shell)

;; Load in any other modes I use frequently
(load "dired-x")
(add-to-list 'load-path "~/.emacs.d/yasnippet")

;; Set up flycheck for frontend syntax checking
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)
;; use eslint with web-mode for jsx files
(flycheck-add-mode 'javascript-eslint 'web-mode)

;; https://github.com/purcell/exec-path-from-shell
;; only need exec-path-from-shell on OSX
;; this hopefully sets up path and other vars better
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; Add some places to the path (probably not going to use < 23 going forward...)
;; (if (< emacs-major-version 23)
;;    (add-to-list 'load-path "~/.emacs.d/nxml/"))

;; (load "~/.emacs.d/nxhtml/autostart.el")
(load "camelcase.el")
;;(require 'ourcomments-widgets)

;; Change some file associations
;; (add-to-list 'auto-mode-alist '("\\.tpl\\'" . html-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'" . php-mode))
(add-to-list 'auto-mode-alist '("\\.inc\\'" . php-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . rjsx-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))
(add-to-list 'auto-mode-alist '("\\.template" . web-mode))
(add-to-list 'auto-mode-alist '("\\.text" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.less" . css-mode))
(add-to-list 'auto-mode-alist '("\\.scss" . scss-mode))

;; Enable backup files to a specific hidden directory, keeping the
;; default number of versions
(setq make-backup-files nil)
;(setq version-control t)
;(setq backup-directory-alist (quote ((".*" . "~/.emacs_backups/"))))

; Load my keyboard macros
(load "kbd-macros.el")

; Really, compatability mode? Apparently this isn't here until Emacs 24
;; (unless (fboundp 'prog-mode) (defalias 'prog-mode 'fundamental-mode))

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

(djcb-program-shortcut (kbd "<S-f8>") "shell" "cd ~/")
(djcb-program-shortcut (kbd "<S-f2>") "paster-shell" "cd ~/")
(djcb-program-shortcut (kbd "<S-f3>") "flask-serve" "cd ~/")
(djcb-program-shortcut (kbd "<S-f4>") "mysql" "cd ~/")
(djcb-program-shortcut (kbd "<S-f5>") "local" "psql")
(djcb-program-shortcut (kbd "<S-f6>") "log" "cd ~/log")
(djcb-program-shortcut (kbd "<S-f7>") "shell2" "cd ~/")
(djcb-program-shortcut (kbd "\C-cs") "shell" "cd ~/")
(djcb-program-shortcut (kbd "\C-cs") "shell" "cd ~/")
(global-set-key "\C-cy" 'my-term-paste)

;; Couple functions that rename common buffers to better names for me
;; (defun prepend-one-dir ()
;;   "Renames a buffer with the parent folder name"
;;   (interactive)
;;   (let ((pkg (first (last (split-string (buffer-file-name) "/") 2))))
;;     (rename-buffer (concat pkg "/" (buffer-name)))))

;; (defun prepend-hook ()
;;   (let ((filename (car (last (split-string (buffer-file-name) "/"))))
;;         (pkg (first (last (split-string (buffer-file-name) "/") 2))))
;;     (if (and (member filename '("__init__.py", "index.html"))
;;              (not (string-match "/" (buffer-name)))
;;              (get-buffer (concat pkg "/" (buffer-name))))
;;         (prepend-one-dir))))

;; (add-hook 'find-file-hook 'prepend-hook)
(require 'uniquify)

;; Fix the color in terminals, and load my color theme
(setq term-default-bg-color nil)
(setq term-default-fg-color "#FFFFFF")
(load "blue-mood-theme.el")
(load-theme 'blue-mood t)
(setq ansi-term-color-vector  [term
                               term-color-black
                               term-color-red
                               term-color-green
                               term-color-yellow
                               term-color-blue
                               term-color-magenta
                               term-color-cyan
                               term-color-white])
(setq ansi-term-color-vector
      [default "#000000" "#963F3C" "#5FFB65" "#FFFD65"
                   "#0082FF" "#FF2180" "#57DCDB" "#FFFFFF"])
 '(ansi-color-names-vector ["black" "red" "green" "yellow" "blue" "magenta" "cyan" "white"])

;; Haskell mode
;; (load "~/.emacs.d/haskell-mode/haskell-site-file")
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)


;; stdout is bound to ASCII encoding in python 3 by default; Idiots.
(setenv "LC_CTYPE" "UTF-8")

;; Set me up a python IDE!
;;
;; Installations require the following:
;; ropemacs: pip install http://bitbucket.org/agr/ropemacs/get/tip.tar.gz
;; pymacs: apt-get install pymacs (OS X: clone from repo, sudo make install)
;; ropemode: pip install ropemode
;; rope: apt-get install rope (OS X: clone bitbucket repo, python setup.py install)
;; ropemacs: ??? (OS X: clone bitbucket repo, python setup.py install)
;;
;; Everything really works best if you just clone the repos and install from
;; there. Probably best to use the right emacs tag of pymacs, though (23 is
;; included in the repo).
;;
;; It appears that all python packages have to be installed system-wide, because
;; I haven't figured out a way to make Pymacs see virtualenvs yet.
(defun load-ropemacs ()
  "Load pymacs and ropemacs"
  (interactive)
  (require 'pymacs)
  (pymacs-load "ropemacs" "rope-")
  ;; Automatically save project python buffers before refactorings
  (setq ropemacs-confirm-saving 'nil)

  (define-key ropemacs-local-keymap "\M-/" 'rope-lucky-assist)
  (define-key ropemacs-local-keymap "\C-co" 'rope-goto-definition)
  (define-key ropemacs-local-keymap "\C-cb" 'rope-pop-mark)
  (define-key ropemacs-local-keymap "\C-cd" 'rope-show-doc)
  (define-key ropemacs-local-keymap "\C-cF" 'rope-find-occurrences)
  (define-key ropemacs-local-keymap "\M-?" 'rope-code-assist))

(global-set-key "\C-xpl" 'load-ropemacs)

(setq dev-ini-buffer-name "development.ini")

(defun set-py-buffer (name func)
  (save-excursion
    (let ((buf (buffer-name)))
      (switch-to-buffer name t)
      (if (eq major-mode 'python-mode)
          (funcall func t)))))

(defun set-rope-all ()
  (interactive)
  (mapcar (lambda (b) (set-py-buffer b 'ropemacs-mode)) (buffer-list)))

(defun insert-trace ()
  (interactive)
  (insert "import pdb; pdb.set_trace()\n"))

(eval-after-load 'python
  '(define-key python-mode-map "\C-c b" 'insert-trace))

;; Some functions for easily changing the logging level in python ini files
(defun change-logging-inter ()
  (interactive)
  (let ((section (read-string "Handler to change [sqlalchemy]: " nil nil "sqlalchemy"))
        (level (read-string "Logging level [WARN]: " nil nil "WARN")))
    (change-logging section level)))

(defun change-logging (section level)
  (interactive)
  (let ((cur (current-buffer)))
    (save-current-buffer
      (set-buffer (get-buffer-create (current-buffer)))
      (switch-to-buffer dev-ini-buffer-name)
      (goto-char (point-min))
      (search-forward (concat "[logger_" (downcase section) "]"))
      (search-forward-regexp (concat "level =.*"))
      (replace-match (concat "level = " (upcase level)))
      (save-buffer)
      (switch-to-buffer cur))))

(defun enable-filters-logging ()
  (interactive)
  (change-logging "filters" "DEBUG"))
(defun disable-filters-logging ()
  (interactive)
  (change-logging "filters" "WARN"))
(defun enable-sqlalchemy-logging ()
  (interactive)
  (change-logging "sqlalchemy" "INFO"))
(defun disable-sqlalchemy-logging ()
  (interactive)
  (change-logging "sqlalchemy" "WARN"))

(defadvice zap-to-char (after my-zap-to-char-advice (arg char) activate)
  "Kill up to the ARG'th occurence of CHAR, and leave CHAR. If
  you are deleting forward, the CHAR is replaced and the point is
  put before CHAR"
  (insert char)
  (if (< 0 arg) (forward-char -1)))

(defun clean-copy ()
  "Removes the first newline in a buffer."
  (interactive)
  (move-end-of-line 1)
  (kill-line))

(defun up-one-newline ()
  "Removes the newline and all whitespace before the current point."
  (interactive)
  (save-excursion
    (move-beginning-of-line nil)
    (just-one-space 0)
    (set-mark-command nil)
    (previous-line)
    (move-end-of-line 1)
    (kill-region (point) (mark))))

;; Direct access to certain buffers
(defun goto-buffer-func (key)
  (interactive)
  (let ((buf (buffer-name)))
    (global-set-key
     key (lambda ()
           (interactive)
           (switch-to-buffer buf)))))

(defun set-goto-buffer-func (setkey gokey)
  (let ((newgokey gokey))
    (global-set-key setkey (lambda ()
                             (interactive)
                             (goto-buffer-func newgokey)))))

(set-goto-buffer-func [?\C-!] [?\C-1])
(set-goto-buffer-func [?\C-@] [?\C-2])
(set-goto-buffer-func [?\C-#] [?\C-3])
(set-goto-buffer-func [?\C-$] [?\C-4])
(set-goto-buffer-func [?\C-%] [?\C-5])
(set-goto-buffer-func [?\C-^] [?\C-6])
(set-goto-buffer-func [?\C-&] [?\C-7])
(set-goto-buffer-func [?\C-*] [?\C-8])

;; Insert the current filename with f3
(define-key minibuffer-local-map
  "\C-n" (lambda () (interactive)
       (insert (buffer-name (current-buffer-not-mini)))))

(defun current-buffer-not-mini ()
  "Return current-buffer if current buffer is not the *mini-buffer*
  else return buffer before minibuf is activated."
  (if (not (window-minibuffer-p)) (current-buffer)
      (if (eq (get-lru-window) (next-window))
    	  (window-buffer (previous-window)) (window-buffer (next-window)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bind some keys for me
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key "\C-cg" 'goto-line)
(global-set-key "\C-ca" 'align-regexp)
(global-set-key "\C-cf" 'vc-git-grep)
(global-set-key "\C-cq" 'query-replace-regexp)
(global-set-key "\C-ci" 'indent-region)
(global-set-key "\C-cn" 'flycheck-next-error)
(global-set-key "\C-cp" 'flycheck-previous-error)
(global-set-key "\C-cc" 'comment-region)
(global-set-key "\C-cu" 'uncomment-region)
(global-set-key "\C-ct" 'global-auto-complete-mode)
(global-set-key "\C-cm" 'make-pprint-from-print)
(global-set-key "\C-c\C-o" 'c-set-offset)
(global-set-key "\C-cr" (lambda () (interactive) (revert-buffer t t)))
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-xm" 'execute-extended-command)
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-t" 'transpose-chars)
(global-set-key (kbd "<f12>") 'clean-copy)
(global-set-key (kbd "<f9>") 'my-theme-cycle)
(global-set-key "\C-x\C-p" 'up-one-newline)
(global-set-key "\M-\`" 'other-frame)
(global-set-key "\M-n" 'make-frame-command)
(global-set-key "\C-ce" 'flycheck-compile)
(global-set-key (kbd "<f9>") 'variable-pitch-mode)
(global-set-key "\C-c\C-e" (lambda () (interactive)
                             (shell-command (concat (buffer-runpylint-loc) " " buffer-file-name " &"))))

;; God these defaults are annoying
(global-unset-key "\C-x\C-b")
(global-unset-key "\C-x\C-n")

(load "sql-transform.el")
(add-hook 'sql-mode-hook
	  (function (lambda ()
		      (local-set-key "\M-q" 'sql-to-select))))

;; Swap some keys on OS X
(setq mac-command-modifier 'control)
(setq mac-control-modifier 'meta)
(setq mac-option-modifier 'meta)

;; Don't be intelligent about how to split the window
(setq split-height-threshold nil)

;; Allow for root editing on remote machines
;; (set-default 'tramp-default-proxies-alist (quote ((".*" "\\`root\\'" "/ssh:%h:"))))
(set-default 'tramp-default-proxies-alist '())

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
(defun setf9 ()
  (interactive)
  (name-last-kbd-macro 'f9)
  (global-set-key (kbd "<f9>") 'f9)
  (message "F9 set to last macro."))
(defun setf10 ()
  (interactive)
  (name-last-kbd-macro 'f10)
  (global-set-key (kbd "<f10>") 'f10)
  (message "F10 set to last macro."))
(defun setf11 ()
  (interactive)
  (name-last-kbd-macro 'f11)
  (global-set-key (kbd "<f11>") 'f11)
  (message "F11 set to last macro."))
(defun setf12 ()
  (interactive)
  (name-last-kbd-macro 'f12)
  (global-set-key (kbd "<f12>") 'f12)
  (message "F12 set to last macro."))
(global-set-key (kbd "<C-f5>") 'setf5)
(global-set-key (kbd "<C-f6>") 'setf6)
(global-set-key (kbd "<C-f7>") 'setf7)
(global-set-key (kbd "<C-f8>") 'setf8)
(global-set-key (kbd "<C-f9>") 'setf9)
(global-set-key (kbd "<C-f10>") 'setf10)
(global-set-key (kbd "<C-f11>") 'setf11)
(global-set-key (kbd "<C-f12>") 'setf12)

;; (when (and (equal emacs-major-version 24)
;;            (equal emacs-minor-version 3))
;;   (eval-after-load "mumamo"
;;     '(setq mumamo-per-buffer-local-vars
;;            (delq 'buffer-file-name mumamo-per-buffer-local-vars)))
;;   (eval-after-load "bytecomp"
;;     '(add-to-list 'byte-compile-not-obsolete-vars
;;                   'font-lock-beginning-of-syntax-function))
;;   ;; tramp-compat.el clobbers this variable!
;;   (eval-after-load "tramp-compat"
;;     '(add-to-list 'byte-compile-not-obsolete-vars
;;                   'font-lock-beginning-of-syntax-function)))

(server-start)

;; End of file.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-for-comint-mode t)
 '(ansi-color-names-vector
   ["black" "red" "green" "yellow" "blue" "magenta" "cyan" "white"])
 '(browse-url-browser-function (quote browse-url-default-windows-browser))
 '(case-fold-search t)
 '(coffee-tab-width 4)
 '(css-indent-offset 2)
 '(custom-safe-themes
   (quote
    ("db510eb70cf96e3dbd48f5d24de12b03db30674ea0853f06074d4ccf7403d7d3" default)))
 '(dired-omit-files "^\\.?#\\|^\\.$\\|^\\.\\.$\\|\\(^\\..*\\)")
 '(dired-recursive-copies (quote always))
 '(dired-recursive-deletes (quote always))
 '(dvc-tips-enabled nil)
 '(exec-path
   (quote
    ("~/bin" "/usr/bin" "/bin" "/usr/sbin" "/sbin" "/Applications/Emacs.app/Contents/MacOS/bin")))
 '(fill-column 80)
 '(flycheck-disabled-checkers (quote (javascript-jshint)))
 '(flycheck-javascript-eslint-executable "")
 '(flymake-gui-warnings-enabled nil)
 '(flymake-log-level -1)
 '(gdb-use-separate-io-buffer t)
 '(global-auto-revert-mode t)
 '(grep-find-command "grep -nH -e .")
 '(ibuffer-deletion-char 68)
 '(ibuffer-expert t)
 '(ibuffer-formats (quote ((mark " " (name 16 -1) " " filename))))
 '(ibuffer-git-column-length 8)
 '(ido-default-buffer-method (quote switch-to-buffer))
 '(ido-mode (quote both) nil (ido))
 '(indent-tabs-mode nil)
 '(ispell-program-name "/usr/local/bin/ispell")
 '(js-auto-indent-flag nil)
 '(js-expr-indent-offset 0)
 '(js-indent-level 2)
 '(json-reformat:indent-width 2)
 '(jsx-indent-level 2)
 '(kill-whole-line t)
 '(matlab-indent-level 4)
 '(matlab-keyword-list
   (quote
    ("global" "persistent" "for" "while" "if" "elseif" "else" "endfunction" "return" "break" "continue" "switch" "case" "otherwise" "try" "catch" "tic" "toc" "Warning" "classdef" "properties" "methods")))
 '(matlab-mode-install-path (quote ("/usr/matlab/bin/toolbox/")))
 '(matlab-shell-command "/usr/matlab/bin/matlab")
 '(matlab-shell-command-switches (quote ("-nodesktop" "-nosplash")))
 '(matlab-shell-enable-gud-flag t)
 '(matlab-shell-input-ring-size 128)
 '(matlab-shell-logo "/usr/share/emacs/22.1/etc/matlab.xpm")
 '(matlab-shell-mode-hook nil)
 '(mumamo-major-modes
   (quote
    ((asp-js-mode js-mode javascript-mode espresso-mode ecmascript-mode)
     (asp-vb-mode visual-basic-mode)
     (javascript-mode js-mode javascript-mode espresso-mode ecmascript-mode)
     (java-mode jde-mode java-mode)
     (groovy-mode groovy-mode)
     (nxhtml-mode nxhtml-mode html-mode))))
 '(org-hide-leading-stars t)
 '(org-level-color-stars-only t)
 '(package-selected-packages
   (quote
    (visual-fill typescript groovy-mode docker rjsx-mode jsx-mode js2-mode web-mode-edit-element yasnippet-bundle yaml-mode web-mode thrift scss-mode sass-mode python-mode pymacs php-mode persistent-scratch menu-bar+ markdown-mode less-css-mode json-mode jinja2-mode ibuffer-git haskell-mode frame-cmds flycheck facemenu+ exec-path-from-shell doremi-frm doremi-cmd dockerfile-mode color-theme coffee-mode auto-complete)))
 '(python-default-interpreter (quote cpython))
 '(python-guess-indent t)
 '(python-honour-comment-indentation nil)
 '(python-indent-guess-indent-offset t)
 '(python-python-command "ipython")
 '(python-skeleton-autoinsert nil)
 '(python-use-skeletons nil)
 '(revert-without-query (quote (".*")))
 '(ropemacs-enable-autoimport nil)
 '(ropemacs-enable-shortcuts nil)
 '(safe-local-variable-values
   (quote
    ((eval add-hook
           (quote write-file-hooks)
           (quote time-stamp))
     (c-hanging-comment-ender-p))))
 '(scroll-bar-mode nil)
 '(select-enable-clipboard t)
 '(shell-file-name "/bin/bash")
 '(sort-fold-case t t)
 '(tramp-default-method "scp")
 '(transient-mark-mode t)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(user-mail-address "terral.jordan@gmail.com")
 '(warning-suppress-types nil)
 '(web-mode-attr-indent-offset 2)
 '(web-mode-attr-value-indent-offset 2)
 '(web-mode-code-indent-offset 4)
 '(web-mode-enable-auto-opening nil)
 '(web-mode-markup-indent-offset 2))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mumamo-background-chunk-major ((((class color) (min-colors 8)) nil)))
 '(mumamo-background-chunk-submode1 ((((class color) (min-colors 8)) nil)))
 '(mumamo-background-chunk-submode2 ((((class color) (min-colors 8)) nil)))
 '(term-color-blue ((t (:background "light steel blue" :foreground "light steel blue")))))


(put 'upcase-region 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
