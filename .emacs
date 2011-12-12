;;;
;;; TR's .emacs file
;;;

;; Stupid desktop-save hack ... UTF-8 problem? Not really sure.
(setq Î 206)

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
(menu-bar-mode 0)
(if (boundp 'tool-bar-mode) (tool-bar-mode 0))

;; The good reference on specifying fonts:
;; http://www.gnu.org/software/libtool/manual/emacs/Fonts.html
;;
;; This appears to work, too.
;; (set-default-font "Menlo-12:weight=demibold")
(setq apple-font "-apple-Menlo-medium-normal-normal-*-10.5-*-*-*-m-0-fontset-auto1")
(setq linux-font "Bitstream Vera Sans Mono-9")
(if (member "Menlo" (font-family-list))
    (setq preferred-font apple-font)
    (setq preferred-font linux-font)) 
(set-default-font preferred-font) 

;; Open the main projects dir
(find-file "~/repos")

;; Spaces, not tabs!
(setq c-basic-offset 4)

;; Load in any other modes I use frequently
(autoload 'php-mode "php-mode" "Major mode for editing php code." t)
(load "dired-x")
;(require 'ibuffer-git)
(require 'markdown-mode)
(require 'flymake-jslint)
(defun my-jslint-hook () 
  (if (and (< (buffer-size) (* 250 1024))
           buffer-file-name)
      (flymake-mode 1)))
(add-hook 'js-mode-hook 'my-jslint-hook)
(require 'flymake-pylint)
(add-hook 'python-mode flymake-mode)

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
(add-to-list 'auto-mode-alist '("\\.text" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md" . markdown-mode))

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
(djcb-program-shortcut (kbd "<S-f4>") "mysql" "mysql")
(djcb-program-shortcut (kbd "<S-f5>") "cassandra" "cassandra-cli --host 127.0.0.1" )
(djcb-program-shortcut (kbd "<S-f6>") "tf" "cd ~/repos/tracelons/transformer/etl && runtf")
(djcb-program-shortcut (kbd "<S-f7>") "etl" "cd ~/repos/tracelons/transformer/etl && runetl -B")
(djcb-program-shortcut (kbd "\C-cs") "shell" "cd ~/repos/tracelons/transformer/etl && runetl -B")
(djcb-program-shortcut (kbd "\C-cs") "shell" "tl")
(global-set-key "\C-cy" 'my-term-paste)

;; Fix the color in terminals, and load my color theme
(setq term-default-bg-color nil)
(setq term-default-fg-color "#FFFFFF")
(require 'color-theme)
(color-theme-initialize)
(color-theme-calm-forest)
(setq ansi-term-color-vector 
      [unspecified "#000000" "#963F3C" "#5FFB65" "#FFFD65"
                   "#0082FF" "#FF2180" "#57DCDB" "#FFFFFF"])

;; Haskell mode
(load "~/.emacs.d/haskell-mode/haskell-site-file")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)



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

  (define-key ropemacs-local-keymap "\M-/" 'rope-code-assist)
  (define-key ropemacs-local-keymap "\C-co" 'rope-goto-definition)
  (define-key ropemacs-local-keymap "\C-cb" 'rope-pop-mark)
  (define-key ropemacs-local-keymap "\C-cd" 'rope-show-doc)
  (define-key ropemacs-local-keymap "\C-cF" 'rope-find-occurrences)
  (define-key ropemacs-local-keymap "\M-?" 'rope-lucky-assist))

(global-set-key "\C-xpl" 'load-ropemacs)

(setq dev-ini-buffer-name "development.ini")

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

(defun ensure-flymake ()
  (interactive)
  (flymake-mode t)
  (flymake-start-syntax-check))

;; Direct access to certain buffers
(defun goto-buffer-func (key)
  (interactive)
  (lexical-let ((buf (buffer-name)))
    (global-set-key 
     key (lambda () 
           (interactive)
           (switch-to-buffer buf)))))

(defun set-goto-buffer-func (setkey gokey)
  (lexical-let ((newgokey gokey))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Provide a way to run nose directly
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Root directory of tracelons repo
(setq tracelons-dir "~/repos/tracelons/")
;; Value for CELERY_CONFIG_MODULE
(setq tracelons-etl-config "etl.config")
;; Development.ini -- can be relative to tracelons/tracelytics
(setq tracelons-ini "development.ini")

(defun tracelons-web-nose-cmd () 
  (concat "/venv/bin/nosetests --with-pylons=" tracelons-ini))
(defun tracelons-etl-nose-cmd () 
  (concat "CELERY_CONFIG_MODULE=" tracelons-etl-config " nosetests"))

(defun run-nose (root test-cmd)
  "Root is relative to tracelons-dir, test-cmd is a nosetests cmd run in that directory."
  (save-buffer)
  (shell-command (concat "cd " tracelons-dir root " && " test-cmd " "
                         (expand-file-name buffer-file-name)
                         " &")))

(defun run-etl-test ()
  (interactive)
  (run-nose "transformer" (tracelons-etl-nose-cmd)))

(defun run-web-test ()
  (interactive)
  (run-nose "tracelytics" (tracelons-web-nose-cmd)))

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
(global-set-key "\C-cn" 'flymake-goto-next-error)
(global-set-key "\C-cp" 'flymake-goto-prev-error)
(global-set-key "\C-cc" 'comment-region)
(global-set-key "\C-cu" 'uncomment-region)
(global-set-key "\C-ct" 'global-auto-complete-mode)
(global-set-key "\C-cm" 'make-pprint-from-print)
(global-set-key "\C-c\C-o" 'c-set-offset)
(global-set-key "\C-cr" (lambda () (interactive) (revert-buffer t t)))
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-t" 'transpose-chars)
(global-set-key (kbd "<f12>") 'clean-copy)
(global-set-key "\C-x\C-p" 'up-one-newline)
(global-set-key "\M-`" 'other-frame)
(global-set-key "\M-n" 'make-frame-command)
(global-set-key "\C-ce" 'ensure-flymake)
(global-set-key "\C-c\C-e" (lambda () 
                             (interactive) 
                             (shell-command (concat "run_pylint.sh " buffer-file-name " &"))))

;; God these defaults are annoying
(global-unset-key "\C-x\C-b")
(global-unset-key "\C-x\C-n")

(load "sql-transform.el")
(add-hook 'sql-mode-hook
	  (function (lambda ()
		      (local-set-key "\M-q" 'sql-to-select))))

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

;; End of file.
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(ansi-color-for-comint-mode t)
 '(ansi-color-names-vector ["black" "red" "green" "yellow" "blue" "magenta" "cyan" "white"])
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
 '(ibuffer-formats (quote ((mark " " (name 16 -1) " " filename))))
 '(ibuffer-git-column-length 8)
 '(ido-mode (quote both) nil (ido))
 '(indent-tabs-mode nil)
 '(js-auto-indent-flag nil)
 '(js-expr-indent-offset 0)
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
 '(org-hide-leading-stars t)
 '(org-level-color-stars-only t)
 '(python-default-interpreter (quote cpython))
 '(python-guess-indent t)
 '(python-honour-comment-indentation nil)
 '(python-python-command "ipython")
 '(python-use-skeletons nil)
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
