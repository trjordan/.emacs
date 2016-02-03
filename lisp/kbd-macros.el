(fset 'make-pprint-from-print
   "\C-a\C-[f\C-[bp\C-[f\C-[bfrom pprint m\C-?import pprint\C-o\C-n\C-i)\C-?\C-e\C-x")

(fset 'pylons-set-debug-true
   "\C-xbdevelopment.ini\C-m\C-[<\C-sset debug\C-e\C-[\C-?true\C-x\C-s\C-xb\C-m")
(fset 'pylons-set-debug-false
   "\C-xbdevelopment.ini\C-m\C-[<\C-sset debug\C-e\C-[\C-?false\C-x\C-s\C-xb\C-m")

(fset 'pylons-set-sqlalchemy-info
   "\C-xbdevelopment.ini\C-m\C-[<\C-slogger_sqlalchemy\C-n\C-e\C-[\C-?INFO\C-x\C-s\C-xb\C-m")
(fset 'pylons-set-sqlalchemy-warn
   "\C-xbdevelopment.ini\C-m\C-[<\C-slogger_sqlalchemy\C-n\C-e\C-[\C-?WARN\C-x\C-s\C-xb\C-m")

(fset 'query-sub-date
   "\C-[>\C-a\C-[z]\C-[z]\C-d\C-sdatetime\C-[b\C-[z)\C-[z)\C-d\C-d\C-rfirst\C-r\C-[f\C-[f\C-[f\C-?\C-?subdate(now(), interval 1 day)\C-[d\C-[d\C-[d\C-[d\C-d\C-d\C-d\C-d\C-d\C-[>")
(fset 'query-sub-last-arg
   "\C-b\C-@\C-r,\C-m\C-f\C-w\C-?\C-r%s\C-m\C-d\C-d\C-y\C-u\C-@\C-u\C-@\C-e")

(defun query-sub-all ()
  (interactive)
  (query-sub-date)
  (save-excursion
    (goto-char 0)
    (let ((count (count-matches "%s")))
      (while (> count 0)
        (query-sub-last-arg)
        (setq count (- count 1))))))
