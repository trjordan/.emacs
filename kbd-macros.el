(fset 'make-pprint-from-print
   "\C-a\C-[f\C-[bp\C-[f(\C-[bfrom pprint m\C-?import pprint\C-o\C-n\C-i)\C-?\C-e)\C-x")

(fset 'pylons-set-debug-true
   "\C-xbdevelopment.ini\C-m\C-[<\C-sset debug\C-e\C-[\C-?true\C-x\C-s\C-xb\C-m")
(fset 'pylons-set-debug-false
   "\C-xbdevelopment.ini\C-m\C-[<\C-sset debug\C-e\C-[\C-?false\C-x\C-s\C-xb\C-m")

(fset 'pylons-set-sqlalchemy-info
   "\C-xbdevelopment.ini\C-m\C-[<\C-slogger_sqlalchemy\C-n\C-e\C-[\C-?INFO\C-x\C-s\C-xb\C-m")
(fset 'pylons-set-sqlalchemy-warn
   "\C-xbdevelopment.ini\C-m\C-[<\C-slogger_sqlalchemy\C-n\C-e\C-[\C-?WARN\C-x\C-s\C-xb\C-m")
