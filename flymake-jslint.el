(require 'flymake)

(defun flymake-jslint-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list (expand-file-name "~/repos/tracelons/tracelytics/run_jslint.sh") (list local-file))))

(setq flymake-allowed-file-name-masks
      (cons '(".+\\.js$"
              flymake-jslint-init
              flymake-simple-cleanup
              flymake-get-real-file-name)
            flymake-allowed-file-name-masks))

(setq flymake-err-line-patterns
      (cons '("^\\(.+\\)\. (\\(.+\\)\.lint:\\([[:digit:]]+\\):\\([[:digit:]]+\\)"
              nil 3 2 1)
            flymake-err-line-patterns))

(provide 'flymake-jslint)
