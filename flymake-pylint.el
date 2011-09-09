(require 'flymake)

(defun flymake-pylint-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list (expand-file-name "~/repos/tracelons/tracelytics/run_pylint.sh") (list local-file))))

(setq flymake-allowed-file-name-masks
      (cons '(".+\\.py$"
              flymake-pylint-init
              flymake-simple-cleanup
              flymake-get-real-file-name)
            flymake-allowed-file-name-masks))

(setq flymake-err-line-patterns 
      (cons '("^.+: *\\([[:digit:]]+\\), *\\([[:digit:]]+\\)\\(:.+\\)?:\\(.+\\)$"
              nil 1 2 3)
            flymake-err-line-patterns))

(provide 'flymake-pylint)
