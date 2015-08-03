(require 'flymake)

(defun extract (elems seq)
  (mapcar (lambda (x) (nth x seq)) elems))

(defun buffer-runpylint-loc ()
  (interactive)
  (concat (mapconcat 'identity
                     (extract '(0 1 2 3 4)
                              (split-string (expand-file-name buffer-file-name)
                                            "/")) "/")
   "/run_pylint.sh"))

(defun flymake-pylint-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list (buffer-runpylint-loc) (list local-file))))

(setq flymake-allowed-file-name-masks
      (cons '(".+\\.py$"
              flymake-pylint-init
              flymake-simple-cleanup
              flymake-get-real-file-name)
            flymake-allowed-file-name-masks))

(setq flymake-err-line-patterns
      (cons '("^.+: *\\([[:digit:]]+\\), *\\([[:digit:]]+\\)\\(:.+\\)?:\\(.+\\)$"
              nil 1 2 4)
            flymake-err-line-patterns))

(provide 'flymake-pylint)



