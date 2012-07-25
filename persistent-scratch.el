(defvar persistent-scratch-filename
    "~/Dropbox/scratch.el"
    "Location of *scratch* file contents for persistent-scratch.")

(defun save-persistent-scratch ()
  "Write the contents of *scratch* to the file name
  PERSISTENT-SCRATCH-FILENAME"
  (with-current-buffer (get-buffer "*scratch*")
    (write-region (point-min) (point-max)
                  persistent-scratch-filename)))

(defun load-persistent-scratch ()
  "Load the contents of PERSISTENT-SCRATCH-FILENAME into the
  scratch buffer, clearing its contents first."
  (if (file-exists-p persistent-scratch-filename)
      (with-current-buffer (get-buffer "*scratch*")
        (delete-region (point-min) (point-max))
        (shell-command (format "cat %s" persistent-scratch-filename) (current-buffer)))))

(provide 'persistent-scratch)