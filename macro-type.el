(defun mt-overfullness ()
  (interactive)
  (with-temp-buffer
    (let ((mt-overfull-boxes 0))
      (insert-file-contents "pride-and-prejudice.log")
      (while (re-search-forward
              "^Overfull \\\\hbox (\\([[:digit:]\.]+\\)pt too wide).*" nil t)
        (setq mt-overfull-boxes
              (+ mt-overfull-boxes (string-to-number (match-string 1)))))
      (message (number-to-string mt-overfull-boxes)))))



