(with-temp-buffer (insert-file-contents "pride-and-prejudice.log")
                  (while (re-search-forward "Overfull" nil t)
                    (replace-match "whackiness" nil nil))
                  (write-file "testbed.log"))
