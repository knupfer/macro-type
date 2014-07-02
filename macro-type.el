(defun mt-overfullness (mt-log)
  (with-temp-buffer
    (let ((mt-overfull-boxes 0))
      (insert mt-log)
      (goto-char (point-min))
      (while (re-search-forward
              "^Overfull \\\\hbox (\\([[:digit:]\.]+\\)pt too wide).*" nil t)
        (setq mt-overfull-boxes
              (+ mt-overfull-boxes (string-to-number (match-string 1)))))
      mt-overfull-boxes)))

(defun mt-change-pagesize (mt-tex-file-name mt-margin-increase)
  (with-temp-buffer
    (insert-file-contents mt-tex-file-name)
    (re-search-forward
     "\\\\begin{document}" nil t)
    (replace-match (concat "
%%%%%%%%%%%%%%% Macro-type %%%%%%%%%%%%%%%%%
    \\\\addtolength{\\\\oddsidemargin }{ "
                           (number-to-string mt-margin-increase) "mm}
    \\\\addtolength{\\\\evensidemargin}{ "
                           (number-to-string mt-margin-increase) "mm}
    \\\\addtolength{\\\\textwidth     }{"
                           (number-to-string (* -2 mt-margin-increase)) "mm}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\\\\begin{document}") nil nil)
    (write-file "/tmp/tmp.macro-type.tex")))

(defun mt-pdflatex (mt-tex-file-name)
  (with-temp-buffer
    (shell-command (concat "pdflatex -output-directory /tmp -draftmode -interaction nonstopmode -jobname tmp.macro-type " mt-tex-file-name) t)
    (buffer-string)))

(progn
  (mt-change-pagesize "pride-and-prejudice.tex" 0.105)
  (mt-overfullness (mt-pdflatex "/tmp/tmp.macro-type.tex")))

