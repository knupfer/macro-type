(defun mt-overfullness (mt-log-file-name)
  (with-temp-buffer
    (let ((mt-overfull-boxes 0))
      (insert-file-contents mt-log-file-name)
      (while (re-search-forward
              "^Overfull \\\\hbox (\\([[:digit:]\.]+\\)pt too wide).*" nil t)
        (setq mt-overfull-boxes
              (+ mt-overfull-boxes (string-to-number (match-string 1)))))
      mt-overfull-boxes)))

(defun mt-change-pagesize (mt-tex-file-name)
  (interactive)
  (with-temp-buffer
    (insert-file-contents mt-tex-file-name)
    (re-search-forward
     "\\\\begin{document}" nil t)
    (replace-match "
%%%%%%%%%%%%%%% Macro-type %%%%%%%%%%%%%%%%%
    \\\\addtolength{\\\\oddsidemargin}{-20.0mm}
    \\\\addtolength{\\\\evensidemargin}{-20.0mm}
    \\\\addtolength{\\\\textwidth}{40.0mm}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\\\\begin{document}" nil nil)
    (write-file (concat "tmp.macro-type." mt-tex-file-name))))

(shell-command "pdflatex -interaction nonstopmode -draftmode -jobname tmp.macro-type tmp.macro-type.pride-and-prejudice.tex" t)
;;(call-process "pdflatex" "tmp.macro-type.pride-and-prejudice.tex" t nil "-interaction nonstopmode")

(mt-change-pagesize "pride-and-prejudice.tex")
(mt-overfullness "pride-and-prejudice.log")
