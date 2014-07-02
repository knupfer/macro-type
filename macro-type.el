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
(setq mt-list '())

(defun mt-generate-list (mt-start mt-increment mt-times mt-file)
  (mt-change-pagesize mt-file mt-start)
  (if mt-list
      (setq mt-list (list mt-list mt-start (mt-overfullness (mt-pdflatex "/tmp/tmp.macro-type.tex"))))
    (setq mt-list (list mt-start (mt-overfullness (mt-pdflatex "/tmp/tmp.macro-type.tex")))))
  (when (> mt-times 1) (mt-generate-list (+ mt-start mt-increment) mt-increment (- mt-times 1) mt-file))
  mt-list)

(defun mt-macro-type-tex-file (mt-file mt-range mt-times)
  (interactive (list (read-file-name
                      "Choose a .tex file:" nil nil t nil 'mt-file-check)
                     (read-number
                      "How many mm may the page shrink:" 0.5)
                     (read-number
                      "How many times you would like to compile:" 5)))
  (if (car (file-attributes mt-file 0))
      (error "You can't choose a directory")
    (setq mt-list '())
    (mt-generate-list 0 (/ mt-range mt-times) mt-times mt-file)))

(defun mt-file-check (mt-file)
  (with-temp-buffer
    (insert mt-file)
    (goto-char (point-min))
    (re-search-forward "/$\\|\\.tex$" nil t)))







