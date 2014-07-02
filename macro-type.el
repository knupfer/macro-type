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
    (shell-command
     (concat "pdflatex -draftmode -interaction nonstopmode "
             mt-tex-file-name) t)
    (buffer-string)))

(defun mt-generate-list (mt-start mt-increment mt-times mt-file)
  (mt-change-pagesize mt-file mt-start)
  (if mt-original-hboxes
      (progn
        (message
         (concat "Overfull hboxes reduced by "
                 (number-to-string
                  (round
                   (/ (* 100 (- mt-original-hboxes mt-best-hboxes))
                      mt-original-hboxes))) "%% from "
                      (number-to-string (round mt-original-hboxes)) "pt to "
                      (number-to-string (round mt-best-hboxes)) "pt"))
        (setq mt-best-hboxes
              (min mt-best-hboxes (mt-overfullness
                                   (mt-pdflatex "/tmp/tmp.macro-type.tex")))))
    (setq mt-original-hboxes (mt-overfullness
                              (mt-pdflatex "/tmp/tmp.macro-type.tex"))
          mt-best-hboxes mt-original-hboxes))
  (message
   (concat "Overfull hboxes reduced by "
           (number-to-string
            (round
             (/ (* 100 (- mt-original-hboxes mt-best-hboxes))
                mt-original-hboxes))) "%% from "
                (number-to-string (round mt-original-hboxes)) "pt to "
                (number-to-string (round mt-best-hboxes)) "pt"))
  (when (> mt-times 1) (mt-generate-list (+ mt-start mt-increment)
                                         mt-increment
                                         (- mt-times 1)
                                         mt-file)))

(defun mt-macro-type-tex-file (mt-file mt-range mt-times)
  (interactive (list (read-file-name
                      "Choose a .tex file:" nil nil t nil 'mt-file-check)
                     (read-number
                      "How many mm may the page shrink:" 0.5)
                     (read-number
                      "How many times you would like to compile:" 5)))
  (if (car (file-attributes mt-file 0))
      (error "You can't choose a directory")
    (setq mt-original-hboxes nil)
    (mt-generate-list 0 (/ mt-range mt-times) mt-times mt-file)))

(defun mt-file-check (mt-file)
  (with-temp-buffer
    (insert mt-file)
    (goto-char (point-min))
    (re-search-forward "/$\\|\\.tex$" nil t)))





