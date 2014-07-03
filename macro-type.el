;; TODO: accept 0 underfull and 0 overfull, implement underfull

(defun mt-overfullness (mt-log)
  (setq mt-underfull-boxes 0)
  (with-temp-buffer
    (let ((mt-overfull-boxes 0))
      (insert mt-log)
      (goto-char (point-min))
      (while (re-search-forward
              "^Overfull \\\\hbox (\\([[:digit:]\.]+\\)pt too wide).*" nil t)
        (setq mt-overfull-boxes
              (+ mt-overfull-boxes (string-to-number (match-string 1)))))
      (goto-char (point-min))
      (while (re-search-forward
              "^Underfull \\\\hbox (badness \\([[:digit:]\.]+\\)).*" nil t)
        (setq mt-underfull-boxes
              (+ mt-underfull-boxes (string-to-number (match-string 1)))))
      mt-overfull-boxes)))

(defun mt-change-pagesize (mt-tex-file-name
                           mt-margin-increase
                           mt-times
                           mt-increment)
  (while (> mt-times 0)
    (with-temp-buffer
      (insert-file-contents mt-tex-file-name)
      (when (not (= mt-times 1))
        (re-search-forward
         "\\\\begin{document}" nil t)
        (replace-match (concat "
%%%%%%%%%%%%%%% Macro-type %%%%%%%%%%%%%%%%%
    \\\\addtolength{\\\\oddsidemargin }{ "
                               (number-to-string
                                (+ mt-margin-increase
                                   (* (- mt-times 2) mt-increment))) "mm}
    \\\\addtolength{\\\\evensidemargin}{ "
                                   (number-to-string
                                    (+ mt-margin-increase
                                       (* (- mt-times 2) mt-increment)))
                                   "mm}
    \\\\addtolength{\\\\textwidth     }{"
                                   (number-to-string
                                    (* -2 (+ mt-margin-increase
                                             (* (- mt-times 2) mt-increment))))
                                   "mm}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\\\\begin{document}") nil nil))
      (write-file
       (concat "/tmp/tmp.macro-type." (number-to-string mt-times) ".tex")))
    (setq mt-times (- mt-times 1)))
  (message "Starting multicore calculation..."))

(defun mt-pdflatex (mt-cores)
  (setq mt-start-count (+ mt-start-count 1))
  (async-start
   `(lambda ()
      (with-temp-buffer
        ;; Pass in the variable environment for smtpmail
        ,(async-inject-variables "mt-start-count")
        (shell-command (concat
                        "pdflatex -output-directory /tmp -draftmode -interaction nonstopmode /tmp/tmp.macro-type."
                        (number-to-string mt-start-count) ".tex") t)
        (buffer-string)))
   (lambda (result)
     (if mt-best-hboxes
         (setq mt-best-hboxes
               (min mt-best-hboxes (mt-overfullness result))
               mt-best-badness
               (min mt-best-badness mt-underfull-boxes))
       (setq mt-original-hboxes (mt-overfullness result)
             mt-best-hboxes mt-original-hboxes
             mt-best-badness mt-underfull-boxes
             mt-original-badness mt-underfull-boxes))
     (setq mt-receive-count (+ mt-receive-count 1))
     (message
      (concat "Overfull hboxes reduced by "
              (number-to-string (round (/  (* 100 (- mt-original-hboxes mt-best-hboxes)) mt-original-hboxes)))
              "%% from "
              (number-to-string (round mt-original-hboxes)) "pt to "
              (number-to-string (round mt-best-hboxes)) "pt         " (number-to-string mt-receive-count) "/" (number-to-string mt-calculations) " processes returned"))
     (when (< (- mt-start-count mt-receive-count) mt-forks) (when (< mt-start-count mt-calculations) (mt-pdflatex mt-forks)))))
  (when (and (< (- mt-start-count mt-receive-count) mt-forks)
             (< mt-start-count mt-calculations)
             (> mt-start-count 1))
    (mt-pdflatex mt-forks)))

(defun mt-generate-list (mt-start mt-increment mt-times mt-file mt-cores)
  (mt-change-pagesize mt-file mt-start mt-times mt-increment)
  (mt-pdflatex mt-cores))

(defun mt-macro-type-tex-file (mt-file mt-range mt-times mt-cores)
  (interactive (list (read-file-name
                      "Choose a .tex file:" nil nil t nil 'mt-file-check)
                     (read-number
                      "How many mm may the page shrink:" 0.5)
                     (read-number
                      "How many times you would like to compile:" 25)
                     (read-number
                      "How many cores would you like to use:" 4)))
  (if (car (file-attributes mt-file 0))
      (error "You can't choose a directory")
    (setq mt-receive-count 0)
    (setq mt-start-count 0)
    (setq mt-calculations mt-times)
    (setq mt-original-hboxes nil)
    (setq mt-best-hboxes nil)
    (setq mt-forks mt-cores)
    (setq mt-underfull-boxes 0)
    (setq mt-original-badness nil)
    (setq mt-best-badness nil)
    (mt-generate-list (- 0 (* 0.25 mt-range)) (/ mt-range (max 1 (- mt-times 2))) mt-times mt-file mt-cores)))

(defun mt-file-check (mt-file)
  (with-temp-buffer
    (insert mt-file)
    (goto-char (point-min))
    (re-search-forward "/$\\|\\.tex$" nil t)))
