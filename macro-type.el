;;; macro-type.el --- optimize margins of tex-files

;; Copyright (C) 2014 Florian Knupfer

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA

;; Author: Florian Knupfer

;;; Code:
(require 'async)

(defvar mt-receive-count)
(defvar mt-best-file)
(defvar mt-underfull-matrix)
(defvar mt-overfull-matrix)
(defvar mt-init-underfull-boxes)
(defvar mt-init-overfull-boxes)
(defvar mt-best-underfull-boxes)
(defvar mt-best-overfull-boxes)
(defvar mt-benchmark)
(defvar mt-number-of-blurs 4)

(defun mt-macro-type-tex-file (file range calcs forks)
  "Change the pagesize of a tex file to optimize it.
It compiles a lot of times the same file and looks at the log files to
minimize overfull and underfull hboxes.  Afterwards, it uses mdframes to
 alter the pagesize of individual sections."
  (interactive (list (read-file-name
                      "Choose a .tex file: " nil nil t nil 'mt-file-check)
                     (read-number
                      "How many mm may the size change: " 0.5)
                     (read-number
                      "How many times you would like to compile: " 25)
                     (read-number
                      "How many cores would you like to use: " 4)))
  (if (car (file-attributes file 0))
      (error "You can't choose a directory")
    (setq mt-receive-count 0
          mt-benchmark (current-time)
          ;; Get line numbers of sections, including begin and end document.
          mt-underfull-matrix (make-vector calcs 0)
          mt-overfull-matrix (make-vector calcs 0))
    ;; Save the header and the body of the tex file to access them faster
    (let ((local-count 1)
          (section-list (mt-retrieve-file-info-IO file)))
      (while (and (<= local-count forks)
                  (<= local-count calcs))
        (mt-pdflatex-IO range file forks calcs local-count section-list)
        (setq local-count (+ 1 local-count))))))

(defun mt-retrieve-file-info-IO (file)
  (with-temp-buffer
    (insert-file-contents file)
    (let ((this-buffer (buffer-string)))
      (with-temp-buffer
        (insert
         (car (split-string this-buffer "\n.*\\\\begin{document}.*\n")))
        (write-file "/tmp/tmp.macro-type.begin"))
      (with-temp-buffer
        (insert
         (car (cdr (split-string this-buffer "\n.*\\\\begin{document}.*\n"))))
        (write-file "/tmp/tmp.macro-type.end")))
    (shell-command-on-region
     (point-min) (point-max)
     (concat
      "grep -n 'section{.*}\\|begin{document}\\|end{document}' "
      file " | grep -o ^[0-9]*")
     t t)
    (map 'list 'string-to-number (split-string (buffer-string)))))

(defun mt-pdflatex-IO (range file forks calcs local-count section-list)
  "Starts multiple emacsen to work asynchronosly."
  (let ((script (mt-async-shell-script (+ (* -0.5 range)
                                          (* (- local-count 2)
                                             (/ range 1.0
                                                (max 1 (- calcs 2)))))
                                       local-count)))
    (async-start
     `(lambda ()
        (shell-command ,script))
     ;; Analyze the result of the async process.
     `(lambda (x)
        (when (mt-evaluate-result ,local-count ,file ,forks ',section-list
                                  (with-temp-buffer
                                    (insert-file-contents
                                     (concat "/tmp/tmp.macro-type."
                                             (number-to-string ,local-count)
                                             ".log"))
                                    (buffer-string)))
          (shell-command (concat "rm /tmp/tmp.macro-type."
                                 (number-to-string ,local-count) ".*")))
        (when (boundp 'mt-init-overfull-boxes)
          (message (mt-minibuffer-message mt-init-overfull-boxes
                                          mt-best-overfull-boxes
                                          mt-init-underfull-boxes
                                          mt-best-underfull-boxes
                                          mt-receive-count ,calcs 'file)))
        (when (>= mt-receive-count ,calcs)
          (mt-final-calculation-IO ,calcs ,file ',section-list ,range))
        (when (<= (+ ,local-count ,forks) ,calcs)
          (mt-pdflatex-IO ,range ,file ,forks ,calcs
                          (+ ,local-count ,forks) ',section-list))))))

(defun mt-final-calculation-IO (calcs file section-list range)
  (let ((local-vector (mt-inject-mdframes calcs mt-best-file
                                          mt-overfull-matrix
                                          mt-underfull-matrix
                                          mt-number-of-blurs
                                          (make-vector
                                           (- (length section-list) 1)
                                           mt-best-file))))
    (mt-write-injection-IO mt-best-file section-list
                           (map 'list
                                (lambda (x)
                                  (mt-calculate-margin-change range calcs x
                                                              mt-best-file))
                                local-vector)))
  (shell-command
   (concat "cp /tmp/tmp.macro-type."
           (number-to-string mt-best-file) ".tex "
           (car (split-string file "\.tex$")) ".macro-type.tex; "
           "rm /tmp/tmp.macro-type.* ; "
           "pdflatex -output-directory "
           (car (split-string file "/[^/]+\.tex$"))
           " -interaction nonstopmode "
           (car (split-string file "\.tex$")) ".macro-type.tex"
           " > /dev/null"))
  (mt-dump-log-file-IO file)
  (shell-command (mt-plot-log-file-script file
                                          (length (elt mt-overfull-matrix 0))
                                          (length mt-overfull-matrix)))
  ;; Recalculate badness after mdframe injection.
  (with-temp-buffer
    (insert-file-contents (concat
                           (car (split-string file "\.tex$"))
                           ".macro-type.log"))
    (message
     (mt-minibuffer-message
      mt-init-overfull-boxes
      (let ((y 0))
        (mapc (lambda (x) (setq y (+ x y)))
              (mt-sum-errors-in-sections
               (buffer-string)
               section-list
               "^Overfull \\\\hbox (\\([0-9\.]+\\)pt too wide).*lines \\([0-9]+\\)"))
        y)
      mt-init-underfull-boxes
      (let ((y 0))
        (mapc (lambda (x) (setq y (+ x y)))
              (mt-sum-errors-in-sections
               (buffer-string)
               section-list
               "^Underfull \\\\hbox (badness \\([0-9\.]+\\)).*lines \\([0-9]+\\)"))
        y)
      mt-receive-count
      calcs
      file
      t))))

(defun mt-dump-log-file-IO (file)
  (with-temp-buffer
    (insert (replace-regexp-in-string
             "\\[\\|]" ""
             (format "%s" mt-overfull-matrix)))
    (newline)
    (insert (replace-regexp-in-string
             "\\[\\|]" ""
             (format "%s" mt-underfull-matrix)))
    (newline)
    (write-file (concat (car (split-string file "\.tex$"))
                        ".macro-type.plot.log"))))

(defun mt-write-injection-IO (file-number section-list margin-list)
  (let ((this-section-line (pop section-list))
        (next-section-line (car section-list))
        (margin-change (pop margin-list)))
    (when (not (= margin-change 0))
      (shell-command (concat "sed -i '" (number-to-string this-section-line)
                             " s/\\(.*\\)/\\1 \\\\begin{mdframed}"
                             "[hidealllines=true,"
                             "innertopmargin=2.1pt,skipabove=0mm,"
                             "innerleftmargin="
                             (number-to-string margin-change) "mm,"
                             "innerrightmargin="
                             (number-to-string margin-change) "mm]"
                             "/' /tmp/tmp.macro-type."
                             (number-to-string file-number) ".tex"))
      (shell-command (concat "sed -i '" (number-to-string next-section-line)
                             " s/\\(.*\\)/\\\\end{mdframed} \\1/' "
                             "/tmp/tmp.macro-type."
                             (number-to-string file-number) ".tex"))))
  (when margin-list
    (mt-write-injection-IO file-number section-list margin-list)))

(defun mt-evaluate-result (current-count file forks section-list result)
  ;; Count overfull and underfull hboxes.
  (let ((underfull-boxes 0)
        (overfull-boxes 0)
        (to-delete nil))
    ;; Make a matrix of the badness of all sections with all sizes.
    (aset mt-underfull-matrix
          (- current-count 1)
          (mapc (lambda (x) (setq underfull-boxes (+ x underfull-boxes)))
                (mt-sum-errors-in-sections
                 result
                 section-list
                 "^Underfull \\\\hbox (badness \\([0-9\.]+\\)).*lines \\([0-9]+\\)")))
    (aset mt-overfull-matrix
          (- current-count 1)
          (mapc (lambda (x) (setq overfull-boxes (+ x overfull-boxes)))
                (mt-sum-errors-in-sections
                 result
                 section-list
                 "^Overfull \\\\hbox (\\([0-9\.]+\\)pt too wide).*lines \\([0-9]+\\)")))
    ;; Remember the best page size.
    (if (> current-count 1)
        (if (> (+ (* 100 mt-best-overfull-boxes) mt-best-underfull-boxes)
               (+ (* 100 overfull-boxes) underfull-boxes))
            (setq mt-best-overfull-boxes overfull-boxes
                  mt-best-underfull-boxes underfull-boxes
                  mt-best-file current-count)
          (setq to-delete t))
      ;; Set initial badness.
      (setq mt-init-overfull-boxes overfull-boxes
            mt-best-overfull-boxes overfull-boxes
            mt-init-underfull-boxes underfull-boxes
            mt-best-underfull-boxes underfull-boxes
            mt-best-file current-count))
    (setq mt-receive-count (+ mt-receive-count 1))
    to-delete))

(defun mt-async-shell-script (size local-count)
  (concat
   (concat "echo"
           " \"\\usepackage{mdframed}"
           (when (and (> local-count 1)
                      (or (>= size 0.0001)
                          (<= size -0.0001)))
             (concat
              "\\addtolength"
              "{\\oddsidemargin}{" (number-to-string size) "mm}"
              "\\addtolength"
              "{\\evensidemargin}{" (number-to-string size) "mm}"
              "\\addtolength"
              "{\\textwidth}{" (number-to-string (* -2 size)) "mm}"))
           "\n\\begin{document} \" > "
           "/tmp/tmp.macro-type."
           (number-to-string local-count) ".header.tex;")
   (concat "cat"
           " /tmp/tmp.macro-type.begin"
           " /tmp/tmp.macro-type."
           (number-to-string local-count) ".header.tex"
           " /tmp/tmp.macro-type.end > /tmp/tmp.macro-type."
           (number-to-string local-count) ".tex;")
   (concat "pdflatex"
           " -output-directory /tmp"
           " -draftmode"
           " -interaction nonstopmode /tmp/tmp.macro-type."
           (number-to-string local-count) ".tex"
           " > /dev/null")))

(defun mt-plot-log-file-script (file sections calcs)
  (concat "R -e \"data = read.table('"
          (car (split-string file "\.tex$")) ".macro-type.plot.log"
          "', sep=' ', colClasses='numeric', dec='.') ;"
          "over = sqrt(data[1,]/max(c(0.01,max(data[1,])[1])));"
          "under = sqrt(data[2,]/max(c(0.01,max(data[2,])[1])));"
          "library(ggplot2);"
          "library(grid);"
          "df = data.frame(col=rgb(0.9*(1 - over), 0.9*(1 - under),"
          "0.9*(1.0 - 0.5*(over**2 + under**2))), expand.grid"
          "(x=1:" (number-to-string sections)
          ", y=1:" (number-to-string calcs) "));"
          "pdf('" (car (split-string file "\.tex$"))
          ".macro-type.plot.pdf" "') ;"
          "ggplot(df, aes(x=x, y=y))"
          " + theme_bw()"
          " + theme(rect=element_blank(),"
          "line = element_blank(), text=element_blank())"
          " + theme(axis.ticks.margin = unit(0, 'cm'),"
          "plot.margin = unit(c(-0.9,-0.9,-1.3,-1.3), 'cm'))"
          " + geom_tile(aes(fill=col))"
          " + scale_fill_identity();"
          "dev.off()\" > /dev/null"))

(defun mt-inject-mdframes (calcs
                           best-file
                           overfull-matrix
                           underfull-matrix
                           number-of-blurs
                           used-calculation-vector)
  "Change pagesize for sections."
  (let* ((section-count 0)
         (initial-vector used-calculation-vector))
    ;; Do this for every section.
    (while (< section-count (length used-calculation-vector))
      (let* ((local-best-file (elt initial-vector section-count))
             (overfull-slice (map 'list (lambda (x) (elt x section-count)) overfull-matrix))
             (underfull-slice (map 'list (lambda (x) (elt x section-count)) underfull-matrix))
             (combined-list (map 'list (lambda (x) (+ (* 100 x)
                                                      (pop underfull-slice)))
                                 overfull-slice)))
        ;; Calculate only when there is badness.
        (when (mt-is-there-badness-p combined-list
                                     local-best-file)
          ;; Look at all alternative pagesizes.
          (aset used-calculation-vector section-count
                (mt-nearest-good-file-number local-best-file
                                             combined-list))))
      (setq section-count (+ 1 section-count)))
    (if (<= number-of-blurs 0)
        (append used-calculation-vector nil)
      (mt-inject-mdframes calcs best-file overfull-matrix
                          underfull-matrix (- number-of-blurs 1)
                          (mt-blur-mdframe used-calculation-vector best-file)))))

(defun mt-sum-errors-in-sections (input section-list error-search-string)
  (let ((error-vector (make-vector (- (length section-list) 1) 0)))
    (with-temp-buffer
      (insert input)
      (goto-char (point-min))
      (while (re-search-forward error-search-string nil t)
        (let ((local-count 0))
          (while (and (< local-count (- (length section-list) 1))
                      (<= (nth local-count section-list)
                          (string-to-number (match-string 2))))
            (setq local-count (+ local-count 1)))
          (aset error-vector (- local-count 1)
                (+ (string-to-number (match-string 1))
                   (elt error-vector (- local-count 1)))))))
    error-vector))

(defun mt-file-check (file)
  "Return t when file ends in .tex."
  (with-temp-buffer
    (insert file)
    (goto-char (point-min))
    (re-search-forward "/$\\|\\.tex$" nil t)))

(defun mt-nearest-good-file-number (best-file-number combined-list)
  (let ((calcs (length underfull-matrix))
        (local-file-count 0)
        (local-file-number best-file-number))
    (while (<= local-file-count calcs)
      (when (and (<= (+ local-file-count best-file-number) calcs)
                 (< (nth (+ local-file-count (- best-file-number 1))
                         combined-list)
                    (nth (- local-file-number 1)
                         combined-list)))
        (setq local-file-number (+ local-file-count best-file-number)))
      (when (and (> (- best-file-number local-file-count) 0)
                 (< (nth (- (- best-file-number 1) local-file-count)
                         combined-list)
                    (nth (- local-file-number 1)
                         combined-list)))
        (setq local-file-number (- best-file-number local-file-count)))
      (setq local-file-count (+ 1 local-file-count)))
    local-file-number))

(defun mt-is-there-badness-p (combined-list best-file-number)
  (> (nth (- best-file-number 1) combined-list) 0))

(defun mt-calculate-margin-change (range
                                   calcs
                                   local-file-number
                                   best-file-number)
  (let ((margin-increase (* -0.5 range))
        (increment (/ range 1.0 (max 1 (- calcs 2)))))
    (if (= local-file-number 1)
        (* -1 (+ margin-increase
                 (* (- best-file-number 2) increment)))
      (- (+ margin-increase
            (* (- local-file-number 2) increment))
         (+ margin-increase
            (* (- best-file-number 2) increment))))))

(defun mt-minibuffer-message (init-overfull best-overfull
                                            init-underfull
                                            best-underfull
                                            receive
                                            calcs
                                            result-file
                                            &optional last-run)
  (concat
   (if (and (= init-overfull 0)
            (= best-overfull 0)) "No overfull hboxes"
     (concat "Overfull hboxes reduced by "
             (number-to-string
              (round
               (/ (* 100 (- init-overfull best-overfull))
                  init-overfull))) "%% from "
                  (number-to-string (round init-overfull)) "pt to "
                  (number-to-string (round best-overfull)) "pt"))
   "  ||  "
   (if (and (= init-underfull 0)
            (= best-underfull 0)) "No underfull hboxes"
     (concat "Underfull hboxes reduced by "
             (number-to-string
              (round
               (/ (* 100 (- init-underfull best-underfull))
                  init-underfull))) "%% from "
                  (number-to-string (round init-underfull)) " to "
                  (number-to-string (round best-underfull))))
   "  ||  "
   (number-to-string receive) "/"
   (number-to-string calcs) " compiled"
   (when last-run (concat "
    output: " (car (split-string result-file "\.tex$"))
    ".macro-type.* calculated in "
    (format-time-string "%s" (time-since mt-benchmark)) "s"))))

(defun mt-blur-mdframe (input best-file &optional result)
  (when (not result)
    (setq input (map 'list (lambda (x) (- x best-file)) input))
    (setq input (append (list (nth 1 input)) input
                        (list (nth (- (length input) 2) input)))))
  (if (not (cdr (cdr input))) (vconcat (reverse result))
    (let ((new (cons (+ best-file (/ (+ (pop input) (car input)
                                        (car (cdr input))) 3)) result)))
      (mt-blur-mdframe input best-file new))))

(provide 'macro-type)

;;; macro-type.el ends here
