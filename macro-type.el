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

;;; Commentary:

;; This library is used to enhance the quality of LaTeX output.  It
;; parses the log file and manipulates afterwards the margins by a
;; small amount and compares whether the output is with less over- and
;; underfull hboxes.  The standard manipulates the margins on three
;; levels: on document, section and paragraph level.  This can
;; changed, especially the paragraph level is *very* resource
;; intensive on big documents.  Furthermore it's possible to chose on
;; execution the amount of manipulation which is allowed and number of
;; times which are used to find a better solution.  Additonaly it puts
;; out a graph, for this case an installation of GNU R is necessary,
;; but the plot can be omitted.

;;; Code:
(require 'async)

(defvar mt-receive-count)
(defvar mt-best-file)
(defvar mt-underfull-matrix)
(defvar mt-overfull-matrix)
(defvar mt-init-underfull-boxes nil)
(defvar mt-init-overfull-boxes nil)
(defvar mt-best-underfull-boxes)
(defvar mt-best-overfull-boxes)
(defvar mt-benchmark)
(defvar mt-number-of-blurs 4)
(defvar mt-no-overfull)
(defvar mt-no-underfull)
(defvar do-section t)
(defvar do-paragraph t)
(defvar do-graph t)
(defvar mt-max-plot-size 100)

(defun mt-macro-type-tex-file (file range times forks)
  "Change the pagesize of a tex file to optimize it.
It compiles TIMES the same FILE and looks at the log files to
minimize overfull and underfull hboxes by changing the margins by
different lengths in RANGE.  It uses a number of FORKS to
parallelize the calculation."
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
          mt-no-overfull t
          mt-no-underfull t
          mt-best-overfull-boxes nil
          mt-benchmark (current-time)
          ;; Get line numbers of sections, including begin and end document.
          mt-underfull-matrix (make-vector times 0)
          mt-overfull-matrix (make-vector times 0))
    ;; Save the header and the body of the tex file to access them faster
    (let ((local-count 1)
          (section-list (mt-retrieve-file-info-IO file)))
      (when do-paragraph
        (setq section-list (mt-find-paragraphs-IO file)))
      (while (and (<= local-count forks)
                  (<= local-count times))
        (mt-pdflatex-IO range file forks times local-count section-list)
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
      "grep -n 'section\\**{.*}\\|begin{document}\\|end{document}' "
      file " | grep -o ^[0-9]*")
     t t)
    (map 'list 'string-to-number (split-string (buffer-string)))))

(defun mt-find-paragraphs-IO (file)
  (let ((paragraph-list nil))
    (with-temp-buffer
      (let ((counts 0))
        (insert-file-contents file)
        (goto-char (point-min))
        (while (re-search-forward "^[^\\\\\n]+\\(\n[ \n]*$\\)" nil t)
          (setq paragraph-list (append paragraph-list
				       (list (line-number-at-pos (match-end 0))))))))
    paragraph-list))

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
        (when mt-init-overfull-boxes
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
  (when do-section
    (let ((local-vector (mt-inject-mdframes calcs mt-best-file
                                            mt-overfull-matrix
                                            mt-underfull-matrix
                                            mt-number-of-blurs
                                            (make-vector
                                             (- (length section-list) 1)
                                             mt-best-file))))
      (mt-write-injection-IO mt-best-file (cdr section-list)
                             (cdr
                              (map 'list
                                   (lambda (x)
                                     (mt-calculate-margin-change range calcs x
                                                                 mt-best-file))
                                   local-vector)))))
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
  (when do-graph
    (mt-dump-log-file-IO file mt-no-underfull mt-no-overfull)
    (shell-command (mt-plot-log-file-script file
                                            (length (elt mt-overfull-matrix 0))
                                            (length mt-overfull-matrix)
                                            mt-no-underfull
                                            mt-no-overfull)))
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

(defun mt-dump-log-file-IO (file no-underfull no-overfull)
  (with-temp-buffer
    (when (not no-overfull)
      (let* ((local-max 0)
             (local-overfull mt-overfull-matrix)
             (local-count 1)
             (acc-list (make-list (length (elt local-overfull 0)) 0))
             (result-count 0)
             (acc-count 0)
             (result-matrix (make-vector mt-max-plot-size 0)))
        (when (> (length local-overfull) mt-max-plot-size)
          (mapc (lambda (x)
                  (setq local-count (+ 1 local-count))
                  (setq acc-list (map 'list (lambda (y) (+ y (pop acc-list))) x))
                  (setq acc-count (+ 1 acc-count))
                  (when (>= local-count (/ (* 1.0 (length local-overfull)) mt-max-plot-size))
                    (progn
                      (setq local-count (- local-count (/ (* 1.0 (length local-overfull)) mt-max-plot-size)))
                      (aset result-matrix result-count (map 'list (lambda (z) (/ z acc-count))
                                                            acc-list))
                      (setq acc-list (make-list (length (elt local-overfull 0)) 0))
                      (setq acc-count 0)
                      (setq result-count (+ 1 result-count)))))
                local-overfull)
          (setq local-overfull result-matrix))

        (when (> (length (elt local-overfull 0)) mt-max-plot-size)
          (setq local-count 1)
          (setq acc-list nil)
          (setq result-count 0)
          (setq acc-count 0)
          (setq div-count 0)
          (setq result-matrix (make-vector (length local-overfull) 0))
          (mapc (lambda (x)
                  (mapc (lambda (y)
                          (setq local-count (+ 1 local-count))
                          (setq acc-count (+ acc-count y))
                          (setq div-count (+ 1 div-count))
                          (when (>= local-count (/ (* 1.0 (length x)) mt-max-plot-size))
                            (progn
                              (setq local-count (- local-count (/ (* 1.0 (length x)) mt-max-plot-size)))
                              (setq acc-list (append acc-list (list (/ acc-count div-count))))
                              (setq div-count 0)
                              (setq acc-count 0))))
                        x)
                  (aset result-matrix result-count acc-list)
                  (setq result-count (+ 1 result-count))
                  (setq acc-list nil)
                  (setq local-count 1)
                  (setq acc-count 0))
                local-overfull)
          (setq local-overfull result-matrix))

        (mapc
         (lambda (x) (mapc
                      (lambda (y) (setq local-max (max local-max y)))
                      x))
         local-overfull)
        (insert (replace-regexp-in-string
                 "\\[\\|]" ""
                 (format "%s"
                         (map 'vector
                              (lambda (x)
                                (map 'vector
                                     (lambda (y)
                                       (truncate (* 1000
                                                    (sqrt (/ (* 1.0 y) local-max)))))
                                     x))
                              local-overfull)))))
      (newline))
    
    (when (not no-underfull)
      (let* ((local-max 0)
             (local-underfull mt-underfull-matrix)
             (local-count 1)
             (acc-list (make-list (length (elt local-underfull 0)) 0))
             (result-count 0)
             (acc-count 0)
             (result-matrix (make-vector mt-max-plot-size 0)))
        (when (> (length local-underfull) mt-max-plot-size)
          (mapc (lambda (x)
                  (setq local-count (+ 1 local-count))
                  (setq acc-list (map 'list (lambda (y) (+ y (pop acc-list))) x))
                  (setq acc-count (+ 1 acc-count))
                  (when (>= local-count (/ (* 1.0 (length local-underfull)) mt-max-plot-size))
                    (progn
                      (setq local-count (- local-count (/ (* 1.0 (length local-underfull)) mt-max-plot-size)))
                      (aset result-matrix result-count (map 'list (lambda (z) (/ z acc-count))
                                                            acc-list))
                      (setq acc-list (make-list (length (elt local-underfull 0)) 0))
                      (setq acc-count 0)
                      (setq result-count (+ 1 result-count)))))
                local-underfull)
          (setq local-underfull result-matrix))

        (when (> (length (elt local-underfull 0)) mt-max-plot-size)
          (setq local-count 1)
          (setq acc-list nil)
          (setq acc-count 0)
          (setq result-count 0)
          (setq div-count 0)
          (setq result-matrix (make-vector (length local-underfull) 0))
          (mapc (lambda (x)
                  (mapc (lambda (y)
                          (setq local-count (+ 1 local-count))
                          (setq acc-count (+ acc-count y))
                          (setq div-count (+ 1 div-count))
                          (when (>= local-count (/ (* 1.0 (length x)) mt-max-plot-size))
                            (progn
                              (setq local-count (- local-count (/ (* 1.0 (length x)) mt-max-plot-size)))
                              (setq acc-list (append acc-list (list (/ acc-count div-count))))
                              (setq div-count 0)
                              (setq acc-count 0))))
                        x)
                  (aset result-matrix result-count acc-list)
                  (setq result-count (+ 1 result-count))
                  (setq acc-list nil)
                  (setq local-count 1)
                  (setq acc-count 0))
                local-underfull)
          (setq local-underfull result-matrix))



        (mapc
         (lambda (x) (mapc
                      (lambda (y) (setq local-max (max local-max y)))
                      x))
         local-underfull)
        (insert (replace-regexp-in-string
                 "\\[\\|]" ""
                 (format "%s"
                         (map 'vector
                              (lambda (x)
                                (map 'vector
                                     (lambda (y)
                                       (truncate (* 1000
                                                    (sqrt (/ (* 1.0 y) local-max)))))
                                     x))
                              local-underfull)))))
      (newline))
    (write-file (concat (car (split-string file "\.tex$"))
                        ".macro-type.plot.log"))))

(defun mt-write-injection-IO (file-number section-list margin-list)
  (while margin-list
    (let ((this-section-line (pop section-list))
	  (next-section-line (car section-list))
	  (margin-change (pop margin-list)))
      (when (not (= margin-change 0))
	(shell-command (concat "sed -i '" (number-to-string this-section-line)
			       " s/\\(.*\\)/\\1 \\\\begingroup\\\\leftskip"
			       (number-to-string (/ margin-change 2)) "mm"
			       "\\\\rightskip\\\\leftskip"
			       "/' /tmp/tmp.macro-type."
			       (number-to-string file-number) ".tex"))
       (shell-command (concat "sed -i '" (number-to-string next-section-line)
			      " s/\\(.*\\)/\\\\endgroup"
			      "\\1/' /tmp/tmp.macro-type."
			      (number-to-string file-number) ".tex"))))))

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
    (when (and mt-no-underfull (> underfull-boxes 0))
      (setq mt-no-underfull nil))
    (when (and mt-no-overfull (> overfull-boxes 0))
      (setq mt-no-overfull nil))
    ;; Remember the best page size.
    (when (= current-count 1)
      (setq mt-init-overfull-boxes overfull-boxes
            mt-init-underfull-boxes underfull-boxes))
    (if mt-best-overfull-boxes
        (if (> (+ (* 100 mt-best-overfull-boxes) mt-best-underfull-boxes)
               (+ (* 100 overfull-boxes) underfull-boxes))
            (setq mt-best-overfull-boxes overfull-boxes
                  mt-best-underfull-boxes underfull-boxes
                  mt-best-file current-count)
          (setq to-delete t))
      ;; Set initial badness.
      (setq mt-best-overfull-boxes overfull-boxes
            mt-best-underfull-boxes underfull-boxes
            mt-best-file current-count))
    (setq mt-receive-count (+ mt-receive-count 1))
    to-delete))

(defun mt-async-shell-script (size local-count)
  (concat
   (concat "echo"
           " \"\\usepackage{changepage}"
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

(defun mt-plot-log-file-script (file sections calcs no-underfull no-overfull)
  (concat "R -e \"mydata = read.table('"
          (car (split-string file "\.tex$")) ".macro-type.plot.log"
          "', sep=' ') ;"
          (if no-overfull (concat "over = 0;"
                                  "under = mydata[1,]/1000;")
            (if no-underfull (concat "over = mydata[1,]/1000;"
                                     "under = 0;")
              (concat "over = mydata[1,]/1000;"
                      "under = mydata[2,]/1000;")))
          "rm(mydata);"
          "library(ggplot2);"
          "library(grid);"
          "df = data.frame(col=rgb(0.9*(1 - over), 0.9*(1 - under),"
          "0.9*(1.0 - 0.5*(over**2 + under**2))), expand.grid"
          "(x=1:" (number-to-string (min sections mt-max-plot-size))
          ", y=1:" (number-to-string (min calcs mt-max-plot-size)) "));"
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
             (overfull-slice (map 'list (lambda (x) (elt x section-count))
                                  overfull-matrix))
             (underfull-slice (map 'list (lambda (x) (elt x section-count))
                                   underfull-matrix))
             (combined-list (map 'list (lambda (x) (+ (* 100 x)
                                                      (pop underfull-slice)))
                                 overfull-slice)))
        ;; Calculate only when there is badness.
        (when (mt-is-there-badness-p combined-list local-best-file)
          ;; Look at all alternative pagesizes.
          (aset used-calculation-vector section-count
                (mt-nearest-good-file-number local-best-file combined-list))))
      (setq section-count (+ 1 section-count)))
    (if (<= number-of-blurs 0)
        (append used-calculation-vector nil)
      (mt-inject-mdframes calcs best-file overfull-matrix
                          underfull-matrix (- number-of-blurs 1)
                          (mt-blur-mdframe used-calculation-vector
                                           best-file)))))

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
	  (when (> local-count 0)
	    (aset error-vector (- local-count 1)
		  (+ (string-to-number (match-string 1))
		     (elt error-vector (- local-count 1))))))))
    error-vector))

(defun mt-file-check (file)
  "Return t when file ends in .tex."
  (with-temp-buffer
    (insert file)
    (goto-char (point-min))
    (re-search-forward "/$\\|\\.tex$" nil t)))

(defun mt-nearest-good-file-number (best-file-number combined-list)
  (let ((calcs (length combined-list))
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
                  (max init-overfull 1)))) "%% from "
                  (number-to-string (round init-overfull)) "pt to "
                  (number-to-string (round best-overfull)) "pt"))
   "  ||  "
   (if (and (= init-underfull 0)
            (= best-underfull 0)) "No underfull hboxes"
     (concat "Underfull hboxes reduced by "
             (number-to-string
              (round
               (/ (* 100 (- init-underfull best-underfull))
		  (max init-underfull 1)))) "%% from "
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
