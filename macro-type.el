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

(defun mt-macro-type-tex-file (file range calculations forks)
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
          mt-range range
          mt-best-file 1
          ;; Get line numbers of sections, including begin and end document.
          mt-section-list
          (map 'list 'string-to-number
               (split-string
                (shell-command-to-string
                 (concat
                  "grep -n 'section{.*}\\|begin{document}' "
                  file " | grep -o ^[0-9]*"))))
          mt-underfull-matrix (make-vector calculations 0)
          mt-overfull-matrix (make-vector calculations 0))
    ;; Save the header and the body of the tex file to access them faster
    (with-temp-buffer
      (insert-file-contents file)
      (let ((this-buffer (buffer-string)))
        (with-temp-buffer
          (insert
           (car
            (split-string this-buffer "\n.*\\\\begin{document}.*\n")))
          (write-file "/tmp/tmp.macro-type.begin"))
        (with-temp-buffer
          (insert
           (car
            (cdr (split-string this-buffer "\n.*\\\\begin{document}.*\n"))))
          (write-file "/tmp/tmp.macro-type.end"))))
    (let ((local-count 1))
      (while (<= local-count forks)
        (mt-pdflatex-IO range file forks calculations local-count)
        (setq local-count (+ 1 local-count))))))

(defun mt-evaluate-result-IO (current-count file forks calculations)
  ;; Count overfull and underfull hboxes.
  (let ((underfull-boxes 0)
        (overfull-boxes 0)
        (result nil))
    (with-temp-buffer
      (insert-file-contents (concat "/tmp/tmp.macro-type."
                                    (number-to-string current-count) ".log"))
      (setq result (buffer-string)))
    ;; Make a matrix of the badness of all sections with all sizes.
    (aset mt-underfull-matrix
          (- current-count 1)
          (mapc (lambda (x) (setq underfull-boxes (+ x underfull-boxes)))
                (mt-sum-errors-in-sections
                 result
                 mt-section-list
                 "^Underfull \\\\hbox (badness \\([0-9\.]+\\)).*lines \\([0-9]+\\)")))
    (aset mt-overfull-matrix
          (- current-count 1)
          (mapc (lambda (x) (setq overfull-boxes (+ x overfull-boxes)))
                (mt-sum-errors-in-sections
                 result
                 mt-section-list
                 "^Overfull \\\\hbox (\\([0-9\.]+\\)pt too wide).*lines \\([0-9]+\\)")))
    ;; Remember the best page size.
    (if (> current-count 1)
        (if (> (+ (* 100 mt-best-overfull-boxes) mt-best-underfull-boxes)
               (+ (* 100 overfull-boxes) underfull-boxes))
            (setq mt-best-overfull-boxes overfull-boxes
                  mt-best-underfull-boxes underfull-boxes
                  mt-best-file current-count)
          (shell-command (concat "rm /tmp/tmp.macro-type."
                                 (number-to-string current-count) ".*")))
      ;; Set initial badness.
      (setq mt-init-overfull-boxes overfull-boxes
            mt-best-overfull-boxes overfull-boxes
            mt-init-underfull-boxes underfull-boxes
            mt-best-underfull-boxes underfull-boxes))
    (setq mt-receive-count (+ mt-receive-count 1))
    ;; Show the current state.
    (when (boundp 'mt-init-overfull-boxes)
      (message (mt-minibuffer-message mt-init-overfull-boxes
                                      mt-best-overfull-boxes
                                      mt-init-underfull-boxes
                                      mt-best-underfull-boxes
                                      mt-receive-count
                                      calculations
                                      file)))
    ;; Finish calculations.
    (when (>= mt-receive-count calculations)
      (mt-final-calculation-IO calculations file))))

(defun mt-final-calculation-IO (calculations file)
  (mt-inject-mdframes calculations)
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
  (mt-plot-log-file-IO file
                       (length (elt mt-overfull-matrix 0))
                       (length mt-overfull-matrix))
  ;; Recalculate badness after mdframe injection.
  (with-temp-buffer
    (insert-file (concat
                  (car (split-string file "\.tex$"))
                  ".macro-type.log"))
    (message
     (mt-minibuffer-message
      mt-init-overfull-boxes
      (let ((y 0))
        (mapc (lambda (x) (setq y (+ x y)))
              (mt-sum-errors-in-sections
               (buffer-string)
               mt-section-list
               "^Overfull \\\\hbox (\\([0-9\.]+\\)pt too wide).*lines \\([0-9]+\\)"))
        y)
      mt-init-underfull-boxes
      (let ((y 0))
        (mapc (lambda (x) (setq y (+ x y)))
              (mt-sum-errors-in-sections
               (buffer-string)
               mt-section-list
               "^Underfull \\\\hbox (badness \\([0-9\.]+\\)).*lines \\([0-9]+\\)"))
        y)
      mt-receive-count
      calculations
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

(defun mt-plot-log-file-IO (file sections calculations)
  (shell-command
   (concat "R -e \"data = read.table('"
           (car (split-string file "\.tex$")) ".macro-type.plot.log"
           "', sep=' ', colClasses='numeric', dec='.') ;"
           "over = sqrt(data[1,]/max(c(0.01,max(data[1,])[1])));"
           "under = sqrt(data[2,]/max(c(0.01,max(data[2,])[1])));"
           "library(ggplot2);"
           "library(grid);"
           ;; "df = data.frame(col=rgb(0.9*(1 - over),0.9*(1 - under),0.9*(1.0 - 0.5*over - 0.5*under)), expand.grid"
           ;; "df = data.frame(col=rgb(0.9*(1 - over),0.9*(1 - under),0.9*(1.0 - 0.5*(over**2 + under**2))), expand.grid"
           "df = data.frame(col=rgb(0.9*(1 - sqrt(0.8*over+0.2*under)),0.9*(1 - sqrt(0.8*under+0.2*over)),0.9*(1.0 - sqrt(0.2*(over + under)))), expand.grid"
           "(x=1:" (number-to-string sections)
           ", y=1:" (number-to-string calculations) "));"
           "pdf('" (car (split-string file "\.tex$"))
           ".macro-type.plot.pdf" "') ;"
           "ggplot(df, aes(x=x, y=y))"
           " + theme_bw()"
           " + theme(rect=element_blank(),line = element_blank(), text=element_blank())"
           " + theme(axis.ticks.margin = unit(0, 'cm'),plot.margin = unit(c(-0.9,-0.9,-1.3,-1.3), 'cm'))"
           " + geom_tile(aes(fill=col))"
           " + scale_fill_identity();"
           "dev.off()\" > /dev/null")))

(defun mt-write-injection-IO (this-section-line
                              next-section-line
                              margin-change
                              file-number)
  (when (or (>= margin-change 0.0001)
            (<= margin-change -0.0001))
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

(defun mt-pdflatex-IO (range file forks calculations local-count)
  "Starts multiple emacsen to work asynchronosly."
  (async-start
   `(lambda ()
      ;; Inject variables into new emacsen.
      (let* ((local-count ,local-count)
             (size (+ (* -0.5 ,range)
                      (* (- local-count 2)
                         (/ ,range 1.0 (max 1 (- ,calculations 2)))))))
        ;; Save different page sizes, using echo is for speed
        (shell-command
         (concat "echo \"\\usepackage{mdframed}"
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
                 (concat "/tmp/tmp.macro-type."
                         (number-to-string local-count) ".header.tex")))
        ;; Concatenate parts of the file, compile it and return log as string.
        ;; Doing this in emacs buffers is very slow.
        (shell-command
         (concat "cat"
                 " /tmp/tmp.macro-type.begin"
                 " /tmp/tmp.macro-type."
                 (number-to-string local-count) ".header.tex"
                 " /tmp/tmp.macro-type.end > /tmp/tmp.macro-type."
                 (number-to-string local-count) ".tex;"
                 "pdflatex"
                 " -output-directory /tmp"
                 " -draftmode"
                 " -interaction nonstopmode /tmp/tmp.macro-type."
                 (number-to-string local-count) ".tex"
                 " > /dev/null"))))
   ;; Analyze the result of the async process.
   `(lambda (result) (mt-evaluate-result-IO ,local-count
                                            ,file
                                            ,forks
                                            ,calculations)
      (when (<= (+ ,local-count ,forks) ,calculations)
        (mt-pdflatex-IO ,range ,file ,forks ,calculations (+ ,local-count ,forks))))))

(defun mt-inject-mdframes (calculations)
  "Change pagesize for sections."
  (let ((section-count 0)
        (blur-count 0)
        (blur-vector (make-vector (- (length mt-section-list) 1)
                                  mt-best-file))
        (used-calculation-vector
         (make-vector (- (length mt-section-list) 1) mt-best-file)))
    (while (< blur-count 3)
      (setq blur-count (+ blur-count 1)
            section-count 0
            blur-vector (mt-blur-mdframe used-calculation-vector
                                         mt-best-file)
            used-calculation-vector blur-vector)
      ;; Do this for every section.
      (while (< section-count (- (length mt-section-list) 1))
        (let ((best-file (elt blur-vector section-count)))
          ;; Calculate only when there is badness.
          (when (mt-is-there-badness-p mt-overfull-matrix
                                       mt-underfull-matrix
                                       best-file
                                       section-count)
            ;; Look at all alternative pagesizes.
            (aset used-calculation-vector section-count
                  (mt-nearest-good-file-number best-file
                                               mt-underfull-matrix
                                               mt-overfull-matrix
                                               section-count))))
        (setq section-count (+ 1 section-count))))
    (setq section-count 0)
    (while (< section-count (- (length mt-section-list) 1))
      ;; Calculate change of the margins, considering already changed size.
      (when (not (= (elt used-calculation-vector section-count)
                    mt-best-file))
        ;; Inject new margin size.
        (mt-write-injection-IO (nth section-count mt-section-list)
                               (nth (+ 1 section-count) mt-section-list)
                               (mt-calculate-margin-change
                                mt-range
                                calculations
                                (elt used-calculation-vector
                                     section-count)
                                mt-best-file)
                               mt-best-file))
      (setq section-count (+ 1 section-count))))
  ;; Actually, they are already injected.
  (message "Injecting mdframes"))


(defun mt-sum-errors-in-sections (input section-list error-search-string)
  (let ((error-vector (make-vector (length section-list) 0)))
    (with-temp-buffer
      (insert input)
      (goto-char (point-min))
      (while (re-search-forward error-search-string nil t)
        (let ((local-count 0))
          (while (and (< local-count (length section-list))
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

(defun mt-nearest-good-file-number (best-file-number
                                    underfull-matrix
                                    overfull-matrix
                                    current-section)
  (let ((calculations (length underfull-matrix))
        (local-file-number best-file-number)
        (local-file-count 0))
    (while (< local-file-count calculations)
      (when (and (<= (+ local-file-count best-file-number) calculations)
                 (< (+ (* 100 (elt (elt overfull-matrix
                                        (+ local-file-count
                                           (- best-file-number 1)))
                                   current-section))
                       (elt (elt underfull-matrix
                                 (+ local-file-count
                                    (- best-file-number 1)))
                            current-section))
                    (+ (* 100 (elt (elt overfull-matrix
                                        (- local-file-number 1))
                                   current-section))
                       (elt (elt underfull-matrix
                                 (- local-file-number 1))
                            current-section))))
        (setq local-file-number (+ local-file-count best-file-number)))
      (when (and (> (- best-file-number local-file-count) 0)
                 (< (+ (* 100 (elt (elt overfull-matrix
                                        (- (- best-file-number 1)
                                           local-file-count))
                                   current-section))
                       (elt (elt underfull-matrix
                                 (- (- best-file-number 1)
                                    local-file-count))
                            current-section))
                    (+ (* 100 (elt (elt overfull-matrix
                                        (- local-file-number 1))
                                   current-section))
                       (elt (elt underfull-matrix
                                 (- local-file-number 1))
                            current-section))))
        (setq local-file-number (- best-file-number local-file-count)))
      (setq local-file-count (+ 1 local-file-count)))
    local-file-number))

(defun mt-is-there-badness-p (overfull-matrix
                              underfull-matrix
                              best-file-number
                              current-section)
  (> (+ (* 100 (elt (elt overfull-matrix (- best-file-number 1))
                    current-section))
        (elt (elt underfull-matrix
                  (- best-file-number 1))
             current-section)) 0))

(defun mt-calculate-margin-change (range
                                   calculations
                                   local-file-number
                                   best-file-number)
  (let ((margin-increase (* -0.5 range))
        (increment (/ range 1.0 (max 1 (- calculations 2)))))
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
                                            calculations
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
   (number-to-string calculations) " compiled"
   (when last-run (concat "
    output: " (car (split-string result-file "\.tex$"))
    ".macro-type.*"))))

(defun mt-blur-mdframe (input-list best-file)
  (let ((count 0)
        (blur (make-vector (length input-list) 0))
        (local-list (map 'list (lambda (x) (- x best-file)) input-list)))
    (add-to-list 'local-list (nth 1 local-list) nil (lambda (x y) nil))
    (add-to-list 'local-list (nth (- (length local-list) 2) local-list)
                 t (lambda (x y) nil))
    (while (< count (length input-list))
      (aset blur count (+ best-file (/ (+ (nth count local-list)
                                          (nth (+ count 1) local-list)
                                          (nth (+ count 2) local-list)) 3)))
      (setq count (+ count 1)))
    blur))

(provide 'macro-type)

;;; macro-type.el ends here
