;; TODO: refactor, functional, smooth mdframes out
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

(defun mt-macro-type-tex-file (file range times cores)
  "Change the pagesize of a tex file to optimize it.
It compiles a lot of times the same file and looks at the log files to minimize
overfull and underfull hboxes.  Afterwards, it uses mdframes to alter the
pagesize of individual sections."
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
          mt-start-count 0
          mt-calculations times
          mt-forks cores
          mt-range range
          mt-best-file 1
          mt-result-file file
          mt-benchmark (current-time)
          mt-current-count 1
          ;; Get line numbers of sections, including begin and end document.
          mt-section-list
          (map 'list 'string-to-number
               (split-string
                (shell-command-to-string
                 (concat
                  "grep -n 'section{.*}\\|begin{document}\\|end{document}' "
                  file " | grep -o ^[0-9]*"))))
          mt-section-underfull-vector (make-vector (length mt-section-list) 0)
          mt-section-overfull-vector (make-vector (length mt-section-list) 0)
          mt-all-underfull-vector (make-vector times 0)
          mt-all-overfull-vector (make-vector times 0))
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
    (mt-pdflatex)))

(defun mt-file-check (file)
  "Return t when file ends in .tex."
  (with-temp-buffer
    (insert file)
    (goto-char (point-min))
    (re-search-forward "/$\\|\\.tex$" nil t)))

(defun mt-pdflatex ()
  "Starts multiple emacsen to work asynchronosly."
  (setq mt-start-count (+ mt-start-count 1))
  (async-start
   `(lambda ()
      ;; Inject variables into new emacsen.
      (setq mt-range ,mt-range
            mt-margin-increase (- 0 (* 0.5 mt-range))
            mt-times ,mt-start-count
            mt-increment (/ mt-range 1.0 (max 1 (- ,mt-calculations 2))))
      ;; Save different page sizes, using echo is for speed
      (shell-command
       (concat "echo \""
               (when (> mt-times 1)
                 (let ((size (+ mt-margin-increase
                                (* (- mt-times 2) mt-increment))))
                   (concat " \\usepackage{mdframed}\\usepackage{color}\\definecolor{theme}{rgb}{1,0.5,0.5}\\addtolength{\\oddsidemargin}{"
                           (number-to-string size)
                           "mm}\\addtolength{\\evensidemargin}{"
                           (number-to-string size)
                           "mm}\\addtolength{\\textwidth}{"
                           (number-to-string (* -2 size)) "mm}")))
               "\n\\begin{document} \" > "
               (concat "/tmp/tmp.macro-type.header."
                       (number-to-string mt-times) ".tex")))
      ;; Concatenate parts of the file, compile it and return log as string.
      ;; Doing this in emacs buffers is very slow.
      (shell-command-to-string
       (concat "cat"
               " /tmp/tmp.macro-type.begin"
               " /tmp/tmp.macro-type.header."
               (number-to-string ,mt-start-count) ".tex"
               " /tmp/tmp.macro-type.end > /tmp/tmp.macro-type."
               (number-to-string ,mt-start-count)
               ".tex ; pdflatex"
               " -output-directory /tmp"
               " -draftmode"
               " -interaction nonstopmode /tmp/tmp.macro-type."
               (number-to-string ,mt-start-count) ".tex")))
   ;; Analyze the result of the async process.
   'mt-evaluate-result)
  ;; If there are less processes than usable cores, start a new one.
  ;; TODO: start all processes at the same time.
  (when (and (< (- mt-start-count mt-receive-count) mt-forks)
             (< mt-start-count mt-calculations)
             (> mt-start-count 1))
    (mt-pdflatex)))

(defun mt-evaluate-result (result)
  ;; Count overfull and underfull hboxes.
  (mt-evaluate-boxes result)
  ;; Make a matrix of the badness of all sections with all sizes.
  (aset mt-all-underfull-vector
        (- mt-current-count 1) (copy-sequence mt-section-underfull-vector))
  (aset mt-all-overfull-vector
        (- mt-current-count 1) (copy-sequence mt-section-overfull-vector))
  ;; Remember the best page size.
  (if (> mt-current-count 1)
      (when (> (+ (* 100 mt-best-overfull-boxes) mt-best-underfull-boxes)
               (+ (* 100 mt-overfull-boxes) mt-underfull-boxes))
        (setq mt-best-overfull-boxes mt-overfull-boxes
              mt-best-underfull-boxes mt-underfull-boxes
              mt-best-file mt-current-count))
    ;; Set initial badness.
    (setq mt-init-overfull-boxes mt-overfull-boxes
          mt-best-overfull-boxes mt-overfull-boxes
          mt-init-underfull-boxes mt-underfull-boxes
          mt-best-underfull-boxes mt-underfull-boxes))
  (setq mt-receive-count (+ mt-receive-count 1))
  ;; Show the current state.
  (message (mt-minibuffer-message))
  ;; If there are less processes than usable cores, start a new one.
  (when (and (< (- mt-start-count mt-receive-count) mt-forks)
             (< mt-start-count mt-calculations))
    (mt-pdflatex))
  ;; Finish calculations.
  (when (>= mt-receive-count mt-calculations)
    (mt-inject-mdframes)
    (shell-command
     (concat "cp /tmp/tmp.macro-type."
             (number-to-string mt-best-file)
             ".tex "
             (car (split-string mt-result-file "\.tex$"))
             ".macro-type.tex ; rm /tmp/tmp.macro-type.* ; pdflatex -output-directory "
             (car (split-string mt-result-file "/[^/]+\.tex$"))
             " -interaction nonstopmode "
             (car (split-string mt-result-file "\.tex$"))
             ".macro-type.tex > /dev/null" ))
    ;; Recalculate badness after mdframe injection.
    (with-temp-buffer
      (insert-file (concat
                    (car (split-string mt-result-file "\.tex$"))
                    ".macro-type.log"))
      (mt-evaluate-boxes (buffer-string)))
    (when (> (+ (* 100 mt-best-overfull-boxes) mt-best-underfull-boxes)
             (+ (* 100 mt-overfull-boxes) mt-underfull-boxes))
      (setq mt-best-overfull-boxes mt-overfull-boxes
            mt-best-underfull-boxes mt-underfull-boxes))
    ;; Show the result.
    (message (mt-minibuffer-message t))))

(defun mt-inject-mdframes ()
  "Change pagesize for sections."
  (setq mt-used-calculation-vector
        (make-vector (- (length mt-section-list) 1) mt-best-file))
  (let ((section-count 0)
        (margin-change 0)
        (local-file mt-best-file)
        (local-file-count 0))
    ;; Do this for every section.
    (while (< section-count (- (length mt-section-list) 1))
      (setq local-file-count 0
            local-file mt-best-file
            margin-change 0)
      ;; Calculate only when there is badness.
      (when (> (+ (* 100 (elt (elt mt-all-overfull-vector (- mt-best-file 1))
                              section-count))
                  (elt (elt mt-all-underfull-vector
                            (- mt-best-file 1))
                       section-count)) 0)
        ;; Look at all alternative pagesizes.
        (while (< local-file-count mt-calculations)
          ;; Look at lesser pagesizes.
          (when (<= (+ local-file-count mt-best-file) mt-calculations)
            (when (< (+ (* 100 (elt (elt mt-all-overfull-vector
                                         (+ local-file-count
                                            (- mt-best-file 1)))
                                    section-count))
                        (elt (elt mt-all-underfull-vector
                                  (+ local-file-count (- mt-best-file 1)))
                             section-count))
                     (+ (* 100 (elt (elt mt-all-overfull-vector
                                         (- local-file 1))
                                    section-count))
                        (elt (elt mt-all-underfull-vector
                                  (- local-file 1))
                             section-count)))
              ;; Remember best pagesize.
              (setq local-file (+ local-file-count mt-best-file))
              (aset mt-used-calculation-vector section-count local-file)))
          ;; Look at greater pagesizes.
          (when (> (- mt-best-file local-file-count) 0)
            (when (< (+ (* 100 (elt (elt mt-all-overfull-vector
                                         (- (- mt-best-file 1)
                                            local-file-count))
                                    section-count))
                        (elt (elt mt-all-underfull-vector
                                  (- (- mt-best-file 1) local-file-count))
                             section-count))
                     (+ (* 100 (elt (elt mt-all-overfull-vector
                                         (- local-file 1))
                                    section-count))
                        (elt (elt mt-all-underfull-vector
                                  (- local-file 1))
                             section-count)))
              ;; Remember best pagesize.
              (setq local-file (- mt-best-file local-file-count))
              (aset mt-used-calculation-vector section-count local-file)))
          (setq local-file-count (+ local-file-count 1)))

        ;; Calculate change of the margins, considering already changed size.
        (setq mt-margin-increase (- 0 (* 0.5 mt-range)))
        (setq mt-increment (/ mt-range 1.0 (max 1 (- mt-calculations 2))))
        (setq margin-change (if (= local-file 1)
                                (* -1 (+ mt-margin-increase
                                         (* (- mt-best-file 2) mt-increment)))
                              (- (+ mt-margin-increase
                                    (* (- local-file 2) mt-increment))
                                 (+ mt-margin-increase
                                    (* (- mt-best-file 2) mt-increment)))))
        ;; Inject new margin size.
        (shell-command
         (concat "sed -i '" (number-to-string
                             (nth section-count mt-section-list))
                 " s/\\(.*\\)/\\1 \\\\begin{mdframed}[backgroundcolor=theme,hidealllines=true,skipabove=0mm,innerleftmargin="
                 (number-to-string margin-change) "mm,innerrightmargin="
                 (number-to-string margin-change) "mm]/' /tmp/tmp.macro-type."
                 (number-to-string mt-best-file)
                 ".tex"))
        (shell-command
         (concat "sed -i '" (number-to-string
                             (- (nth (+ 1 section-count) mt-section-list) 1))
                 " s/\\(.*\\)/\\1 \\\\end{mdframed}/' /tmp/tmp.macro-type."
                 (number-to-string mt-best-file)
                 ".tex")))
      (setq section-count (+ 1 section-count))))
  ;; Actually, they are already injected.
  (message "Injecting mdframes"))

(defun mt-minibuffer-message (&optional last-run)
  (concat
   (if (and (= mt-init-overfull-boxes 0)
            (= mt-best-overfull-boxes 0)) "No overfull hboxes"
     (concat "Overfull hboxes reduced by "
             (number-to-string
              (round
               (/ (* 100 (- mt-init-overfull-boxes mt-best-overfull-boxes))
                  mt-init-overfull-boxes))) "%% from "
                  (number-to-string (round mt-init-overfull-boxes)) "pt to "
                  (number-to-string (round mt-best-overfull-boxes)) "pt"))
   "  ||  "
   (if (and (= mt-init-underfull-boxes 0)
            (= mt-best-underfull-boxes 0)) "No underfull hboxes"
     (concat "Underfull hboxes reduced by "
             (number-to-string
              (round
               (/ (* 100 (- mt-init-underfull-boxes mt-best-underfull-boxes))
                  mt-init-underfull-boxes))) "%% from "
                  (number-to-string (round mt-init-underfull-boxes)) " to "
                  (number-to-string (round mt-best-underfull-boxes))))
   "  ||  "
   (number-to-string mt-receive-count) "/"
   (number-to-string mt-calculations) " compiled"
   (when last-run (concat "
    output: " (car (split-string mt-result-file "\.tex$"))
    ".macro-type.*  calculated in "
    (format-time-string "%s" (time-since mt-benchmark)) "s"))))

(defun mt-blur-mdframe (input-list best-file)
  (interactive)
  (let ((count 0)
        (local-list))
    (setq local-list (map 'list (lambda (x) (- x best-file)) input-list))
    (add-to-list 'local-list (nth 1 local-list) nil (lambda (x y) nil))
    (add-to-list 'local-list (nth (- (length local-list) 2) local-list)
                 t (lambda (x y) nil))
    (setq mt-blur (make-vector (length input-list) 0))
    (while (< count (length input-list))
      (aset mt-blur count (/ (+ (nth count local-list)
                                (nth (+ count 1) local-list)
                                (nth (+ count 2) local-list)) 3))
      (setq count (+ count 1)))
    mt-blur))


(defun mt-evaluate-boxes (mt-log)
  (setq mt-underfull-boxes 0)
  (setq mt-overfull-boxes 0)
  (fillarray mt-section-underfull-vector 0)
  (fillarray mt-section-overfull-vector 0)
  (with-temp-buffer
    (insert mt-log)
    (goto-char (point-min))
    ;; Sum up all overfull hboxes.
    (while (re-search-forward
            "^Overfull \\\\hbox (\\([[:digit:]\.]+\\)pt too wide).*lines \\([[:digit:]]+\\)" nil t)
      (setq mt-overfull-boxes
            (+ mt-overfull-boxes (string-to-number (match-string 1))))
      ;; Add to the corresponding section.
      (let ((local-count 0))
        (while (and (< local-count (length mt-section-list))
                    (< (nth local-count mt-section-list)
                       (string-to-number (match-string 2))))
          (setq local-count (+ local-count 1)))
        (aset mt-section-overfull-vector (- local-count 1)
              (+ (string-to-number (match-string 1))
                 (elt mt-section-overfull-vector (- local-count 1))))))
    (goto-char (point-min))
    ;; Sum up all underfull hboxes.
    (while (re-search-forward
            "^Underfull \\\\hbox (badness \\([[:digit:]\.]+\\)).*lines \\([[:digit:]]+\\)" nil t)
      (setq mt-underfull-boxes
            (+ mt-underfull-boxes (string-to-number (match-string 1))))
      ;; Add to the corresponding section.
      (let ((local-count 0))
        (while (and (< local-count (length mt-section-list))
                    (< (nth local-count mt-section-list)
                       (string-to-number (match-string 2))))
          (setq local-count (+ local-count 1)))
        (aset mt-section-underfull-vector (- local-count 1)
              (+ (string-to-number (match-string 1))
                 (elt mt-section-underfull-vector (- local-count 1))))))
    ;; Lookup the file name of the analysed log.
    (when (re-search-forward "Transcript written on /tmp/tmp\.macro-type\.\\([[:digit:]]+\\)\.log" nil t)
      (setq mt-current-count (string-to-number (match-string 1))))))

(provide 'macro-type)

;;; macro-type.el ends here
