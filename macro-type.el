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
(defun mt-evaluate-hboxes (mt-log)
  (setq mt-underfull-boxes 0)
  (setq mt-overfull-boxes 0)
  (with-temp-buffer
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
            (+ mt-underfull-boxes (string-to-number (match-string 1)))))))

(defun mt-change-pagesize (mt-tex-file-name
                           mt-margin-increase
                           mt-times
                           mt-increment)
  (while (> mt-times 0)
    (with-temp-buffer
      (insert-file-contents mt-tex-file-name)
      (when (> mt-times 1)
        (let ((size (+ mt-margin-increase (* (- mt-times 2) mt-increment))))
          (re-search-forward "\\\\begin{document}" nil t)
          (replace-match (concat "
%%%%%%%%%%%%%%% Macro-type %%%%%%%%%%%%%%%%%
    \\\\addtolength{\\\\oddsidemargin }{ " (number-to-string size)        "mm}
    \\\\addtolength{\\\\evensidemargin}{ " (number-to-string size)        "mm}
    \\\\addtolength{\\\\textwidth     }{"  (number-to-string (* -2 size)) "mm}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\\\\begin{document}"))))
      (write-file (concat "/tmp/tmp.macro-type."
                          (number-to-string mt-times)
                          ".tex")))
    (setq mt-times (- mt-times 1)))
  (message "Starting multicore calculation..."))

(defun mt-pdflatex ()
  (setq mt-start-count (+ mt-start-count 1))
  (async-start
   `(lambda ()
      (with-temp-buffer
        ;; Pass in the variable environment for smtpmail
        ,(async-inject-variables "mt-start-count")
        (shell-command
         (concat
          "pdflatex -output-directory /tmp -draftmode -interaction nonstopmode /tmp/tmp.macro-type."
          (number-to-string mt-start-count) ".tex") t)
        (buffer-string)))
   (lambda (result)
     (mt-evaluate-hboxes result)
     (if mt-best-hboxes
         (when (> (+ (* 100 mt-best-hboxes) mt-best-badness)
                  (+ (* 100 mt-overfull-boxes) mt-underfull-boxes))
           (setq mt-best-hboxes mt-overfull-boxes
                 mt-best-badness mt-underfull-boxes
                 mt-best-file mt-start-count))
       (setq mt-original-hboxes mt-overfull-boxes
             mt-best-hboxes mt-original-hboxes
             mt-best-badness mt-underfull-boxes
             mt-original-badness mt-underfull-boxes))
     (setq mt-receive-count (+ mt-receive-count 1))
     (message
      (concat (if (= mt-original-hboxes 0) "There are no overfull hboxes"
                (concat "Overfull hboxes reduced by "
                        (number-to-string
                         (round
                          (/
                           (* 100 (- mt-original-hboxes mt-best-hboxes))
                           mt-original-hboxes)))
                        "%% from "
                        (number-to-string (round mt-original-hboxes)) "pt to "
                        (number-to-string (round mt-best-hboxes)) "pt"))
              "  ||  "
              (if (= mt-original-badness 0) "There are no underfull hboxes"
                (concat "Underfull hboxes reduced by "
                        (number-to-string
                         (round
                          (/
                           (* 100 (- mt-original-badness mt-best-badness))
                           mt-original-badness)))
                        "%% from "
                        (number-to-string (round mt-original-badness)) " to "
                        (number-to-string (round mt-best-badness))))
              "  ||  "
              (number-to-string mt-receive-count) "/"
              (number-to-string mt-calculations) " processes returned"))
     (when (< (- mt-start-count mt-receive-count) mt-forks)
       (when (< mt-start-count mt-calculations)
         (mt-pdflatex)))
     (when (>= mt-receive-count mt-calculations)
       (shell-command
        (concat "cp /tmp/tmp.macro-type."
                (number-to-string mt-best-file)
                ".tex " (car (split-string mt-result-file "\.tex$"))
                ".macro-type.tex; pdflatex -output-directory "
                (car (split-string mt-result-file "/[^/]+\.tex$"))
                " -interaction nonstopmode "
                (car (split-string mt-result-file "\.tex$"))
                ".macro-type.tex > /dev/null; rm /tmp/tmp.macro-type.*"))

       (message
        (concat (if (= mt-original-hboxes 0) "There are no overfull hboxes"
                  (concat "Overfull hboxes reduced by "
                          (number-to-string
                           (round
                            (/ (* 100 (- mt-original-hboxes mt-best-hboxes))
                               mt-original-hboxes)))
                          "%% from "
                          (number-to-string (round mt-original-hboxes))
                          "pt to "
                          (number-to-string (round mt-best-hboxes)) "pt"))
                "  ||  "
                (if (= mt-original-badness 0) "There are no underfull hboxes"
                  (concat "Underfull hboxes reduced by "
                          (number-to-string
                           (round
                            (/
                             (* 100 (- mt-original-badness mt-best-badness))
                             mt-original-badness)))
                          "%% from "
                          (number-to-string (round mt-original-badness))
                          " to "
                          (number-to-string (round mt-best-badness))))
                "  ||  "
                "All " (number-to-string mt-calculations) " processes returned
    output: "
                (car (split-string mt-result-file "\.tex$"))
                ".macro-type.*")))))
  (when (and (< (- mt-start-count mt-receive-count) mt-forks)
             (< mt-start-count mt-calculations)
             (> mt-start-count 1))
    (mt-pdflatex)))

(defun mt-macro-type-tex-file (file range mt-times cores)
  (interactive (list (read-file-name
                      "Choose a .tex file:" nil nil t nil 'mt-file-check)
                     (read-number
                      "How many mm may the page shrink:" 0.5)
                     (read-number
                      "How many times you would like to compile:" 25)
                     (read-number
                      "How many cores would you like to use:" 4)))
  (if (car (file-attributes file 0))
      (error "You can't choose a directory")
    (setq mt-receive-count 0
          mt-start-count 0
          mt-calculations mt-times
          mt-original-hboxes nil
          mt-best-hboxes nil
          mt-forks cores
          mt-underfull-boxes 0
          mt-original-badness nil
          mt-best-badness nil
          mt-best-file 1
          mt-result-file file)
    (mt-change-pagesize file
                        (- 0 (* 0.25 range))
                        mt-times
                        (/ range 1.0 (max 1 (- mt-times 2))))
    (mt-pdflatex)))

(defun mt-file-check (file)
  (with-temp-buffer
    (insert file)
    (goto-char (point-min))
    (re-search-forward "/$\\|\\.tex$" nil t)))
