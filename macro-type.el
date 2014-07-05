;; TODO: adjust for each warning.
;; sed '2 s/\(.*\)/\1MACRO-TYPE/'

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
  (interactive (list (read-file-name
                      "Choose a .tex file: " nil nil t nil 'mt-file-check)
                     (read-number
                      "How many mm may the page shrink: " 0.5)
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
          mt-section-list
          (map 'list 'string-to-number
               (split-string (shell-command-to-string
                              (concat "grep -n 'section{.*}\\|begin{document}' "
                                      file " | grep -o ^[0-9]*"))))
          mt-section-underfull-vector (make-vector (length mt-section-list) 0)
          mt-section-overfull-vector (make-vector (length mt-section-list) 0)
          mt-all-underfull-vector (make-vector times 0)
          mt-all-overfull-vector (make-vector times 0))
    (add-to-list 'mt-section-list (+ 1 (pop mt-section-list)))
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

(defun mt-error-in-section ()
  (interactive)
  (setq mt-flawed-sections nil)
  (mapc
   (lambda (this-error)
     (let ((error-section 0))
       (mapc (lambda (this-section)
               (when (< this-section this-error)
                 (setq error-section this-section)))
             mt-section-list)
       (when error-section (add-to-list 'mt-flawed-sections error-section))))
   mt-error-list))

(defun mt-file-check (file)
  (with-temp-buffer
    (insert file)
    (goto-char (point-min))
    (re-search-forward "/$\\|\\.tex$" nil t)))

(defun mt-pdflatex ()
  (setq mt-start-count (+ mt-start-count 1))
  (async-start
   `(lambda ()
      (setq mt-range ,mt-range
            mt-margin-increase (- 0 (* 0.5 mt-range))
            mt-times ,mt-start-count
            mt-increment (/ mt-range 1.0 (max 1 (- ,mt-calculations 2))))
      (shell-command (concat "echo \""
                             (when (> mt-times 1)
                               (let ((size (+ mt-margin-increase
                                              (* (- mt-times 2) mt-increment))))
                                 (concat " \\usepackage{mdframed} \\usepackage{color} \\definecolor{theme}{rgb}{1,0.5,0.5} \\newenvironment{definition}{\\begin{mdframed}[backgroundcolor=theme, hidealllines=true, skipabove=0cm, innerleftmargin=-0.2mm, innerrightmargin=-0.2mm]}{\\end{mdframed}}\\addtolength{\\oddsidemargin }{ " (number-to-string size)        "mm}\\addtolength{\\evensidemargin}{ " (number-to-string size)        "mm}\\addtolength{\\textwidth     }{ " (number-to-string (* -2 size)) "mm}")))
                             "\n\\begin{document} \" > "
                             (concat "/tmp/tmp.macro-type.header."
                                     (number-to-string mt-times)
                                     ".tex")))
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
   'mt-evaluate-result)
  (when (and (< (- mt-start-count mt-receive-count) mt-forks)
             (< mt-start-count mt-calculations)
             (> mt-start-count 1))
    (mt-pdflatex)))

(defun mt-evaluate-result (result)
  (mt-evaluate-boxes result)
  (aset mt-all-underfull-vector (- mt-current-count 1) (copy-sequence mt-section-underfull-vector))
  (aset mt-all-overfull-vector (- mt-current-count 1) (copy-sequence mt-section-overfull-vector))
  (if (> mt-current-count 1)
      (when (> (+ (* 100 mt-best-overfull-boxes) mt-best-underfull-boxes)
               (+ (* 100 mt-overfull-boxes) mt-underfull-boxes))
        (setq mt-best-overfull-boxes mt-overfull-boxes
              mt-best-underfull-boxes mt-underfull-boxes
              mt-best-file mt-current-count
              mt-error-list mt-error-positions))
    (setq mt-init-overfull-boxes mt-overfull-boxes
          mt-best-overfull-boxes mt-overfull-boxes
          mt-init-underfull-boxes mt-underfull-boxes
          mt-best-underfull-boxes mt-underfull-boxes
          mt-error-list mt-error-positions))
  (setq mt-receive-count (+ mt-receive-count 1))
  (message (mt-minibuffer-message))
  (when (and (< (- mt-start-count mt-receive-count) mt-forks)
             (< mt-start-count mt-calculations))
    (mt-pdflatex))
  (when (>= mt-receive-count mt-calculations)
    (mt-inject-mdframes)
    (shell-command
     (concat "cp /tmp/tmp.macro-type."
             (number-to-string mt-best-file)
             ".tex "
             (car (split-string mt-result-file "\.tex$"))
             ".macro-type.tex; pdflatex -output-directory "
             (car (split-string mt-result-file "/[^/]+\.tex$"))
             " -interaction nonstopmode "
             (car (split-string mt-result-file "\.tex$"))
             ".macro-type.tex > /dev/null;"
             " rm /tmp/tmp.macro-type.*"))
    (message (mt-minibuffer-message t))))

(defun mt-inject-mdframes ()
  (shell-command
   (concat "sed -i '" (number-to-string (car (cdr mt-section-list)))
           " s/\\(.*\\)/\\\\begin{definition}  \\1/' /tmp/tmp.macro-type."
           (number-to-string mt-best-file)
           ".tex"))
    (shell-command
     (concat "sed -i '" (number-to-string (- (car (cdr (cdr mt-section-list))) 1))
             " s/\\(.*\\)/\\\\end{definition}  \\1/' /tmp/tmp.macro-type."
             (number-to-string mt-best-file)
             ".tex"))
    )

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

(defun mt-evaluate-boxes (mt-log)
  (setq mt-underfull-boxes 0)
  (setq mt-overfull-boxes 0)
  (setq mt-error-positions nil)
  (fillarray mt-section-underfull-vector 0)
  (fillarray mt-section-overfull-vector 0)
  (setq mt-debug mt-log)
  (with-temp-buffer
    (insert mt-log)
    (goto-char (point-min))
    (while (re-search-forward
            "^Overfull \\\\hbox (\\([[:digit:]\.]+\\)pt too wide).*lines \\([[:digit:]]+\\)" nil t)
      (setq mt-overfull-boxes
            (+ mt-overfull-boxes (string-to-number (match-string 1))))
      (add-to-list 'mt-error-positions (string-to-number (match-string 2)))
      (let ((local-count 0))
        (while (and (< local-count (- (length mt-section-list) 1))
                    (< (nth local-count mt-section-list)
                       (string-to-number (match-string 2))))
          (setq local-count (+ local-count 1)))
        (aset mt-section-overfull-vector local-count
              (+ (string-to-number (match-string 1))
                 (elt mt-section-overfull-vector local-count)))))
    (goto-char (point-min))
    (while (re-search-forward
            "^Underfull \\\\hbox (badness \\([[:digit:]\.]+\\)).*lines \\([[:digit:]]+\\)" nil t)
      (setq mt-underfull-boxes
            (+ mt-underfull-boxes (string-to-number (match-string 1))))
      (add-to-list 'mt-error-positions (string-to-number (match-string 2)))
      (let ((local-count 0))
        (while (and (< local-count (- (length mt-section-list) 1))
                    (< (nth local-count mt-section-list)
                       (string-to-number (match-string 2))))
          (setq local-count (+ local-count 1)))
        (aset mt-section-underfull-vector local-count
              (+ (string-to-number (match-string 1))
                 (elt mt-section-underfull-vector local-count)))))
    (re-search-forward "Transcript written on /tmp/tmp\.macro-type\.\\([[:digit:]]+\\)\.log" nil t)
    (setq mt-current-count (string-to-number (match-string 1)))))
