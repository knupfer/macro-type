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
          mt-calculations times
          mt-forks cores
          mt-range range
          mt-best-file 1
          mt-result-file file)
    (with-temp-buffer
      (insert-file-contents file)
      (setq mt-begin-buffer
            (car (split-string (buffer-string) "\\\\begin{document}")))
      (setq mt-end-buffer
            (car (cdr (split-string (buffer-string) "\\\\begin{document}")))))
    (mt-pdflatex)))

(defun mt-file-check (file)
  (with-temp-buffer
    (insert file)
    (goto-char (point-min))
    (re-search-forward "/$\\|\\.tex$" nil t)))

(defun mt-change-pagesize (mt-margin-increase
                           mt-times
                           mt-increment)
  (with-temp-buffer
    (insert (concat mt-begin-buffer
                    (when (> mt-times 1)
                      (let ((size (+ mt-margin-increase (* (- mt-times 2) mt-increment))))
                        (concat "
%%%%%%%%%%%%%%% Macro-type %%%%%%%%%%%%%%%%%
    \\addtolength{\\oddsidemargin }{ " (number-to-string size)        "mm}
    \\addtolength{\\evensidemargin}{ " (number-to-string size)        "mm}
    \\addtolength{\\textwidth     }{ " (number-to-string (* -2 size)) "mm}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")))
                    "\n\\begin{document}"
                    mt-end-buffer))
    (write-file (concat "/tmp/tmp.macro-type."
                        (number-to-string mt-times)
                        ".tex"))))

(defun mt-pdflatex ()
  (setq mt-start-count (+ mt-start-count 1))
  (mt-change-pagesize (- 0 (* 0.25 mt-range))
                      mt-start-count
                      (/ mt-range 1.0 (max 1 (- mt-calculations 2))))
  (async-start
   `(lambda ()
      (with-temp-buffer
        (shell-command
         (concat "pdflatex"
                 " -output-directory /tmp"
                 " -draftmode"
                 " -interaction nonstopmode"
                 " /tmp/tmp.macro-type."
                 (number-to-string ,mt-start-count)
                 ".tex")
         t)
        (buffer-string)))
   'mt-evaluate-result)
  (when (and (< (- mt-start-count mt-receive-count) mt-forks)
             (< mt-start-count mt-calculations)
             (> mt-start-count 1))
    (mt-pdflatex)))

(defun mt-evaluate-result (result)
  (mt-evaluate-boxes result)
  (if (> mt-start-count 1)
      (when (> (+ (* 100 mt-best-overfull-boxes) mt-best-underfull-boxes)
               (+ (* 100 mt-overfull-boxes) mt-underfull-boxes))
        (setq mt-best-overfull-boxes mt-overfull-boxes
              mt-best-underfull-boxes mt-underfull-boxes
              mt-best-file mt-start-count))
    (setq mt-init-overfull-boxes mt-overfull-boxes
          mt-best-overfull-boxes mt-overfull-boxes
          mt-init-underfull-boxes mt-underfull-boxes
          mt-best-underfull-boxes mt-underfull-boxes))
  (setq mt-receive-count (+ mt-receive-count 1))
  (message (mt-minibuffer-message))
  (when (and (< (- mt-start-count mt-receive-count) mt-forks)
             (< mt-start-count mt-calculations))
    (mt-pdflatex))
  (when (>= mt-receive-count mt-calculations)
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
             " rm /tmp/tmp.macro-type.*"
             ))
    (message (mt-minibuffer-message t))))

(defun mt-minibuffer-message (&optional last-run)
  (concat
   (if (= mt-init-overfull-boxes 0) "There are no overfull hboxes"
     (concat "Overfull hboxes reduced by "
             (number-to-string
              (round
               (/ (* 100 (- mt-init-overfull-boxes mt-best-overfull-boxes))
                  mt-init-overfull-boxes))) "%% from "
                  (number-to-string (round mt-init-overfull-boxes)) "pt to "
                  (number-to-string (round mt-best-overfull-boxes)) "pt"))
   "  ||  "
   (if (= mt-init-underfull-boxes 0) "There are no underfull hboxes"
     (concat "Underfull hboxes reduced by "
             (number-to-string
              (round
               (/ (* 100 (- mt-init-underfull-boxes mt-best-underfull-boxes))
                  mt-init-underfull-boxes))) "%% from "
                  (number-to-string (round mt-init-underfull-boxes)) " to "
                  (number-to-string (round mt-best-underfull-boxes))))
   "  ||  "
   (number-to-string mt-receive-count) "/"
   (number-to-string mt-calculations) " processes returned"
   (when last-run (concat "
    output:  " (car (split-string mt-result-file "\.tex$"))
    ".macro-type.*"))))

(defun mt-evaluate-boxes (mt-log)
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
