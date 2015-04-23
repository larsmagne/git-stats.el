;;; git-stats.el --- functions for querying and setting git-stats data in mp3 files

;; Copyright (C) 2015 Lars Magne Ingebrigtsen

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: music

;; git-stats.el is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; git-stats.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

(defun git-stats (dir)
  "Tally up commits per author per month in DIR."
  (interactive "DGit directory to get logs for: ")
  (git-stats-output (git-stats-compute dir)))

(defun git-stats-output (authors &optional commits)
  (let ((monthly (make-hash-table :test 'equal))
	totals)
    (maphash
     (lambda (key count)
       (incf (gethash (cons (car key) (cadr key))
		      monthly 0)
	     (if commits count 1)))
     authors)
    (maphash
     (lambda (key count)
       (push (cons (format "%04d-%02d-01" (car key) (cdr key))
		   count)
	     totals))
     monthly)
    (pop-to-buffer "*totals*")
    (erase-buffer)
    (dolist (elem (sort totals
			(lambda (e1 e2)
			  (string< (car e1) (car e2)))))
      (insert (format "%s %d\n" (car elem) (cdr elem))))))

(defun git-stats-compute (dir)
  (let ((default-directory dir)
	(case-fold-search t)
	(authors (make-hash-table :test 'equal)))
    (with-temp-buffer
      (call-process "git" nil t nil "log")
      (goto-char (point-min))
      (while (re-search-forward "^commit " nil t)
	(forward-line 1)
	(let (author date)
	  (while (not (eq (following-char) ?\n))
	    (cond
	     ((looking-at "author:[ \t]+\\(.*\\)")
	      (setq author (match-string 1)))
	     ((looking-at "date:[ \t]+\\(.*\\)")
	      (setq date (match-string 1))))
	    (forward-line 1))
	  (when (and author date)
	    (setq date (parse-time-string date))
	    (incf (gethash (list (nth 5 date) (nth 4 date)
				 (car (mail-header-parse-address author)))
			   authors 0))))))
    authors))

(provide 'git-stats)

;;; git-stats.el ends here
