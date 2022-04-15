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
  (git-stats-output (setq stats (git-stats-compute dir)) t t))

(defun git-stats-output (authors &optional commits smalls)
  (let ((monthly (make-hash-table :test #'equal))
	totals)
    (maphash
     (lambda (key count)
       (let ((time (car key))
	     (total 0))
	 (when (and (not (string-match "corallo" (cadr key)))
		    (not (string-match "akrl" (cadr key))))
	   (dotimes (i 28)
	     (setq time (+ time (* i 24 60 60)))
	     (incf total (gethash (list time (cadr key))
				  authors
				  0)))
	   (setq time (car key))
	   (when (or (not smalls)
		     (< total 2))
	     (dotimes (i 28)
	       (setq time (+ time (* i 24 60 60)))
	       (when (<= time (time-convert nil 'integer))
		 (incf (gethash time monthly 0)
		       (if commits count 1))))))))
     authors)
    (maphash
     (lambda (key count)
       (push (cons (format-time-string "%Y-%m-%d" key "Z")
		   count)
	     totals))
     monthly)
    (pop-to-buffer "*totals*")
    (erase-buffer)
    (let ((data (sort totals
		      (lambda (e1 e2)
			(string< (car e1) (car e2)))))
	  (len 28)
	  elem)
      (while (setq elem (car data))
	(when (length> data len)
	  (insert (format "%s %d\n" (car elem)
			  (/ (reduce #'+ (mapcar #'cdr (seq-take data len)))
			     (float len)))))
	(pop data)))))

(defun git-stats-compute (dir &optional committer)
  (let ((default-directory dir)
	(case-fold-search t)
	merge
	(authors (make-hash-table :test 'equal)))
    (with-temp-buffer
      (if committer
	  (call-process "git" nil t nil "log" "--pretty=fuller")
	(call-process "git" nil t nil "log"))
      (goto-char (point-min))
      (while (re-search-forward "^commit " nil t)
	(forward-line 1)
	(let (author date)
	  (while (not (eq (following-char) ?\n))
	    (cond
	     ((looking-at "Merge:")
	      (setq merge t))
	     ((or (and (not committer)
		       (looking-at "author:[ \t]+\\(.*\\)"))
		  (and committer
		       (looking-at "commit:[ \t]+\\(.*\\)")))
	      (setq author (match-string 1))
	      (when merge
		(setq author nil
		      merge nil)))
	     ((looking-at "commitdate:[ \t]+\\(.*\\)")
	      (setq date (match-string 1))))
	    (forward-line 1))
	  (when (and author date)
	    (setq date (parse-time-string date))
	    (incf (gethash (list (time-convert
				  (encode-time
				   (decoded-time-set-defaults
				    (make-decoded-time :year (nth 5 date)
						       :month (nth 4 date)
						       :day (nth 3 date)
						       :zone "Z")))
				  'integer)
				 (car (mail-header-parse-address author)))
			   authors 0))))))
    authors))

(provide 'git-stats)

;;; git-stats.el ends here
