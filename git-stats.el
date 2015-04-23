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
  (let ((default-directory dir)
	(case-fold-search t)
	(prev-month -1)
	authors totals)
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
	    (setq author (car (mail-header-parse-address author))
		  date (parse-time-string date))
	    (when (or (not (= prev-month (nth 4 date)))
		      (not (save-excursion
			     (search-forward "\ncommit " nil t))))
	      (when authors
		(push (cons (format-time-string "%Y-%d-01"
						(apply 'encode-time date))
			    (hash-table-count authors))
		      totals))
	      (setq authors (make-hash-table :test 'equal)
		    prev-month (nth 4 date)))
	    (incf (gethash author authors 0))))))
    (pop-to-buffer "*totals*")
    (erase-buffer)
    (dolist (elem (nreverse totals))
      (insert (format "%s %d\n" (car elem) (cdr elem))))))
		      

(provide 'git-stats)

;;; git-stats.el ends here
