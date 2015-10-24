;;; ox-md-pandoc.el --- Markdown Back-End for Org Export Engine

;; Copyright (C) 2012-2015 Free Software Foundation, Inc.

;; Author: Nicolas Goaziou <n.goaziou@gmail.com>, Matt Price <matt.price@utoronto.ca>
;; Keywords: org, wp, markdown, pandoc

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This library implements a Markdown back-end (pandoc flavor) for
;; Org exporter, based on the `md' back-end.  See Org manual for more
;; information.

;;; Code:

(eval-when-compile (require 'cl))
(require 'ox-html)
(require 'ox-publish)

;;; User-Configurable Variables

(defgroup org-export-md-pandoc nil
  "Options specific to Pandoc Markdown export back-end."
  :tag "Org Pandoc Markdown"
  :group 'org-export
  :version "24.4"
  :package-version '(Org . "8.0"))

(defcustom org-md-pandoc-headline-style 'atx
  "Style used to format headlines.
This variable can be set to either `atx' or `setext'."
  :group 'org-export-md-pandoc
  :type '(choice
	  (const :tag "Use \"atx\" style" atx)
	  (const :tag "Use \"Setext\" style" setext)))


;;; Define Back-End

(org-export-define-derived-backend 'md-pandoc 'md
  :filters-alist '((:filter-parse-tree . org-md-separate-elements))
  :menu-entry
  '(?M "Export to Markdown Pandoc Style"
       ((?M "To temporary buffer"
	    (lambda (a s v b) (org-md-export-as-markdown-pandoc a s v)))
	(?m "To file" (lambda (a s v b) (org-md-export-to-markdown-pandoc a s v)))
	(?o "To file and open"
	    (lambda (a s v b)
	      (if a (org-md-export-to-markdown-pandoc t s v)
		(org-open-file (org-md-export-to-markdown-pandoc nil s v)))))))
  :translate-alist '((export-block . org-md-pandoc-export-block)
		     (template . org-md-pandoc-template)
		     (headline . org-md-pandoc-headline)
		     )
  ;;  :export-block '("NOTES")
  :export-block '("NOTES")
  )


;;;; template
(defun org-md-pandoc-template (contents info)
  "Return complete document string after Markdown conversion.
CONTENTS is the transcoded contents string.  INFO is a plist used
as a communication channel."
  (concat
   ;;(message (concat info))
   (and (plist-get info :with-author)
	(let ((author (org-export-data (plist-get info :author) info)))
	  (and (org-string-nw-p author)
	       (concat  "\% " author "\n"))))
   (and (plist-get info :with-title)
	(concat "\% " (org-export-data (plist-get info :title) info) "\n"))
   (and (plist-get info :with-date)
	(let ((date (org-export-data (org-export-get-date info) info)))
	  (and (org-string-nw-p date)
	       (concat "\% " date "\n"))))
   
   contents)
  )

;;;; Example Block, Src Block and export Block

(defun org-md-plus-export-block
    (export-block contents info)
  "Transcode a EXPORT-BLOCK NOTES element from Org to md-plus.
    CONTENTS is nil.  INFO is a plist holding contextual information."
  (if (equal (org-element-property :type export-block) "NOTES")
      (concat "<aside class=\"speaker-notes\">\n"
	      (org-element-property :value export-block)
	      "</aside>\n")
    (let ((parent-backend (org-export-backend-parent backend)))
      (if parent-backend
	  (org-export-with-backend
	   parent-backend export-block contents info)))))


(defun org-md-pandoc-export-block (export-block contents info)
  "Transcode a EXPORT-BLOCK NOTES element from Org to Markdown.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (if (equal (org-element-property :type export-block) "NOTES")
      (concat "<div class=\"notes\">\n"
	      (org-element-property :value export-block)
	      "</div>\n")
    (let ((parent-backend (org-export-backend-parent backend)))
      (if parent-backend
	  (org-export-with-backend
	   parent-backend export-block contents info))))
  )


;;;; Headline

(defun org-md-pandoc-headline (headline contents info)
  "Transcode HEADLINE element into Markdown format.
CONTENTS is the headline contents.  INFO is a plist used as
a communication channel."
  (unless (org-element-property :footnote-section-p headline)
    (let* ((level (org-export-get-relative-level headline info))
	   (title (org-export-data (org-element-property :title headline) info))
	   (todo (and (plist-get info :with-todo-keywords)
		      (let ((todo (org-element-property :todo-keyword
							headline)))
			(and todo (concat (org-export-data todo info) " ")))))
	   (tags (and (plist-get info :with-tags)
		      (let ((tag-list (org-export-get-tags headline info)))
			(and tag-list
			     (format "     :%s:"
				     (mapconcat 'identity tag-list ":"))))))
	   (priority
	    (and (plist-get info :with-priority)
		 (let ((char (org-element-property :priority headline)))
		   (and char (format "[#%c] " char)))))
	   (anchor
	    (and (plist-get info :with-toc)
		 (org-html--anchor
		  (org-export-get-reference headline info) nil nil info)))
	   ;; Headline text without tags.
	   (heading (concat todo priority title))
	   (style (plist-get info :md-headline-style)))
      (cond
       ;; Cannot create a headline.  Fall-back to a list.
       ((or (org-export-low-level-p headline info)
	    (not (memq style '(atx setext)))
	    (and (eq style 'atx) (> level 6))
	    (and (eq style 'setext) (> level 2)))
	(let ((bullet
	       (if (not (org-export-numbered-headline-p headline info)) "-"
		 (concat (number-to-string
			  (car (last (org-export-get-headline-number
				      headline info))))
			 "."))))
	  (concat bullet (make-string (- 4 (length bullet)) ? ) heading tags
		  "\n\n"
		  (and contents
		       (replace-regexp-in-string "^" "    " contents)))))
       ;; Use "Setext" style.
       ((eq style 'setext)
	(concat   heading tags anchor "\n"
		(make-string (length heading) (if (= level 1) ?= ?-))
		"\n\n"
		contents))
       ;; Use "atx" style.
       (t (concat "\n---\n\n" (make-string level ?#) " " heading tags anchor "\n\n" contents))))))



;;; Interactive function

;;;###autoload
(defun org-md-export-as-markdown-pandoc (&optional async subtreep visible-only)
  "Export current buffer to a Markdown buffer.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Export is done in a buffer named \"*Org MD Export*\", which will
be displayed when `org-export-show-temporary-export-buffer' is
non-nil."
  (interactive)
  (org-export-to-buffer 'md-pandoc "*Org MD Export*"
    async subtreep visible-only nil nil (lambda () (text-mode))))

;;;###autoload
(defun org-md-export-to-markdown-pandoc (&optional async subtreep visible-only)
  "Export current buffer to a Markdown file.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Return output file's name."
  (interactive)
  (let ((outfile (org-export-output-file-name ".md" subtreep)))
    (org-export-to-file 'md-pandoc outfile async subtreep visible-only)))

;;;###autoload
(defun org-md-publish-to-md (plist filename pub-dir)
  "Publish an org file to Markdown.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  (org-publish-org-to 'md-pandoc filename ".md" plist pub-dir))

(provide 'ox-md-pandoc)

;; Local variables:
;; generated-autoload-file: "org-loaddefs.el"
;; End:

;;; ox-md.el ends here
