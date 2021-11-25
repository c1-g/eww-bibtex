;;; eww-bibtex.el --- Cite a website with EWW -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'esxml)
(require 'eww)
(require 'dash)
(require 'dom)

(defcustom eww-bibtex-default-bibliography bibtex-files
  "List of BibTeX files that `eww-bibtex' will append new entry to.

Each element can be an absolute file names or a directory.

The `eww-bibtex' command will have user select only one element from
this list. If an element is a directory, the command will have user
select all BibTeX files in it.")

(defvar eww-bibtex-build-field-list-functions
  (list
   #'eww-bibtex-get-author
   #'eww-bibtex-get-title
   #'eww-bibtex-get-year
   #'eww-bibtex-get-url
   #'eww-bibtex-get-note))

(defcustom eww-bibtex-modify-field-list-functions
  (list
   #'eww-bibtex-find-author-for-wikipedia)
  ""
  :type 'hook)

(defcustom eww-bibtex-find-ref-key-functions
  (list #'eww-bibtex-find-key-for-wikipedia)
  ""
  :type 'hook)

(defun eww-bibtex-build-field-list (field-list dom func-list)
  (cond ((null func-list) field-list)
        (t (eww-bibtex-build-field-list
            (if (string-prefix-p "eww-bibtex-get-" (symbol-name (car func-list)))
                 (let* ((field-name (string-remove-prefix "eww-bibtex-get-"
                                                          (symbol-name (car func-list))))
                        (value-list (-list (funcall (car func-list) field-list dom)))
                        (value (if (> (length value-list) 1)
                                   (completing-read (concat "Which " field-name "? ") value-list)
                                 (car value-list)))
                        (field-elt (--find (equal (car it) field-name) field-list)))
                   (if field-elt
                       (-replace field-elt `(,field-name nil ,value) field-list)
                     (push `(,field-name nil ,value) field-list))
                   )
               (-list (funcall (car func-list) field-list dom)))
             dom
            (cdr func-list)))))

(defun eww-bibtex-modify-field-list (field-list dom func-list)
  (cond ((null func-list) field-list)
        (t (let ((modified-field-list (funcall (car func-list) field-list dom)))
             (eww-bibtex-modify-field-list
              (or modified-field-list field-list)
              dom
              (cdr func-list))))))

(defun eww-bibtex-get-author (field-list dom)
  (eww-bibtex--query dom
                     '("meta[name=author]"
                       "meta[name=citation_author]"
                       "[rel=author] > [itemprop=name]"
                       "[rel=author]"
                       "[itemprop=author] > *"
                       "[itemprop=author]"
                       ".author")))

(defun eww-bibtex-get-title (field-list dom)
  (let ((title (cl-caddr
                 (car (dom-by-tag dom 'title)))))
    (when title
      (->>
       title
       (s-replace "\n" " ")
       (s-trim)
       (s-collapse-whitespace)))))

(defun eww-bibtex-get-url (field-list dom)
  (append (eww-bibtex--query dom '("link[rel=canonical]"
                                      "meta[name=citation_fulltext_html_url]"))
          (list (eww-current-url))))

(defun eww-bibtex-get-year (field-list dom)
  (append (eww-bibtex--query dom
                             '("meta[name=citation_publication_date]"
                               "meta[name=date]"
                               "[itemprop=dateModified]"
                               "[itemprop=datePublished]"
                               "[itemprop=dateModified]"
                               "[itemprop=datePublished]"
                               "[id*=updated]"
                               "time[pubdate]"
                               ".post_date"
                               "time"))
          (list (format-time-string "%Y"))))

(defun eww-bibtex-get-note (field-list _dom)
  (format "[Online; accessed %s]" (format-time-string "%F")))

(defun eww-bibtex-find-key-for-wikipedia (field-list)
  (let ((url (car (last (assoc "url" field-list)))))
    (if (string-match-p "wikipedia" url)
        (concat "wiki:" (file-name-base url)))))

(defun eww-bibtex-find-author-for-wikipedia (field-list _dom)
  (when-let ((url (car (last (assoc "url" field-list)))))
    (if (string-match-p "wikipedia" url)
        (-replace '("author" nil nil) '("author" nil "{Wikipedia Contributors}") field-list))))

;;;###autoload
(defun eww-bibtex (html-source &optional interactive)
  "Alter the \"Misc\" entry in `bibtex-BibTeX-entry-alist' based on
`eww-bibtex-field-alist'.

This function works by replacing the initial content of all the fields
in the \"Misc\" entry defined `eww-bibtex-field-alist'.

For example, here is the default \"Misc\" entry type,

 (\"Misc\" \"Miscellaneous\" nil nil
  ((\"author\")
   (\"title\" \"Title of the work (BibTeX converts it to lowercase)\")
   (\"howpublished\" \"The way in which the work was published\")
   (\"month\")
   (\"year\")
   (\"note\")))

We focus on modifying the field alist which is,

  ((\"author\")
   (\"title\" \"Title of the work (BibTeX converts it to lowercase)\")
   (\"howpublished\" \"The way in which the work was published\")
   (\"month\")
   (\"year\")
   (\"note\"))

Each of these elements is in the form of (FIELD COMMENT INIT ALTERNATIVE).

The function will replace or append the INIT argument, which dictates
the initial content of the field, based on `eww-bibtex-field-alist'.

Take the \"note\" field, then replace its INIT argument with the result
of `eww-bibtex-get-note' created automatically by
`eww-bibtex-update-field-alist'."
  (interactive (list (plist-get eww-data :source) t) eww-mode)
  (let* ((misc-entry (assoc "Misc" bibtex-BibTeX-entry-alist))
         (dom (with-temp-buffer
                (insert html-source)
                (libxml-parse-html-region
                 (point-min)
                 (point-max))))
         (field-list (eww-bibtex-build-field-list
                               (cl-fifth misc-entry)
                               dom
                               eww-bibtex-build-field-list-functions))
         (target-files (flatten-tree (--map
                                      (if (file-directory-p it)
                                          (directory-files-recursively it ".bib")
                                        it)
                                      eww-bibtex-default-bibliography))))
    (setq misc-entry (-replace-at
                      4
                      (eww-bibtex-modify-field-list
                       field-list
                       dom
                       eww-bibtex-modify-field-list-functions)
                      misc-entry))
    
    (pop-to-buffer (find-file-noselect (completing-read "Which BibTeX file? " target-files nil t)))
    
    (let ((bibtex-entry-alist (list misc-entry))
          (bibtex-autokey-before-presentation-function (apply-partially #'eww-bibtex-replace-autokey field-list)))
      
      (bibtex-entry "Misc")
      (when (yes-or-no-p "Is the entry correct? ")
        (bibtex-clean-entry)))))

(defun eww-bibtex-replace-autokey (field-list autokey)
  (or (run-hook-with-args-until-success 'eww-bibtex-find-ref-key-functions field-list)
      autokey))

(defun eww-bibtex--query (dom selectors)
  (flatten-list (mapcar (lambda (selector)
                          (-difference
                           (--> selector
                                (esxml-query-all it dom)
                                (flatten-tree it)
                                (-filter #'stringp it))
                           (--> selector
                                (esxml-parse-css-selector selector)
                                (flatten-tree it)
                                (-filter #'stringp it))))
                        selectors)))
(provide 'eww-bibtex)
;;; eww-bibtex.el ends here
