;;; eww-bibtex.el --- Cite a website with EWW -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'esxml)
(require 'eww)
(require 'dash)
(require 'dom)

;;; Customize variables
(defcustom eww-bibtex-default-bibliography bibtex-files
  "List of BibTeX files that `eww-bibtex' will append new entry to.

Each element can be an absolute file names or a directory.

The `eww-bibtex' command will have user select only one element from
this list. If an element is a directory, the command will have user
select all BibTeX files in it.")

(defcustom eww-bibtex-modify-field-list-functions
  (list
   #'eww-bibtex-find-author-for-wikipedia)
  "TODO"
  :type 'hook)

(defcustom eww-bibtex-find-ref-key-functions
  (list #'eww-bibtex-find-key-for-wikipedia)
  "TODO"
  :type 'hook)

(defvar eww-bibtex--build-field-list-functions
  (list
   #'eww-bibtex-get-author
   #'eww-bibtex-get-title
   #'eww-bibtex-get-year
   #'eww-bibtex-get-url
   #'eww-bibtex-get-note)
  "Functions for building the field list, should return a list of
possible initial content (string) for its field.

Each function should take 1 arguments, the DOM for a document given by
`eww-bibtex'.

For example, `eww-bibtex-get-url' returns all possible values for
the \"url\" field e.g.
 (\"https://en.wikipedia.com/John_Doe/\" \"example.com\") etc.

This list will get processed by `eww-bibtex--build-field-list'
See its documentation or its definition for more.

Note: This list shouldn't be modified unless you want to add another
field to the \"Misc\" entry like \"file\" or \"editor\", and their
fuctions should have the namespace of eww-bibtex-get-FIELD_NAME") 

;;;###autoload
(defun eww-bibtex-get-author (dom)
  (eww-bibtex--query dom
                     '("meta[name=author]"
                       "meta[name=citation_author]"
                       "[rel=author] > [itemprop=name]"
                       "[rel=author]"
                       "[itemprop=author] > *"
                       "[itemprop=author]"
                       ".author")))

;;;###autoload
(defun eww-bibtex-get-title (dom)
  (let ((title (cl-caddr
                (car (dom-by-tag dom 'title)))))
    (when title
      (->>
       title
       (s-replace "\n" " ")
       (s-trim)
       (s-collapse-whitespace)))))

;;;###autoload
(defun eww-bibtex-get-url (dom)
  (append (eww-bibtex--query dom '("link[rel=canonical]"
                                    "meta[name=citation_fulltext_html_url]"))
           (list (eww-current-url))))

;;;###autoload
(defun eww-bibtex-get-year (dom)
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

;;;###autoload
(defun eww-bibtex-get-note (_dom)
  (format "[Online; accessed %s]" (format-time-string "%F")))



(defun eww-bibtex--build-field-list (field-list dom func-list)
  "Recursively build field list by pass each function in FUNC-LIST
the DOM then use its return value to edit the FIELD-LIST.

The FUNC-LIST should be the `eww-bibtex--build-field-list-functions'
and the DOM should be given by `eww-bibtex'."
  (cond (;; Check if FUNC-LIST is exhausted, if yes, finally return FIELD-LIST.
         (null func-list) field-list)
        
        ;; Check if the first function in FUNC-LIST doesn't have an "eww-bibtex-get" prefix,
        ;; if it doesn't, raise error.
        ((not (string-prefix-p "eww-bibtex-get-" (symbol-name (car func-list))))
         (error "Function \"%s\" name is not in the format of eww-bibtex-get-FIELD_NAME" (car func-list)))

        ;; Else, do this. VVV
        (t (eww-bibtex--build-field-list

            (let* ((field-name (string-remove-prefix "eww-bibtex-get-"
                                                     (symbol-name (car func-list))))
                   ;; FIELD-NAME is the string after "eww-bibtex-get-"
                   ;; e.g. FIELD-NAME for "eww-bibtex-get-title" is "title".

                   (value-list (-list (funcall (car func-list) dom)))
                   ;; VALUE-LIST is what the first function in FUNC-LIST returns.
                   
                   
                   (value (if (> (length value-list) 1)
                              (completing-read (concat "Which " field-name "? ") value-list)
                            ;; VALUE is the string from having user select one of the strings inside VALUE-LIST
                            (car value-list)
                            ;; If VALUE-LIST have one string, use it without asking user.  
                            ))
                   (field-elt (--find (equal (car it) field-name) field-list))
                   ;; Find a field with FIELD-NAME as its name in FIELD-LIST
                   )

              ;; Is there any field with our FIELD-NAME inside the FIELD-LIST?
              (if field-elt
                  ;; Yes, there is a field with our FIELD-NAME.
                  ;; Replace that field's INIT (short for INITial content) with our VALUE.
                  (-replace field-elt `(,field-name nil ,value) field-list)
                
                ;; No, there isn't a field with our FIELD-NAME.
                ;; Then add a new field called FIELD-NAME with our VALUE
                ;; to FIELD-LIST.
                (push `(,field-name nil ,value) field-list)))

            ;; Pass the DOM argument to the next function.
            dom
            ;; Do this again for the next function in FUNC-LIST.
            (cdr func-list)))))

;;; TODO Documentation
(defun eww-bibtex-modify-field-list (field-list dom func-list)
  (cond ((null func-list) field-list)
        (t (let ((modified-field-list (funcall (car func-list) field-list dom)))
             (eww-bibtex-modify-field-list
              (or modified-field-list field-list)
              dom
              (cdr func-list))))))

(defun eww-bibtex-find-key-for-wikipedia (field-list)
  (let ((url (car (last (assoc "url" field-list)))))
    (if (string-match-p "wikipedia" url)
        (concat "wiki:" (url-unhex-string (file-name-base url))))))

(defun eww-bibtex-find-author-for-wikipedia (field-list _dom)
  (when-let ((url (car (last (assoc "url" field-list)))))
    (if (string-match-p "wikipedia" url)
        (-replace '("author" nil nil) '("author" nil "{Wikipedia Contributors}") field-list))))

(defun eww-bibtex-find-title-for-wikipedia (field-list _dom)
  (let ((url (car (last (assoc "url" field-list)))))
    (when (string-match-p "wikipedia" url)
      (-replace (assoc "title" field-list) `("title" nil ,(url-unhex-string (file-name-base url)))))))

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


;;;###autoload
(defun eww-bibtex (html-source &optional interactive)
  "Alter the \"Misc\" entry in `bibtex-BibTeX-entry-alist' based on
`eww-bibtex--build-field-list-functions'.

Then modify the field list with `eww-bibtex-modify-field-list-functions'.

And when user don't explicitly state the key of the entry, overrides
the auto-generated key from `bibtex-clean-entry' with `eww-bibtex-find-ref-key-functions'


This function works by replacing the initial content of all the fields
in the \"Misc\" entry.

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
the initial content of the field via `eww-bibtex--build-field-list'."
  
  (interactive (list (plist-get eww-data :source)))
  (let* ((misc-entry (assoc "Misc" bibtex-BibTeX-entry-alist))
         (dom (with-temp-buffer
                (insert html-source)
                (libxml-parse-html-region
                 (point-min)
                 (point-max))))
         (field-list (eww-bibtex--build-field-list
                      (cl-fifth misc-entry)
                      dom
                      eww-bibtex--build-field-list-functions))
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

(provide 'eww-bibtex)
;;; eww-bibtex.el ends here
