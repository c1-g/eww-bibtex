;;; eww-bibtex.el --- Cite a website with EWW -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'esxml)
(require 'eww)
(require 'dash)

(defcustom eww-bibtex-default-bibliography bibtex-files
  "List of BibTeX files that `eww-bibtex' will append new entry to.

Each element can be an absolute file names or a directory.

The `eww-bibtex' command will have user select only one element from
this list. If an element is a directory, the command will have user
select all BibTeX files in it.")

(defcustom eww-bibtex-finalize-field-list-functions
  (list #'eww-bibtex-find-author-for-wikipedia
        #'identity)
  "")

(defcustom eww-bibtex-find-ref-key-functions
  (list #'eww-bibtex-find-key-for-wikipedia)
  ""
  :type 'hook)

(defun eww-bibtex-update-field-alist ()
  (dolist (elt eww-bibtex-field-alist)
    ;; Entry commands
    (let* ((field-name (car elt))
           (field (cadr elt))
           (fname (intern (format "eww-bibtex-get-%s" field-name))))
      (cond ((symbolp field)
             (defalias fname
               (symbol-function field)))
            ((or (eq 'lambda (car field))
                 (eq 'function (car field)))
             (defalias fname
               field
               "Alias for an anonymous function defined by `eww-bibtex-field-alist'"))
            (t (defalias fname
                 (lambda ()
                   (:documentation
                    "Automatically defined by `eww-bibtex-update-field-alist'.")
                   (interactive)
                   (eww-bibtex--query field-name t))))))))

(defcustom eww-bibtex-field-alist
  '(("author" . ("meta[name=author]"
                 "meta[name=citation_author]"
                 "[rel=author] > [itemprop=name]"
                 "[rel=author]"
                 "[itemprop=author] > *"
                 "[itemprop=author]"
                 ".author"))
    ("title" . (lambda nil (plist-get eww-data :title)))
    ("url" . ("link[rel=canonical]"
              eww-current-url
              "meta[name=citation_fulltext_html_url]"))
    ("year" . ("meta[name=citation_publication_date]"
               "[itemprop=dateModified]"
               "[itemprop=datePublished]"
               "[itemprop=dateModified]"
               "[itemprop=datePublished]"
               "[id*=updated]"
               "time[pubdate]"
               ".post_date"
               "time"
               (lambda nil (format-time-string "%Y"))))
    ("note" . (lambda nil (format "[Online; accessed %s]" (format-time-string "%F")))))
  ""
  :type 'alist
  :set (lambda (sym val)
         (set-default sym val)
         (eww-bibtex-update-field-alist)))

(defun eww-bibtex-find-key-for-wikipedia (fields)
  (let ((url (car (last (assoc "url" fields)))))
    (if (string-match-p "wikipedia" url)
        (concat "wiki:" (file-name-base url)))))

(defun eww-bibtex-find-author-for-wikipedia (fields)
  (when-let ((url (car (last (assoc "url" fields)))))
    (if (string-match-p "wikipedia" url)
        (-replace '("author" nil nil) '("author" nil "{Wikipedia Contributors}") fields))))

;;; TODO documentation needed!

(defun eww-bibtex ()
  (interactive nil eww-mode)
  (let* ((misc-entry (assoc "Misc" bibtex-BibTeX-entry-alist))
         (field-list (cl-fifth misc-entry))
         (new-field-list)
         (target-files (car (cl-loop
                             for file in eww-bibtex-default-bibliography
                             if (file-directory-p file)
                             collect (directory-files-recursively file ".bib")
                             else
                             collect file))))
    (setq new-field-list (cl-loop
                          for elt in-ref eww-bibtex-field-alist
                          collect
                          (let* ((field-name (car elt))
                                 (get-field-fn (intern (format "eww-bibtex-get-%s" field-name)))
                                 (field (assoc field-name field-list)))
                            (if field
                                (-replace-at 2 (funcall get-field-fn)
                                             (if (null (cdr field))
                                                 (append field '(nil nil))
                                               field))
                              (list field-name nil (funcall get-field-fn))))))

    (setq misc-entry (-replace-at
                      4
                      (run-hook-with-args-until-success 'eww-bibtex-finalize-functions new-field-list)
                      misc-entry))
    
    (pop-to-buffer (find-file-noselect (completing-read "Which BibTeX file? " target-files nil t)))
    
    (let ((bibtex-entry-alist (list misc-entry))
          (bibtex-autokey-before-presentation-function (apply-partially #'eww-bibtex-replace-autokey new-field-list)))
      (bibtex-entry "Misc")
      (bibtex-clean-entry))))

(defun eww-bibtex-replace-autokey (field-list autokey)
  (or (run-hook-with-args-until-success 'eww-bibtex-find-ref-key-functions field-list)
      autokey))

(defun eww-bibtex--query (field &optional interactive)
  (interactive
   (list (completing-read "Which field?" (mapcar #'car eww-bibtex-field-alist))
         t))
  (when-let* ((selectors (if (eq (caadr (assoc field eww-bibtex-field-alist)) 'lambda)
                             (list (cadr (assoc field eww-bibtex-field-alist)))
                           (cadr (assoc field eww-bibtex-field-alist))))
              (eww-source (plist-get eww-data :source))
              (eww-dom (with-temp-buffer
                         (insert eww-source)
                         (libxml-parse-html-region (point-min) (point-max))))
              (value-list (seq-filter
                           #'stringp
                           (flatten-tree
                            (mapcar #'(lambda (selector)
                                        (cond ((stringp selector)
                                               (esxml-query-all selector eww-dom))
                                              (t
                                               (funcall selector))))
                                    selectors)))))
    (if (and interactive (> (length value-list) 1))
         (completing-read (format "Which %s?" field) value-list)
      (if interactive
          (message (car value-list))
        (car value-list)))))

(provide 'eww-bibtex)
;;; eww-bibtex.el ends here
