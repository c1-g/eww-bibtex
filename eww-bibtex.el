;;; eww-bibtex.el --- Cite a website with EWW -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'esxml)
(require 'eww)

(defun eww-bibtex-update-selector-alist ()
  (dolist (elt eww-bibtex-selector-alist)
    ;; Entry commands
    (let* ((field (car elt))
           (fname (intern (format "eww-bibtex-get-%s" field))))
      (cond ((or (symbolp (cdr elt))
                 (symbolp (cadr elt)))
             (defalias fname
               (symbol-function (cadr elt))))
            ((eq 'function (caadr elt))
             (defalias fname
               (cadadr elt)
               "Alias for an anonymous function defined by `eww-bibtex-selector-alist'"))
            (t (defalias fname
                 (lambda ()
                   (:documentation
                    "Automatically defined by `eww-bibtex-update-selector-alist'.")
                   (interactive)
                   (eww-bibtex-query field t))))))))

(defcustom eww-bibtex-selector-alist
  '(("author" ("meta[name=author]"
               "meta[name=citation_author]"
               "[rel=author] > [itemprop=name]"
               "[rel=author]"
               "[itemprop=author] > *"
               "[itemprop=author]"
                 ".author"))
    ("title" #'(lambda nil (plist-get eww-data :title)))
    ("url" (eww-current-url
            "meta[name=citation_fulltext_html_url]"))
    ("date" ("meta[name=citation_publication_date]"
             "[itemprop=dateModified]"
             "[itemprop=datePublished]"
             "[itemprop=dateModified]"
             "[itemprop=datePublished]"
             "[id*=updated]"
             "time[pubdate]"
             ".post_date"
             "time"))
    ("urldate" #'(lambda nil (format-time-string "%F %r")))
    ("note" ("meta[name=description]")))
  ""
  :type 'alist
  :set (lambda (sym val)
         (set-default sym val)
         (eww-bibtex-update-selector-alist)))

;;; TODO documentation needed!

(defun eww-bibtex ()
  (interactive nil eww-mode)
  (let ((entry-alist
         (cl-loop
          with fields = (assoc "Online"
                               (buffer-local-value
                                'bibtex-entry-alist
                                (find-file-noselect "/storage/bib/web.bib")))
          for elt in fields
          if (listp elt)
          collect
          (cl-loop
           for field in elt
           if (assoc (car field) eww-bibtex-selector-alist)
           collect
           (if (null (cdr field))
               (append field (list nil (funcall (intern (format "eww-bibtex-get-%s" (car field))))))
             (-replace-at 2 (funcall (intern (format "eww-bibtex-get-%s" (car field)))) field))
           else
           collect field)
          else
          collect elt)))
    (switch-to-buffer (find-buffer-visiting "/storage/bib/web.bib"))
    (let ((bibtex-entry-alist (list entry-alist)))
      (bibtex-entry "Online"))))

(defun eww-bibtex-query (field &optional interactive)
  (interactive
   (list (completing-read "Which field?" (mapcar #'car eww-bibtex-selector-alist))
         t))
  (when-let* ((selectors (cadr (assoc field eww-bibtex-selector-alist)))
              (eww-source (plist-get eww-data :source))
              (eww-dom (with-temp-buffer
                         (insert eww-source)
                         (libxml-parse-html-region (point-min) (point-max))))
              (value-list (seq-filter
                           #'stringp
                           (flatten-tree
                            (mapcar #'(lambda (selector)
                                        (if (symbolp selector)
                                            (funcall selector)
                                          (esxml-query-all selector eww-dom)))
                                    selectors)))))
    (if (and interactive (> (length value-list) 1))
        (completing-read (format "Which %s?" field) value-list)
      (car value-list))))

(provide 'eww-bibtex)
;;; eww-bibtex.el ends here
