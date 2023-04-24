;;; readwise.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 David Wilson
;;
;; Author: David Wilson <davidwilson@Davids-Air-2>
;; Maintainer: David Wilson <davidwilson@Davids-Air-2>
;; Created: April 21, 2023
;; Modified: April 21, 2023
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/davidwilson/readwise
;; Package-Requires: ((emacs "27.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:
(require 'cl-lib)
(require 'request)
(require 'org-roam)
(require 'org-id)


(defconst readwise-url "https://readwise.io/api/v2/" "URL for Readwise API")


(defgroup readwise nil
  "Customization options for readwise."
  :group 'org
  :prefix "readwise-")

(defcustom readwise-sync-db-path ""
  "Path to the file where the last sync time is stored."
  :type 'string
  :group 'readwise)

(defcustom readwise-api-token nil
  "Readwise API key"
  :type 'string
  :group 'readwise)


(defun readwise--save-last-sync ()
  "Save the most recent sync time."
  (let ((ts (format-time-string "%Y-%m-%dT%H:%M:%SZ" (current-time))))
    (with-file! readwise-sync-db-path
      (erase-buffer)
      (insert ts))))

(defun readwise--get-last-sync ()
  "Retrive the ISO 8601 timestamp of the last sync time."
  (if (file-exists-p readwise-sync-db-path)
      (with-temp-buffer
        (insert-file-contents readwise-sync-db-path)
        (buffer-string))
    nil))

(defun array-to-list (array)
  "Coerce ARRAY to a list."
  (-map #'identity array))

(defun readwise--fetch-highlights (api-key db-path &optional cursor since more)
  "Exhaustively fetch highlights from Readwise using API-KEY.
Saves each to the appropriate org-mode files.
API-KEY is the Readwise API key.
DB-PATH is the path to the file where the last sync time is stored.
CURSOR is the pagination cursor for fetching the next page of results.
SINCE is the ISO 8601 timestamp to limit highlights.
MORE indicates that there are more results to fetch."
  (let* ((params (append (when since `(("updatedAfter" . ,since)))
                         (when cursor `(("pageCursor" . ,cursor))))))
    (when more
      (request (concat readwise-url "export/")
        :type "GET"
        :params params
        :parser 'json-read
        :headers `(("Authorization" . ,(concat "Token " readwise-api-token)))
        :error (cl-function (lambda (_)
                              (message "Error fetching highlights from Readwise. Check your API key and try again.")))
        :success (cl-function (lambda (&key data &allow-other-keys)
                                (let-alist data
                                  (readwise--add-highlights (array-to-list .results))
                                  (if .nextPageCursor
                                      (readwise--fetch-highlights api-key db-path .nextPageCursor since 't)
                                    (readwise--save-last-sync)))))))))

(defun readwise--add-highlights (highlights)
  "Add all new highlights to org mode notes."
  (cl-flet
      ((import-highlights-for-book
        (book)
        (let* ((file (readwise--get-or-create-node-from-book book))
               (highlights (array-to-list (alist-get 'highlights book))))
          (org-roam-with-file file
              (widen)
            (goto-char (org-find-exact-headline-in-buffer "Highlights" (current-buffer) t))
            (org-narrow-to-subtree)
            (goto-char (point-max))
            (mapc (lambda (highlight)
                    (let-alist highlight
                      (newline)
                      (insert (concat "- " .text))))
                  highlights)
            (widen)
            (save-buffer)))))
    (mapc #'import-highlights-for-book highlights)))

(defun readwise--get-or-create-node-from-book (book)
  "Retrieve an org-mode note for BOOK, creating it if it doesn't exist. "
  (let* ((url (or (alist-get 'source_url book) (alist-get 'readwise_url book)))
         (node (org-roam-node-from-ref url)))
    (if node
        (org-roam-node-file node)
      (readwise--create-node book))))

(defun readwise--create-node (book)
  "Create an org-mode note for BOOK."
  (let ((id (org-id-new)))
    (let-alist book
      (org-roam-with-file
          (readwise--file-path-from-book book)
          nil
        (org-roam-property-add "ID" id)
        (org-roam-property-add "ROAM_REFS" (or .source_url .readwise_url))
        (org-roam-property-add "AUTHOR" (or l.author "N/A"))
        (goto-char (point-max))
        (insert (concat "#+title: " .title))
        (newline)
        (insert (concat "#+filetags: " ":ref:"))
        (newline)
        (when .cover_image_url (insert (concat "[[" .cover_image_url "]]")))
        (goto-char (point-max))
        (org-insert-heading)
        (insert "Highlights")
        (save-buffer))
      (org-roam-db-update-file (readwise--file-path-from-book book))
      (readwise--file-path-from-book book))))

(defun readwise--file-path-from-book (book)
  "Retrieve the file path for BOOK"
  (concat
   (file-name-as-directory org-roam-directory)
   (file-name-as-directory "highlights")
   (replace-regexp-in-string "[^A-Za-z0-9\_\-]" "" (replace-regexp-in-string "[/\s]" "-" (readwise--get-title book))) ".org"))

;;;###autoload
(defun readwise-pull ()
  "Import new highlights from Readwise into org-roam directory."
  (interactive)
  (readwise--fetch-highlights
   readwise-api-token
   readwise-sync-db-path
   nil
   (readwise--get-last-sync)
   't)
  (message "Done syncing highlights."))

(provide 'readwise)
;;; readwise.el ends here
