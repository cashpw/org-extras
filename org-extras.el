;;; org-extras.el --- org-mode utility functions -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Cash Prokop-Weaver
;;
;; Author: Cash Prokop-Weaver <cashbweaver@gmail.com>
;; Maintainer: Cash Prokop-Weaver <cashbweaver@gmail.com>
;; Created: April 04, 2024
;; Modified: April 04, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/cashweaver/org-extras
;; Package-Requires: ((emacs "24.3") (dash "2.19.1")
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  org-mode utility functions
;;
;;; Code:

(require 'dash)

(defun org-extras-property-set-on-all-top-level-headings (property value)
  "Set PROPERTY to VALUE for all top-level headings in the current buffer."
  (interactive "sEnter property name: \nsEnter property value: ")
  (org-map-entries
   (lambda ()
     (when (= (org-outline-level) 1)
       (org-entry-put (point) property value)))
   nil 'file))

(defun org-extras-files-in-directory (dir-path &optional include-archive)
  "Return a list of all .org$ files at DIR-PATH; include .org_archive if INCLUDE-ARCHIVE is non-nil."
  (directory-files dir-path
                   t
                   (if include-archive
                       "\\.org\\(_archive\\)?$"
                     "\\.org$")))

(defun org-extras-files-archive-files (file-paths)
  "Return list of archive files for FILE-PATHS."
  (remove
   nil
   (--map
    (let ((archive-path (concat it "_archive")))
      (when (file-exists-p archive-path)
        archive-path))
    file-paths)))

(defun org-extras-get-inbuffer-option (option)
  "Return value of #+OPTION in current buffer.

Example: `(org-extras-get-inbuffer-option \"title\")'."
  ;; We use `org-export-get-environment' here, instead of `org-element-parse-buffer' because the
  ;; later is much slower in large buffers.
  (plist-get (org-export-get-environment) (intern (concat ":" option))))

(defun org-extras-inbuffer-option-exists-p (option)
  "Set #+OPTION to VALUE in current buffer."
  (save-excursion
    (let ((case-fold-search t))
      (goto-char (point-min))
      (search-forward-regexp (format "#\\+%s: " option) nil 'noerror))))

(defun org-extras-set-inbuffer-option (option value)
  "Set #+OPTION to VALUE in current buffer."
  (if-let ((option-point (org-extras-inbuffer-option-exists-p option)))
      (progn
        (goto-char option-point)
        (kill-line)
        (insert value))
    (goto-char (org-extras--add-empty-inbuffer-option option))
    (insert (format "%s\n" value))))

(defun org-extras--add-empty-inbuffer-option (option)
  "Insert \"#+OPTION: \" into buffer with other inbuffer options."
  (save-excursion
    (goto-char
     (or
      ;; Existing option
      (search-forward-regexp "#\\+[^:]: " nil 'noerror)
      ;; :END: of buffer-level :PROPERTIES:
      (when (save-excursion
              (goto-char (point-min))
              (string= (org-current-line-string) ":PROPERTIES:"))
        (search-forward-regexp "^:END:")
        (next-line))
      (point-min)))
    (beginning-of-line)
    (insert (format "#+%s: " option))
    (point)))

(defun org-extras-get-property (pom property)
  "Return value of PROPERTY at POM, else nil."
  (let ((property-list (org-entry-properties pom property)))
    (when property-list
      (cdr (car property-list)))))

(defun org-extras-get-all-tags-in-file ()
  "Returns a list of all unique tags used in the current org-mode file."
  (let ((all-tags '()))
    (org-map-entries
     (lambda ()
       (when-let ((tags (org-make-tag-string (org-get-tags (point) t))))
         (setq all-tags (append all-tags (split-string tags ":")))))
     nil 'file)
    (delete-dups (-remove #'string-empty-p all-tags))))

(defun org-extras-set-created (&optional time)
  "Set the 'Created' property to now, or TIME if present."
  (let ((time (or time (current-time))))
    (org-set-property "Created" (format-time-string "[%F %a %H:%M]" time))))

(defun org-extras-heading-text-for-today ()
  "Return inactive `org-mode' timestamp for today."
  (format-time-string "[%F %a]" (current-time)))

(defun org-extras-insert-heading-for-today (&optional top)
  "Insert a heading for today's date, with relevant tags."
  (interactive)
  (if top
      (org-insert-heading nil t t)
    (org-insert-heading-respect-content))
  (insert (org-extras-heading-text-for-today))
  (org-extras-set-created))

(defun org-extras-heading-marker-for-today ()
  "Return t if a heading for today exists.

Refer to `org-extras-mode-insert-heading-for-today'."
  (org-find-exact-headline-in-buffer (org-extras-heading-text-for-today)))

(defun org-extras-scheduled-to-repeat-daily-p (pom)
  "Return non-nil if the headline at POM repeats daily."
  (when-let* ((scheduled-alist (org-entry-properties pom "SCHEDULED"))
              (scheduled-string (cdr (car scheduled-alist))))
    (or (s-contains-p "++1d" scheduled-string)
        (s-contains-p ".+1d" scheduled-string))))

(defun org-extras-scheduled-to-repeat-weekly-p (pom)
  "Return non-nil if the headline at POM repeats weekly."
  (when-let* ((scheduled-alist (org-entry-properties pom "SCHEDULED"))
              (scheduled-string (cdr (car scheduled-alist))))
    (or (s-contains-p "++1w" scheduled-string)
        (s-contains-p ".+1w" scheduled-string))))

(defun org-extras-scheduled-to-repeat-p (pom)
  "Return non-nil if the headline at POM repeats weekly."
  (when-let* ((scheduled-alist (org-entry-properties pom "SCHEDULED"))
              (scheduled-string (cdr (car scheduled-alist))))
    (or (s-contains-p "++" scheduled-string)
        (s-contains-p ".+" scheduled-string))))


(defun org-extras-scheduled-in-past-p (&optional point-or-marker)
  "Return non-nil if the heading at POINT-OR-MARKER is scheduled in the past."
  (let ((point-or-marker (or point-or-marker (point))))
    (time-less-p (org-get-scheduled-time point-or-marker) (current-time))))

(defun org-extras-scheduled-in-future-p (&optional point-or-marker)
  "Return non-nil if the heading at POINT-OR-MARKER is scheduled in the future."
  (let ((point-or-marker (or point-or-marker (point))))
    (time-less-p (current-time) (org-get-scheduled-time point-or-marker))))

(defun org-extras-reschedule-overdue-todo (&optional point-or-marker)
  "Reschedule a todo (at point, or POINT-OR-MARKER) to its next valid repetition date."
  (interactive)
  (save-excursion
    (let ((point-or-marker (or point-or-marker (point)))
          (org-log-done nil))
      (goto-char point-or-marker)
      (when (org-get-repeat)
        (while (org-extras-scheduled-in-past-p point-or-marker)
          (org-todo "RESCHEDULE"))))))

(defun org-extras-reschedule-overdue-todo-agenda ()
  "Invoke `org-extras-reschedule-overdue-todo' for an agenda view.

Based on `org-agenda-date-later'."
  (let* ((marker (or (org-get-at-bol 'org-marker) (org-agenda-error)))
         (buffer (marker-buffer marker)))
    (with-current-buffer buffer
      (org-extras-reschedule-overdue-todo marker))))

(defun org-extras-link-description-at-point ()
  "Reference: https://emacs.stackexchange.com/a/38297"
  (let ((link (org-element-context)))
    (buffer-substring-no-properties
     (org-element-property :contents-begin link)
     (org-element-property :contents-end link))))

(defun org-extras-get-priority (point-or-marker)
  "Return priority at POINT-OR-MARKER, or nil if priority isn't set."
  (let ((priority (org-entry-get point-or-marker "PRIORITY")))
    (unless (string= "" priority)
      priority)))

(defun org-extras-get-content-under-heading (&optional pos)
  "Return immediate content of heading at point or (optionally) POS."
  (interactive)
  (save-excursion
    (when pos
      (goto-char pos))
    (org-back-to-heading)
    (forward-line)
    (unless (= (point) (point-max))
      (let ((start (point))
            (end (or (outline-next-heading) (point-max))))
        (buffer-substring-no-properties start end)))))

(provide 'org-extras)
;;; org-extras.el ends here
