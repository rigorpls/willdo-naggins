;;; willdo-naggins.el --- A companion for org-mode  -*- lexical-binding:t -*-

;; Copyright (C) 2024 rigor

;; Author: rigor <>
;;
;; Version: 0.1
;; Package-Requires: ((org "9.3") (f "0.20.0") (dash "2.19.1"))
;; Keywords: org, reminder
;; URL: https://

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; This package provides willdo-naggins. Check beta-testing.org for instructions.

(require 'f)
(require 'dash)

;;; Configs

;; TODO these should be defcustom:s

(defvar willdo-naggins--data-home (xdg-data-home))

(defvar willdo-naggins-snooze-duration-hours 1)

(defvar willdo-naggins-win-background-color "black")

(defvar willdo-naggins-enable-tooltip t)

(defvar willdo-naggins--tooltip-text "󰽒")

(defvar willdo-naggins-enable-tooltip-explainer-message t)

(defvar willdo-naggins--bufn "*willdo-naggins-query-single*")

(defvar willdo-naggins--suggest-bufn "*willdo-naggins-suggest-single*")

(defvar willdo-naggins--suggest-ignore-tag "wnignore")

(defvar willdo-naggins--maint-snooze-duration-minutes 5
  "How often WN should check snoozed nags.

Nil, zero or negative values disable periodic checking.")

(defvar willdo-naggins--tooltip-check-duration-minutes 5)

;;; Variables

(defvar willdo-naggins--data-dir (f-join willdo-naggins--data-home "willdo-naggins"))

(defvar willdo-naggins--db-path (f-join willdo-naggins--data-dir "db.sqlite"))
(defvar willdo-naggins--db nil)

(defvar willdo-naggins--maint-snooze-timer nil)
(defvar willdo-naggins--tooltip-check-timer nil)
(defvar willdo-naggins--tooltip-show-timer nil)

;;; Data structures

(defclass willdo-naggins--q ()
  ((cursor :type integer
           :initarg :cursor
           :initform 0)
   (queue :type list
          :initarg :queue
          :initform (list))))

(cl-defmethod move ((wnq willdo-naggins--q) inc)
  "Move cursor INC steps"
  (with-slots (cursor queue) wnq
    (when-let ((new-cursor (+ cursor inc))
               ((<= 0 new-cursor))
               (val (nth new-cursor queue)))
      (setf cursor new-cursor)
      val)))

(cl-defmethod next ((wnq willdo-naggins--q))
  "Move cursor to next"
  (move wnq 1))

(cl-defmethod prev ((wnq willdo-naggins--q))
  "Move cursor to previous"
  (move wnq -1))

(cl-defmethod pos ((wnq willdo-naggins--q))
  "Position (1-based)"
  (1+ (slot-value wnq :cursor)))

(cl-defmethod cursor ((wnq willdo-naggins--q))
  "Cursor value"
  (slot-value wnq :cursor))

(cl-defmethod len ((wnq willdo-naggins--q))
  "Current queue length"
  (length (slot-value wnq :queue)))

(cl-defmethod emptyp ((wnq willdo-naggins--q))
  (= (len wnq) 0))

(cl-defmethod begp ((wnq willdo-naggins--q))
  "Are we at the beginning?"
  (= 0 (slot-value wnq :cursor)))

(cl-defmethod endp ((wnq willdo-naggins--q))
  "Are we at the end?"
  (not (nth (1+ (slot-value wnq :cursor)) (slot-value wnq :queue))))

(cl-defmethod val ((wnq willdo-naggins--q))
  (with-slots (cursor queue) wnq
    (nth cursor queue)))

(cl-defmethod pop-and-next ((wnq willdo-naggins--q))
  (when (= 0 (len wnq))
    (error "Cannot pop empty queue"))

  (with-slots (cursor queue) wnq
    (when-let ((curval (val wnq))
               (oldcursor cursor))
      (setf queue (-remove-at cursor queue))
      (when (eq oldcursor (len wnq))    ;popping from the end
        (setf cursor (1- cursor)))
      curval)))

;;; Acquisition functions

(defun willdo-naggins-learn ()
  (interactive)
  (let ((context (org-element-context))
        id)
    (cond
     ;; we need a node at point
     ((eq (car context) 'headline)
      (willdo-naggins--ensure-db)       ; load database
      (setq id (org-id-get-create))     ; ensure node has an org-id
      (willdo-naggins--add-to-db id)    ; add node id to database
      (message (concat "willdo-naggins: Just learned about node '"
                       (plist-get (cadr context) :raw-value) "'")))

     ;; TODO (message (substitute-command-keys "Hello \\[save-buffer] and \\`C-c'."))
     ;;
     (t
      (message "willdo-naggins: no headline at point")))))

;;; Maintenance functions

(defun willdo-naggins--stop-tooltips ()
  (cancel-timer willdo-naggins--tooltip-check-timer)
  (cancel-timer willdo-naggins--tooltip-show-timer))

(defun willdo-naggins--schedule-next-tooltip ()
  (when willdo-naggins--tooltip-show-timer
    (cancel-timer willdo-naggins--tooltip-show-timer))
  (setq willdo-naggins--tooltip-show-timer
        (run-with-idle-timer 30 nil #'willdo-naggins--show-tooltip)))

(defun willdo-naggins--show-tooltip ()
  (when (and willdo-naggins-enable-tooltip
             (not (equal (buffer-name) willdo-naggins--bufn))
             (eq (frame-visible-p (selected-frame)) t)
             (eq (frame-focus-state) t))
    (pcase-let* ((`(,win-left . ,win-top) (willdo-naggins--get-window-corner-xy nil t))
                 (`(,x . ,y) (posn-x-y (posn-at-point)))
                 (tx (+ win-left x 30))  ; TODO more robust values
                 (ty (+ win-top  y -80)) ; TODO more robust values
                 (tooltip-frame-parameters `((left . ,tx)
                                             (top . ,ty)))
                 (tooltip-hide-delay 10))
      (tooltip-show willdo-naggins--tooltip-text)
      (when willdo-naggins-enable-tooltip-explainer-message
        (message "willdo-naggins invites you to go on an adventure! Call `M-x willdo-naggins-query-single`."))))

  ;;TODO (message (substitute-command-keys "Hello \\[save-buffer] and \\`C-c'."))

  ;; reschedule tooltip for later, when idle
  (when willdo-naggins-enable-tooltip
    (when willdo-naggins--tooltip-check-timer
      (cancel-timer willdo-naggins--tooltip-check-timer))
    (setq willdo-naggins--tooltip-check-timer
          (run-with-timer (* willdo-naggins--tooltip-check-duration-minutes 60.0)
                          nil
                          #'willdo-naggins--schedule-next-tooltip))))

(defun willdo-naggins--update-snoozed-nags ()
  ;; remove old snooze timestamps
  (willdo-naggins--db-snooze-maint)
  ;; reschedule
  (when willdo-naggins--maint-snooze-timer
    (cancel-timer willdo-naggins--maint-snooze-timer))
  (when (and willdo-naggins--maint-snooze-duration-minutes
             (> willdo-naggins--maint-snooze-duration-minutes 0))
    (setq willdo-naggins--maint-snooze-timer
          (run-at-time (* willdo-naggins--maint-snooze-duration-minutes 60.0) nil
                       #'willdo-naggins--update-snoozed-nags))))

;;; Operational stuff

(defun willdo-naggins--format-header (wnq)
  (format "What should we do about this node? [%d/%d]" (pos wnq) (len wnq)))

(defun willdo-naggins--fetch-nags ()
  ;; update snooze timer
  (willdo-naggins--update-snoozed-nags)
  ;; return nags from the db
  (willdo-naggins--db-fetch-nags))

(defun willdo-naggins--get-window-corner-xy (&optional win absolute)
  (let ((win-coords (window-edges win nil absolute t)))
    (cons (nth 0 win-coords)
          (nth 1 win-coords))))

(defun willdo-naggins--find-id (id)
  (let* ((mkr (org-id-find id t))
         (buf (marker-buffer mkr)))
    (cons mkr buf)))

(defun willdo-naggins--open-childframe (wn-buf)
  (pcase-let* ((`(,win-left . ,win-top) (willdo-naggins--get-window-corner-xy))
               (abs-pos (cons (+ win-left 100) ;TODO more robust values
                              (+ win-top 100)))
               (alist `((child-frame-parameters . ((left . ,(car abs-pos))
                                                   (top . ,(cdr abs-pos))
                                                   (height . 20)
                                                   (width . 60)
                                                   (child-frame-border-width . 5)
                                                   (background-color . ,willdo-naggins-win-background-color)))
                        (window-parameters . ((mode-line-format . none))))))
    (display-buffer-in-child-frame wn-buf alist)))

(defun willdo-naggins--configure-zoom-buffer (&optional mkr)
  (display-line-numbers-mode -1) ; no line numbers (no way to do this from display parameters?)
  (goto-char mkr)                ; go to node
  (org-narrow-to-element)        ; narrow
  (org-show-entry)
  )

(defun willdo-naggins--close-popups ()
  (dolist (bufn `(,willdo-naggins--bufn ,willdo-naggins--suggest-bufn))
    (when-let ((wn-buf (get-buffer bufn))
                (wn-win (get-buffer-window wn-buf))
                (wn-frame (window-frame wn-win)))
      (delete-frame wn-frame)
      (kill-buffer wn-buf))))

(defun willdo-naggins--position-if-has-date (el buf)
  (let* ((sch (org-element-property :scheduled el))
         (dead (org-element-property :deadline el)))
    (when (or sch dead)
      (set-marker (make-marker) (org-element-property :begin el) buf))))

(defun willdo-naggins--entry-is-known ()
  "Has an id and WN knows about it?"
  (and-let* ((id (org-id-get))
             (entry (willdo-naggins--db-nag-exists id))
             (t))))

(defun willdo-naggins--scan-and-create-markers ()
  (org-scan-tags
   (lambda ()
     (willdo-naggins--position-if-has-date (org-element-at-point) (current-buffer)))
   (lambda (todo tags-list level)
     (and
      (not (member willdo-naggins--suggest-ignore-tag tags-list))
      (not (willdo-naggins--entry-is-known))))
   nil))

(defun willdo-naggins--find-entries-in-agenda-files ()
  (cl-loop for file in (org-agenda-files)
           for pointers = (with-current-buffer (find-file-noselect file)
                            (willdo-naggins--scan-and-create-markers))
           for markers = (seq-keep #'identity pointers)
           when markers
           collect (cons file markers)))

(defun willdo-naggins--fetch-suggestions ()
  (cl-loop for (file . pointers) in (willdo-naggins--find-entries-in-agenda-files)
           nconc (mapcar (lambda (p) (cons file p)) pointers)))

;;; Query functions

(defun willdo-naggins-query-single (&optional same-frame)
  (interactive)

  (unless willdo-naggins--db
    (willdo-naggins--ensure-db))

  ;; prepare tooltip timer for later
  ;; TODO put this somewhere else?
  (run-with-idle-timer 3 nil #'willdo-naggins--show-tooltip)

  (if-let ((nags (willdo-naggins--fetch-nags))
           (scope (willdo-naggins--q :queue nags))
           (marker-and-buf (willdo-naggins--fetch-nag (val scope)))
           (mkr (car marker-and-buf))
           (buf (cdr marker-and-buf))
           (zoom-buf
            (progn
              (when same-frame
                ;; prevents deleting the frame when killing the buffer below
                ;; FIXME find a better way
                (display-buffer-same-window buf nil))
              (when-let ((sbuf (get-buffer willdo-naggins--bufn)))
                (kill-buffer sbuf))
              (make-indirect-buffer buf willdo-naggins--bufn t)))
           ;; open child frame
           (win (if same-frame
                    (progn
                      (display-buffer-same-window zoom-buf nil)
                      (selected-window))
                  (willdo-naggins--open-childframe zoom-buf))))
      (progn
        ;; select the frame and window that will show the transient
        (select-frame-set-input-focus (window-frame win))
        (select-window win)
        ;; configure buffer and show
        (willdo-naggins--configure-zoom-buffer mkr)
        ;; show transient
        (run-at-time 0.1 nil
                     ;; FIXME I don't quite understand why this needs to be in a
                     ;; timer, but without it the transient sometimes loses
                     ;; focus and vanishes as soon as the childframe opens
                     (lambda ()
                       (transient-setup 'willdo-naggins-single-dispatch nil nil :scope scope))))
    (message "No nags, for now.")))

(defun willdo-naggins-suggest-single (&optional same-frame)
  (interactive)

  (if-let ((suggestions (willdo-naggins--fetch-suggestions)) ; NOTE can be nil, which fails this if-let
           (scope (willdo-naggins--q :queue suggestions))
           (marker-and-buf (willdo-naggins--fetch-suggestion (val scope)))
           (mkr (car marker-and-buf))
           (buf (cdr marker-and-buf))
           (zoom-buf
            (progn
              (when same-frame
                ;; prevents deleting the frame when killing the buffer below
                ;; FIXME find a better way
                (display-buffer-same-window buf nil))
              (when-let ((sbuf (get-buffer willdo-naggins--suggest-bufn)))
                (kill-buffer sbuf))
              (make-indirect-buffer buf willdo-naggins--suggest-bufn t)))
           ;; open child frame
           (win (if same-frame
                    (progn
                      (display-buffer-same-window zoom-buf nil)
                      (selected-window))
                  (willdo-naggins--open-childframe zoom-buf))))
      (progn
        ;; select the window that will show the transient
        (select-frame-set-input-focus (window-frame win))
        (select-window win)
        ;; configure buffer and show
        (willdo-naggins--configure-zoom-buffer mkr)
        ;; show transient
        (run-at-time 0.1 nil
                     ;; FIXME I don't quite understand why this needs to be in a
                     ;; timer, but without it the transient sometimes loses
                     ;; focus and vanishes as soon as the childframe opens
                     (lambda ()
                       (transient-setup 'willdo-naggins-suggest-dispatch nil nil :scope scope))))

    (message "No more suggestions from willdo-naggins!")))

;;; Utils for transients

(defun willdo-naggins--update-popup (wnq fetchfun)
  (pcase-let* ((pointer (val wnq))
               (`(,mkr . ,buf) (funcall fetchfun pointer))
               (oldbuf (current-buffer))
               (oldbufn (buffer-name))
               (zoom-buf
                (progn
                  ;; prevents deleting the frame when killing the buffer below
                  ;; FIXME find a better way
                  (display-buffer-same-window buf nil)
                  (kill-buffer oldbuf)
                  (make-indirect-buffer buf oldbufn t))))
        (display-buffer-same-window zoom-buf nil)
        (with-current-buffer zoom-buf
          ;; configure buffer and show
          (willdo-naggins--configure-zoom-buffer mkr)
          ;; show transient
          (transient-setup transient-current-command nil nil :scope wnq))))

(transient-define-suffix willdo-naggins--pop-scope ()
  (pop-and-next (transient-scope)))

(transient-define-suffix willdo-naggins--show-or-dismiss (fetchfun)
  (let ((wnq (transient-scope)))
    (if (emptyp wnq)
        (willdo-naggins--suffix-dismiss)
      (willdo-naggins--update-popup wnq fetchfun))))

(defun willdo-naggins--fetch-nag (wnqval)
  (let ((id (car wnqval)))
    (willdo-naggins--find-id id)))

(defun willdo-naggins--fetch-suggestion (wnqval)
  (pcase-let* ((`(,file . ,mkr) wnqval)
               (buf (find-file-noselect file)))
    (cons mkr buf)))

;;; Transients and commands

(transient-define-prefix willdo-naggins-single-dispatch ()
  [:description (lambda () (willdo-naggins--format-header (transient-scope)))
                ["Edit"
                 ("et" "TODO" willdo-naggins--suffix-org-todo :transient transient--do-recurse)
                 ("es" "schedule" willdo-naggins--suffix-org-schedule :transient transient--do-recurse)
                 ("ed" "deadline" willdo-naggins--suffix-org-deadline :transient transient--do-recurse)
                 ("eu" "dumb undo" undo-only :transient t) ;TODO use a more intelligent undo mechanism
                 ]

                ["Nag"
                 ("rs" "snooze" (lambda ()
                                  (interactive)
                                  (willdo-naggins--snooze) ; TODO what if this errors?
                                  (willdo-naggins--pop-scope)
                                  (willdo-naggins--show-or-dismiss #'willdo-naggins--fetch-nag))
                  :transient transient--do-exit)
                 ("rf" "forget" (lambda ()
                                  (interactive)
                                  (willdo-naggins--forget) ;TODO what if this errors?
                                  (willdo-naggins--pop-scope)
                                  (willdo-naggins--show-or-dismiss #'willdo-naggins--fetch-nag))
                  :transient transient--do-exit)]

                ["Navigate"
                 ("n" "next nag" (lambda ()
                                   (interactive)
                                   (willdo-naggins--suffix-next #'willdo-naggins--fetch-nag))
                  :transient transient--do-exit
                  :inapt-if (lambda () (endp (transient-scope))))
                 ("p" "prev nag" (lambda ()
                                   (interactive)
                                   (willdo-naggins--suffix-prev #'willdo-naggins--fetch-nag))
                  :transient transient--do-exit
                  :inapt-if (lambda () (begp (transient-scope))))
                 ]]

  ["Dismiss willdo naggins"
   ("v" "go to item" willdo-naggins--suffix-focus-closing)
   ("q" "close and sleep" willdo-naggins--suffix-dismiss)])

(transient-define-prefix willdo-naggins-suggest-dispatch ()
  [:description (lambda () (willdo-naggins--format-header (transient-scope)))
                ["Edit"
                 ("a" "Acquire" (lambda ()
                                  (interactive)
                                  (willdo-naggins-learn)
                                  (willdo-naggins--pop-scope)
                                  (willdo-naggins--show-or-dismiss #'willdo-naggins--fetch-suggestion))
                  :transient transient--do-exit)

                 ("i" "Ignore" (lambda ()
                                 (interactive)
                                 (org-set-tags (cons willdo-naggins--suggest-ignore-tag (org-get-tags nil t))) ; add the "ignore" tag
                                 (willdo-naggins--pop-scope)
                                 (willdo-naggins--show-or-dismiss #'willdo-naggins--fetch-suggestion))
                  :transient transient--do-exit)]

                ["Navigate"
                 ("n" "next suggestion" (lambda ()
                                          (interactive)
                                          (willdo-naggins--suffix-next #'willdo-naggins--fetch-suggestion))
                  :transient transient--do-exit
                  :inapt-if (lambda () (endp (transient-scope))))
                 ("p" "prev sugestion" (lambda ()
                                         (interactive)
                                         (willdo-naggins--suffix-prev #'willdo-naggins--fetch-suggestion))
                  :transient transient--do-exit
                  :inapt-if (lambda () (begp (transient-scope))))
                 ]]

  ["Dismiss willdo naggins"
   ("q" "close" willdo-naggins--suffix-dismiss)])

(transient-define-suffix willdo-naggins--suffix-org-todo ()
  (interactive)
  ;; NOTE usage with prefix arg is not great, the extra window confuses
  ;; transient's "recurse" behavior for some reason
  (org-todo))

(transient-define-suffix willdo-naggins--suffix-org-schedule (arg)
  (interactive "P")
  (org-schedule arg))

(transient-define-suffix willdo-naggins--suffix-org-deadline (arg)
  (interactive "P")
  (org-deadline arg))

(transient-define-suffix willdo-naggins--suffix-focus-closing ()
  (interactive)
  (pcase-let* ((pointer (val (transient-scope)))
               (`(,mkr . ,buf) (willdo-naggins--fetch-nag pointer)))
    ;; TODO should not call this if not using a childframe
    (willdo-naggins--close-popups)
    (select-frame (make-frame))
    (display-buffer-full-frame buf nil)
    (with-current-buffer buf
      (goto-char mkr)
      (recenter-top-bottom))))

(transient-define-suffix willdo-naggins--suffix-next (fetchfun)
  (interactive)
  (willdo-naggins--suffix-strafe #'next fetchfun))

(transient-define-suffix willdo-naggins--suffix-prev (fetchfun)
  (interactive)
  (willdo-naggins--suffix-strafe #'prev fetchfun))

(transient-define-suffix willdo-naggins--suffix-strafe (strafefun fetchfun)
  (interactive)
  (if-let ((wnq (transient-scope))
           ((funcall strafefun wnq)))
      (willdo-naggins--update-popup wnq fetchfun)
    (error "Oops, something went wrong with navigation")))

(transient-define-suffix willdo-naggins--suffix-dismiss ()
  (interactive)
  ;; TODO should not call this if not using a childframe
  (willdo-naggins--close-popups))

(defun willdo-naggins--snooze (&optional id)
  (interactive)
  (let* ((id (or id (org-id-get)))
         (ts (time-convert (current-time) 'integer))
         (snooze-duration (* willdo-naggins-snooze-duration-hours 60.0 60.0))
         (snooze-ts (truncate (+ ts snooze-duration))))
    (willdo-naggins--db-snooze-id-until id snooze-ts)))

(defun willdo-naggins--forget (&optional id)
  (interactive)
  (let* ((id (or id (org-id-get))))
    (willdo-naggins--db-forget id)))

;;; Database functions

(defun willdo-naggins--prepare ()
  (willdo-naggins--create-datadir)
  (willdo-naggins--open-db)
  (willdo-naggins--ensure-db))

(defun willdo-naggins--create-datadir ()
  (apply #'f-mkdir (f-split willdo-naggins--data-dir)))

(defun willdo-naggins--open-db ()
  (setq willdo-naggins--db (sqlite-open willdo-naggins--db-path)))

(defun willdo-naggins--ensure-db ()
  ;; open db
  (unless willdo-naggins--db
    (willdo-naggins--open-db))

  ;; create the tables if empty
  ;; TODO should we avoid this with some sort of check query?
  (sqlite-execute
   willdo-naggins--db
   (concat
    "CREATE TABLE IF NOT EXISTS nags ("
    " id INTEGER PRIMARY KEY AUTOINCREMENT,"
    " uuid TEXT NOT NULL,"
    " snooze INTEGER,"
    " forget INTEGER NOT NULL DEFAULT 'FALSE'"
    ")"
    )))

(defun willdo-naggins--db-nag-exists (id)
  (sqlite-select
   willdo-naggins--db
   "SELECT uuid FROM nags WHERE uuid = (?)"
   (list id)))

(defun willdo-naggins--add-to-db (id)
  (sqlite-execute
   willdo-naggins--db
   "INSERT INTO nags (uuid) VALUES (?)" (list id)))

(defun willdo-naggins--db-fetch-nags ()
  (sqlite-select
   willdo-naggins--db
   (concat
    "SELECT uuid FROM nags WHERE"
    " (snooze IS NULL OR snooze < unixepoch())"
    " AND"
    " (forget <> TRUE)"
    )))

(defun willdo-naggins--db-forget (id)
  (sqlite-execute
   willdo-naggins--db
   "UPDATE nags SET forget = TRUE WHERE uuid = (?)"
   (list id)))

(defun willdo-naggins--db-snooze-id-until (id snooze-ts)
  (sqlite-execute
   willdo-naggins--db
   "UPDATE nags SET snooze = (?) WHERE uuid = (?)"
   (list snooze-ts id)))

(defun willdo-naggins--db-snooze-maint ()
  (sqlite-execute
   willdo-naggins--db
   "UPDATE nags SET snooze = NULL WHERE snooze < unixepoch()"))

(provide 'willdo-naggins)
