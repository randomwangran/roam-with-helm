;;; roam-with-helm --- A source for navigation Org-roam  -*- lexical-binding: nil; -*-

;; Copyright (C) 2022 Ran Wang

;; Author: Ran Wang
;; URL: https://github.com/randomwangran/roam-with-helm
;; Version: 0.0.3
;; Package-Requires: ((emacs "27.1") (org "9.4") (helm "3.6.2") (org-roam "2.2.1"))
;; Keywords: org-mode, navigation, writing, note-taking

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;;; Installation

;;;;; MELPA

;; This package is not available on MELPA. Manual installation required.

;;;;; Manual

;; Install these required packages:

;; Then put this file in your load-path, and put this in your init
;; file:
;; (require 'roam-with-helm)


(require 'helm-mode)
(require 'org-roam)
(require 'helm-org-walk)


(setq previous-node-title nil)

;;;; [Very Very Very important]
;; Walk around for dynamically capture a thoughts into a node.
;; Notice `(dynamic-node ,title-or-id), only works for Helm user.
;;
(defun org-roam-capture--setup-target-location ()
  "Initialize the buffer, and goto the location of the new capture.
Return the ID of the location."
  (let (p new-file-p)
    (pcase (org-roam-capture--get-target)
      (`(file ,path)
       (setq path (org-roam-capture--target-truepath path)
             new-file-p (org-roam-capture--new-file-p path))
       (when new-file-p (org-roam-capture--put :new-file path))
       (set-buffer (org-capture-target-buffer path))
       (widen)
       (setq p (goto-char (point-min))))
      (`(file+olp ,path ,olp)
       (setq path (org-roam-capture--target-truepath path)
             new-file-p (org-roam-capture--new-file-p path))
       (when new-file-p (org-roam-capture--put :new-file path))
       (set-buffer (org-capture-target-buffer path))
       (setq p (point-min))
       (let ((m (org-roam-capture-find-or-create-olp olp)))
         (goto-char m))
       (widen))
      (`(file+head ,path ,head)
       (setq path (org-roam-capture--target-truepath path)
             new-file-p (org-roam-capture--new-file-p path))
       (set-buffer (org-capture-target-buffer path))
       (when new-file-p
         (org-roam-capture--put :new-file path)
         (insert (org-roam-capture--fill-template head 'ensure-newline)))
       (widen)
       (setq p (goto-char (point-min))))
      (`(file+head+olp ,path ,head ,olp)
       (setq path (org-roam-capture--target-truepath path)
             new-file-p (org-roam-capture--new-file-p path))
       (set-buffer (org-capture-target-buffer path))
       (widen)
       (when new-file-p
         (org-roam-capture--put :new-file path)
         (insert (org-roam-capture--fill-template head 'ensure-newline)))
       (setq p (point-min))
       (let ((m (org-roam-capture-find-or-create-olp olp)))
         (goto-char m)))
      (`(file+datetree ,path ,tree-type)
       (setq path (org-roam-capture--target-truepath path))
       (require 'org-datetree)
       (widen)
       (set-buffer (org-capture-target-buffer path))
       (unless (file-exists-p path)
         (org-roam-capture--put :new-file path))
       (funcall
        (pcase tree-type
          (`week #'org-datetree-find-iso-week-create)
          (`month #'org-datetree-find-month-create)
          (_ #'org-datetree-find-date-create))
        (calendar-gregorian-from-absolute
         (cond
          (org-overriding-default-time
           ;; Use the overriding default time.
           (time-to-days org-overriding-default-time))
          ((org-capture-get :default-time)
           (time-to-days (org-capture-get :default-time)))
          ((org-capture-get :time-prompt)
           ;; Prompt for date.  Bind `org-end-time-was-given' so
           ;; that `org-read-date-analyze' handles the time range
           ;; case and returns `prompt-time' with the start value.
           (let* ((org-time-was-given nil)
                  (org-end-time-was-given nil)
                  (prompt-time (org-read-date
                                nil t nil "Date for tree entry:")))
             (org-capture-put
              :default-time
              (if (or org-time-was-given
                      (= (time-to-days prompt-time) (org-today)))
                  prompt-time
                ;; Use 00:00 when no time is given for another
                ;; date than today?
                (apply #'encode-time 0 0
                       org-extend-today-until
                       (cl-cdddr (decode-time prompt-time)))))
             (time-to-days prompt-time)))
          (t
           ;; Current date, possibly corrected for late night
           ;; workers.
           (org-today)))))
       (setq p (point)))
      (`(node ,title-or-id)
       ;; first try to get ID, then try to get title/alias
       (let ((node (or (org-roam-node-from-id title-or-id)
                       (org-roam-node-from-title-or-alias title-or-id)
                       (user-error "No node with title or id \"%s\"" title-or-id))))
         (set-buffer (org-capture-target-buffer (org-roam-node-file node)))
         (goto-char (org-roam-node-point node))
         (setq p (org-roam-node-point node))))
      (`(dynamic-node ,title-or-id)
       ;; (nth 0 canadidate) is a hack for helm user.
       (let ((node (or (org-roam-node-from-id (nth 0 canadidate))
                       (user-error "No node with title or id \"%s\"" title-or-id))))
         (set-buffer (org-capture-target-buffer (org-roam-node-file node)))
         (goto-char (org-roam-node-point node))
         (setq p (org-roam-node-point node))))

      (`(dynamic-node-2nd ,title-or-id)
       ;; (nth 0 canadidate) is a hack for helm user.
       (let ((node (or (org-roam-node-from-id new-candidates)
                       (user-error "No node with title or id \"%s\"" title-or-id))))
         (set-buffer (org-capture-target-buffer (org-roam-node-file node)))
         (goto-char (org-roam-node-point node))
         (setq p (org-roam-node-point node))))
      )
    ;; Setup `org-id' for the current capture target and return it back to the
    ;; caller.
    (save-excursion
      (goto-char p)
      (if-let ((id (org-entry-get p "ID")))
          (setf (org-roam-node-id org-roam-capture--node) id)
        (org-entry-put p "ID" (org-roam-node-id org-roam-capture--node)))
      (prog1
          (org-id-get)
        (run-hooks 'org-roam-capture-new-node-hook)))))

;;;;
(defun helm-org-roam-node-walk--subheadings-at-point ()
  "Return a list of subheadings."
  (if (org-before-first-heading-p)
      (save-excursion
        (let ((pred (lambda () (if (org-id-get)
                                   (cons (org-entry-get nil "ITEM")
                                         (org-id-get))))))
          (progn
            (org-show-subtree)
            (remq nil (cdr (cl-loop collect (funcall pred)
                          until (let ((pos (point)))
                                  (null (org-forward-heading-same-level nil t))
                                  (eq pos (point)))))))))
    (save-excursion
        (let ((pred (lambda () (if (org-id-get)
                                   (cons (org-entry-get nil "ITEM")
                                         (org-id-get))))))
          (progn
            (org-back-to-heading t)
            (org-show-subtree)
            (remq nil (if (org-goto-first-child)
                  (cl-loop collect (funcall pred)
                           until (let ((pos (point)))
                                   (null (org-forward-heading-same-level nil t))
                                   (eq pos (point)))))))))))

(defun helm-org-roam-node-walk (my-id)
  "Process content of MY-ID. If it is a title, insert the whole
file. Otherwise, just insert the content of the subtree."
  (interactive)
  (push my-id roam-walk-stack)
  ;; if a node does not have any child, then jump into the node;
  ;; otherwise, show the children.
  (if (nth 0 (nth 0 (if (= (org-roam-node-level (org-roam-node-from-id my-id)) 0)
                        (progn
                          (setq node-does-not-have-any-head-flag t)
                          (with-temp-buffer
                            (insert-file-contents (org-roam-node-file (org-roam-node-from-id my-id)))
                            (org-mode)
                            (setq my-new-candidates (helm-org-roam-node-walk--subheadings-at-point))))
                      (with-temp-buffer
                        (insert-file-contents (org-roam-node-file (org-roam-node-from-id my-id)))
                        (org-mode)
                        (re-search-forward (concat ":ID:       " my-id))
                        (setq my-new-candidates (helm-org-roam-node-walk--subheadings-at-point))))))
      (helm
              :sources (list
                        (helm-build-sync-source "We have some children?: "
                          :candidates my-new-candidates
                          :keymap helm-org-node-walk-map
                          :action
                          '(("Open" . (lambda (new-candidates)
                                        (or (helm-org-roam-node-walk new-candidates)
                                            (org-id-goto new-candidates))))

                            ("Find File" . (lambda (canadidate)
                                             (setq roam-visit-immediately canadidate)))

                            ("Capture as a child" . (lambda (new-candidates)
                                                      (org-roam-capture-
                                                       :templates '(("v" "Test before 1st head" entry
                                                                     "* %?\n:PROPERTIES:\n:ID: %(org-id-uuid)\n:END:\n"
                                                                     :target (dynamic-node-2nd title-or-id)
                                                                     ))
                                                       :node (org-roam-node-from-id new-candidates)
                                                       :props '(:immediate-finish nil))))

                            ("[C-c i  ] Insert link" . (lambda (canadidate)
                                                         (let ((note-id (org-roam-node-from-id canadidate)))
                                                           (if default
                                                               (progn
                                                                 (delete-region (region-beginning) (region-end))
                                                                 (insert
                                                                  (format
                                                                   "[[id:%s][%s]]"
                                                                   (org-roam-node-id note-id)
                                                                   default)))
                                                             (insert
                                                              (format
                                                               "[[id:%s][%s]]"
                                                               (org-roam-node-id note-id)
                                                               (org-roam-node-title note-id)))))))

                            ("[C-c M-i] Insert links with transclusions" . (lambda (x)
                                                                             (let ((note (helm-marked-candidates)))
                                                                               (cl-loop for n in note
                                                                                        do (--> n
                                                                                                (let ((note-id (org-roam-node-from-id n)))
                                                                                                  (insert
                                                                                                   (format
                                                                                                    "#+transclude: [[id:%s][%s]] :only-contents\n\n"
                                                                                                    (org-roam-node-id note-id)
                                                                                                    (org-roam-node-title note-id)))))))))

                            ("[C-c M-I] Insert as transclusion exclude headline" . (lambda (x)
                                                                                     (let ((note (helm-marked-candidates)))
                                                                                       (cl-loop for n in note
                                                                                                do (--> n
                                                                                                        (let ((note-id (org-roam-node-from-id n)))
                                                                                                          (insert
                                                                                                           (format
                                                                                                            "#+transclude: [[id:%s][%s]] :only-contents :exclude-elements \"headline\"\n\n"
                                                                                                            (org-roam-node-id note-id)
                                                                                                            (org-roam-node-title note-id)))))))))))))

    (setq final-node-id (car roam-walk-stack)))

  (if my-new-candidates
      nil
    (org-id-goto final-node-id))

  (if roam-visit-immediately
      (org-id-goto roam-visit-immediately))

  (if (and node-does-not-have-any-head-flag
           (eq (length roam-walk-stack) 1))
      (org-id-goto my-id)))

(defun fast/org-roam-node-random ()
  "Jump into a random node but very fast."
  (interactive)
  (org-roam-node-visit (org-roam-node-from-id
                        (seq-random-elt
                         (mapcar 'car (mapcar 'cdr (node-candidates))))) nil))

(defun node-candidates ()
  "Returns candidates for the org-roam nodes."
  (cl-loop for cand in (org-roam-db-query
                 "SELECT
    id,
    aliases as aliases,
    Null as Col2
  FROM
    (
    SELECT
      nodes.id as id,
      aliases.alias as aliases
    FROM nodes
    LEFT JOIN aliases ON aliases.node_id = nodes.id
    GROUP BY nodes.id, aliases.alias
)
UNION ALL
SELECT
  id,
  title,
  '(' || group_concat(tags, ' ') || ')' as tags
FROM
  (
  SELECT
    id,
    title,
    tags
  FROM
    (
    SELECT
      nodes.id as id,
      nodes.title as title,
      tags.tag as tags
    FROM nodes
    LEFT JOIN tags ON tags.node_id = nodes.id
    LEFT JOIN aliases ON aliases.node_id = nodes.id
    GROUP BY nodes.id, tags.tag )
  GROUP BY id, tags )
GROUP BY id
")
        collect (cons (if (nth 2 cand)
                            (format "%s   #%s"
                                    (truncate-string-to-width (nth 1 cand) 80 nil ?\s "…")
                                    (mapconcat 'identity (nth 2 cand) " #"))
                          (format "%s"
                                  (truncate-string-to-width (or (nth 1 cand) "") 80 nil ?\s "…")))
                      cand)))


;;;; tools
(defun my-org-day-of-week ()
  (org-day-of-week (nth 3 (decode-time))
                   (nth 4 (decode-time))
                   (nth 5 (decode-time))))

(defun wr/jump-into-workout-log (org-id)
  (interactive)
  (org-id-goto org-id)
  (org-narrow-to-subtree)
  (org-columns)
  (org-overview)
  (show-children)
  (move-beginning-of-line-or-indentation)
  (re-search-forward (concat "-D" (number-to-string (my-org-day-of-week))) nil t)
  (move-beginning-of-line-or-indentation)
  (org-show-entry))

;;;; main functions
(defun helm-org-roam (&optional default ini candidates)
  "Original see from a blog post by Andrea:
<https://ag91.github.io/blog/2022/02/05/an-helm-source-for-org-roam-v2/>

The code was inspired by the code https://github.com/ag91/escalator.
I add a function to insert multiply notes. Then, those notes can be
transcluded into the current buffer.

After using Org-Roam extensively, the speed for query is too
slow. So, I had to do a customized work to use Org-Roam.

The bottleneck is found to be `(org-roam-list-files)`. So I tested two things:

1. query the db using the default function
2. customized my own version, which is very basic information

The details can be found in a GitHub issue discussion
<https://github.com/org-roam/org-roam/issues/2133>. The first one
is the default query. I found the speed was too slow for just
using it. The latter just queries the titles/alias/tags, so it is
very fast.

;; → run time: 5.713161 seconds
;; → run time: 0.130336 seconds
"
  (interactive)
  (let ((default (when (use-region-p)
                   (buffer-substring-no-properties
                    (region-beginning) (region-end)))))
    (setq roam-walk-stack nil)
    (setq roam-visit-immediately nil)
    (setq node-does-not-have-any-head-flag nil)
    (helm
     :input (or default ini)
     :sources (list
               (helm-build-sync-source "Roam: "
                 :must-match nil
                 :fuzzy-match nil
                 :keymap roam-with-helm-map
                 :candidates #'node-candidates
                 :action
                 '(("Helm-org-roam-node-walk" . (lambda (canadidate)
                                                  (setq previous-node-title helm-pattern)
                                                  (helm-org-roam-node-walk (nth 0 canadidate))))

                   ("Find File" . (lambda (canadidate)
                                    (org-roam-node-visit
                                     (org-roam-node-from-id
                                      (nth 0 canadidate))
                                     nil)))

                   ("Capture as a child" . (lambda (canadidate)
                                             (org-roam-capture-
                                              :templates '(("v" "Test before 1st head" entry
                                                            "* %?\n:PROPERTIES:\n:ID: %(org-id-uuid)\n:END:\n"
                                                            :target (dynamic-node title-or-id)
                                                            ))
                                              :node (org-roam-node-from-id (nth 0 canadidate))
                                              :props '(:immediate-finish nil))))

                   ("Add alias" . (lambda (canadidate)
                                    (let ((node (org-roam-node-from-id
                                                 (nth 0 canadidate))))
                                      (org-roam-node-visit node nil)
                                      (save-excursion
                                        (goto-char (org-roam-node-point node))
                                        (let ((x))
                                          (org-roam-property-add "ROAM_ALIASES" (read-from-minibuffer "What ALIAS?")))))))

                   ("[C-c i  ] Insert link" . (lambda (canadidate)
                                      (let ((note-id (org-roam-node-from-id (nth 0 canadidate))))
                                        (if default
                                            (progn
                                              (delete-region (region-beginning) (region-end))
                                              (insert
                                               (format
                                                "[[id:%s][%s]]"
                                                (org-roam-node-id note-id)
                                                default)))
                                          (insert
                                           (format
                                            "[[id:%s][%s]]"
                                            (org-roam-node-id note-id)
                                            (org-roam-node-title note-id)))))))

                   ("[NaN   ] Insert link using ALIAS" . (lambda (canadidate)
                                                           (let ((note-id (org-roam-node-from-id (nth 0 canadidate))))
                                                             (insert
                                                                (format
                                                                 "[[id:%s][%s]]"
                                                                 (org-roam-node-id note-id)
                                                                 (nth 1 canadidate))))))

                   ("[No KBD  ] Insert workout link" . (lambda (canadidate)
                                      (let ((note-id (org-roam-node-from-id (nth 0 canadidate))))
                                        (insert
                                           (format
                                            "[[elisp:(wr/jump-into-workout-log \"%s\")][Training daily: %s]]"
                                            (org-roam-node-id note-id)
                                            (org-roam-node-title note-id))))))

                   ("[C-c M-i] Insert links with transclusions" . (lambda (x)
                                                          (let ((note (helm-marked-candidates)))
                                                            (cl-loop for n in note
                                                                     do (--> n
                                                                             (let ((note-id (org-roam-node-from-id (nth 0 n))))
                                                                               (insert
                                                                                (format
                                                                                 "#+transclude: [[id:%s][%s]] :only-contents\n\n"
                                                                                 (org-roam-node-id note-id)
                                                                                 (org-roam-node-title note-id)))))))))

                   ("[C-c M-I] Insert as transclusion exclude headline" . (lambda (x)
                                                          (let ((note (helm-marked-candidates)))
                                                            (cl-loop for n in note
                                                                     do (--> n
                                                                             (let ((note-id (org-roam-node-from-id (nth 0 n))))
                                                                               (insert
                                                                                (format
                                                                                 "#+transclude: [[id:%s][%s]] :only-contents :exclude-elements \"headline\"\n\n"
                                                                                 (org-roam-node-id note-id)
                                                                                 (org-roam-node-title note-id)))))))))

                   ;; Thank Dustin Lacewell for inspiration.
                   ("Helm-org-walk" . (lambda (canadidate)
                                        (save-excursion (helm-org-walk
                                         (org-roam-node-file (org-roam-node-from-id
                                      (nth 0 canadidate)))))))))

               (helm-build-dummy-source
                   "Create note"
                 :action '(("Capture note" . (lambda (candidate)
                                               (org-roam-capture-
                                                :node (org-roam-node-create :title candidate)
                                                :props '(:finalize find-file))))))

               (helm-build-dummy-source
                   "Search on Net"
                 :action '(("Open" . (lambda (candidate)
                                       (browse-url (concat "https://encrypted.google.com/search?ie=UTF-8&oe=UTF-8&q=" candidate))))))

               (helm-build-dummy-source
                   "Search on G-scholar"
                 :action '(("Open" . (lambda (candidate)
                                       (browse-url (concat "https://scholar.google.ca/scholar?hl=en&q=" candidate))))))))))


;;;; recursion
;;
(defun helm-org-node-walk--test ()
  "It returns back to the intial query."
  (interactive)
  (helm-run-after-exit
   (lambda () (helm
               :input previous-node-title
               :sources (list
                         (helm-build-sync-source "Roam: "
                           :must-match nil
                           :fuzzy-match nil
                           :candidates #'node-candidates
                           :keymap roam-with-helm-map
                           :action
                           '(("Helm-org-roam-node-walk" . (lambda (canadidate)
                                                            (setq previous-node-title helm-pattern)
                                                            (helm-org-roam-node-walk (nth 0 canadidate))))

                             ("Find File" . (lambda (canadidate)
                                              (org-roam-node-visit
                                               (org-roam-node-from-id
                                                (nth 0 canadidate))
                                               nil)))

                             ("Capture as a child" . (lambda (canadidate)
                                                       (org-roam-capture-
                                                        :templates '(("v" "Test before 1st head" entry
                                                                      "* %?\n:PROPERTIES:\n:ID: %(org-id-uuid)\n:END:\n"
                                                                      :target (dynamic-node title-or-id)
                                                                      ))
                                                        :node (org-roam-node-from-id (nth 0 canadidate))
                                                        :props '(:immediate-finish nil))))

                             ("Add alias" . (lambda (canadidate)
                                              (let ((node (org-roam-node-from-id
                                                           (nth 0 canadidate))))
                                                (org-roam-node-visit node nil)
                                                (save-excursion
                                                  (goto-char (org-roam-node-point node))
                                                  (let ((x))
                                                    (org-roam-property-add "ROAM_ALIASES" (read-from-minibuffer "What ALIAS?")))))))

                             ("[C-c i  ] Insert link" . (lambda (canadidate)
                                                (let ((note-id (org-roam-node-from-id (nth 0 canadidate))))
                                                  (if default
                                                      (progn
                                                        (delete-region (region-beginning) (region-end))
                                                        (insert
                                                         (format
                                                          "[[id:%s][%s]]"
                                                          (org-roam-node-id note-id)
                                                          default)))
                                                    (insert
                                                     (format
                                                      "[[id:%s][%s]]"
                                                      (org-roam-node-id note-id)
                                                      (org-roam-node-title note-id)))))))

                             ("[C-c M-i] Insert links with transclusions" . (lambda (x)
                                                                    (let ((note (helm-marked-candidates)))
                                                                      (cl-loop for n in note
                                                                               do (--> n
                                                                                       (let ((note-id (org-roam-node-from-id (nth 0 n))))
                                                                                         (insert
                                                                                          (format
                                                                                           "#+transclude: [[id:%s][%s]] :only-contents\n\n"
                                                                                           (org-roam-node-id note-id)
                                                                                           (org-roam-node-title note-id)))))))))

                             ("[C-c M-I] Insert as transclusion exclude headline" . (lambda (x)
                                                                            (let ((note (helm-marked-candidates)))
                                                                              (cl-loop for n in note
                                                                                       do (--> n
                                                                                               (let ((note-id (org-roam-node-from-id (nth 0 n))))
                                                                                                 (insert
                                                                                                  (format
                                                                                                   "#+transclude: [[id:%s][%s]] :only-contents :exclude-elements \"headline\"\n\n"
                                                                                                   (org-roam-node-id note-id)
                                                                                                   (org-roam-node-title note-id)))))))))

                             ;; Thank Dustin Lacewell for inspiration.
                             ("Helm-org-walk" . (lambda (canadidate)
                                                  (save-excursion (helm-org-walk
                                                                   (org-roam-node-file (org-roam-node-from-id
                                                                                        (nth 0 canadidate)))))))))

                         (helm-build-dummy-source
                             "Create note"
                           :action '(("Capture note" . (lambda (candidate)
                                                         (org-roam-capture-
                                                          :node (org-roam-node-create :title candidate)
                                                          :props '(:finalize find-file))))))

                         (helm-build-dummy-source
                             "Search on Net"
                           :action '(("Open" . (lambda (candidate)
                                                 (browse-url (concat "https://encrypted.google.com/search?ie=UTF-8&oe=UTF-8&q=" candidate))))))

                         (helm-build-dummy-source
                             "Search on G-scholar"
                           :action '(("Open" . (lambda (candidate)
                                                 (browse-url (concat "https://scholar.google.ca/scholar?hl=en&q=" candidate))))))


                         )))))

(defun helm-org-node-walk--new ()
  "Function to process <Walk back to previous node>."
  (interactive)
  (helm-run-after-exit
   (lambda ()
     (helm
     :input (org-roam-node-title (org-roam-node-from-id (car roam-walk-stack)))
     :sources (list
               (helm-build-sync-source "Roam: "
                 :must-match nil
                 :fuzzy-match nil
                 :keymap roam-with-helm-map
                 :candidates #'node-candidates
                 :action
                 '(("Helm-org-roam-node-walk" . (lambda (canadidate)
                                                  (setq previous-node-title helm-pattern)
                                                  (helm-org-roam-node-walk (nth 0 canadidate))))

                   ("Find File" . (lambda (canadidate)
                                    (org-roam-node-visit
                                     (org-roam-node-from-id
                                      (nth 0 canadidate))
                                     nil)))

                   ("Capture as a child" . (lambda (canadidate)
                                             (org-roam-capture-
                                              :templates '(("v" "Test before 1st head" entry
                                                            "* %?\n:PROPERTIES:\n:ID: %(org-id-uuid)\n:END:\n"
                                                            :target (dynamic-node title-or-id)
                                                            ))
                                              :node (org-roam-node-from-id (nth 0 canadidate))
                                              :props '(:immediate-finish nil))))

                   ("Add alias" . (lambda (canadidate)
                                    (let ((node (org-roam-node-from-id
                                                 (nth 0 canadidate))))
                                      (org-roam-node-visit node nil)
                                      (save-excursion
                                        (goto-char (org-roam-node-point node))
                                        (let ((x))
                                          (org-roam-property-add "ROAM_ALIASES" (read-from-minibuffer "What ALIAS?")))))))

                   ("[C-c i  ] Insert link" . (lambda (canadidate)
                                      (let ((note-id (org-roam-node-from-id (nth 0 canadidate))))
                                        (if default
                                            (progn
                                              (delete-region (region-beginning) (region-end))
                                              (insert
                                               (format
                                                "[[id:%s][%s]]"
                                                (org-roam-node-id note-id)
                                                default)))
                                          (insert
                                           (format
                                            "[[id:%s][%s]]"
                                            (org-roam-node-id note-id)
                                            (org-roam-node-title note-id)))))))

                   ("[C-c M-i] Insert links with transclusions" . (lambda (x)
                                                          (let ((note (helm-marked-candidates)))
                                                            (cl-loop for n in note
                                                                     do (--> n
                                                                             (let ((note-id (org-roam-node-from-id (nth 0 n))))
                                                                               (insert
                                                                                (format
                                                                                 "#+transclude: [[id:%s][%s]] :only-contents\n\n"
                                                                                 (org-roam-node-id note-id)
                                                                                 (org-roam-node-title note-id)))))))))

                   ("[C-c M-I] Insert as transclusion exclude headline" . (lambda (x)
                                                          (let ((note (helm-marked-candidates)))
                                                            (cl-loop for n in note
                                                                     do (--> n
                                                                             (let ((note-id (org-roam-node-from-id (nth 0 n))))
                                                                               (insert
                                                                                (format
                                                                                 "#+transclude: [[id:%s][%s]] :only-contents :exclude-elements \"headline\"\n\n"
                                                                                 (org-roam-node-id note-id)
                                                                                 (org-roam-node-title note-id)))))))))

                   ;; Thank Dustin Lacewell for inspiration.
                   ("Helm-org-walk" . (lambda (canadidate)
                                        (save-excursion (helm-org-walk
                                         (org-roam-node-file (org-roam-node-from-id
                                      (nth 0 canadidate)))))))))

               (helm-build-dummy-source
                   "Create note"
                 :action '(("Capture note" . (lambda (candidate)
                                               (org-roam-capture-
                                                :node (org-roam-node-create :title candidate)
                                                :props '(:finalize find-file))))))

               (helm-build-dummy-source
                   "Search on Net"
                 :action '(("Open" . (lambda (candidate)
                                       (browse-url (concat "https://encrypted.google.com/search?ie=UTF-8&oe=UTF-8&q=" candidate))))))

               (helm-build-dummy-source
                   "Search on G-scholar"
                 :action '(("Open" . (lambda (candidate)
                                       (browse-url (concat "https://scholar.google.ca/scholar?hl=en&q=" candidate)))))))))))


(defun call-find-file ()
  "Thanks to xuchunyang:
<https://emacs.stackexchange.com/a/57734>"
  (interactive)
  (helm-select-nth-action 1))

(defun call-find-file-in-walking ()
  ""
  (interactive)
  (helm-select-nth-action 1))

(defun call-add-as-link ()
  ""
  (interactive)
  (helm-select-nth-action 4))

(defun call-add-as-transclusion ()
  ""
  (interactive)
  (helm-select-nth-action 5))

(defun call-add-as-transclusion-only-head ()
  ""
  (interactive)
  (helm-select-nth-action 6))


(defun call-add-as-link-in-walking ()
  ""
  (interactive)
  (helm-select-nth-action 2))

(defun call-add-as-transclusion-in-walking ()
  ""
  (interactive)
  (helm-select-nth-action 3))

(defun call-add-as-transclusion-only-head-in-walking ()
  ""
  (interactive)
  (helm-select-nth-action 4))


;;;; kbd
(setq helm-org-node-walk-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "C-<backspace>") 'helm-org-node-walk--new)
    (define-key map (kbd "C-<return>") 'call-find-file-in-walking)
    (define-key map (kbd "C-c i") 'call-add-as-link-in-walking)
    (define-key map (kbd "C-c M-i") 'call-add-as-transclusion-in-walking)
    (define-key map (kbd "C-c M-I") 'call-add-as-transclusion-only-head-in-walking)
    map))

(setq roam-with-helm-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "C-<return>") 'call-find-file)
    (define-key map (kbd "C-<backspace>") 'helm-org-node-walk--test)
    (define-key map (kbd "C-c i") 'call-add-as-link)
    (define-key map (kbd "C-c M-i") 'call-add-as-transclusion)
    (define-key map (kbd "C-c M-I") 'call-add-as-transclusion-only-head)
    (define-key map (kbd "<RET>") 'helm-maybe-exit-minibuffer)
    map))


(provide 'roam-with-helm-v2)
