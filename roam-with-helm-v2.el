;;; roam-with-helm --- A source for navigation Org-roam  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Ran Wang

;; Author: Ran Wang
;; URL: https://github.com/randomwangran/roam-with-helm
;; Version: 0.0.1
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

(defun fast/org-roam-node-random ()
  "Jump into a random node but very fast."
  (interactive)
  (org-roam-node-visit (org-roam-node-from-id
                        (seq-random-elt
                         (mapcar 'car (mapcar 'cdr (node-candidates))))) nil))

(defun node-candidates ()
  "Returns candidates for the org-roam nodes."
  (loop for cand in (org-roam-db-query
                 "SELECT
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
UNION ALL
SELECT
  id,
  aliases,
  Null as Col2
FROM
  (
  SELECT
    id,
    aliases as aliases
  FROM
    (
    SELECT
      nodes.id as id,
      aliases.alias as aliases
    FROM nodes
    LEFT JOIN aliases ON aliases.node_id = nodes.id
    GROUP BY nodes.id, aliases.alias )
  GROUP BY id )
GROUP BY id
")
        collect (cons (if (nth 2 cand)
                            (format "%s   #%s"
                                    (nth 1 cand)
                                    (mapconcat 'identity (nth 2 cand) "#"))
                          (format "%s"
                                  (nth 1 cand)))
                      cand)))

(defun helm-org-roam (&optional input candidates)
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

The drawback is it lacks some features. For example, tags and
formatting stuff. When I saw you have 5 seconds issue, I thought that you
might have similar issues."
  (interactive)
  (let ((default (when (use-region-p)
                     (buffer-substring-no-properties
                      (region-beginning) (region-end)))))
    (helm
     :input default
     :sources (list
               (helm-build-sync-source "Roam: "
                 :must-match nil
                 :fuzzy-match nil
                 :candidates #'node-candidates
                 :action
                 '(("Find File" . (lambda (canadidate)
                                    (org-roam-node-visit
                                     (org-roam-node-from-id
                                      (nth 0 canadidate))
                                     nil)))

                   ("Insert link" . (lambda (canadidate)
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

                   ("Insert links with transclusions" . (lambda (x)
                                                          (let ((note (helm-marked-candidates)))
                                                            (cl-loop for n in note
                                                                     do (--> n
                                                                             (let ((note-id (org-roam-node-from-id (nth 0 n))))
                                                                               (insert
                                                                                (format
                                                                                 "#+transclude: [[id:%s][%s]] :only-contents\n\n"
                                                                                 (org-roam-node-id note-id)
                                                                                 (org-roam-node-title note-id)))))))))))
               (helm-build-dummy-source
                   "Create note"
                 :action '(("Capture note" . (lambda (candidate)
                                               (org-roam-capture-
                                                :node (org-roam-node-create :title candidate)
                                                :props '(:finalize find-file))))))))))

(provide 'roam-with-helm-v2)
