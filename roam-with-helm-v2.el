;;;;
;;  (defun candidate-contain-alias (s1 s2)
;;    (if (string-match-p "@" (car s2))
;;        nil
;;      t))

(defun node-candidates ()
  "Returns candidates for the org-roam nodes."
  (loop for cand in (org-roam-db-query
               "SELECT
  id,
  title,
  '(' || group_concat(tags, ' ') || ')' as tags,
  aliases
FROM
  (
  SELECT
    id,
    title,
    tags,
    '(' || group_concat(aliases, ' ') || ')' as aliases
  FROM
    (
    SELECT
      nodes.id as id,
      nodes.title as title,
      tags.tag as tags,
      aliases.alias as aliases
    FROM nodes
    LEFT JOIN tags ON tags.node_id = nodes.id
    LEFT JOIN aliases ON aliases.node_id = nodes.id
    GROUP BY nodes.id, tags.tag, aliases.alias )
  GROUP BY id, tags )
GROUP BY id")
        collect (cons (if (nth 3 cand)
                          (if (nth 2 cand)
                              (format "%s    @%s   #%s"
                                      (nth 1 cand)
                                      (mapconcat 'identity (nth 3 cand) "@")
                                      (mapconcat 'identity (nth 2 cand) "#"))
                            (format "%s   @%s"
                                    (nth 1 cand)
                                    (mapconcat 'identity (nth 3 cand) "@")))
                        (if (nth 2 cand)
                            (format "%s   #%s"
                                    (nth 1 cand)
                                    (mapconcat 'identity (nth 2 cand) "#"))
                          (format "%s"
                                  (nth 1 cand)
                                  (mapconcat 'identity (nth 3 cand) "@"))))
                      cand)))

(defun helm-org-roam (&optional input candidates)
  "Original see from a blog post by Andrea:
https://ag91.github.io/blog/2022/02/05/an-helm-source-for-org-roam-v2/

The code is almost copied from https://github.com/ag91/escalator.

I add a function to insert multiply notes. Then, those notes are
hard-coded to be transcluded into the current buffer.
"
  (interactive)
  (require 'helm-mode)
  (require 'org-roam)
  (helm
   :input input
   :sources (list
             (helm-build-sync-source "Roam: "
               :must-match nil
               :fuzzy-match t
               :candidates #'node-candidates

                                        ;:filtered-candidate-transformer (-sort my-h-sort-fn candidates)
                                        ; maybe filter at building candidates?
               :action
               '(("Find File" . (lambda (canadidate)
                                  (org-roam-node-visit
                                   (org-roam-node-from-title-or-alias
                                    (nth 1 canadidate))
                                   nil)))

                 ("Insert link" . (lambda (canadidate)
                                    (let ((note-id (org-roam-node-from-title-or-alias (nth 1 canadidate))))
                                      (insert
                                       (format
                                        "[[id:%s][%s]]"
                                        (org-roam-node-id note-id)
                                        (org-roam-node-title note-id))))))

                 ("Insert links with transclusions" . (lambda (x)
                                                        (let ((note (helm-marked-candidates)))
                                                          (cl-loop for n in note
                                                                   do (--> n
                                                                           (let ((note-id (org-roam-node-from-title-or-alias (nth 1 n))))
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
                                              :props '(:finalize find-file)))))))))

(provide 'roam-with-helm-v2)
