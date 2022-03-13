;;;;
(defun node-candidates ()
  "Returns candidates for the org-roam nodes."
  (loop for cand in (org-roam-db-query
                     [:select :distinct [title id node-id tag]
                              :from nodes
                              :left-join tags
                              :on (= tags:node-id nodes:id)])
        collect (cons (format "%s#%s"
                              (nth 0 cand)    ;; title 0, id 1
                                              ;; node-id,2
                                              ;; tag,3
                              (nth 3 cand))
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
               :action
               '(("Find File" . (lambda (canadidate)
                                  (org-roam-node-visit
                                   (org-roam-node-from-title-or-alias
                                    (nth 0 canadidate))
                                   nil)))
                 ("Insert link" . (lambda (canadidate)
                                    (let ((note-id (org-roam-node-from-title-or-alias (nth 0 canadidate))))
                                      (insert
                                       (format
                                        "[[id:%s][%s]]"
                                        (org-roam-node-id note-id)
                                        (org-roam-node-title note-id))))))

                 ("Insert links with transclusions" . (lambda (x)
                                                        (let ((note (helm-marked-candidates)))
                                                          (cl-loop for n in note
                                                                   do (--> n
                                                                           (let ((note-id (org-roam-node-from-title-or-alias (nth 0 n))))
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
