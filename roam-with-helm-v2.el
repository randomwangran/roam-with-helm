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
               :candidates (or candidates (org-roam--get-titles))
               :action
               '(("Find File" . (lambda (x)
                                  (--> x
                                       org-roam-node-from-title-or-alias
                                       (org-roam-node-visit it nil))))
                 ("Insert link" . (lambda (x)
                                    (--> x
                                         org-roam-node-from-title-or-alias
                                         (insert
                                          (format
                                           "[[id:%s][%s]]"
                                           (org-roam-node-id it)
                                           (org-roam-node-title it))))))
                 ("Insert links with transclusions" . (lambda (x)
                                       (let ((note (helm-marked-candidates :with-wildcard t)))
                                         (cl-loop for n in note
                                                  do (--> n
                                                          org-roam-node-from-title-or-alias
                                                          (insert
                                                           (format
                                                            "#+transclude: [[id:%s][%s]] :only-contents\n\n"
                                                            (org-roam-node-id it)
                                                            (org-roam-node-title it))))))))
                 ("Follow backlinks" . (lambda (x)
                                         (let ((candidates
                                                (--> x
                                                     org-roam-node-from-title-or-alias
                                                     org-roam-backlinks-get
                                                     (--map
                                                      (org-roam-node-title
                                                       (org-roam-backlink-source-node it))
                                                      it))))
                                           (helm-org-roam nil (or candidates (list x))))))))
             (helm-build-dummy-source
                 "Create note"
               :action '(("Capture note" . (lambda (candidate)
                                             (org-roam-capture-
                                              :node (org-roam-node-create :title candidate)
                                              :props '(:finalize find-file)))))))))

(provide 'roam-with-helm-v2)
