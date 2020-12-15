(setq org-roam-completion-system 'helm)

(cl-defun org-roam-completion--completing-read (prompt choices &key
                                                       require-match initial-input
                                                       action)
  (let (res)
    (setq res
          (cond
           ((eq org-roam-completion-system 'helm)
            (unless (and (fboundp 'helm)
                         (fboundp 'helm-make-source))
              (user-error "Please install helm from \
https://github.com/emacs-helm/helm"))
            (let ((source (helm-make-source prompt 'helm-source-sync
                            :candidates (mapcar #'car choices)
                            :action (lambda (_candidate)
                                      (let ((note (helm-marked-candidates :with-wildcard t)))
                                        (if (> (length note) 1)
                                            (cl-loop for n in note
                                                     do (roam-with-helm--get-path-from-title n))
                                          (org-roam-find-file _candidate nil nil t)
                                          )
                                        ))
                            :filtered-candidate-transformer
                            (and (not require-match)
                                 #'org-roam-completion--helm-candidate-transformer)))
                  (buf (concat "*org-roam "
                               (s-downcase (s-chop-suffix ":" (s-trim prompt)))
                               "*")))
              (or (helm :sources source
                        :prompt prompt
                        :input initial-input
                        :buffer buf)
                  (keyboard-quit))))))
    (if action
        (funcall action res)
      res)))

(defun roam-with-helm--get-path-from-title (title)
  (cl-loop for ns in (mapcar #'cdr (org-roam--get-title-path-completions))
           do (pcase ns
                (`(:path ,foo :title ,bar) (if (string-match title bar)
                                               (insert (format "#+transclude: t\n[[file:%s][%s]]\n\n" (roam-with-helm--return-path-relative-to-home foo) bar)))))))

(defun roam-with-helm--return-path-relative-to-home (full-path)
  (concat "~/"(string-remove-prefix (file-truename "~/") full-path)))

(provide 'roam-with-helm)
