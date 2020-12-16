(setq roam-with-helm-org-roam-completion-system 'helm+)

(defun roam-with-helm-org-roam-find-file (&optional initial-prompt completions filter-fn no-confirm)
  (interactive)
  (unless org-roam-mode (org-roam-mode))
  (let* ((completions (funcall (or filter-fn #'identity)
                               (or completions (org-roam--get-title-path-completions))))
         (title-with-tags (if no-confirm
                              initial-prompt
                            (+org-roam-completion--completing-read "File: " completions
                                                                  :initial-input initial-prompt)))
         (res (cdr (assoc title-with-tags completions)))
         (file-path (plist-get res :path)))
    (if file-path
        (org-roam--find-file file-path)
      (let ((org-roam-capture--info `((title . ,title-with-tags)
                                      (slug  . ,(funcall org-roam-title-to-slug-function title-with-tags))))
            (org-roam-capture--context 'title))
        (setq org-roam-capture-additional-template-props (list :finalize 'find-file))
        (org-roam-capture--capture)))))

(cl-defun +org-roam-completion--completing-read (prompt choices &key
                                                       require-match initial-input
                                                       action)
  (let (res)
    (setq res
          (cond
           ((eq roam-with-helm-org-roam-completion-system 'helm+)
            (unless (and (fboundp 'helm)
                         (fboundp 'helm-make-source))
              (user-error "Please install helm from \
https://github.com/emacs-helm/helm"))
            (let ((source (helm-make-source prompt 'helm-source-sync
                            :candidates (mapcar #'car choices)
                            :action
                            (lambda (_candidate)
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
