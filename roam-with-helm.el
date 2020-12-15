(setq org-roam-completion-system 'helm)

(cl-defun org-roam-completion--completing-read (prompt choices &key
                                                       require-match initial-input
                                                       action)
  "Present a PROMPT with CHOICES and optional INITIAL-INPUT.
If REQUIRE-MATCH is t, the user must select one of the CHOICES.
Return user choice."
  (let (res)
    (setq res
          (cond
           ((eq org-roam-completion-system 'ido)
            (let ((candidates (mapcar #'car choices)))
              (ido-completing-read prompt candidates nil require-match initial-input)))
           ((eq org-roam-completion-system 'default)
            (completing-read prompt choices nil require-match initial-input))
           ((eq org-roam-completion-system 'ivy)
            (if (fboundp 'ivy-read)
                (ivy-read prompt choices
                          :initial-input initial-input
                          :require-match require-match
                          :action (prog1 action
                                    (setq action nil))
                          :caller 'org-roam--completing-read)
              (user-error "Please install ivy from \
https://github.com/abo-abo/swiper")))
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
                                                     do (wr/get-path-from-title n))
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

(defun wr/get-path-from-title (title)
  (cl-loop for ns in (mapcar #'cdr (org-roam--get-title-path-completions))
           do (pcase ns
                (`(:path ,foo :title ,bar) (if (string-match title bar)
                                               (insert (format "#+transclude: t\n[[file:%s][%s]]\n\n" (wr/return-path-relative-to-home foo) bar)))))))

(defun wr/return-path-relative-to-home (full-path)
  (concat "~/"(string-remove-prefix (file-truename "~/") full-path)))
