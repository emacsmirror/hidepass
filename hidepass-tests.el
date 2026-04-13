;;; hidepass-tests.el --- test  -*- lexical-binding: t -*-
;; (eval-buffer)
;; (ert t)


(ert-deftest hidepass-test-single-line-mask ()
  (with-temp-buffer
    (let ((hidepass-patterns '("[pP]ass:? \\(.+\\)$"
                               "[lL]ogin:? \\(.+\\)$"
                               "[tT]oken:? \\(.+\\)$")))
      (insert "password: secret123\nPass: topsecret\nlogin: user\nno secret here")
      (hidepass-mode 1)
      ;; Check each line for proper masking property
      (goto-char (point-min))

      ;; (text-properties-at (line-end-position))))

      ;; For lines with password, check display property exists and matches mask
      (end-of-line)
      (backward-char)
      (should (equal (get-text-property (point) 'display) nil))
      (forward-line 1)
      (end-of-line)
      (backward-char)
      (should (equal (get-text-property (point) 'display) hidepass-mask))
      (forward-line 1)
      (end-of-line)
      (backward-char)
      (should (equal (get-text-property (point) 'display) hidepass-mask))
      (forward-line 1)
      (end-of-line)
      (backward-char)
      ;; For a line with no secret, should NOT be masked
      (should-not (equal (get-text-property (point) 'display) hidepass-mask)))))


(ert-deftest hidepass-multiline-masking1 ()
  "Should mask content between multiline block patterns."
  (with-temp-buffer
    (let ((hidepass-multiline-patterns '(("<secret>" . "</secret>"))))
      (insert "<secret>\npassword123\nanotherpw\n</secret>\nnotsecret")
      (hidepass-mode 1)
      ;; Check that everything between <secret> and </secret> is masked
      (goto-char (point-min))
      (search-forward "<secret>")
      (forward-line 1)
      ;; Check masking for multiline lines
      (dotimes (_ 2)
        (let* ((start (point))
               (end (progn (forward-line 1) (point)))
               (text (buffer-substring start end)))
          (should (eq (car (get-text-property 0 'display text)) hidepass-mask)))))
      ;; Check unmasked outside block
      (search-forward "\nnotsecret")
      (let ((text (buffer-substring (point) (progn (forward-line 1) (point)))))
        (should-not (eq (get-text-property 0 'display text) hidepass-mask)))))

(ert-deftest hidepass-test-custom-mask ()
  (let ((hidepass-mask "# MASKED #")
        (hidepass-patterns '("[lL]ogin:? \\(.+\\)$"
                             "[tT]oken:? \\(.+\\)$")))
    (with-temp-buffer
      (insert "token: apikey\nlogin: user\n")
      (hidepass-mode 1)
      (goto-char (point-min))
      (search-forward "apikey")
      (backward-char)
      ;; (get-text-property (point) 'display)))
      (should (equal (get-text-property (point) 'display) "# MASKED #"))
      (forward-line 1)
      (search-forward "user")
      (backward-char)
      (should (equal (get-text-property (point) 'display) "# MASKED #")))))

(ert-deftest hidepass-test-hide-first-line ()
  (with-temp-buffer
    (let ((hidepass-hide-first-line t))
      (insert "Just some intro line\npassword: secret")
      (hidepass-mode 1)
      (goto-char (point-min))
      ;; First line should be masked
      (should (equal (get-text-property (point) 'display) hidepass-mask)))))


(ert-deftest hidepass-test-multiline-mask-copy-original ()
    (with-temp-buffer
      (let ((hidepass-multiline-patterns '(("<secret>" . "</secret>"))))
        (insert "<secret>\nvery\nsecret\nstuff\n</secret>\n")
        (hidepass-mode 1)
        (goto-char (point-min))
        ;; Select region and copy, should not be masked in kill-ring
        (search-forward "<secret>")
        (let ((block-start (point)))
          (search-forward "</secret>")
          (let ((block-end (match-beginning 0)))
            (let ((copied (buffer-substring block-start block-end)))
              (should (string-match "secret" copied))))))))

(ert-deftest hidepass-multiline-masking2 ()
  "Should mask content between multiline password block delimiters."
  (with-temp-buffer
    ;; Insert an example buffer with two multiline blocks and normal text
    (insert "normal text\n<secret>\nmy super password\nanother secret\n</secret>\nnot secret\n---BEGIN PASSWORD---\nfoo\nbar\n---END PASSWORD---\n")
    ;; Set up multiline patterns matching <secret>..</secret> and ---BEGIN PASSWORD---..---END PASSWORD---
    (let ((hidepass-multiline-patterns
          '(("<secret>" . "</secret>")
            ("---BEGIN PASSWORD---" . "---END PASSWORD---"))))
      (hidepass-mode 1)
      ;; Check inside <secret> block
      (goto-char (point-min))
      (search-forward "my super password")
      (backward-char)
      (let ((props (get-text-property (point) 'display)))
        (should (string= (car props) hidepass-mask)))
      (search-forward "another secret")
      (backward-char)
      (let ((props (get-text-property (point) 'display)))
        (should (string= (car props) hidepass-mask)))
      ;; Check delimiter lines are NOT masked
      (goto-char (point-min))
      (search-forward "<secret>")
      (backward-char)
      (let ((props (get-text-property (point) 'display)))
        (should-not (string= (car props) hidepass-mask)))
      (search-forward "</secret>")
      (backward-char)
      (let ((props (get-text-property (point) 'display)))
        (should-not (string= (car props) hidepass-mask)))
      ;; Check content in second block (---BEGIN PASSWORD---..---END PASSWORD---)
      (goto-char (point-min))
      (search-forward "foo")
      (backward-char)
      (let ((props (get-text-property (point) 'display)))
        (should (string= (car props) hidepass-mask)))
      (search-forward "bar")
      (backward-char)
      (let ((props (get-text-property (point) 'display)))
        (should (string= (car props) hidepass-mask)))
      ;; Delimiter not masked
      (goto-char (point-min))
      (search-forward "---BEGIN PASSWORD---")
      (backward-char)
      (let ((props (get-text-property (point) 'display)))
        (should-not (string= (car props) hidepass-mask)))
      (search-forward "---END PASSWORD---")
      (backward-char)
      (let ((props (get-text-property (point) 'display)))
        (should-not (string= (car props) hidepass-mask))))))


(ert-deftest hidepass-multiline-masking3 ()
  "Should mask content between multiline password block delimiters."
  (with-temp-buffer
    (let ((hidepass-multiline-patterns
           '(("<secret>" . "</secret>")
             ("^---BEGIN PASSWORD---$" . "^---END PASSWORD---$"))))
      (insert "normal text\n<secret>\nmy super password\nanother secret\n</secret>\nnot secret\n---BEGIN PASSWORD---\nfoo\nbar\n---END PASSWORD---\n")
      (hidepass-mode 1)
      ;; Check content inside <secret> block is masked
      (goto-char (point-min))
      (search-forward "<secret>")
      (forward-line 1)
      (dotimes (_ 2)
        (let* ((start (point))
               (end (progn (forward-line 1) (point)))
               (text (buffer-substring start end)))
          (should (eq (car (get-text-property 0 'display text)) hidepass-mask))))
      ;; Check delimiter lines are NOT masked
      (goto-char (point-min))
      (search-forward "<secret>")
      (let* ((start (line-beginning-position))
             (end   (line-end-position))
             (text  (buffer-substring start end)))
        (should-not (eq (get-text-property 0 'display text) hidepass-mask)))
      (search-forward "</secret>")
      (let* ((start (line-beginning-position))
             (end   (line-end-position))
             (text  (buffer-substring start end)))
        (should-not (eq (get-text-property 0 'display text) hidepass-mask)))
      ;; Check content in second block is masked
      (goto-char (point-min))
      (search-forward "---BEGIN PASSWORD---")
      (forward-line 1)
      (dotimes (_ 2)
        (let* ((start (point))
               (end (progn (forward-line 1) (point)))
               (text (buffer-substring start end)))
          (should (eq (car (get-text-property 0 'display text)) hidepass-mask))))
      ;; Check delimiters in second block are NOT masked
      (goto-char (point-min))
      (search-forward "---BEGIN PASSWORD---")
      (let* ((start (line-beginning-position))
             (end   (line-end-position))
             (text  (buffer-substring start end)))
        (should-not (eq (get-text-property 0 'display text) hidepass-mask)))
      (search-forward "---END PASSWORD---")
      (let* ((start (line-beginning-position))
             (end   (line-end-position))
             (text  (buffer-substring start end)))
        (should-not (eq (get-text-property 0 'display text) hidepass-mask))))))

;; -=-= provide
(provide 'hidepass-tests)

;;; hidepass-tests.el ends here
