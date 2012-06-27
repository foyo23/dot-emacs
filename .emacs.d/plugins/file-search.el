;;; file-search.el 

(defconst fs-file-search-buffer-name "*file-search*")

(defvar fs-file-list-length 0)
(defvar fs-file-search-str "")
(defvar fs-match-regexp "")
(defvar fs-file-search-keymap (make-sparse-keymap))
(define-key fs-file-search-keymap (kbd "RET") 'fs-result-choose)

(defface fs-file-name-face '((t (:foreground "red" :bold t :underline t)))
  "Face used to highlight the file name")
(defface fs-line-face '((t (:foreground "green" :bold nil :underline nil)))
  "Face used to highlight the line number")

;;查找列表中所有文件
(defun fs-search-list-file (list)
  (while list
    (let ((file-name (car list)))
      (if (not (file-accessible-directory-p file-name))
          (progn
            (message "current file:%s %d in %d" 
                     file-name 
                     ( + (- fs-file-list-length (length list)) 1)
                     fs-file-list-length)
            (fs-search-file file-name 1))))
    (setq list (cdr list))))

;;查找指定目录
(defun fs-search-directory (dir-name)
  (if (file-accessible-directory-p dir-name)
      (let ((list (cdr (cdr (directory-files dir-name t fs-match-regexp)))))
        (setq fs-file-list-length (length list))
        (fs-search-list-file list))
    (message "%s is a invaild directory name" dir-name)))

;;查找指定文件
(defun fs-search-file (file-name point)
  (let ((buf (get-buffer (file-name-nondirectory file-name)))
        (bpt 0))
    (with-current-buffer (find-file-noselect file-name)
      (if buf
          (setq bpt (point)))
      (while point
        (goto-char point)
        (setq point (search-forward fs-file-search-str nil '(nil)))
        (if point
            (let ((b-pt (line-beginning-position))
                  (e-pt (line-end-position)))
              (fs-insert-result 
               (count-lines 1 point)
               file-name
               (buffer-substring b-pt e-pt)))))
      (if buf
          (goto-char bpt)
          (kill-buffer (current-buffer))))))

;;给出结果buffer
(defun fs-get-search-result-buffer()
  (get-buffer-create fs-file-search-buffer-name))
  
;;插入查找结果
(defun fs-insert-result (line file text)
  (with-current-buffer (fs-get-search-result-buffer)
      (setq buffer-read-only nil)
      (goto-char (point-max))
      (let ((bpt (point)))
        (insert (file-name-nondirectory file))
        (set-text-properties bpt (point-max) '(face fs-file-name-face))
        (put-text-property bpt (point-max) 'keymap fs-file-search-keymap)
        (put-text-property bpt (point-max) 'filename file)
        (put-text-property bpt (point-max) 'nline line))
      (goto-char (point-max))
      (let ((bpt (point)))
        (insert "(" (number-to-string line)  "): ")
        (set-text-properties bpt (point-max) '(face fs-line-face)))
      (insert text "\n")
      (setq buffer-read-only t)))


;;查找指定文件
(defun fs-search-file (file-name point)
  (let ((buf (get-buffer (file-name-nondirectory file-name)))
        (bpt 0))
    (with-current-buffer (find-file-noselect file-name)
      (if buf
          (setq bpt (point)))
      (while point
        (goto-char point)
        (setq point (search-forward fs-file-search-str nil '(nil)))
        (if point
            (let ((b-pt (line-beginning-position))
                  (e-pt (line-end-position)))
              (fs-insert-result 
               (count-lines 1 point)
               file-name
               (buffer-substring b-pt e-pt)))))
      (if buf
          (goto-char bpt)
          (kill-buffer (current-buffer))))))

;;给出结果buffer
(defun fs-get-search-result-buffer()
  (get-buffer-create fs-file-search-buffer-name))
  
;;插入查找结果
(defun fs-insert-result (line file text)
  (with-current-buffer (fs-get-search-result-buffer)
      (setq buffer-read-only nil)
      (goto-char (point-max))
      (let ((bpt (point)))
        (insert (file-name-nondirectory file))
        (set-text-properties bpt (point-max) '(face fs-file-name-face))
        (put-text-property bpt (point-max) 'keymap fs-file-search-keymap)
        (put-text-property bpt (point-max) 'filename file)
        (put-text-property bpt (point-max) 'nline line))
      (goto-char (point-max))
      (let ((bpt (point)))
        (insert "(" (number-to-string line)  "): ")
        (set-text-properties bpt (point-max) '(face fs-line-face)))
      (insert text "\n")
      (setq buffer-read-only t)))


;;显示结果窗口
(defun fs-show-result-window ()
  (with-current-buffer (fs-get-search-result-buffer)
    (let ((w (or (get-buffer-window fs-file-search-buffer-name)
                (split-window-vertically -10))))
      (set-window-buffer w (current-buffer))
      (select-window w)
      (goto-line 1))))

;;在查找结果上按回车的处理函数
(defun fs-result-choose()
  (interactive)
  (let ((bpt (line-beginning-position)))
    (let ((line (get-text-property bpt 'nline))
          (file (get-text-property bpt 'filename)))
      (select-window (next-window))
      (find-file file)
      (goto-line line))))

;;查找目录
;;;###autoload
(defun search-directory(str dir match-regexp)
  (interactive "ssearch string: \nDsearch directory: \nsmatch-regexp:")
  (kill-buffer (fs-get-search-result-buffer))
  (setq fs-file-search-str str)
  (setq fs-match-regexp match-regexp)
  (fs-search-directory dir)
  (fs-show-result-window))

;;查找目录
;;;###autoload
(defun search-file(str file)
  (interactive "ssearch string: \nffile:")
  (kill-buffer (fs-get-search-result-buffer))
  (setq fs-file-search-str str)
  (fs-search-file file 1)
  (fs-show-result-window))

(provide 'file-search)
