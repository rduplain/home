;;;; Non-interactive user functions.

(require 'subr-x)

(defun dominating-file (filename)
  "Provide filepath of dominating file, or nil, walking up directory tree."
  (when-let ((directory (locate-dominating-file "." filename)))
    (concat (file-name-as-directory directory) filename)))

(defun dominating-directory (filename)
  "Provide directory of dominating file, or ., walking up directory tree."
  (or (locate-dominating-file "." filename) "."))

(defun file-string (filepath)
  "Read the contents of a file and return as a string."
  (when filepath
    (with-temp-buffer
      (insert-file-contents filepath)
      (buffer-string))))

(defun filter (pred coll)
  "Return list of items in coll for which pred returns true."
  (let ((null-item (gensym)))
    (delete null-item
            (mapcar (lambda (item)
                      (if (funcall pred item)
                          item
                        null-item))
                    coll))))

(defun find-buffer (regexp)
  "Find existing buffer with name matching regular expression"
  (car
   (filter 'identity
           (mapcar (lambda (buffer)
                     (when (string-match regexp (buffer-name buffer))
                       buffer))
                   (buffer-list)))))

(defun on-keyword (fn filepath &rest keywords)
  "Apply fn to keywords found when searching keywords in a given file."
  (when filepath
    (let* ((pattern (string-join keywords "\\|"))
           (command (format "grep -e '%s' -o %s" pattern filepath))
           (result (string-trim (shell-command-to-string command))))
      (unless (string= "" result)
        (funcall fn (split-string result "\n"))))))

(defun project-path-from (dominating-filename path)
  "Provide filepath within project, with directory of dominating-file as root."
  (when-let ((directory (locate-dominating-file "." dominating-filename)))
    (concat (file-name-as-directory directory) path)))

(defun sniff (filepath &rest keywords)
  "Return the first keyword found when searching keywords in a given file."
  (apply 'on-keyword 'car filepath keywords))

(defun tcp-listening (port)
  "Return non-nil if a process is listening on localhost for given TCP port."
  (when-let ((proc (ignore-errors
                     (open-network-stream "tcp-listening" nil
                                          "localhost" port))))
    (process-send-eof proc)
    t))
