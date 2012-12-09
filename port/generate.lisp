(defun file-contents (path)
  "Sucks up an entire file from PATH into a freshly-allocated string,
      returning two values: the string and the number of bytes read."
  (with-open-file (s path)
    (let* ((len (file-length s))
           (data (make-string len)))
      (values data (read-sequence data s)))))

(defun read-file (filename)
  "Read a lisp file and parse it into a lisp datastructure."
  (when (probe-file filename)
    (read-from-string (file-contents filename))))

(defun load-http-request-defun-form (file)
  "Grab the http-request defun form from the specified file."
  (let* ((contents (file-contents file))
         (defun-pos (search "(defun http-request" contents))
         (defun-str (subseq contents defun-pos)))
    (read-from-string defun-str)))

(pprint (load-http-request-defun-form "port/drakma-1.2.8.request.lisp"))

(defun generate-http-request-async (original-drakma-request-file)
  (let ((http-request-defun-form (load-http-request-defun-form original-drakma-request-file)))
    'x))

