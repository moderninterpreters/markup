(in-package :markup)


(defclass markup-stream (trivial-gray-streams:fundamental-character-stream)
  ((delegate
    :initarg :delegate
    :reader stream-delegate)
   (last-char
    :initform nil
    :accessor last-read-char)
   (history
    :reader history-stream
    :initform (make-string-output-stream))))

(defmethod trivial-gray-streams:stream-read-char ((stream markup-stream))
  (let ((ch (trivial-gray-streams:stream-read-char (stream-delegate stream))))
    (flush-last-read stream)
    (setf (last-read-char stream) ch)
    ch))

(defmethod trivial-gray-streams:stream-unread-char ((stream markup-stream) ch)
  (assert (last-read-char stream))
  (setf (last-read-char stream) nil)
  (trivial-gray-streams:stream-unread-char (stream-delegate stream) ch))

(defun flush-last-read (stream)
  (let ((last-read (last-read-char stream)))
    (when last-read
      (write-char last-read (history-stream stream))
      (setf (last-read-char stream) nil))))

(defun wrap-stream (stream)
  (make-instance 'markup-stream
                 :delegate stream))

(defun read-so-far (stream)
  (flush-last-read stream)
  (let ((resp (get-output-stream-string (history-stream stream))))
    ;; get-output-stream-string clears up the stream so let's write it
    ;; back
    (write-string resp (history-stream stream))
    resp))
