(in-package :markup)

(defgeneric walk (tree fn)
  (:documentation "Walk the tree, giving you the option to transform
  each element"))

(defmethod walk (tree fn)
  ;; do nothin
  tree)

(Defmethod walk ((tree xml-merge-tag) fn)
  (make-xml-merge-tag
   :children (loop for child in (xml-merge-tag-children tree) collect
                  (walk child fn))))

(defmethod walk ((tree list) fn)
  (loop for x in tree collect
       (walk x fn)))

(Defmethod walk ((tree xml-tag) fn)
  (let ((ret (funcall fn tree)))
    (make-xml-tag (xml-tag-name ret)
                  :attributes (xml-tag-attributes ret)
                  :children (walk (xml-tag-children ret) fn))))

(defmethod add-attrs ((tag xml-tag) &rest args &key &allow-other-keys)
  (let ((attr (xml-tag-attributes tag)))
    (dolist (item (alexandria:plist-alist args))
      (destructuring-bind (key . value) item
        (setf (alexandria:assoc-value attr (string-downcase (string key))) value)))
    (make-xml-tag (xml-tag-name tag)
                  :attributes attr
                  :children (xml-tag-children tag))))
