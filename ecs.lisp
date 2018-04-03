(in-package :ecs)

(defclass world ()
  ((entity-id :initform 0)
   (entities :initform (make-hash-table :test #'equal)
             :accessor entities)
   (transforms :initform (make-hash-table :test #'equal))
   (users :initform (make-hash-table :test #'equal))))

(defmethod has-entity ((w world) entity-id)
  (gethash entity-id (entities w)))

(defmethod add-entity ((w world) entity-id)
  (check-type entity-id integer)
  (let ((e (gethash entity-id (entities w))))
    (if (not e)
        (setf (gethash entity-id (entities w)) t)
        (error (format nil "Entity ~a already exists~%" entity-id)))))


(defmethod new-entity ((w world))
  "Create a new entity and return it"
  (let* ((id (slot-value w 'entity-id))
         (entities (slot-value w 'entities)))

    (setf (gethash id entities) t)

    (incf (slot-value w 'entity-id))

    (return-from new-entity id)))
         
(defmethod remove-entity ((w world) entity-id)
  "Remove the given entity"
  (remhash entity-id (slot-value w 'entities))
  (remhash entity-id (slot-value w 'transforms))
  (remhash entity-id (slot-value w 'users)))

(defmethod set-component ((w world) entity-id component-type component)
  "Attach a new component to the given entity"
  (check-type entity-id integer)
  (let ((sym (find-symbol (symbol-name component-type) 'ecs)))
    (if (gethash entity-id (entities w))
        ;; entity exists
        (setf (gethash entity-id (slot-value w sym))
              component)
        (let ((known-ids

               (loop
                  for e-id being the hash-keys of (entities w)
                  collect e-id)))

          (error (format nil "entity ~a does not exist (~a)" entity-id known-ids))))))


  
(defmethod get-component ((w world) entity-id component-type)
  (let ((sym (find-symbol (symbol-name component-type) 'ecs)))
    (gethash entity-id (slot-value w sym))))


(defmethod components-by-kw ((w world) kw)
  (check-type kw keyword)
  (let ((slot-sym (find-symbol (symbol-name kw) 'ecs)))
    (slot-value w slot-sym)))

(defmethod update-components ((w world) td f c1 &rest cs)
  "Applies the given function f to every entity that has all the requested
components."
  (check-type td number)
  (let ((hts (mapcar (alexandria:curry #'components-by-kw w) cs)))
    (maphash
     #'(lambda (entity-id component)

         (if (consp cs)

             (loop
                for ht being the elements of hts
                for the-component = (gethash entity-id ht)
                always the-component
                collect the-component into the-components
                finally (apply f (cons entity-id (cons td (cons component the-components)))))

             ;; call directly if only one component is needed
             (funcall f entity-id td component)))

     (components-by-kw w c1))))
