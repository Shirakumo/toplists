#|
 This file is a part of Toplists
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.radiance.toplists)

(define-hook list-created (list))
(define-hook list-updated (list))
(define-hook list-deleted (list))
(define-hook order-created (order))
(define-hook order-updated (order))
(define-hook order-deleted (order))

(define-trigger db:connected ()
  (db:create 'lists '((author (:varchar 32))
                      (title (:varchar 32)))
             :indices '(author))
  (db:create 'items '((list :id)
                      (id :integer)
                      (text (:varchar 64))
                      (image (:varchar 2048)))
             :indices '(author list))
  (db:create 'orders '((list :id)
                       (author (:varchar 32))
                       (order :text))
             :indices '(author)))

(define-trigger user:ready ()
  (defaulted-config (list
                     (perm toplists create)
                     (perm toplists view)
                     (perm toplists order)
                     (perm toplists edit own)
                     (perm toplists delete own))
                    :permissions :default)
  (defaulted-config (list
                     (perm toplists view))
                    :permissions :anonymous)

  (apply #'user:add-default-permissions (config :permissions :default))
  (apply #'user:grant "anonymous" (config :permissions :anonymous)))

(define-version-migration toplists (NIL 1.0.0)
  (let ((anonymous (user:get "anonymous")))
    (dolist (user (user:list))
      (unless (eql user anonymous)
        (apply #'user:grant user (config :permissions :default))))))

(defun ensure-id (id-ish)
  (etypecase id-ish
    (dm:data-model (dm:id id-ish))
    (db:id id-ish)
    (string (db:ensure-id id-ish))))

(defun ensure-list (list-ish)
  (etypecase list-ish
    (dm:data-model list-ish)
    (db:id (or (dm:get-one 'lists (db:query (:= '_id list-ish)))
               (error 'request-not-found :message (format NIL "No list with ID ~a was found." list-ish))))
    (string (ensure-list
             (or (ignore-errors (db:ensure-id list-ish))
                 (error 'request-not-found :message (format NIL "No list with ID ~a was found." list-ish)))))))

(defun list-items (list-ish)
  (let ((id (ensure-id list-ish)))
    (dm:get 'items (db:query (:= 'list id)))))

(defun list-orders (list-ish)
  (let ((id (ensure-id list-ish)))
    (dm:get 'orders (db:query (:= 'list id)))))

(defun create-items (list items)
  (unless items (error 'api-error :message "At least one item is required."))
  (let ((item (dm:hull 'items)))
    (setf (dm:field item "list") (dm:id list))
    (loop for i from 0
          for props in items
          do (setf (dm:field item "id") i)
             (setf (dm:field item "text") (or (getf props :text)
                                              (error "Item ~a is missing text." i)))
             (setf (dm:field item "image") (getf props :image ""))
             (dm:insert item))))

(defun create-list (title items &key author)
  (let ((list (dm:hull 'lists))
        (author (etypecase author
                  (string author)
                  (user:user (user:username author)))))
    (db:with-transaction ()
      (setf (dm:field list "author") author)
      (setf (dm:field list "title") title)
      (dm:insert list)
      (create-items list items))
    (trigger 'list-created list)
    list))

(defun delete-list (list)
  (let ((list (ensure-list list)))
    (db:with-transaction ()
      (db:remove 'items (db:query (:= 'list (dm:id list))))
      (db:remove 'orders (db:query (:= 'list (dm:id list))))
      ;; FIXME: trigger orders deleted.
      (dm:delete list))
    (trigger 'list-deleted list)
    list))

(defun update-list (list &key title items)
  (let ((list (ensure-list list)))
    (db:with-transaction ()
      (when title
        (setf (dm:field list "title") title))
      (when items
        (db:remove 'items (db:query (:= 'list (dm:id list))))
        (create-items list items))
      (dm:save list))
    list))

(defun list-url (list)
  (let ((id (ensure-id list)))
    (uri-to-url (format NIL "toplists/~a" id) :representation :external)))

(defun ensure-order (order-ish)
  (etypecase order-ish
    (dm:data-model order-ish)
    (db:id (or (dm:get-one 'orders (db:query (:= '_id order-ish)))
               (error 'request-not-found :message (format NIL "No order with ID ~a was found." order-ish))))
    (string (ensure-order
             (or (ignore-errors (db:ensure-id order-ish))
                 (error 'request-not-found :message (format NIL "No order with ID ~a was found." order-ish)))))))

(defun create-order (list items &key author)
  (let ((list (ensure-list list))
        (order (dm:hull 'orders))
        (author (etypecase author
                  (string author)
                  (user:user (user:username author)))))
    (db:with-transaction ()
      (setf (dm:field order "author") author)
      (setf (dm:field order "list") (dm:id list))
      (setf (dm:field order "order") (format NIL "~{~a~^,~}" items))
      (dm:insert order))
    (trigger 'order-created order)
    order))

(defun update-order (order items)
  (let ((order (ensure-order order)))
    (db:with-transaction ()
      (setf (dm:field order "order") (format NIL "~{~a~^,~}" items))
      (dm:save order))
    (trigger 'order-updated order)
    order))

(defun delete-order (order)
  (let ((order (ensure-order order)))
    (db:with-transaction ()
      (dm:delete order))
    (trigger 'order-deleted order)
    order))

(defun order-url (order)
  (let ((order (ensure-order order)))
    (uri-to-url (format NIL "toplists/~a/~a" (dm:field order "list") (dm:id order))
                :representation :external)))

(defun split (string by)
  (let ((parts ())
        (out (make-string-output-stream)))
    (flet ((out ()
             (let ((string (get-output-stream-string out)))
               (when (string/= "" string) (push string parts)))))
      (loop for char across string
            do (if (char= char by)
                   (out)
                   (write-char char out))
            finally (out))
      (nreverse parts))))

(defun sort-by-order (items order)
  (loop for id in (split (dm:field order "order") #\,)
        collect (find (parse-integer id) items :key (lambda (item) (dm:field item "id")))))

(defun permitted-p (action &optional list (user (or (auth:current) (user:get "anonymous"))))
  (if (listp action)
      (loop for a in action thereis (permitted-p a list user))
      (or (and list
               (equal (dm:field list "author") (user:username user))
               (user:check user `(toplists ,action own)))
          (user:check user `(toplists ,action)))))

(defun check-permission (action &optional list (user (or (auth:current) (user:get "anonymous"))))
  (unless (permitted-p action list user)
    (error 'request-denied :message (format NIL "You do not have the permission to ~a lists."
                                            action))))
