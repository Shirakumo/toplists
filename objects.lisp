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
             :index '(author))
  (db:create 'items '((list :id)
                      (id :integer)
                      (text (:varchar 32))
                      (image (:varchar 2048)))
             :index '(author list))
  (db:create 'orders '((author (:varchar 32))
                       (order :text))
             :index '(author)))

(define-trigger user:ready ()
  (defaulted-config (list
                     (perm toplists new)
                     (perm toplists view)
                     (perm toplists edit own)
                     (perm toplists delete own))
                    :permissions :default)
  (defaulted-config (list
                     (perm toplists view))
                    :permissions :anonymous)

  (apply #'user:add-default-permissions (config :permissions :default))
  (apply #'user:grant "anonymous" (config :permissions :anonymous)))

(defun ensure-list (list-ish)
  (etypecase list-ish
    (dm:data-model list-ish)
    (db:id (or (dm:get-one 'lists (db:query (:= '_id list-ish)))
               (error 'request-not-found :message (format NIL "No list with ID ~a was found." list-ish))))
    (string (ensure-list
             (or (ignore-errors (db:ensure-id list-ish))
                 (error 'request-not-found :message (format NIL "No list with ID ~a was found." list-ish)))))))

(defun list-items (list-ish)
  (let ((id (etypecase list-ish
              (dm:data-model (dm:id list-ish))
              (db:id list-ish)
              (string (db:ensure-id list-ish)))))
    (dm:get 'items (db:query (:= 'list id)))))

(defun create-list (title items &key author)
  (let ((list (dm:hull 'lists))
        (item (dm:hull 'items))
        (author (etypecase author
                  (string author)
                  (user:user (user:username author)))))
    (db:with-transaction ()
      (setf (dm:field list "author") author)
      (setf (dm:field list "title") title)
      (dm:insert list)
      (setf (dm:field item "list") (dm:id list))
      (loop for i from 0
            for props in items
            do (setf (dm:field item "id") i)
               (setf (dm:field item "text") (or (getf props :text)
                                                (error "Item ~i is missing text." i)))
               (setf (dm:field item "image") (getf props :image ""))
               (dm:insert item)))
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
  ;; FIXME
  )

(defun ensure-order (order-ish)
  (etypecase order-ish
    (dm:data-model order-ish)
    (db:id (or (dm:get-one 'orders (db:query (:= '_id list-ish)))
               (error 'request-not-found :message (format NIL "No order with ID ~a was found." list-ish))))
    (string (ensure-list
             (or (ignore-errors (db:ensure-id list-ish))
                 (error 'request-not-found :message (format NIL "No order with ID ~a was found." list-ish)))))))

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

(defun permitted-p (action &optional ???? (user (or (auth:current) (user:get "anonymous"))))
  (if (listp action)
      (loop for a in action thereis (permitted-p a ???? user))
      (or (and ????
               (equal (dm:field ???? "author") (user:username user))
               (user:check user `(toplists ,action own)))
          (user:check user `(toplists ,action)))))

(defun check-permission (action &optional ???? (user (or (auth:current) (user:get "anonymous"))))
  (unless (permitted-p action event user)
    (error 'request-denied :message (format NIL "You do not have the permission to ~a events."
                                            action))))
