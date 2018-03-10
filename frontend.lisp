#|
 This file is a part of Toplists
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.radiance.toplists)

(define-page all ("toplists/^$" 10) (:clip "all.ctml")
  (check-permission 'view)
  (let ((lists (dm:get 'lists (db:query :all))))
    (r-clip:process T :lists lists
                      :message (post/get "message")
                      :error (post/get "error"))))

(define-page new-list ("toplists/^new$" 10) (:clip "edit.ctml")
  (check-permission 'create)
  (let ((list (dm:hull 'lists)))
    (setf (dm:field list "items") '((:text "" :image "")))
    (r-clip:process T :list list
                      :message (post/get "message")
                      :error (post/get "error"))))

(define-page view-list "toplists/^([^/]+)$" (:uri-groups (list) :clip "view.ctml")
  (let ((list (ensure-list list)))
    (check-permission 'view list)
    (setf (dm:field list "items") (list-items list))
    (setf (dm:field list "orders") (list-orders list))
    (r-clip:process T :list list
                      :message (post/get "message")
                      :error (post/get "error"))))

(define-page edit-list ("toplists/^([^/]+)/edit$" 10) (:uri-groups (list) :clip "edit.ctml")
  (let ((list (ensure-list list)))
    (check-permission '(edit delete) list)
    (setf (dm:field list "items") (list-items list))
    (r-clip:process T :list list
                      :message (post/get "message")
                      :error (post/get "error"))))

(define-page new-order ("toplists/^([^/]+)/order$" 10) (:uri-groups (list) :clip "order.ctml")
  (let ((list (ensure-list list))
        (order (dm:hull 'orders)))
    (check-permission 'create list)
    (setf (dm:field list "items") (list-items list))
    (setf (dm:field list "editable") T)
    (setf (dm:field order "author") (user:username (or (auth:current) (user:get "anonymous"))))
    (r-clip:process T :list list
                      :order order
                      :message (post/get "message")
                      :error (post/get "error"))))

(define-page view-order "toplists/^([^/]+)/([^/]+)$" (:uri-groups (list order) :clip "order.ctml")
  (let* ((list (ensure-list list))
         (order (ensure-order order))
         (user (or (auth:current) (user:get "anonymous"))))
    (check-permission 'view order user)
    (setf (dm:field list "items") (sort-by-order (list-items list) order))
    (setf (dm:field list "editable") (equal (dm:field order "author") (user:username user)))
    (r-clip:process T :list list
                      :order order
                      :message (post/get "message")
                      :error (post/get "error"))))
