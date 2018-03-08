#|
 This file is a part of Toplists
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.radiance.toplists)

(define-page all "toplists/^$" (:clip "all.ctml")
  (check-permission 'view)
  (let ((lists (dm:get 'lists (db:query :all))))
    (r-clip:process T :lists lists)))

(define-page new-list "toplists/new" (:clip "edit.ctml")
  (check-permission 'create)
  (r-clip:process T))

(define-page view-list "toplists/(.+)" (:uri-groups (list) :clip "view.ctml")
  (let* ((list (ensure-list list))
         (items (list-items list)))
    (check-permission 'view list)
    (r-clip:process T :title (dm:field list "title")
                      :items items)))

(define-page edit-list "toplists/(.+)/edit" (:uri-groups (list) :clip "edit.ctml")
  (let* ((list (ensure-list list))
         (items (list-items list)))
    (check-permission '(edit delete) list)
    (r-clip:process T :title (dm:field list "title")
                      :items items)))

(define-page new-order "toplists/(.+)/new" (:uri-groups (list) :clip "view.ctml")
  (let* ((list (ensure-list list))
         (items (list-items list)))
    (check-permission 'create list)
    (r-clip:process T :title (dm:field list "title")
                      :editable T
                      :items items)))

(define-page view-order "toplists/(.+)/(.+)" (:uri-groups (list order) :clip "view.ctml")
  (let* ((list (ensure-list list))
         (order (ensure-order order))
         (items (list-items (dm:id list)))
         (user (or (auth:current) (user:get "anonymous"))))
    (check-permission 'view list user)
    (r-clip:process T :title (dm:field list "title")
                      :editable (equal (dm:field order "author") (user:username user))
                      :items items)))
