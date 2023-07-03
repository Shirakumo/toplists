(in-package #:org.shirakumo.radiance.toplists)

(defun api-list-output (list)
  (cond ((string= "true" (post/get "browser"))
         (redirect (list-url list)))
        (T
         (api-output (dm:fields list)))))

(defun api-order-output (order)
  (cond ((string= "true" (post/get "browser"))
         (redirect (order-url order)))
        (T
         (api-output `(("_id" . (dm:id order))
                       ("author" . (dm:field order "author"))
                       ("items" . ,(split (dm:field order "items") #\,)))))))

(defun items-from-args (texts images)
  (loop for text in texts
        for image in images
        when (and text (string/= text ""))
        collect (list :text text :image image)))

(define-api toplists/list/view (id) ()
  (let ((list (ensure-list id)))
    (check-permission 'view list)
    (api-list-output list)))

(define-api toplists/list/create (title text[] image[]) ()
  (check-permission 'create)
  (let ((items (items-from-args text[] image[])))
    (api-list-output (create-list title items :author (auth:current)))))

(define-api toplists/list/edit (id &optional title text[] image[]) ()
  (let ((list (ensure-list id))
        (items (items-from-args text[] image[])))
    (check-permission 'edit list)
    (update-list list :title title :items items)
    (api-list-output list)))

(define-api toplists/list/delete (id) ()
  (let ((list (ensure-list id)))
    (check-permission 'delete list)
    (delete-list list)
    (if (string= "true" (post/get "browser"))
        (redirect (uri-to-url "toplists/" :representation :external
                                          :query '(("message" . "List deleted."))))
        (api-output `(("_id" . ,(dm:id list)))))))

(define-api toplists/order/view (id) ()
  (let ((order (ensure-order id)))
    (check-permission 'view order)
    (api-order-output order)))

(define-api toplists/order/create (list item[]) ()
  (let ((list (ensure-list list)))
    (check-permission 'order list)
    (api-order-output (create-order list item[] :author (auth:current)))))

(define-api toplists/order/edit (id item[]) ()
  (let ((order (ensure-order id)))
    (check-permission 'edit order)
    (update-order order item[])
    (api-order-output order)))

(define-api toplists/order/delete (id) ()
  (let ((order (ensure-order id)))
    (check-permission 'delete order)
    (delete-order order)
    (if (string= "true" (post/get "browser"))
        (redirect (uri-to-url "toplists/" :representation :external
                                          :query '(("message" . "Order deleted."))))
        (api-output `(("_id" . ,(dm:id order)))))))
