;;;; The Scheme code for Programming Exercise 02

;;; ITEM A:
;;; ITEM B:
;;; ITEM C:
;;; ITEM D:


;;; ITEM E:

;;; MY-REVERSE-AUX
;;;     (my-reverse-aux lis acc)
;;;
;;; PURPOSE
;;;     It is a helper function for reversing a list.
;;;
;;; PARAMETERS
;;;     lis - The list to be reversed.
;;;     acc - The accumulated reversed list.
;;;
;;; RETURNS
;;;     The reversed list of the given list.
;;;
;;; EXAMPLE
;;;     (my-reverse-aux '(1 2 3 4 5) '()) => '(5 4 3 2 1)
(define (my-reverse-aux lis acc)
    (if (null? lis)
        acc
        (my-reverse-aux (cdr lis) (cons (car lis) acc))))


;;; MY-REVERSE
;;;     (my-reverse lis)
;;;
;;; PURPOSE
;;;     It reverses a list.
;;;
;;; PARAMETERS
;;;     lis - The list to be reversed.
;;;
;;; RETURNS
;;;     The reversed list of the given list.
;;;
;;; EXAMPLE
;;;     (my-reverse '(1 2 3 4 5)) => '(5 4 3 2 1)
(define (my-reverse lis) (my-reverse-aux lis '()))


