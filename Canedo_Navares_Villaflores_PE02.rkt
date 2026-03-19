;;;; The Scheme code for Programming Exercise 02

;;; ITEM A:
; T_Ice function
(define (T-Ice n)
  (local [(define (loop currN result)
            (cond ; main count checker, count goes from n to 0
              [(= currN 0) result] ; if count is 0, print out result
              [else ; runs if count is still positive
               (let ([ans (cond ; choose what to add to list (T-Ice, T, Ice, count)
                            [(and (= (modulo currN 2) 0) (= (modulo currN 3) 0)) "T-ICE"]
                            [(= (modulo currN 2) 0) "T"]
                            [(= (modulo currN 3) 0) "ICE"]
                            [else currN])])
                 (loop (- currN 1) (cons ans result)))]))] ; run loop again, do current -= 1, add the correct result to the list
    (loop n empty))) ; run the loop itself, setting currN or count to the input n, and setting the result list to empty

;;; ITEM B:

;;; ITEM C:
; count-factors function
; function logic: store n in a new variable newN, check if newN mod m is 0. if yes, set newN to (n/m), and add 1 to result counter, and check newN again. if newN mod m is not 0, or if newN itself is 0, display the result count
(define (count-factors n m)
  (local [(define (loop newN result)
            (cond ; main condition
              [(= newN 0) result] ; stop when newN is 0 and print out result. without this, infinite loop will occur
              [else
               (cond ; if newN is not 0, check if newN mod m is =
                 [(not (= (modulo newN m) 0)) result] ; if newN mod m is not 0, end the loop and print out result
                 [else (loop (/ newN m) (+ result 1))])]))] ; else, run the loop again, and input newN/m as the new "newN" input for the loop, and add 1 to the result counter
          (loop n 0))) ; run the loop itself, setting newN as n, and setting the result counter to 0

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


