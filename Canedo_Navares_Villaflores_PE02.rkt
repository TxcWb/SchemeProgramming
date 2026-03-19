;;;; The Scheme code for Programming Exercise 02

;;; ITEM A:
;;; T-ICE
;;;     (T-Ice n)
;;;
;;; PURPOSE
;;;     Counts from 1 to 'n' and "writes" it in a list. If the current number is divisible by both 2 and 3, write "T-Ice" to the list instead of the number itself. If the current number is divisible by 2 but not 3, write "T" instead, and if the current number is divisible by 3 but not 2, write "ICE".
;;;
;;; PARAMETERS
;;;     n - The number to count towards (from 1 to n).
;;;     
;;;     currN - The variable passed into the loop, which goes from 1 to n, and adds 1 to itself every iteration.
;;;     result - The resulting list that contains the "answer" for each number ("T", "ICE", "T-ICE", n).
;;;
;;; RETURNS
;;;     A list that contains all the numbers from 1 to 'n', but with the applied rules, like the number being divisible by 2 and 3 is written as "T-ICE" instead of the number itself.
;;;
;;; EXAMPLE
;;;     (T-Ice 10) => (1 "T" "ICE" "T" 5 "T-ICE" 7 "T" "ICE" "T")
(define (T-Ice n)
  (local [(define (loop currN result)
            (cond 
              [(= currN 0) result] 
              [else 
               (let ([ans (cond 
                            [(and (= (modulo currN 2) 0) (= (modulo currN 3) 0)) "T-ICE"]
                            [(= (modulo currN 2) 0) "T"]
                            [(= (modulo currN 3) 0) "ICE"]
                            [else currN])])
                 (loop (- currN 1) (cons ans result)))]))] 
    (loop n empty))) 

;;; ITEM B:

;;; ITEM C:
;;; COUNT-FACTORS
;;;     (count-factors n m)
;;;
;;; PURPOSE
;;;     It counts how many factors m does a number n have.
;;;
;;; PARAMETERS
;;;     n - The number whose factors will be checked.
;;;     m - The factor to check in 'n'.
;;;     
;;;     newN - The variable passed into the loop, which gets changed for every iteration.
;;;     result - The number of times 'm' is a factor of 'n'.
;;;
;;; RETURNS
;;;     The number of times 'm' is a factor of 'n'.
;;;
;;; EXAMPLE
;;;     (count-factors 64 2) => 6

(define (count-factors n m)
  (local [(define (loop newN result)
            (cond 
              [(= newN 0) result] 
              [else
               (cond 
                 [(not (= (modulo newN m) 0)) result] 
                 [else (loop (/ newN m) (+ result 1))])]))] 
          (loop n 0))) 

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


