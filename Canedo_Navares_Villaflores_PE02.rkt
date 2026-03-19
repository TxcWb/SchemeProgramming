;;;; The Scheme code for Programming Exercise 02"


;;; ITEM A:

;;; T-ICE-AUX
;;;     (T-Ice-aux currN result)
;;;
;;; PURPOSE
;;;     It is a helper function for the T-Ice function.
;;;
;;; PARAMETERS
;;;     currN - The variable passed into the loop, which goes from n down to 1.
;;;     result - The resulting list that contains the "answer" for each number ("T", "ICE", "T-ICE", n).
;;;
;;; RETURNS
;;;     A list that contains all the numbers from 1 to 'n', but with the applied rules.
;;;
;;; EXAMPLE
;;;     (T-Ice-aux 10 '()) => '(1 "T" "ICE" "T" 5 "T-ICE" 7 "T" "ICE" "T")
(define (T-Ice-aux currN result)
        (cond 
            ((= currN 0) result)
            (else 
                (let ((ans (cond 
                                ((and (= (modulo currN 2) 0) (= (modulo currN 3) 0)) "T-ICE")
                                ((= (modulo currN 2) 0) "T")
                                ((= (modulo currN 3) 0) "ICE")
                                (else currN))))
                     (T-Ice-aux (- currN 1) (cons ans result))))))


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
(define (T-Ice n) (T-Ice-aux n '()))


;;; ITEM B:

;;; CHECK-DIVISOR
;;;     (check-divisor num d)
;;;
;;; PURPOSE
;;;     A helper function that checks if a number has any divisors starting from 'd'.
;;;
;;; PARAMETERS
;;;     num - The number being checked for primality.
;;;     d - The current divisor being tested.
;;;
;;; RETURNS
;;;     True (#t) if no divisors are found up to the square root of num, false (#f) otherwise.
(define (check-divisor num d)
    (cond
        ((> (* d d) num) #t)
        ((= (modulo num d) 0) #f)
        (else (check-divisor num (+ d 1)))))


;;; PRIME?
;;;     (prime? num)
;;;
;;; PURPOSE
;;;     Determines if a given number is prime.
;;;
;;; PARAMETERS
;;;     num - The integer to check.
;;;
;;; RETURNS
;;;     True (#t) if the number is prime, false (#f) otherwise.
(define (prime? num)
    (if (< num 2)
        #f
        (check-divisor num 2)))


;;; SUMPRIMES-AUX
;;;     (sumprimes-aux currN result)
;;;
;;; PURPOSE
;;;     It is a helper function for the sumprimes function.
;;;
;;; PARAMETERS
;;;     currN - The current number being checked, counting down from n to 2.
;;;     result - The accumulated sum of prime numbers found.
;;;
;;; RETURNS
;;;     The final sum of all prime numbers in the range.
(define (sumprimes-aux currN result)
    (cond
        ((< currN 2) result)
        ((prime? currN) (sumprimes-aux (- currN 1) (+ result currN)))
        (else (sumprimes-aux (- currN 1) result))))


;;; SUMPRIMES
;;;     (sumprimes n)
;;;
;;; PURPOSE
;;;     Calculates the sum of all prime numbers from 1 to n.
;;;
;;; PARAMETERS
;;;     n - The upper bound of the range to check for primes.
;;;
;;; RETURNS
;;;     The sum of all prime numbers from 1 to n.
;;;
;;; EXAMPLE
;;;     (sumprimes 10) => 17
(define (sumprimes n) (sumprimes-aux n 0))


;;; ITEM C:

;;; COUNT-FACTORS-AUX
;;;     (count-factors-aux newN m result)
;;;
;;; PURPOSE
;;;     It is a helper function for the count-factors function.
;;;
;;; PARAMETERS
;;;     newN - The current value of n as it is divided by m.
;;;     m - The factor being counted.
;;;     result - The accumulated count of factors found.
;;;
;;; RETURNS
;;;     The number of times 'm' is a factor of 'n', or "None" if count is 0.
(define (count-factors-aux newN m result)
    (cond 
        ((= newN 0) result)
        ((not (= (modulo newN m) 0)) 
            (if (= result 0) "None" result))
        (else (count-factors-aux (/ newN m) m (+ result 1)))))


;;; COUNT-FACTORS
;;;     (count-factors n m)
;;;
;;; PURPOSE
;;;     Counts how many times m is a factor of the number n.
;;;
;;; PARAMETERS
;;;     n - The number whose factors will be checked.
;;;     m - The factor to check in 'n'.
;;;
;;; RETURNS
;;;     The number of times 'm' is a factor of 'n'.
;;;
;;; EXAMPLE
;;;     (count-factors 64 2) => 6
(define (count-factors n m) (count-factors-aux n m 0))


;;; ITEM D:

;;; MY-SUMS-AUX
;;;     (my-sums-aux lis acc)
;;;
;;; PURPOSE
;;;     It is a helper function for the my-sums function.
;;;
;;; PARAMETERS
;;;     lis - The current list or sub-list being processed.
;;;     acc - The accumulated sum of all numbers found.
;;;
;;; RETURNS
;;;     The total sum of all numbers found within the list structure.
(define (my-sums-aux lis acc)
    (cond
        ((null? lis) acc)
        ((list? (car lis)) (my-sums-aux (cdr lis) (my-sums-aux (car lis) acc)))
        ((number? (car lis)) (my-sums-aux (cdr lis) (+ acc (car lis))))
        (else (my-sums-aux (cdr lis) acc))))


;;; MY-SUMS
;;;     (my-sums lis)
;;;
;;; PURPOSE
;;;     Adds all numbers found in a given list, including nested lists.
;;;
;;; PARAMETERS
;;;     lis - The (possibly nested) list containing numbers to be summed.
;;;
;;; RETURNS
;;;     The sum of all numbers found in the list.
;;;
;;; EXAMPLE
;;;     (my-sums '(1 (2 3) (4 (5 6)))) => 21
(define (my-sums lis) (my-sums-aux lis 0))


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

