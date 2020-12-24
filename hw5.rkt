;To run this code you must
; choose "Language" from the top menu
; select "Choose Language" from the drop down menu
; select "Other Languages" radio button
; select R5RS
; In the REPL window below you will then see
; Language: R5RS
; See Files>Resources/R5RS.mov for a demo
;

(#%require srfi/27)

; Name: Milan Mardia
; Email address: milan.mardia@yale.edu

; CS 201a HW #5  DUE Monday, Nov. 9, 2020 at 11:59 pm, electronically,
; using the submit command.
; Do *not* email your homework -- lateness penalties (5 points per day)
; will accrue until you successfully submit it using your Zoo account.

; Topics: objects, mutators, pseudo-random number generators, Shut the box.

; Unless the problem specifies otherwise:
; * You may write auxiliary procedure(s) in addition to
; the one(s) specified in the problem.
; * Your procedures need *not* fail gracefully if their
; inputs do not satisfy the assumptions of the problem.
; * Please use the specified names (and numbers of arguments)
; for the required procedures in a problem; this is to
; facilitate automatic testing.
; * Only use Racket procedures that have been used in class,
; which were allowed in previous homework, or which
; are introduced in this problem set.

; In general, in these problems if you need to call two or more procedures in a particular order
; use (begin (proc1 args) (proc2 args)) to guarantee that they will be executed in order.

; You may use the Racket procedure (member v lst) which returns the list starting with v
; if v is in the list:
;  (member 2 (list 1 2 3 4)) => (2 3 4)

; You may use the Racket procedure (apply proc lst) which applies the procedure with all of the elements
; of the list as arguments. Here is an example with the procedure + applied to a list of 3 numbers:
;  (apply + '(1 2 3)) => 6

; Important things to do before submitting:
;   Remember to remove tracing and other debugging tools
;   Test your code on a Zoo machine following instructions for hw5. These instructions will be made available by Nov. 2.

; ** problem 0 ** (1 point)
; Modify the following definition to reflect the number of
; hours you spent on this assignment (including reading):

(define hours 11)

; ** problem 1 **
; One common method of generating a sequence of pseudo-random
; numbers is to start with a number (the seed) and
; then repeatedly 
; multiply the current number by a,
; add c to it,
; and reduce modulo m, for appropriately chosen a,c, and m.

; (10 points) Write a procedure (make-prng x a c m) to create
; a pseudo-random number generator of this kind,
; in the style of Racket objects.
; The initial value of x is the seed.
; Each pseudo-random number generator should
; support the following operations (communicated by Racket symbols):

; next!  - update the current number x to be the next number in the
;          sequence and return the resulting value
; reseed! - set the current number x to be the given value

; Any other command should return the symbol error, and not change
; the state of the pseudo-random number generator.
; Reminder: remainder.

; (1 point) Look up good choices for a, c, and m and report one
; such good choice with an (informal) citation of where you got it.

; Examples
; (define random1 (make-prng 0 3 1 17))
; (random1 'next!) => 1
; (random1 'next!) => 4
; (random1 'next!) => 13
; (random1 'next!) => 6
; (random1 'next!) => 2
; (random1 'next!) => 7
; (random1 'next!) => 5
; (random1 'reseed! 8)
; (random1 'next!) => 8
; (random1 'next!) => 8
; (random1 'reseed! 3)
; (random1 'next!) => 10
; (random1 'next!) => 14
; (random1 'next!) => 9
; (random1 'hi) => error


; good choice of a, c, m
; m = 2^31 - 1 = 2,147,483,647
; a = 7^5 = 16,807
; c = 0
; Lehmer random number generator: https://en.wikipedia.org/wiki/Lehmer_random_number_generator

(define make-prng
  (lambda (x a c m)
    (lambda (command . args)
      (case command
        ((next!) (set! x (remainder (+ (* x a) c) m)) x)
        ((reseed!) (set! x (car args)))
        (else 'error)))))
        



; *********************************************************************

; ** problem 2 **
; (9 points) Write a procedure (make-queue) that takes
; no arguments and returns a procedure implementing
; a Racket-style "queue" object, analogous to the stack
; object in lecture.  The data structure should
; be accessible *only* through the operations provided by
; the queue object.  The queue should be initially empty.

; It needs to support the following operations:

; empty?              - return #t if the queue is empty, #f otherwise
; head               - return the value of the front element of the queue
; dequeue!       - remove the front element of the queue
; enqueue! value  - add the value at the back of the queue
;                        as the last element.



; Examples (evaluated in order):
; (define q1 (make-queue))
; (q1 'empty?) => #t
; (q1 'enqueue! 'a)
; (q1 'empty?) => #f
; (q1 'head) => a
; (q1 'enqueue! 'c)
; (q1 'head) => a
; (q1 'dequeue!)
; (q1 'head) => c
; (q1 'dequeue!)
; (q1 'empty?) => #t
; (q1 'hi) => error

; Each operation above should be constant-time. This means that the operations cannot
; use append, length, or any other procedure that has to consider each element in a list.

(define make-queue
  (lambda ()
    (let* ((end (cons 'end '()))
          (queue (cons end end)))
      (lambda (command . args)
        (case command
          ((empty?) (eq? (car queue) (cdr queue)))
          ((head)  (car (car queue)))
          ((enqueue!) (set-car! (cdr queue) (car args)) (set-cdr! (cdr queue) (cons 'end '())) (set-cdr! queue (cdr (cdr queue)))  queue)
          ((dequeue!) (set-car! queue (cdr (car queue))))
          (else 'error))))))



; *********************************************************************

; ** problem 3 ** (11 points)
; Shut-the-box (also called Down-and-out in some commercial versions)
; is the following 2 (or more) person game:
; There are 9 "flappers" numbered 1 to 9.
; Each flapper may be in one of two states: "up" or "down".
; Initially all the flappers are "up".
; In one turn, a player repeatedly does the following:
;  roll two 6-sided dice (numbers 1 to 6 on the six faces), and note the sum S
;  choose a collection of one or more "up" flappers whose sum is S,
;    and change those flappers to "down"
; When a dice roll results in a sum S for which there is no remaining
; set of "up" flappers with sum S, the player's score is the sum
; of the remaining "up" flappers.

; After one turn by each player, the player with the LOW score wins.
; If the player's final score is 0, he or she is said to have "shut the box".

; For example, player 1 might
; roll 2 and 6, and decide to flip down 8, leaving: (1 2 3 4 5 6 7 9)
; roll 6 and 6, and decide to flip down 3 and 9, leaving: (1 2 4 5 6 7)
; roll 1 and 3, and decide to flip down 4, leaving: (1 2 5 6 7)
; roll 6 and 6, and decide to flip down 5 and 7, leaving (1 2 6)
; roll 5 and 6, finishing with a score of 9 = 1 + 2 + 6

; Design and implement an object to represent the state of a
; shut-the-box game in the style of Racket objects. Name your
; template procedure make-box.
; Do not worry about the time your operations take.

; *********************************************************************
; The state of the box is represented by the list of "up" flappers.
; The possible commands are
; initialize!       - make all the flappers "up"
; up-flappers       - return the list of "up" flappers
; make-move! lst    - remove flappers on lst from list of "up" flappers
;                   - note: you can assume the flappers on lst are always valid,
;                   - which means they can all be found in the current "up" flappers

; Any other command should return the symbol error, and not change
; the state of the state of the box.

; Examples
; (define game1 (make-box))
; (game1 'up-flappers) => (1 2 3 4 5 6 7 8 9)
; (game1 'make-move! '(1 5))
; (game1 'up-flappers) => (2 3 4 6 7 8 9)
; (game1 'make-move! '(6))
; (game1 'up-flappers) => (2 3 4 7 8 9)
; (game1 'make-move! '(2 3 4))
; (game1 'up-flappers) => (7 8 9) 
; (game1 'make-move '(9)) => error
; (game1 'up-flappers) => (7 8 9) 
; (game1 'initialize!)
; (game1 'up-flappers) => (1 2 3 4 5 6 7 8 9)
(define delete1
  (lambda (lst itm)
    (cond ((equal? itm (car lst)) (cdr lst))
          (else (cons (car lst) (delete1 (cdr lst) itm))))))
(define change1
  (lambda (lst args)
    (cond ((null? args) lst)
          (else (change1 (delete1 lst (car args)) (cdr args))))))
(define make-box
  (lambda ()
    (let ((up '(1 2 3 4 5 6 7 8 9)))
      (lambda (command . args)
        (case command
           ((up-flappers) up)
           ((initialize!) (set! up '(1 2 3 4 5 6 7 8 9)))
           ((make-move!) (set! up (change1 up (car args))))
           (else 'error))))))

; *********************************************************************

; ** problem 4 ** (11 points)
; Write a procedure (possible-moves sum up-flappers)
; that returns a list of all the possible moves if the
; sum of the dice is sum and the list of "up" flappers is up-flappers.

; sum is an integer between the values 2 and 12, inclusive, and up-flappers
; is a list representing a subset of '(1 2 3 4 5 6 7 8 9).

; Examples
; (possible-moves 10 '(1 2 3 4 5 6 7 8 9)) =>
;         ((1 2 3 4) (1 2 7) (1 3 6) (1 4 5) (1 9) (2 3 5) (2 8) (3 7) (4 6))
; (possible-moves 10 '(2 3 5 6 7 8)) => ((2 3 5) (2 8) (3 7))
; (possible-moves 8 '(3 5 6 7 8)) => ((3 5) (8))
; (possible-moves 9 '(2 5 6 8)) => ()

(define possible-ones
  (lambda (sum lst)
    (cond ((list? (member sum lst)) (list (list sum)))
          (else '()))))

(define possible-twos
  (lambda (sum lst)
    (cond ((null? lst) '())
          ((list? (member (- sum (car lst)) (cdr lst))) (cons (list (car lst) (- sum (car lst))) (possible-twos sum (cdr lst))))
          (else (possible-twos sum (cdr lst))))))

(define possible-threes-helper1
  (lambda (sum lst)
    (cond ((< (length lst) 3) '())
          ((not (null? (possible-twos (- sum (car lst)) (cdr lst)))) (append (map (lambda (x) (append (cons (car lst) '()) x)) (possible-twos (- sum (car lst)) (cdr lst))) (possible-threes-helper1 sum (cdr lst))))
          (else (possible-threes-helper1 sum (cdr lst))))))

(define possible-fours
  (lambda (sum lst)
    (cond ((< (length lst) 4) '())
          ((not (null? (possible-twos (- sum (car lst)) (cdr lst)))) (append (map (lambda (x) (append (cons (car lst) '()) x)) (possible-threes-helper1 (- sum (car lst)) (cdr lst))) (possible-fours sum (cdr lst))))
          (else (possible-fours sum (cdr lst))))))

(define possible-moves
  (lambda (sum up-flappers)
    (let ((x (append (append (possible-twos sum up-flappers) (possible-threes-helper1 sum up-flappers))  (append (possible-ones sum up-flappers) (possible-fours sum up-flappers)) )))
      x)))


; *********************************************************************

; ** problem 5 **
; (6 points) Write one procedure:
; (random-player)
; that creates a shut-the-box object (problem 3)
; and uses it to play one turn of shut-the-box
; by *randomly* choosing a possible move (problem 4)
; until no move remains possible,
; and returns its score.

; The Racket procedure
; (random-integer n) returns a random integer
; between 0 and n-1 inclusive.
; Here is a procedure using random
; to return the sum of two randomly rolled dice.

(define roll-dice
  (lambda () (+ 2 (random-integer 6) (random-integer 6))))

; *********************************************************************


(define play-game
  (lambda (game)
    (let ((x (roll-dice)))
      (cond ((= (length (possible-moves x (game 'up-flappers))) 0) (apply + (game 'up-flappers)))
            (else (game 'make-move! (list-ref (possible-moves x (game 'up-flappers)) (random-integer (length (possible-moves x (game 'up-flappers))))))  (play-game game))))))


  (define random-player
    (lambda ()
      (let ((game (make-box)))
        (play-game game))))

; *********************************************************************
; (5 points) Write an additionalx
; procedure (stats player n) to help estimate, empirically,
; the average final score of the
; random player and the probability
; that it "shuts the box" over n iterations.

; (stats player n)
; Run a player n times and report
; a list containing the average score and
; the number of "shut the box" results divided
; by n.

; Some sample results (the precise values you produce may vary)
; (stats random-player 10000) => '(20.3917 0.0214)
; (stats random-player 10000) => '(20.5616 0.0212)
; Thus the average score is about 20, and the
; probability of shutting the box is about .02

; You may wish to use the procedure exact->inexact to improve the
; readability of your output. Note that doing so will have no affect
; on your autograded results.


(define play-multiple
  (lambda (n count proc)
    (cond ((= n 0) (list count))
          (else (let ((x (proc))) (if (= x 0) (play-multiple (- n 1) (+ count 1) proc) (cons x (play-multiple (- n 1) count proc ))))))))

(define stats
  (lambda (player n)
    (let ((lst (reverse (play-multiple n 0 player))))
      (list (exact->inexact (/ (apply + (cdr lst)) n)) (exact->inexact(/ (car lst) n))))))

; *********************************************************************

; ** problem 6 ** (11 points) 
; Write a procedure (better-player)
; that plays shut-the-box *more cleverly*
; than random-player does (problem 5).
; The (better-player) uses a greedy strategy.
; This player prefers a move with a larger maximum value,
; or if maxima are equal, larger second largest, etc.

; For example, if there are two possible moves (1 5) and (6),
; the greedy strategy will select (6) since 6 > 5.
; If there are two possible moves (1 2 6) and (3 6), the greedy
; strategy will select (3 6) because 6 = 6 but 3 > 2.

; Here are stats on this better player
; for three runs of (stats better-player 10000):
; (stats better-player 10000) => (11.4256 0.0682)
; (stats better-player 10000) => (11.4343 0.065)
; (stats better-player 10000) => (11.3784 0.07)
; So the average score is about 11.3 and the
; probability of shutting the box is about .07



(define compare-two-lsts
  (lambda (lst1 lst2)
    (let ((x (reverse lst1)) (y (reverse lst2)))
      (cond ((or(null? lst1) (null? lst2)) '())
            ((> (car x) (car y)) lst1)
            ((< (car x) (car y)) lst2)
            (else (if (< (length x) (length y)) lst1 lst2))))))

(define best-in-list
  (lambda (lst best)
    (cond ((null? lst) best)
          (else (best-in-list (cdr lst) (compare-two-lsts best (car lst)))))))

(define play-better-game
  (lambda (game)
    (let ((x (roll-dice)))
      (cond ((= (length (possible-moves x (game 'up-flappers))) 0) (apply + (game 'up-flappers)))
            (else (game 'make-move! (best-in-list (possible-moves x (game 'up-flappers)) (car (possible-moves x (game 'up-flappers)))))  (play-better-game game))))))


(define better-player
    (lambda ()
      (let ((game (make-box)))
        (play-better-game game))))

; *********************************************************************

; ** problem 7 ** (11 points)
; Write a procedure
; (match better-player random-player n)
; to play two players against each other
; for the shut-the-box n times, and report
; the (empirical) results of playing 
; your better-player (problem 6)
; against random-player (problem 5).
; Specifically, match should return the probabilities of better-player winning,
; random-players winning, and ties.


; Here are some examples:
; (match better-player random-player 1000) => (0.760 0.214 0.026)
; Explanation: after playing this game 1000 times, better-player wins 760 times,
; random-player wins 214 times, and they tie 26 times. So the probabilities are
; 0.760, 0.214 and 0.026 respectively.
; (match better-player random-player 1000) => (0.769 0.205 0.026)
; (match better-player random-player 1000) => (0.751 0.221 0.028)



(define match-helper
  (lambda (better-player random-player n wins loss tie)
    (cond ((= n 0) (list wins loss tie))
          (else (let ((x (better-player)) (y (random-player))) (cond ((< x y) (match-helper better-player random-player (- n 1) (+ wins 1) loss tie))
                                                                     ((> x y) (match-helper better-player random-player (- n 1)  wins (+ loss 1) tie))
                                                                     (else (match-helper better-player random-player (- n 1)  wins loss (+ tie 1)))))))))

(define match
  (lambda (better-player random-player n)
    (let ((lst (match-helper better-player random-player n 0 0 0)))
      (list (exact->inexact (/ (car lst) n)) (exact->inexact (/ (car (cdr lst)) n)) (exact->inexact (/ (car (cdr (cdr lst))) n))))))


; *********************************************************************

; ** problem 8 **
; (10 points) Write a procedure (make-cycle! lst n)
; that takes a nonempty list lst
; and modifies it so that instead of containing (),
; the cdr of the last cons cell in lst contains a pointer back
; to the nth element of the list, using 0-based indexing.
; It should return the value #t, NOT the modified "list".
; You may assume that lst is nonempty and
; n is between 0 and the length of the list minus 1.

; Example
; (define lst '(a b c))
; (make-cycle! lst 1) => #t
; (list-ref lst 0) => a
; (list-ref lst 1) => b
; (list-ref lst 2) => c
; (list-ref lst 3) => b
; (list-ref lst 4) => c


; *********************************************************************


(define make-cycle-helper
  (lambda (lst n)
    (cond ((= n 0) lst)
          ((null? lst) '())
          (else (cdr (make-cycle-helper lst (- n 1)))))))

(define make-cycle!
  (lambda (lst n)
    (if (null? lst) (set! lst lst)
                    (set-cdr! (make-cycle-helper lst (- (length lst) 1)) (make-cycle-helper lst n))) #t))


; *********************************************************************

; ** problem 9 ** (11 points)
; Write a procedure (is-cycle? lst)
; that returns #t if lst could have
; been created by calling make-cycle! on
; a list, and #f otherwise.  You
; may assume that lst is either an ordinary
; Racket list or was created by calling
; make-cycle! on a list.

; Examples
; (is-cycle? '()) => #f
; (is-cycle? '(a b c)) => #f
; (define lst1 '(a b c d))
; (make-cycle! lst1 2) => #t
; (is-cycle? lst1) => #t

(define is-cycle?
  (lambda (lst)
    (cond ((null? lst) #f)
          (else (not (list? (cdr lst)))))))

; *********************************************************************
