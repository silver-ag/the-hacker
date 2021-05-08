#lang racket

(require "SMOG.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; manage running the game
;;;;

(define (main-menu [current-level 0] [play-immediately? #f])
  (if (equal? current-level (length levels))
      (display "YOU HAVE WON\n")
      (if play-immediately?
          (if (play-level (list-ref levels current-level))
              (main-menu (+ current-level 1) #t)
              (main-menu current-level))
          (let []
            (display (format "// THE HACKER\n\n1) play (current level: ~a)\n2) help\n3) SMOG manual\n4) exit\n\n" current-level))
            (define (get-choice)
              (display "> ")
              (let [[in (read-line)]]
                (case in
                  [("1") (if (play-level (list-ref levels current-level))
                             (main-menu (+ current-level 1) #t)
                             (main-menu current-level))]
                  [("2") (display help) (get-choice)]
                  [("3") (display manual) (get-choice)]
                  [("4") (exit)]
                  [else (display "please choose 1-4\n") (get-choice)])))
            (get-choice)))))

(define (play-level lvl)
  (let/cc cc 
    (run lvl (hash-set
              (hash-set SMOG-standard-functions
                        "escape" (scriptfun (λ () (display "success!\n\n") (cc #t)) '()))
              "give_up" (scriptfun (λ () (cc #f)) '())))
    (display "\nerror encountered, restarting level...\n")
    (play-level lvl)))

;;;;;;;;;;;;;;;;;;
;; define levels
;;;;

(require "level-0.rkt"
         "level-1.rkt"
         "level-2.rkt"
         "level-3.rkt"
         "level-4.rkt")

(define levels (list level-0 level-1 level-2 level-3 level-4))

;;;;;;;;;;;;;;;;;;;;
;; menu text lumps
;;;;

(define help
  (format "* in order to pass each level, you'll need to find and exploit some security issue in order to call the function escape()\n~
           * you can always say 'quit' or 'give up' in levels to return to the main menu\n~
           * SMOG is the language in which levels are written. A manual is available from the menu.\n"))
(define manual
  (format "see the accompanying pdf\n"))

;;;;;;;;;;
;; begin
;;;;

(main-menu 0)