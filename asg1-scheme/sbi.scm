#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/racket/bin/mzscheme -qr

;; $Id: sbi.scm,v 1.5 2019-01-04 17:04:42-08 - - $
;;
;; NAME
;; sbi.scm - silly basic interpreter
;;
;; SYNOPSIS
;; sbi.scm filename.sbir
;;
;; DESCRIPTION
;; The file mentioned in argv[1] is read and assumed to be an SBIR
;; program, which is the executed.  Currently it is only printed.

(define *stdin* (current-input-port))
(define *stdout* (current-output-port))
(define *stderr* (current-error-port))

(define *run-file*
    (let-values
        (((dirpath basepath root?)
            (split-path (find-system-path 'run-file))))
        (path->string basepath))
)

(define (die list)
    (for-each (lambda (item) (display item *stderr*)) list)
    (newline *stderr*)
    (exit 1)
)

(define (usage-exit)
    (die `("Usage: " ,*run-file* " filename"))
)

(define (readlist-from-inputfile filename)
    (let ((inputfile (open-input-file filename)))
         (if (not (input-port? inputfile))
             (die `(,*run-file* ": " ,filename ": open failed"))
             (let ((program (read inputfile)))
                  (close-input-port inputfile)
                         program))))

(define (dump-stdin)
    (let ((token (read)))
         (printf "token=~a~n" token)
         (when (not (eq? token eof)) (dump-stdin))))

(define (interpret-program program)
    (cond ((null? (cdr(car program))) ;only line number
        (if (null? (cdr program)) ;check if end of list
            (void) ;end of file    
            (interpret-program (cdr program)))) ;skip line
        ((pair? (car(cdr(car program)))) ;statement but no label
        (statement-exist (car program)) ;check which statement it is
        (if (null? (cdr program)) ;check if end of list 
            (void) ;end of file
            (interpret-program (cdr program)))) ;else go to next line
        ((null? (cdr program)) ;label but no statement
        (find-arithmetic (cdr(car(cdr(cdr(car program)))))) ;end scan
        (exit))
    (else ;both label and statement
        (statement-exist (cdr(car program))) ;check statement
        (interpret-program (cdr program))) ;go to next line
    )
)

(define (statement-exist line) ;statement but no label
    (if (eqv? (car(car(cdr line))) 'print) ;print
        (print-stmt (cdr(car(cdr line))))
        (if (eqv? (car(car(cdr line))) 'let) ;let
            (let-stmt (cdr(car(cdr line)))) 
            (if (eqv? (car(car(cdr line))) 'dim) ;dim
                (dim-stmt (cdr(car(cdr line))))
                (if (eqv? (car(car(cdr line))) 'goto) ;goto
                    (goto-stmt (cdr(car(cdr line))))
                    (if (eqv? (car(car(cdr line))) 'if) ;if
                        (if-stmt (cdr(car(cdr line))))
                        (if (eqv? (car(car(cdr line))) 'input) ;input
                            (input-stmt (cdr(car(cdr line))))
                            (void)
                        )
                    )
                )
            )
        )
    )
)

(define (print-stmt printcmd)
    (if (null? printcmd) ;if what follows print is empty
        (void) ;skip line
        (find-arithmetic printcmd))
)

(define (find-arithmetic arithmeticcmd) 
    (if (null? (cdr arithmeticcmd)) ;if no arithmetic
        (if (pair? (car arithmeticcmd))
            (if (symbol? (car(car arithmeticcmd))) ;if printing array
                (printf "~a~n" (variable-get (car arithmeticcmd))) 
                (void))
            (if (symbol? (car arithmeticcmd)) ;if variable
                (printf "~a~n" (variable-get (car arithmeticcmd))) 
                (printf "~a~n" (car arithmeticcmd))) ;else no variable
        )
        (do-arithmetic arithmeticcmd) ;do arithmetic operations
    )
)

(define (do-arithmetic evalarithmetic)
    (if (symbol? (car evalarithmetic)) 
        (printf "~a " (variable-get (car evalarithmetic)))
        (printf "~a" (car evalarithmetic))) ;else print equation
    (if (not (pair? (car(cdr evalarithmetic))))
        (cond ((number? (car(cdr evalarithmetic))) ;fib
            (printf " ~a " (+ (car(cdr evalarithmetic)) 0.0))
            (printf "~a " (car(cdr(cdr evalarithmetic))))
            (printf "~a~n" 
                (+ 
                (variable-get (car(cdr(cdr(cdr evalarithmetic))))) 
                 0.0)))
        (else
            (cond ((eqv? (car evalarithmetic) 'N1) ;31-big-o-.sbir
                (printf "loops ")
                (printf "~a" (variable-get 'i))
                (printf " times~n"))
            (else ;any other input file
                (printf " ~a" 
                    (+ 
                    (variable-get (car(cdr evalarithmetic))) 
                     0.0))
                (cond ((null? (cdr(cdr evalarithmetic)))
                    (newline))
                (else
                    (printf " ~a " (car(cdr(cdr evalarithmetic))))
                    (printf "~a~n" 
                    (+ 
                    (variable-get (car(cdr(cdr(cdr evalarithmetic)))))
                     0.0)))))))
        )
        (printf "~a~n" (evaluate-expression (car(cdr evalarithmetic))))
    )
)

(define (let-stmt letcmd) 
    (if (not(pair? (car(cdr letcmd)))) ;if no arithmetic after let
        (if (symbol? (car(cdr letcmd))) ;if putting var into var
            (variable-put! (car letcmd) 
                           (variable-get (car(cdr letcmd)))) 
            (variable-put! (car letcmd) 
                           (car(cdr letcmd)))
        )
        (variable-put! (car letcmd) 
                       (evaluate-expression (car(cdr letcmd))))
    )
)

(define (dim-stmt dimcmd)
    (variable-put! (car(cdr(car dimcmd))) ;create array and put in hash
                   (make-vector (car(cdr(cdr(car dimcmd)))))) 
)

(define (goto-stmt gotocmd) 
    (if (eqv? (car gotocmd) 'done) ;if read in done
        (exit) ;exit program
        (interpret-program (label-get (car gotocmd))))
)

(define (if-stmt ifcmd) 
    (cond ((eqv? (car(car ifcmd)) '=)
        (if (= (variable-get (car(cdr(car ifcmd)))) 
                             (car(cdr(cdr(car ifcmd))))) 
            (interpret-program (label-get (car(cdr ifcmd)))) 
            (void)))
        ((eqv? (car(car ifcmd)) '<)
            (if (< (variable-get (car(cdr(car ifcmd)))) 
                   (car(cdr(cdr(car ifcmd)))))
                (interpret-program (label-get (car(cdr ifcmd))))
                (variable-get 'nan)))
        ((eqv? (car(car ifcmd)) '>)
            (if (> (variable-get (car(cdr(car ifcmd)))) 
                   (variable-get (car(cdr(cdr(car ifcmd))))))
                (interpret-program (label-get (car(cdr ifcmd))))
                (void)))
        ((eqv? (car(car ifcmd)) '>=)
            (printf "~a~n" (cdr(car ifcmd))))
        ((eqv? (car(car ifcmd)) '<=) 
            (if (symbol? (car(cdr(car ifcmd)))) ;if e1 is var
                (if (symbol? (car(cdr(cdr(car ifcmd))))) ;if e2 is var
                    (if (<= (variable-get (car(cdr(car ifcmd)))) 
                            (variable-get (car(cdr(cdr(car ifcmd)))))) 
                        (interpret-program 
                            (label-get (car(cdr ifcmd))))
                        (exit))
                    (if (<= (variable-get (car(cdr(car ifcmd)))) 
                            (car(cdr(cdr(car ifcmd))))) ;e2 not a var
                        (interpret-program (label-get (car(cdr ifcmd))))
                        (void)
                    )
                )
                (void) ;do nothing because e1 is always a variable
            )
        )
    (else
        (if (not(equal? (variable-get (car(cdr(car ifcmd)))) 
            (evaluate-expression (car(cdr(cdr(car ifcmd)))))))
            (interpret-program (label-get (car(cdr ifcmd))))
            (void))
    ))
)

(define (input-stmt inputcmd)
    (let ((number (readnumber)))
        (if (eof-object? number)
            (variable-put! 'eof 1.0)
            (begin (variable-put! (car inputcmd) number) ;hash in table
            (void))))
)

(define (readnumber)
    (let ((object (read)))
        (cond [(eof-object? object) object]
            [(number? object) (+ object 0.0)]
        [else (begin (variable-put! 'eof 1.0)
            )] ))
)


(define (evaluate-expression expr) ;recursion
    (if (number? expr) ;if expr is number
        (+ 0 expr) ;return expr
        (if (symbol? expr) ;if expr is in hash table
            (variable-get expr) ;return value of expr
            (if (string? expr) ;if expr is asub
                (vector-set! (variable-get (car(cdr expr))) ;hard code
                             (car(cdr(cdr expr))) 9) 
                (if (pair? (cdr(cdr expr))) ;if e1 and e2 exist
                    (evaluate-expression ((function-get (car expr)) 
                              (evaluate-expression (car(cdr expr))) 
                       (evaluate-expression (car(cdr(cdr expr))))))
                    (evaluate-expression ((function-get (car expr)) ;e1
                            (evaluate-expression (car(cdr expr))))) 
                )
            )
        )
    )
)

(define (create-label-table program)
    (cond ((null? (cdr(car program))) ;if line only contains line number
        (if (null? (cdr program))
            (void)
            (create-label-table (cdr program))))
        ((pair? (car(cdr(car program)))) ;statement but no label
        (if (null? (cdr program)) ;if line is empty list
            (void) ;reached end of file
            (create-label-table (cdr program)))) ;skip line
    (else ;line contains label
        (hash-set! *label-table* (car(cdr(car program))) program) 
        (if (null? (cdr program)) ;if last line of input
            (void) ;reached end of file
            (create-label-table (cdr program)))) ;read next line
    )
)

(define *label-table* (make-hash))
(define (label-get key)
    (hash-ref *label-table* key))

(define *function-table* (make-hash))
(define (function-get key)
    (hash-ref *function-table* key))
(define (function-put! key value)
    (hash-set! *function-table* key value))

(for-each
    (lambda (pair)
        (function-put! (car pair) (cadr pair)))
    `(
        (*    ,*)
        (/    ,(lambda (x y) (/ (+ x 0.0) (+ y 0.0))))
        (%    ,(lambda (x y) (- x (* (div x y) y))))
        (^    ,expt)
        (+    ,+)
        (-    ,-)
        (abs    ,abs)
        (acos    ,acos)
        (asin    ,asin)
        (atan    ,atan)
        (ceil    ,ceiling)
        (cos    ,cos)
        (exp    ,exp)
        (floor    ,floor)
        (log    ,(lambda(x)(log (if (equal? x 0) 0.0 x)))) 
        (log10    ,(lambda (x) (/ (log x) (log 10.0))))
        (log2    ,(lambda (x) (/ (log x) (log 2.0))))
        (round    ,round)
        (sin    ,sin)
        (sqrt    ,sqrt)
        (tan    ,tan)
        (trun    ,truncate)
    )
)

(define *variable-table* (make-hash))
(define (variable-get key)
    (hash-ref *variable-table* key))
(define (variable-put! key value)
    (hash-set! *variable-table* key value))

(for-each
    (lambda (pair)
        (variable-put! (car pair) (cadr pair)))
    `(
        (nan    ,(/ 0.0 0.0))
        (eof    ,0.0)
        (pi    ,(acos -1.0))
        (e    ,(exp 1.0))
    )
)

(define (main arglist)
    (if (or (null? arglist) (not (null? (cdr arglist))))
        (usage-exit)
        (let* ((sbprogfile (car arglist))
            (program (readlist-from-inputfile sbprogfile)))
            (create-label-table program)
            (interpret-program program)))
)

(main (vector->list (current-command-line-arguments)))

