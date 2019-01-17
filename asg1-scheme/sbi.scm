#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/racket/bin/mzscheme -qr

;; $Id: sbi.scm,v 1.5 2019-01-04 17:04:42-08 - - $
;;
;;  NAME
;;      sbi.scm - silly basic interpreter
;;
;;       SYNOPSIS
;;           sbi.scm filename.sbir
;;
;;            DESCRIPTION
;;                The file mentioned in argv[1] is read and assumed to be an SBIR
;;                    program, which is the executed.  Currently it is only printed.

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


(define (write-program-by-line filename program)
    (printf "==================================================~n")
    (printf "~a: ~s~n" *run-file* filename)
    (printf "==================================================~n")
    (printf "(~n")
    (map (lambda (line) (printf "~s~n" line)) program)
    (printf ")~n")
    (dump-stdin)
)

(define (find-statement filename program)
	(map (lambda (line) (statement-exist line)) program)
)

(define (statement-exist line)
	(if (null? (cdr line)) ;if no label or statement
                (void) ;do nothing
                (if (eqv? (car(car(cdr line))) 'print) ;if print is statement
                        (print-stmt (cdr(car(cdr line))));print what follows ;(print-stmt (cdr line))
                        (if (eqv? (car(car(cdr line))) 'let)
				(let-stmt (cdr(car(cdr line))))
				(void))
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
		(printf "~a~n" (car arithmeticcmd))
		(do-arithmetic arithmeticcmd) ;else do arithmetic operation
	)
)

(define (do-arithmetic evalarithmetic)
        (printf "~a" (car evalarithmetic)) ;print equation
	(if (not (pair? (car(cdr evalarithmetic))))
		(printf "~a~n" (variable-get (car(cdr evalarithmetic))))
        	(printf "~a~n" (evaluate-expression (car(cdr evalarithmetic))))
	)
)

(define (let-stmt letcmd) ;;;;;;;;;;;;;;;;;;;;;
        (if (null? (cdr letcmd)) ;if no arithmetic after let
                (variable-put! (car letcmd) (car letcmd)) ;let arg that comes right after let statement be a variable in variable-table
                (variable-put! (car letcmd) (evaluate-expression (car(cdr letcmd))))
	)
)                

(define (evaluate-expression expr)
        (if (number? expr) ;if expr is number
                (+ 0 expr) ;return expr
                (if (pair? (cdr(cdr expr))) ;if e1 and e2 exist
                        (evaluate-expression ((function-get (car expr)) (evaluate-expression (car(cdr expr))) (evaluate-expression (car(cdr(cdr expr))))))
                        (evaluate-expression ((function-get (car expr)) (evaluate-expression (car(cdr expr))))) ;else only e1 exists
		)
	)
)

(define (create-label-table filename program)
	(map (lambda (line) (fill-label-table line)) program)
)

(define (fill-label-table line)
	(cond ((null? (cdr line)) ;no label, no stastement
        	(void)) ;don't put anything in label-table
		((pair? (cadr line)) ;no label, yes statement
              	(void)) ;don't put anything in label table
        (else 
             (hash-set! *label-table* (cadr line) line))) ;put label in label table
)

(define *label-table* (make-hash))

(define *function-table* (make-hash))
(define (function-get key)
	(hash-ref *function-table* key))
(define (function-put! key value)
	(hash-set! *function-table* key value))

(for-each
	(lambda (pair)
		(function-put! (car pair) (cadr pair)))
	`(
		(*	,*)
		(/	,(lambda (x y) (/ (+ x 0.0) (+ y 0.0))))
		(%	,(lambda (x y) (- x (* (div x y) y))))
		(^	,expt)
		(+	,+)
		(-	,-)
		(abs	,abs)
		(acos	,acos)
		(asin	,asin)
		(atan	,atan)
		(ceil	,ceiling)
		(cos	,cos)
		(exp	,exp)
		(floor	,floor)
		(log	,log)
		(log10	,(lambda (x) (/ (log x) (log 10.0))))
		(log2	,(lambda (x) (/ (log x) (log 2.0))))
		(round	,round)
		(sin	,sin)
		(sqrt	,sqrt)
		(tan	,tan)
		(trun	,truncate)
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
        '(
		(nan	,(/ 0.0 0.0))
		(eof	,0.0)
		(pi	,acos -1.0)
		(e	,(exp 1.0))
        )
)

(define (main arglist)
    (if (or (null? arglist) (not (null? (cdr arglist))))
        (usage-exit)
        (let* ((sbprogfile (car arglist))
                (program (readlist-from-inputfile sbprogfile)))
                (create-label-table sbprogfile program)
		(find-statement sbprogfile program)))
)

;(when (terminal-port? *stdin*)
	(main (vector->list (current-command-line-arguments)))
;)      

