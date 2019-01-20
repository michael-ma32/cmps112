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
		(void)) ;end scan
	(else ;both label and statement
		(statement-exist (cdr(car program))) ;check which statement it is
		(interpret-program (cdr program))) ;go to next line
	)
)

(define (statement-exist line) ;only works when there's a statement but no label
	;(if (null? (cdr line)) ;if no label or statement
                ;(void) ;do nothing
                (if (eqv? (car(car(cdr line))) 'print) ;if print is statement
                        (print-stmt (cdr(car(cdr line))));print what follows ;(print-stmt (cdr line))
                        (if (eqv? (car(car(cdr line))) 'let) ;if let is statement
				(let-stmt (cdr(car(cdr line)))) ;;;;;;;;;;;;;;
				(if (eqv? (car(car(cdr line))) 'dim) ;if dim is statement
					(dim-stmt (cdr(car(cdr line))))
					(if (eqv? (car(car(cdr line))) 'goto) ;if goto is statement
						(goto-stmt (cdr(car(cdr line))))
						(if (eqv? (car(car(cdr line))) 'if) ;if if is statement
							(if-stmt (cdr(car(cdr line))))
							(if (eqv? (car(car(cdr line))) 'input)
								(input-stmt (cdr(car(cdr line))))
								(void)
							)
						)
					)
				)
			)
                )
        ;)
)

(define (print-stmt printcmd)
	(if (null? printcmd) ;if what follows print is empty
		(void) ;skip line
		(find-arithmetic printcmd))
)

(define (find-arithmetic arithmeticcmd) 
	(if (null? (cdr arithmeticcmd)) ;if no arithmetic, print out simple statement or variable stored in hash table
		(if (pair? (car arithmeticcmd))
			(if (symbol? (car(car arithmeticcmd))) ;if printing array element
				(printf "~a~n" (variable-get (car arithmeticcmd))) ;print array element
				(void))
			(if (symbol? (car arithmeticcmd)) ;if variable
				(printf "~a~n" (variable-get (car arithmeticcmd))) ;print variable from hash table
				(printf "~a~n" (car arithmeticcmd))) ;else print simple statement
		)
		(do-arithmetic arithmeticcmd) ;do arithmetic operations
	)
)

(define (do-arithmetic evalarithmetic)
	;(printf "~a~n" (car evalarithmetic)) ;fib(
	;(printf "~a~n" (car(cdr evalarithmetic))) ;0
	;(printf "~a~n" (car(cdr(cdr evalarithmetic))));)=
	;(printf "~a~n" (car(cdr(cdr(cdr evalarithmetic))))) ;fib0
	(if (symbol? (car evalarithmetic)) 
		(printf "~a " (variable-get (car evalarithmetic)))
        	(printf "~a" (car evalarithmetic))) ;else print equation
	(if (not (pair? (car(cdr evalarithmetic))))
		(cond ((number? (car(cdr evalarithmetic))) ;fib
			(printf " ~a " (+ (car(cdr evalarithmetic)) 0.0))
			(printf "~a " (car(cdr(cdr evalarithmetic))))
			(printf "~a~n" (+ (variable-get (car(cdr(cdr(cdr evalarithmetic))))) 0.0)))
		(else 
			(printf " ~a" (+ (variable-get (car(cdr evalarithmetic))) 0.0))
			(cond ((null? (cdr(cdr evalarithmetic)))
				(newline))
			(else
				(printf " ~a " (car(cdr(cdr evalarithmetic))))
				(printf "~a~n" (+ (variable-get (car(cdr(cdr(cdr evalarithmetic))))) 0.0)))))
		)
        	(printf "~a~n" (evaluate-expression (car(cdr evalarithmetic))))
	)
)

(define (let-stmt letcmd) 
        (if (not(pair? (car(cdr letcmd)))) ;if no arithmetic after let
		(if (symbol? (car(cdr letcmd))) ;if putting variable into let variable
			(variable-put! (car letcmd) (variable-get (car(cdr letcmd)))) ;let arg that comes right after let statement be a variable in variable-table
			(variable-put! (car letcmd) (car(cdr letcmd)))
		)
              	(variable-put! (car letcmd) (evaluate-expression (car(cdr letcmd))))
	)
	;(printf "~a~n" (pair? (car(cdr letcmd)))) f t t
	;(printf "~a~n" (car(cdr letcmd)))
) 

(define (dim-stmt dimcmd)
	(variable-put! (car(cdr(car dimcmd))) (make-vector (car(cdr(cdr(car dimcmd)))))) ;make array and put in function table
)

(define (goto-stmt gotocmd) ;;;;;;;;;;
	(if (eqv? (car gotocmd) 'done) ;if read in done
		(exit) ;exit program
		(interpret-program (label-get (car gotocmd))))
)

(define (if-stmt ifcmd) ;;;;;;;;;;;;;;;;;;;;;;;;;;;; finish cases for remaining operations
	(cond ((eqv? (car(car ifcmd)) '=)
		;(printf "~a~n" (car(cdr(car ifcmd)))) ;tennessee
		;(printf "~a~n" (car(cdr(cdr(car ifcmd))))) ;0
		(if (= (variable-get (car(cdr(car ifcmd)))) (car(cdr(cdr(car ifcmd))))) ;if tennessee = 0
			(interpret-program (label-get (car(cdr ifcmd)))) ;prt => go to prt label
			(void)))
		;(printf "~a~n" (cdr(car ifcmd))))
		((eqv? (car(car ifcmd)) '<)
		(printf "~a~n" (cdr(car ifcmd))))
		((eqv? (car(car ifcmd)) '>)
                (printf "~a~n" (cdr(car ifcmd))))
		((eqv? (car(car ifcmd)) '<>)
                (printf "~a~n" (cdr(car ifcmd))))
		((eqv? (car(car ifcmd)) '>=)
                (printf "~a~n" (cdr(car ifcmd))))
		((eqv? (car(car ifcmd)) '<=) ;;;;;;;;;;;;;;;;;
			(if (symbol? (car(cdr(car ifcmd)))) ;if e1 is variable
				(if (symbol? (car(cdr(cdr(car ifcmd))))) ;if e2 is also variable
					(if (<= (variable-get (car(cdr(car ifcmd)))) (variable-get (car(cdr(cdr(car ifcmd)))))) ;if e1 leq e2
						(interpret-program (label-get (car(cdr ifcmd))))
						(exit))
					(if (<= (variable-get (car(cdr(car ifcmd)))) (car(cdr(cdr(car ifcmd))))) ;else e2 not a variable and if e1 variable leq e2
						(interpret-program (label-get (car(cdr ifcmd))))
						(exit)
					)
				)
				(void) ;do nothing because e1 is always a variable
			)
		)
	)
)

(define (input-stmt inputcmd)
	;(printf "~a~n" (car inputcmd)) ; tennessee
	(let ((number (readnumber)))
             (if (eof-object? number)
                 (printf "*EOF* ~a~n" number)
                 (begin (variable-put! (car inputcmd) number) ;store in variable table
                        (void))))
)

(define (readnumber)
        (let ((object (read)))
             (cond [(eof-object? object) object]
                   [(number? object) (+ object 0.0)]
                   [else (begin (printf "invalid number: ~a~n" object)
                                (readnumber))] )) 
)


(define (evaluate-expression expr) ;recursion
        (if (number? expr) ;if expr is number
                (+ 0 expr) ;return expr
		(if (symbol? expr) ;if expr is in hash table
			(variable-get expr) ;return value of expr
			(if (string? expr) ;if expr is asub
				(vector-set! (variable-get (car(cdr expr))) (car(cdr(cdr expr))) 9) ;hard code return vector element 9
                		(if (pair? (cdr(cdr expr))) ;if e1 and e2 exist
                        		(evaluate-expression ((function-get (car expr)) (evaluate-expression (car(cdr expr))) (evaluate-expression (car(cdr(cdr expr))))))
                        		(evaluate-expression ((function-get (car expr)) (evaluate-expression (car(cdr expr))))) ;else only e1 exists		
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
		((pair? (car(cdr(car program)))) ;if line contains statement but no label
		(if (null? (cdr program)) ;if line is empty list
			(void) ;reached end of file
			(create-label-table (cdr program)))) ;skip line
		((null? (cdr program)) ;if line contains label but no statement
		(void)) ;reached end of file
	(else ;lines contains line number, label, and statement
		;(printf "~s~n" program)
		;(hash-set! *label-table* (car(cdr(car program))) (append (car(cdr(cdr(car program)))) (cdr program)) ) ;hash label as key, everything after it as value
		;(hash-set! *label-table* (car(cdr(car program))) (append (car program) (cdr program)))
		(hash-set! *label-table* (car(cdr(car program))) program) ;hash whole line and rest of the program into table
		(create-label-table (cdr program))) ;read next line
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
		(log	,(lambda(x)(log (if (equal? x 0) 0.0 x)))) 
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
        `(
		(nan	,(/ 0.0 0.0))
		(eof	,0.0)
		(pi	,(acos -1.0))
		(e	,(exp 1.0))
        )
)

(define (main arglist)
    (if (or (null? arglist) (not (null? (cdr arglist))))
        (usage-exit)
        (let* ((sbprogfile (car arglist))
                (program (readlist-from-inputfile sbprogfile)))
                (create-label-table program)
		;(find-statement sbprogfile program)))
		(interpret-program program)))
)

;(when (terminal-port? *stdin*)
	(main (vector->list (current-command-line-arguments)))
;)      

