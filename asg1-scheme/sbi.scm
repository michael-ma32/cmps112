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

(define (find-print filename program)
	(map (lambda (line) (print-exist line)) program)
)

(define (print-exist line)
	; tailrecursion
	; if caddr of line is print
	; use recursion to print out next element if it is a string or number
	(if (null? (cdr line))
		(void)
		(printf "~s~n" (car(cdr(car(cdr line))))))
)

(define (main arglist)
    (if (or (null? arglist) (not (null? (cdr arglist))))
        (usage-exit)
        (let* ((sbprogfile (car arglist))
                (program (readlist-from-inputfile sbprogfile)))
                (find-print sbprogfile program)))
)

;(when (terminal-port? *stdin*)
	(main (vector->list (current-command-line-arguments)))
;)                    
