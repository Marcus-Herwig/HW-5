;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |HW 5|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

(define resolve
  (lambda (varName env)
    (cond
      ((null? env) #f)
      ((eq? varName (caar env)) (cadar env))
      (else (resolve varName (cdr env))))))


(define do-mathy-stuff-toaster
  (lambda (op num1 num2)
    (cond
      ((eq? op '+) (+ num1 num2))
      ((eq? op '-) (- num1 num2))
      ((eq? op '/) (/ num1 num2))
      ((eq? op '//) (quotient num1 num2))
      ((eq? op '%) (modulo num1 num2))
      ((eq? op '*) (* num1 num2))
      (else #f))))

(define no-parser
  (lambda (no-code)
    (cond
      ((null? no-code) '())
      ((number? no-code) (list 'num-lit-exp no-code))
      ((symbol? no-code) (list 'var-exp no-code))
       ((eq? (car no-code) 'do-mathy-stuff)
       (list(append '(math-exp) (list(cadr no-code)) (list(no-parser (caddr no-code))) (list(no-parser (cadddr no-code))))))
      ((eq? (car no-code) 'function)
       (list(append '(func-exp)
             (list(append  '(params) (cadr no-code))
             (list 'body
                   (no-parser (cdr no-code)))))))
      ((eq? (car no-code) 'call) (append '(call-exp) (append  (no-parser(cadr no-code))
                             (no-parser (cdr no-code)))))
      (else(if(null? (cdr no-code))
                     '()
                     (append  (no-parser(cadr no-code))
                             (no-parser (cdr no-code))))))))
    
(define env '((age 21) (a 7) (b 5) (c 23)))
(define sample-no-code '(call (function (x y z)x y z)(do-mathy-stuff + a b) c b))
(define sample-no-code2 '(call (function ())()))

(define run-parsed-code
  (lambda (parsed-no-code env)
    (cond
      ((eq? (car parsed-no-code) 'num-lit-exp)
       (cadr parsed-no-code))
      ((eq? (car parsed-no-code) 'var-exp)
       (resolve (cadr parsed-no-code) env)) 
      ((eq? (car parsed-no-code) 'math-exp)
       (do-mathy-stuff-toaster
        (cadr parsed-no-code)
        (run-parsed-code (caddr parsed-no-code) env)
        (run-parsed-code (cadddr parsed-no-code) env)))
      ((eq? (car parsed-no-code) 'func-exp)
       (run-parsed-code (cadr (caddr parsed-no-code)) env))
      (else
       (run-parsed-code
        (cadr parsed-no-code)
        (cons (list (cadr (cadr (cadr parsed-no-code)))
                    (run-parsed-code (caddr parsed-no-code) env))
              env))))))
 sample-no-code
(no-parser sample-no-code)
(run-parsed-code (no-parser sample-no-code) env)
(display '(---------- Example 2 (no parameters) ----------))

(no-parser sample-no-code2)


      

