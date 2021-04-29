#lang racket

(require rackunit)
(require rackunit/text-ui)

(define path "examples/example2.rkt")
(define file (file->lines path))

;Função para o tamanho de uma lista
(define (length lst)
  (cond
    [(empty? lst) 0]
    [(cons? lst) (add1 (length (rest lst)))]))

; Função que verifica se a string pertence a um comentário
(define (isComment str)
  (string-contains? str ";"))

; testes para isComment
(define isComment-tests
  (test-suite
   "isComment-tests"
   (check-equal? (isComment "abc") #f "Esta linha não é um comentário")
   (check-equal? (isComment ";abc") #t "Esta linha é um comentário")
   (check-equal? (isComment "abc;abc") #t "Esta linha é um comentário")))


(define (numberOfComments codeLines)
  (length (filter isComment codeLines)))

; testes para numero de comentários em uma lista
(define numberOfComments-tests
  (test-suite
   "numberOfComments-tests"
   (check-equal? (numberOfComments (list "abc" "abc")) 0)
   (check-equal? (numberOfComments (list ";aaa" "abc;faaa" "abc")) 2)))


; Configura os testes
(define (execute-all-tests . tests)
  (run-tests (test-suite "Executando todos os testes..." tests))
  (void))

; Executa os testes
(execute-all-tests isComment-tests)
(execute-all-tests numberOfComments-tests)