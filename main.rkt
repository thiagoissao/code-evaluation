#lang racket

(require rackunit)
(require rackunit/text-ui)

(define MAX_LINE_LENGTH 70)

(define example1 "examples/example1.rkt")
(define example2 "examples/example2.rkt")

(define path "examples/example1.rkt")
(define testsFile1 (file->lines example1))
(define testsFile2 (file->lines example2))
(define file (file->lines path))

; Função para o tamanho de uma lista
(define (length lst)
  (cond
    [(empty? lst) 0]
    [(cons? lst) (add1 (length (rest lst)))]))

; teste para a quantidade de linhas para o examples/example2.rkt
(define length-tests
  (test-suite
   "length-tests"
   (check-equal? (length testsFile1) 343)
   (check-equal? (length testsFile2) 17)))

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

; Função que verifica se a string pertence a uma definição
(define (isDefine str)
  (string-contains? str "define "))

; Quantidade de definições (define) no código
(define (numberOfDefines codeLines)
  (length (filter isDefine codeLines)))

; testes para número de defines em uma lista
(define numberOfDefines-tests
  (test-suite
   "numberOfDefines-tests"
   (check-equal? (numberOfDefines (list "(define blablabla)" "abc")) 1)
   (check-equal? (numberOfDefines (list "define x" "defines abc" "abc")) 1)))

; Função que retorna um vetor com a quantidade de characteres para cada linha do código
(define (linesLength code)
  (map string-length code))

; Função que retorna a quantidade de linhas no código que ultrapassaram de MAX_LINE_LENGTH characteres
(define (numberOfLargeLines code)
  (length (filter (λ (x) (>= (string-length x) MAX_LINE_LENGTH)) code)))

; Função que testa a quantidade de linhas no código que ultrapassaram de MAX_LINE_LENGTH
; charateres para o examples/example2.rkt
(define numberOfLargeLines-tests
  (test-suite
   "numberOfLargeLines-tests"
   (check-equal? (numberOfLargeLines testsFile1) 6)
   (check-equal? (numberOfLargeLines testsFile2) 0)))

; Função que determina a nota para o número de linhas do código
(define (numberOfLinesGrade code)
  (define n (length code))
    (cond
      [(<= n 300) 10]
      [(and (> n 300) (<= n 400) ) 6]
      [(and (> n 400) (<= n 500) ) 3]
      [else 0]))

; Função que testa a nota para a quantidade de linhas do código
(define numberOfLinesGrade-tests
  (test-suite
   "numberOfLinesGrade-tests"
   (check-equal? (numberOfLinesGrade testsFile1) 6)
   (check-equal? (numberOfLinesGrade testsFile2) 10)))

; Função que determina a nota para o número de comentários do código
(define (commentsGrade code)
  (define nComments (numberOfComments code))
  (define nLines(length code)) 
  (define n (* 100 (/ nComments nLines)))
    (cond
      [(<= n 10) 10]
      [(and (> n 10) (<= n 50) ) 5]
      [(and (> n 50) (<= n 75) ) 2]
      [else 0]))

; Função que testa a nota para a quantidade de comentários do código
(define commentsGrade-tests
  (test-suite
   "commentsGrade-tests"
   (check-equal? (commentsGrade testsFile1) 5)
   (check-equal? (commentsGrade testsFile2) 2)))

; Função que determina a nota para o número de definições do código
(define (numberOfDefinesGrade code)
  (define n (length code))
    (cond
      [(<= n 25) 10]
      [(and (> n 25) (<= n 40) ) 8]
      [(and (> n 40) (<= n 50) ) 6]
      [(and (> n 50) (<= n 60) ) 3]
      [else 2]))

; Função que testa a nota para a quantidade de linhas do código
(define numberOfDefinesGrade-tests
  (test-suite
   "numberOfDefinessGrade-tests"
   (check-equal? (numberOfDefinesGrade testsFile1) 2)
   (check-equal? (numberOfDefinesGrade testsFile2) 10)))

; Função que determina a nota para a quantidade de linhas no
; código que ultrapassaram de MAX_LINE_LENGTH characteres
(define (numberOfLargeLinesGrade code)
  (define n (numberOfLargeLines code))
    (cond
      [(= n 0) 10]
      [else 0]))

; Função que testa a nota para a quantidade de linhas do código
(define numberOfLargeLinesGrade-tests
  (test-suite
   "numberOfLargeLinesGrade-tests"
   (check-equal? (numberOfLargeLinesGrade testsFile1) 0)
   (check-equal? (numberOfLargeLinesGrade testsFile2) 10)))

; Função que obtém a nota final
(define (finalGrade code)
  (define linesGrade (* 5 (numberOfLinesGrade code)))
  (define comtsGrade (* 1 (commentsGrade code)))
  (define definesGrade (* 2 (numberOfDefinesGrade code)))
  (define largeLinesGrade (* 2 (numberOfLargeLinesGrade code)))
  (+ linesGrade comtsGrade definesGrade largeLinesGrade))

; --------------------------------- EXECUÇÃO DOS TESTES ------------------------------------------------

; Configura os testes
(define (execute-all-tests . tests)
  (run-tests (test-suite "Executando todos os testes..." tests))
  (void))
       
; Executa os testes
(execute-all-tests length-tests)
(execute-all-tests isComment-tests)
(execute-all-tests numberOfComments-tests)
(execute-all-tests numberOfDefines-tests)
(execute-all-tests numberOfLargeLines-tests)
(execute-all-tests numberOfLinesGrade-tests)
(execute-all-tests commentsGrade-tests)
(execute-all-tests numberOfDefinesGrade-tests)
(execute-all-tests numberOfLargeLinesGrade-tests)