#lang racket

(require rackunit)
(require rackunit/text-ui)

;;;;;;;;;;;;;;;;;;;;
;; Definição de Ponto

(struct ponto (x y) #:transparent)
;; Ponto representa um ponto no plano cartesiano
;;   x : Número - a coordenada x
;;   y : Número - a coordenada y
;; Exemplos
#; (define p1 (ponto 3 4))
#; (define p2 (ponto 8 2))