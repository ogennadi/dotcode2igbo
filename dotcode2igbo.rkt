#!/usr/bin/racket
#lang racket
(require rackunit)
;; Program that converts dotcode to Unicode characters.

(define dotcode-post '(



           ("\\.e" "ẹ̀")
           ("\\.i" "ị̀")
           ("\\.o" "ọ̀")
           ("\\.u" "ụ̀")
           ("\\.n" "ṅ̀")
           
           ("/.e" "ẹ́")
           ("/.i" "ị́")
           ("/.o" "ọ́")
           ("/.u" "ụ́")
           ("/.n" "ṅ́")
           ("/+" "ǝ́")

           ("-.e" "ẹ̄")
           ("-.i" "ị̄")
           ("-.o" "ọ̄")
           ("-.u" "ụ̄")
           ("-.n" "ṅ̄")
           
           (".." ".")
           ("==" "=")
           ("++" "+")
           ("--" "-")
           ("//" "/")

           ("/+" "ǝ́")
           ("/a" "á")
           ("/e" "é")
           ("/i" "í")
           ("/m" "ḿ")
           ("/n" "ń")
           ("/o" "ó")
           ("/u" "ú")

           ("\\+" "ǝ̀")
           ("\\a" "à")
           ("\\e" "è")
           ("\\i" "ì")
           ("\\o" "ò")
           ("\\u" "ù")
           ("\\m" "m̀")
           ("\\n" "ǹ")

           ("-+" "ǝ̄")
           ("-a" "ā")
           ("-e" "ē")
           ("-i" "ī")
           ("-m" "m̄")
           ("-n" "n̄")
           ("-o" "ō")
           ("-u" "ū")
           
           (".e" "ẹ")
           (".i" "ị")
           (".o" "ọ")
           (".u" "ụ")
           (".n" "ṅ")
           ("=n" "₦")
           
           ("+" "ǝ")))

;;
;(define (->unicode2 string)
;  (foldl proc
;         string
;         table))
;
;(define (proc replacement-pair string)
;  (regexp-replace* (regexp-quote (car replacement-pair))
;                   string
;                   (cadr replacement-pair)
;  ))
;
;;;
;(define (->unicode1 string)
;  (regexp-replace token-regexp string replace))
;
;(define token-regexp
;  (string-join (map (compose regexp-quote car) table) "|"))
;
;(define (replace string) (if string
;                             (cadr (assoc string table) )
;                             ""))
;

;;
(define (->unicode3 string table)
 (string-join (map (lambda (token) (token->unicode token table))
                   (tokenize string table))
              ""))

(define (tokenize string table)
  (reverse (helper (map car table) string '())))

(define (helper tokens string parsed)
  (if (equal? string "")
      parsed
      (let ([match (findf (lambda (token) (string-prefix? string token)) tokens)])
        (if match
            (helper tokens (string-replace string match "" #:all? #f) (cons match parsed))
            (helper tokens (substring string 1) (cons (substring string 0 1) parsed))))))

(define (token->unicode token token-table)
  (let ([match (assoc token token-table)])
    (if match
        (cadr match)
        token)))

(define ->unicode ->unicode3)

(define (orba string)
  (->unicode string dotcode-post))

;(orba (read-string 100))

;; Main
(displayln
 (orba
  (string-join (vector->list (current-command-line-arguments)))))


;; Tests
(check-equal? (->unicode "abcdefghijklmnopqrstuvwxyz.e.i.o.u.n+=n..==//\\--++" dotcode-post)
              "abcdefghijklmnopqrstuvwxyzẹịọụṅǝ₦.=/\\-+")

(check-equal? (->unicode "/abcd/efgh/ijkl/m/n/opqrst/uvwxyz/.e/.i/.o/.u/.n/+=n..=/\\--++" dotcode-post)
              "ábcdéfghíjklḿńópqrstúvwxyzẹ́ị́ọ́ụ́ṅ́ǝ́₦.=/\\-+")

(check-equal? (->unicode "\\abcd\\efgh\\ijkl\\m\\n\\opqrst\\uvwxyz\\.e\\.i\\.o\\.u\\.n\\+=n..==//\\--++" dotcode-post)
               "àbcdèfghìjklm̀ǹòpqrstùvwxyzẹ̀ị̀ọ̀ụ̀ṅ̀ǝ̀₦.=/\\-+")

(check-equal? (->unicode "-abcd-efgh-ijkl-m-n-opqrst-uvwxyz-.e-.i-.o-.u-.n-+=n..==//\\--++" dotcode-post)
              "ābcdēfghījklm̄n̄ōpqrstūvwxyzẹ̄ị̄ọ̄ụ̄ṅ̄ǝ̄₦.=/\\-+")
;(check-equal? (->unicode "aaeeiioouummnn.." dotcode-post)
 ;             "aaeeiioouummnṅ.")