#!/usr/bin/racket
#lang racket
(require rackunit)
;; Program that converts dotcode to Unicode characters.

(define dotcode-post '(

           ("\\.e" "ẹ̀")
           ("\\.E" "Ẹ̀")
           ("\\.i" "ị̀")
           ("\\.I" "Ị̀")
           ("\\.o" "ọ̀")
           ("\\.O" "Ọ̀")
           ("\\.u" "ụ̀")
           ("\\.U" "Ụ̀")
           ("\\.n" "ṅ̀")
           ("\\.N" "Ṅ̀")

           ("/.e" "ẹ́")
           ("/.E" "Ẹ́")
           ("/.i" "ị́")
           ("/.I" "Ị́")
           ("/.o" "ọ́")
           ("/.O" "Ọ́")
           ("/.u" "ụ́")
           ("/.U" "Ụ́")
           ("/.n" "ṅ́")
           ("/.N" "Ṅ́")
           ("/+" "ǝ́")

           ("-.e" "ẹ̄")
           ("-.E" "Ẹ̄")
           ("-.i" "ị̄")
           ("-.I" "Ị̄")
           ("-.o" "ọ̄")
           ("-.O" "Ọ̄")
           ("-.u" "ụ̄")
           ("-.U" "Ụ̄")
           ("-.n" "ṅ̄")
           ("-.N" "Ṅ̄")

           (".." ".")
           ("==" "=")
           ("++" "+")
           ("--" "-")
           ("//" "/")

           ("/+" "ǝ́")
           ("/a" "á")
           ("/A" "Á")
           ("/e" "é")
           ("/E" "É")
           ("/i" "í")
           ("/I" "Í")
           ("/m" "ḿ")
           ("/M" "Ḿ")
           ("/n" "ń")
           ("/N" "Ń")
           ("/o" "ó")
           ("/O" "Ó")
           ("/u" "ú")
           ("/U" "Ú")

           ("\\+" "ǝ̀")
           ("\\a" "à")
           ("\\A" "À")
           ("\\e" "è")
           ("\\E" "È")
           ("\\i" "ì")
           ("\\I" "Ì")
           ("\\o" "ò")
           ("\\O" "Ò")
           ("\\u" "ù")
           ("\\U" "Ù")
           ("\\m" "m̀")
           ("\\M" "M̀")
           ("\\n" "ǹ")
           ("\\N" "Ǹ")

           ("-+" "ǝ̄")
           ("-a" "ā")
           ("-A" "Ā")
           ("-e" "ē")
           ("-E" "Ē")
           ("-i" "ī")
           ("-I" "Ī")
           ("-m" "m̄")
           ("-M" "M̄")
           ("-n" "n̄")
           ("-N" "N̄")
           ("-o" "ō")
           ("-O" "Ō")
           ("-u" "ū")
           ("-U" "Ū")

           (".e" "ẹ")
           (".E" "Ẹ")
           (".i" "ị")
           (".I" "Ị")
           (".o" "ọ")
           (".O" "Ọ")
           (".u" "ụ")
           (".U" "Ụ")
           (".n" "ṅ")
           (".N" "Ṅ")
           ("=n" "₦")
           ("=N" "₦")

           ("+" "ǝ")))

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

;; Returns the first characters in strings, a and b that are
;; not equal
(define (sdiff a b)
  (for ([i a]
        [j b]
        [index (in-naturals)])
    (if (not (char=? i j))
        (printf "~a: ~a ~a~n" index i j)
        '())))

;; Convert a string in dotcode to one in Orba Unicode
(define (orba string)
  (->unicode string dotcode-post))

;; For when you wanna paste in multiple lines of dotcode
(define (multiline-orba) (displayln (orba (read-string 10000))))

;; Main
(displayln
 (orba
  (string-join (vector->list (current-command-line-arguments)))))


;; Tests
(check-equal? (->unicode "abcdefghijklmnopqrstuvwxyz.e.i.o.u.n+=n..==//\\--++" dotcode-post)
              "abcdefghijklmnopqrstuvwxyzẹịọụṅǝ₦.=/\\-+")

(check-equal? (->unicode "ABCDEFGHIJKLMNOPQRSTUVWXYZ.E.I.O.U.N+=N..==//\\--++" dotcode-post)
                         "ABCDEFGHIJKLMNOPQRSTUVWXYZẸỊỌỤṄǝ₦.=/\\-+")


(check-equal? (->unicode "/abcd/efgh/ijkl/m/n/opqrst/uvwxyz/.e/.i/.o/.u/.n/+=n..=/\\--++" dotcode-post)
              "ábcdéfghíjklḿńópqrstúvwxyzẹ́ị́ọ́ụ́ṅ́ǝ́₦.=/\\-+")

(check-equal? (->unicode "/ABCD/EFGH/IJKL/M/N/OPQRST/UVWXYZ/.E/.I/.O/.U/.N/+=N..=/\\--++" dotcode-post)
              "ÁBCDÉFGHÍJKLḾŃÓPQRSTÚVWXYZẸ́Ị́Ọ́Ụ́Ṅ́ǝ́₦.=/\\-+")


(check-equal? (->unicode "\\abcd\\efgh\\ijkl\\m\\n\\opqrst\\uvwxyz\\.e\\.i\\.o\\.u\\.n\\+=n..==//\\--++" dotcode-post)
               "àbcdèfghìjklm̀ǹòpqrstùvwxyzẹ̀ị̀ọ̀ụ̀ṅ̀ǝ̀₦.=/\\-+")

(check-equal? (->unicode "\\ABCD\\EFGH\\IJKL\\M\\N\\OPQRST\\UVWXYZ\\.E\\.I\\.O\\.U\\.N\\+=n..==//\\--++" dotcode-post)
              "ÀBCDÈFGHÌJKLM̀ǸÒPQRSTÙVWXYZẸ̀Ị̀Ọ̀Ụ̀Ṅ̀ǝ̀₦.=/\\-+")


(check-equal? (->unicode "-abcd-efgh-ijkl-m-n-opqrst-uvwxyz-.e-.i-.o-.u-.n-+=n..==//\\--++" dotcode-post)
              "ābcdēfghījklm̄n̄ōpqrstūvwxyzẹ̄ị̄ọ̄ụ̄ṅ̄ǝ̄₦.=/\\-+")

(check-equal? (->unicode "-ABCD-EFGH-IJKL-M-N-OPQRST-UVWXYZ-.E-.I-.O-.U-.N-+=N..==//\\--++" dotcode-post)
              "ĀBCDĒFGHĪJKLM̄N̄ŌPQRSTŪVWXYZẸ̄Ị̄Ọ̄Ụ̄Ṅ̄ǝ̄₦.=/\\-+")
;(check-equal? (->unicode "aaeeiioouummnn.." dotcode-post)
 ;             "aaeeiioouummnṅ.")
