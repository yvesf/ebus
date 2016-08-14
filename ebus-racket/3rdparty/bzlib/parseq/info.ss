#lang setup/infotab
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PARSEQ.PLT 
;; A Parser Combinator library.
;; 
;; Bonzai Lab, LLC.  All rights reserved.
;; 
;; Licensed under LGPL.
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; info.ss 
;; yc 12/31/2009 - first version 
(define name "BZLIB/PARSEQ: a monadic parser combinator library")

(define blurb
  '((p "Inspired by Haskell's Parse, bzlib/parsec provides a monadic parser combinator library that can handle both character and binary data parsing. ")))

(define release-notes
  '((p "0.4 (1 3) - added ability to parse exponents to real-number, and updated read-json to handle single quoted string")
    (p "0.3 (1 2) - added additional tokenizers")
    (p "0.2 (1 1) - fixed a bug with the all-of combinator")
    (p "0.1 (1 0) - first release")))

(define categories
  '(devtools net misc))

(define homepage "http://weblambda.blogspot.com")

(define required-core-version "4.0")

(define version "0.3")

(define repositories '("4.x"))

(define primary-file "main.ss")

