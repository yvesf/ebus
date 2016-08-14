#lang setup/infotab
(define name "bzlib/base: common utilities for bzlib")

(define blurb
  '((p "bzlib/base provides the common utilities that other bzlib packages depend on. Currently this package's interface might drastically change and will not be directly supported until it stablizes.")))

(define release-notes
  '((p "0.6 (1 6) - fixed syntax-identifier-append, added registry-clear!")
    (p "0.5 (1 5) - adding read-bytes-avail, read-byte-list, read-byte-list/timeout, read-bytes/timeout, version.ss, version-case.ss, fixed let/assert!, added let*/assert, fixed bytes.ss needing (version) >= 4.1, added let*/if, added isa/c & typeof/c")
    (p "0.4 (1 3) - adding bytes.ss & require.ss & syntax.ss (args.ss, assert.ss, syntax.ss, & require.ss are likely to be moved to another package)")
    (p "0.3 (1 2) - added assert.ss, args.ss, and refactored group to here from dbd-memcached")
    (p "0.2 (1 1) - added assert! & let/assert!")
    (p "0.1 (1 0) - first release")))

(define categories
  '(devtools net misc))

(define homepage "http://weblambda.blogspot.com")

(define required-core-version "4.0")

(define version "0.6")

(define repositories '("4.x"))

(define primary-file "main.ss")

