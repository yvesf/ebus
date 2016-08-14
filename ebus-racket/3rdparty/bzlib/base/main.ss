#lang scheme/base
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BASE.plt - common routines that are shared by all other bzlib modules 
;; 
;; in a way, base.plt is the most fundamental module of the whole bzlib stack
;; and as such it also is the lowest level code.  We are not likely to 
;; fix the code any time soon, and hence any of the functions here are 
;; explicitly likely to be obsoleted or moved elsewhere. 
;; 
;; Proceed with caution. 
;; 
;; 
;; Bonzai Lab, LLC.  All rights reserved.
;; 
;; Licensed under LGPL.
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; main.ss - provide all other modules... 
;; yc 9/8/2009 - first version 
;; yc 9/11/2009 - added uuid.ss
;; yc 9/25/2009 - added assert.ss & move args.ss from port.plt 
;; yc 10/13/2009 - adding bytes.ss 
;; yc 10/19/2009 - adding require.ss & syntax.ss (it seems that all syntax-based files can be splitted away)... 
;; yc 1/18/2010 - added version.ss & version-case.ss 
(require "args.ss"
         "assert.ss"
         "base.ss"
         "bytes.ss"
         "list.ss"
         "registry.ss"
         "require.ss"
         "syntax.ss"
         "text.ss"
         "uuid.ss"
         "version.ss"
         "version-case.ss"
         )
(provide (all-from-out "args.ss"
                       "assert.ss"
                       "base.ss"
                       "bytes.ss"
                       "list.ss"
                       "registry.ss"
                       "require.ss"
                       "syntax.ss"
                       "text.ss"
                       "uuid.ss"
                       "version.ss"
                       "version-case.ss"
                       ))
