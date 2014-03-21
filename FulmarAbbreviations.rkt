#lang typed/racket

 ; Copyright (c) 2014 The University of Utah
 ;
 ; Permission is hereby granted, free of charge, to any person obtaining a copy
 ; of this software and associated documentation files (the "Software"), to
 ; deal in the Software without restriction, including without limitation the
 ; rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
 ; sell copies of the Software, and to permit persons to whom the Software is
 ; furnished to do so, subject to the following conditions:
 ;
 ; The above copyright notice and this permission notice shall be included in
 ; all copies or substantial portions of the Software.
 ;
 ; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 ; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 ; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 ; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 ; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 ; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
 ; IN THE SOFTWARE.

(require "private/fulmar-core.rkt")
(require "standard-chunk.rkt")

(provide (all-defined-out))

(define l literal)
(define c concat)
(define b between)
(define b/a between/attach)
(define n namespace-define)
(define d described-smts)
(define p paren-list)
(define m macro-define)
(define s (spaces 1))

(: cs (NestofChunks * -> Chunk))
(define (cs . chunks)
  (c chunks s))

(: scs (NestofChunks * -> Chunk))
(define (scs . chunks)
  (c s chunks s))

(: cc (NestofChunks * -> Chunk))
(define (cc . chunks)
  (c chunks ";"))

(: ccs (Chunk Chunk -> Chunk))
(define (ccs first second)
  (c first ";" s second))

(: bs (NestofChunks * -> Chunk))
(define (bs . chunks)
  (apply b s chunks))

(: bb (NestofChunks * -> Chunk))
(define (bb . chunks)
  (apply b blank-line chunks))

(: bn (NestofChunks * -> Chunk))
(define (bn . chunks)
  (apply b new-line chunks))

(: sbs (NestofChunks * -> Chunk))
(define (sbs . chunks)
  (cc (bs chunks)))

(: bl-smts (NestofChunks * -> Chunk))
(define (bl-smts . chunks)
  (internal-smt-list blank-line chunks))

(: nl-smts (NestofChunks * -> Chunk))
(define (nl-smts . chunks)
  (smt-list new-line chunks))

(define typedef typedef-smt)

(: s-typedef (Chunk Chunk -> Chunk))
(define (s-typedef first second)
  (cc (typedef-smt first second)))

(define fc function-call)
(define mfc member-function-call)
(define fcn-def function-define)
(define v-fcn-def void-function-define)
(define v-fcn-dcl void-function-declare)
(define r-fcn-def returning-function-define)
(define fcn-dcl function-declare)
(define gen-fcn-dcl general-function-declare)
(define s-fcn-dcl static-function-declare)
(define tpl-def template-define)
(define srt-dcl struct-declare)

(: s-srt-dcl (Chunk -> Chunk))
(define (s-srt-dcl chunks)
  (cc (srt-dcl chunks)))

(define srt-def struct-define)

(: s-srt-def (Chunk NestofChunks * -> Chunk))
(define (s-srt-def signature . chunks)
  (cc (apply srt-def signature chunks)))

(define tpl-srt-dcl template-struct-declare)

(: s-tpl-srt-dcl (Chunk NestofChunks NestofChunks * -> Chunk))
(define (s-tpl-srt-dcl name params . chunks)
  (cc (apply tpl-srt-dcl name params chunks)))

(define tpl-srt-def template-struct-define)

(: s-tpl-srt-def (Chunk NestofChunks NestofChunks NestofChunks * -> Chunk))
(define (s-tpl-srt-def name params args . chunks)
  (cc (apply tpl-srt-def name params args chunks)))

(define tpl-use template-use)

(: tpl-pmtr (Chunk -> Chunk))
(define (tpl-pmtr pmtr)
  (bs 'typename pmtr))

(: sub-tpl-use (Chunk -> Chunk))
(define (sub-tpl-use pmtr)
  (bs 'template pmtr))

(: tpl-fcn-use (NestofChunks NestofChunks * -> Chunk))
(define (tpl-fcn-use name . pmtrs)
  (sub-tpl-use (apply tpl-use name pmtrs)))

(define pub-sec public-section)
(define priv-sec private-section)
(define cons-asgn constructor-assignment)
(define scope scope-resolution-operator)

(: enum (NestofChunks * -> Chunk))
(define (enum . enums)
  (sbs 'enum (body-list "," enums)))

(: kernel-use (Chunk NestofChunks * -> Chunk))
(define (kernel-use name . args)
  (tpl-use name (tpl-use null (tpl-use null args))))

(: device-use (NestofChunks * -> Chunk))
(define (device-use . chunks)
  (bs '__device__ chunks))

(: gbl-fcn-dcl (Chunk NestofChunks * -> Chunk))
(define (gbl-fcn-dcl name . params)
  (bs '__global__ (general-function-declare name 'void params)))

(: pp-cond-or (Chunk Chunk Chunk -> Chunk))
(define (pp-cond-or name then else)
  (pp-conditional-ifdef name
                        (c then)
                        (if (not else)
                            #false
                            (c else))))

(: par (NestofChunks * -> Chunk))
(define (par . chunks)
  (p (bs chunks)))

(: ter-cond (Chunk Chunk Chunk -> Chunk))
(define (ter-cond if then else)
  (par if '? then ': else))

(: op-asgn (Chunk Chunk NestofChunks * -> Chunk))
(define (op-asgn op lhs . rhs)
  (bs lhs op rhs))

(: n= (Chunk NestofChunks * -> Chunk))
(define (n= lhs . rhs)
  (op-asgn '= lhs rhs))

(: nt= (Chunk Chunk NestofChunks * -> Chunk))
(define (nt= type lhs . rhs)
  (n= (bs type lhs) rhs))

(: nt=c (Chunk Chunk NestofChunks * -> Chunk))
(define (nt=c type lhs . rhs)
  (nt= (bs 'const type) lhs rhs))

(: n+= (Chunk NestofChunks * -> Chunk))
(define (n+= lhs . rhs)
  (op-asgn '+= lhs rhs))

(: n== (Chunk NestofChunks * -> Chunk))
(define (n== lhs . rhs)
  (op-asgn '== lhs rhs))

(: n!= (Chunk NestofChunks * -> Chunk))
(define (n!= lhs . rhs)
  (op-asgn '!= lhs rhs))

(: n< (Chunk NestofChunks * -> Chunk))
(define (n< lhs . rhs)
  (op-asgn '< lhs rhs))

(: n> (Chunk NestofChunks * -> Chunk))
(define (n> lhs . rhs)
  (op-asgn '> lhs rhs))

(: n<= (Chunk NestofChunks * -> Chunk))
(define (n<= lhs . rhs)
  (op-asgn '<= lhs rhs))

(: n>= (Chunk NestofChunks * -> Chunk))
(define (n>= lhs . rhs)
  (op-asgn '>= lhs rhs))

(: n+ (Chunk NestofChunks * -> Chunk))
(define (n+ lhs . rhs)
  (op-asgn '+ lhs rhs))

(: n- (Chunk NestofChunks * -> Chunk))
(define (n- lhs . rhs)
  (op-asgn '- lhs rhs))

(: n* (Chunk NestofChunks * -> Chunk))
(define (n* lhs . rhs)
  (op-asgn '* lhs rhs))

(: n/ (Chunk NestofChunks * -> Chunk))
(define (n/ lhs . rhs)
  (op-asgn '/ lhs rhs))

(: n% (Chunk NestofChunks * -> Chunk))
(define (n% lhs . rhs)
  (op-asgn '% lhs rhs))

(: n++ ((U Symbol String) -> Chunk))
(define (n++ chunk)
  (l chunk '++))

(: n-and (Chunk NestofChunks * -> Chunk))
(define (n-and lhs . rhs)
  (b (scs '&&) lhs rhs))

(: n-or (Chunk NestofChunks * -> Chunk))
(define (n-or lhs . rhs)
  (b (scs "||") lhs rhs))

(: n-not (Chunk -> Chunk))
(define (n-not chunk)
  (c '! (par chunk)))

(: tc (NestofChunks * -> Chunk))
(define (tc . chunks)
  (bs chunks 'const))

(: stc (NestofChunks * -> Chunk))
(define (stc . chunks)
  (cc (tc chunks)))

(: ref (NestofChunks * -> Chunk))
(define (ref . chunks)
  (bs chunks '&))

(: cref (NestofChunks * -> Chunk))
(define (cref . chunks)
  (ref (tc chunks)))

(: ptr (NestofChunks * -> Chunk))
(define (ptr . chunks)
  (bs chunks '*))

(: cptr (NestofChunks * -> Chunk))
(define (cptr . chunks)
  (ptr (tc chunks)))

(: ad (Chunk Chunk -> Chunk))
(define (ad type arg)
  (bs type arg))

(: sad (Chunk Chunk -> Chunk))
(define (sad type arg)
  (cc (ad type arg)))

(: adc (Chunk Chunk -> Chunk))
(define (adc type arg)
  (bs (tc type) arg))

(: sadc (Chunk Chunk -> Chunk))
(define (sadc type arg)
  (cc (adc type arg)))

(: adr (Chunk Chunk -> Chunk))
(define (adr type arg)
  (bs (ref type) arg))

(: sadr (Chunk Chunk -> Chunk))
(define (sadr type arg)
  (cc (adr type arg)))

(: adcr (Chunk Chunk -> Chunk))
(define (adcr type arg)
  (bs (cref type) arg))

(: sadcr (Chunk Chunk -> Chunk))
(define (sadcr type arg)
  (cc (adcr type arg)))

(: adp (Chunk Chunk -> Chunk))
(define (adp type arg)
  (bs (ptr type) arg))

(: sadp (Chunk Chunk -> Chunk))
(define (sadp type arg)
  (cc (adp type arg)))

(: adcp (Chunk Chunk -> Chunk))
(define (adcp type arg)
  (bs (cptr type) arg))

(: sadcp (Chunk Chunk -> Chunk))
(define (sadcp type arg)
  (cc (adcp type arg)))

(: take-ptr (NestofChunks * -> Chunk))
(define (take-ptr . chunks)
  (c '& chunks))

(: nif (Chunk NestofChunks * -> Chunk))
(define (nif check . then)
  (bs (fc 'if check)
      (body then)))

(: nelse (Chunk -> Chunk))
(define (nelse else)
  (bs 'else (body else)))

(: nifelse (Chunk Chunk Chunk -> Chunk))
(define (nifelse check then else)
  (b new-line
     (nif check then)
     (nelse else)))

(: nelseif (Chunk NestofChunks * -> Chunk))
(define (nelseif check . then)
  (bs 'else (nif check then)))

(: nifelseif (Chunk Chunk Chunk Chunk -> Chunk))
(define (nifelseif check1 then1 check2 then2)
  (b new-line
     (nif check1 then1)
     (nelseif check2 then2)))

(: nwhile (Chunk NestofChunks * -> Chunk))
(define (nwhile while . smts)
  (bs (fc 'while while)
      (body smts)))

(: nfor (Chunk Chunk Chunk NestofChunks * -> Chunk))
(define (nfor init check next . smts)
  (bs (fc 'for (bs (cc init)
                   (cc check)
                   next))
      (body smts)))
