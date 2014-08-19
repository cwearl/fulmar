#lang scribble/manual

@(require "../private/doc/doc-gen.rkt")

@title{Fulmar}

@table-of-contents[]

@section{standard-chunk}
The standard chunks are intended to be the useful high-level forms available to
end users of Fulmar. They come for free when you use @racketfont{#lang fulmar}.

@(gen-doc "../standard-chunk.rkt")

@section{type-decls}
@(gen-doc "../type-decls.rkt")

@section{verify}
@(gen-doc "../verify.rkt")

@section{fulmar-core}
@(gen-doc "../private/fulmar-core.rkt")

