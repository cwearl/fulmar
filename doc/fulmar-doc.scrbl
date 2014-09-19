#lang scribble/manual

@(require "../private/doc/doc-gen.rkt")

@title{Fulmar}

@table-of-contents[]

@section{standard-chunk}
The standard chunks are intended to be the useful high-level forms available to
end users of Fulmar. They come for free when you use @racketfont{#lang fulmar}.

@(doc-gen "standard-chunk.rkt")

@section{type-decls}
@(doc-gen "type-decls.rkt")

@section{verify}
@(doc-gen "verify.rkt")

@section{fulmar-core}
@(doc-gen "private/fulmar-core.rkt")
