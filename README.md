Fulmar
======

Fulmar is a C++ code generation framework. It was originally created to generate most of the tens of thousands of lines of code that make up the Nebo PDE domain-specific language.
The goal is to make Fulmar generally useful, though at present it is still very specialized to Nebo.
Fulmar is being developed by a team of computer science researchers at the University of Utah. See AUTHORS for more details.

Intended Audience
-----------------

(Who should use Fulmar?)

Fulmar leverages the power of Racket to generate C++ code. If you're finding yourself writing highly repetitive C++ and neither macros nor template metaprogramming are helping (or you've been using them and they've become cumbersome), Fulmar was designed to solve your problem.

Getting Fulmar
--------------

(or, "I'm sold! How do I get it?")

Hold your horses! Fulmar is still in its early stages of development. It may not be ready for your mission-critical, 24-7-365 build-server-on-a-lunar-rocket application yet. In fact, it's entirely possible that Fulmar may not be ready for your try-project-euler-in-a-new-language application yet.

That said, if you want to download and try Fulmar, you can clone its main git repo:
https://github.com/cwearl/fulmar.git

Dependencies
------------

Fulmar requires Racket version 6 or greater.
Of course, the code Fulmar generates is pretty useless without a C++ compiler, unless you intend to turn it into abstract art.
The compiler compatibility of the C++ code Fulmar generates depends entirely on what code gets generated. Fulmar should be quite capable of generating code that any C++ compiler can handle, so no specific compiler dependencies are necessary.

Installation
------------

(or, "Ooookay, I've got the code. Now what?")

Using Fulmar consists of writing one or more Fulmar files (\*.fmr) and running them with Racket. Before you can do that, Racket needs to be made aware of Fulmar's existence.

Currently, Fulmar has no automated way to install itself. Fortunately, it's fairly straightforward to get it in working order:

 1. Place a copy of the Fulmar source tree where you want it to live on your system.

    If you're the only one using Fulmar on this computer, you can probably just stuff it somewhere in your home directory. (For example, ~/projects/fulmar or ~/src/fulmar)
    If you want a system-wide install, this has to be a location that can be read by everyone who should be able to use Fulmar. (For example, on Linux it might be a good idea to create /usr/local/share/fulmar or /opt/fulmar and put the Fulmar source tree there.)

 2. Create a symbolic link from the Racket collections directory to the Fulmar source tree.

    If you're doing a single-user setup, you can just make a link in ~/.racket/x.y.z/collects where x.y.z is the version number of your Racket installation.

The following link contains more information about how and where to set up the symbolic link to get Fulmar working:
http://docs.racket-lang.org/guide/language-collection.html

There's a suite of pseudo-unit/regression tests in the tests/ directory. Currently, you can run tests/main.rkt to test Fulmar.

Use
---

Once you've gone through the installation above, you should be able to run Fulmar scripts from DrRacket or from the terminal like so:

    racket myscript.fmr

There is a Fulmar script in tests/ called test.fmr that you can try out.

When run, a Fulmar script produces formatted C++ code on standard out. Assuming your terminal shell supports output redirection (most do), you can redirect the C++ code to a file:

    racket myscript.fmr > myscript.C

You can then compile the resulting file with your favorite C++ compiler. If your compiler supports reading from standard input, you can skip the intermediate C++ file altogether. For example, if you are using the GCC C++ compiler:

    racket myscript.fmr | g++ -x c++ -c -o myscript.o -

Of course, this would make it difficult to track down any compile errors, but it could potentially be useful when compiling a large number of Fulmar scripts.

Trivial Example Script
----------------------

A full tutorial on creating Fulmar scripts is beyond the scope of this humble README. There will be separate documentation detailing Fulmar's semantics and how to leverage it.

The following is a very simple Fulmar script:

    #lang fulmar

    (returning-function-define (constize (function-declare 'eval 'double))
                               null
                               'value_)

When run, this script produces the following output:

    inline double eval(void) const { return value_; }

The first argument of returning-function-define is a function signature, the second is the body of the function, and the third argument is the return value of the function.

Documenting Defined Forms
-------------------------

Fulmar provides a define/doc macro that allows for documentation inline with function and variable definition. The supported forms are:

    (define/doc
     (id args ...)
     contract
     doc
     body)

for functions and

    (define/doc
     id
     contract
     doc
     body)

for general bindings.

contract is as defined at http://docs.racket-lang.org/scribble/srcdoc.html#%28form._%28%28lib._scribble%2Fsrcdoc..rkt%29._proc-doc%29%29

doc is a description expression in racket's at-expression
format. See here for details: http://docs.racket-lang.org/scribble/reader.html

An example from tests/test.fmr:

    (define/doc l
      (-> any/c any/c)
      @{Rename for built in @racket[literal]}
      literal)


Known Issues, Caveats, Notes
----------------------------

Because Fulmar is implemented in Racket, and because Fulmar scripts are essentially an extension of Racket, the following link to the Racket documentation has been included in this README. If you have little to no experience with Racket and/or you're having trouble writing a useful Fulmar script, this is an excellent resource to begin with:
http://docs.racket-lang.org/guide/

**TODO** Write this section

License and Copyright
---------------------

Fulmar is Copyright (c) 2014 University of Utah. It is licensed under the terms of the MIT License. A copy of the full text of this license is included in the LICENSE file.
