Fulmar
======

Fulmar is a C++ code generation framework. It was originally created to generate most of the tens of thousands of lines of code that make up the Nebo PDE domain-specific language.
The goal is to make Fulmar generally useful, though at present it is still very specialized to Nebo.
Fulmar is being developed by a team of computer science researchers at the University of Utah. See AUTHORS for more details.

Indended Audience
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

Fulmar requires a decently modern version of Racket. We've not yet pinned down a hard version requirement, but Fulmar is known to work with Racket 5.3.6. Of course, the code Fulmar generates is pretty useless without a C++ compiler, unless you intend to turn it into abstract art.

Installation
------------

(or, "Ooookay, I've got the code. Now what?")

Using Fulmar consists of writing one or more Fulmar files (\*.fmr) and running them with Racket. Before you can do that, Racket needs to be made aware of Fulmar's existence.

Currently, Fulmar has no automated way to install itself. Fortunately, it's fairly straightforward to get it in working order:

 1. Place a copy of the Fulmar source tree where you want it to live on your system.

    If you're the only one using Fulmar on this computer, you can probably just stuff it somewhere in your home directory. (Mine is in ~/heap/projects/fulmar)
    If you want a system-wide install, this has to be a location that can be read by everyone who should be able to use Fulmar. (For example, on Linux it might be a good idea to create /usr/local/share/fulmar or /opt/fulmar and put the Fulmar source tree there.)

 2. Create a symbolic link from the Racket collections directory to the Fulmar source tree.

    If you're doing a single-user setup, you can just make a link in ~/.racket/x.y.z/collects where x.y.z is the version number of your Racket installation.

This link was helpful for figuring out where my symbolic link to Fulmar needed to go in order for the #lang fulmar directive to work:
http://docs.racket-lang.org/guide/language-collection.html

Use
---

Once you've gone through the installation above, you should be able to run Fulmar scripts from DrRacket or from the terminal like so:

    racket myscript.fmr

If you run tests/main.rkt, it will generate a Fulmar script called test.fmr that you can try out.

**TODO** Expand this section

Known Issues, Caveats, Notes
----------------------------

**TODO** Write this section

License and Copyright
---------------------

**TODO** Write this section