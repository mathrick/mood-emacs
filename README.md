> This is my config framework. There are many like it, but this one is mine.

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-generate-toc again -->
**Table of Contents**

- [Intro](#intro)
    - [Goals](#goals)
- [Installation](#installation)
- [Usage](#usage)
- [FAQ](#faq)

<!-- markdown-toc end -->


Intro
=====

This is Mood, a modular Emacs config that isn't Doom.

I wrote Mood after I tried (and ultimately failed) to use my [previous
config](https://github.com/mathrick/emacs-config) with [Doom
Emacs](https://github.com/hlissner/doom-emacs). I tried porting to
Doom after my previous framework, which was based on Grail and
Cask/Pallet, bitrotted so far it was no longer functional. I got tired
of hacking around its issues and simultaneously wishing it could do
all the snazzy, shiny things all the cool kids were doing with their
Emacses. So I thought I'd try Doom, which seemed like the most
flexible and modular framework out of the current crop (Spacemacs,
Prelude, etc.).

I really like the idea of Doom. I like the idea of letting someone
else figure out the hard parts of setting up Python autocompletion and
whatnots. I like the idea of having a config framework (I wrote a
small one previously, after all) that takes care of compiling and
optimising everything and makes it easy to pick and choose from a set
of preconfigured modules that take out the tedium and provide a nice,
shiny-looking Emacs full of modern conveniences. Where Doom and I
didn't really agree is the actual implementation.

It makes some choices that are cumbersome (`bin/doom sync`, which also
makes it very tricky to install in non-standard locations),
non-standard and confusing (popups library), and hardcoded in the core
config and impossible to disable (`winner.el`). It ships with lots and
lots of modules that are shiny, but also had some bad interactions
that made it literally impossible to use over TRAMP because it'd
freeze, and others that make buffer switching unpredictable.

None of the above is to say that Doom is _bad_. It's not, it's a
well-liked, polished, and flexible framework with plenty of great
ideas and code. But I've been using Emacs for 20+ years and I expect
certain things to behave a particular way, and Doom just doesn't work
for me. Mood is my attempt at stealing the best pieces of Doom while
fixing things that are problems to me.

Goals
-----

The goals of Mood are as follows, in order of importance:

1. Works for me and does what I want without undue pain. At the end of
   the day, it's my config and that's my primary goal. I try not to
   impose gratuitous decisions and make things as modular as I can,
   but if something is too hard to make generic, I might make
   decisions that suit me primarily. It's a double standard to be
   sure, considering what I said about Doom not being flexible enough,
   but I'd be shocked to find out that
   [hlissner](https://github.com/hlissner/) didn't find Doom
   convenient :)
2. Modular and with a minimum of hardcoded choices that can't be
   changed. As long as #1 can be satisfied, I try to keep unrelated
   concerns separate and independent. As much as possible, integration
   and polish will be provided by using glue packages loaded
   conditionally when apporiate, rather than assumptions about what
   else will be used.
3. Reasonably broad chunks of functionality with sane defaults and
   functionality out of the box. Whilst occasionally certain things
   might require knobs to provide sufficient flexibility, in general
   the aim is to have the modules provide a significant amount of
   functionality simply by being enabled.
4. Well-documented and readable. The project is extremely young at the
   moment, and still lacking in this regard, but good documentation
   and readable code are something to strive for.
5. Fast. Speed during normal use is more important than initalisation
   or bootstrap speed, but neither should be needlessly
   compromised. Things that might lead to unacceptable performance
   under specific circumstances (e.g. when using TRAMP) should be easy
   to disable.
6. Aesthetically pleasing and functionally modern. I'm not too worried
   about winning screenshot contests against Atom & friends, but
   there's no reason not to have a smart editor that can reasonably
   compete against IDEs in terms of functionality provided, and be
   pretty to look at

Installation
============

__________________
**NOTE**: This section is very incomplete. More to come
__________________

The basic installation steps are as follows:

1. Clone this repository somewhere. For me, this is `~/Dev/mood-emacs/`:
   ```
   $ cd ~/Dev/
   $ git clone https://github.com/mathrick/mood-emacs
   ```
   This location will be referred to as `<mood root>` from now on.
   
   **NOTE**: This is _not_ the same as your emacs config dir! See next step.
2. Create a directory where your Mood-powered emacs config will
   live. This can either be the standard location (`~/.emacs.d/`), or
   somewhere else. For me, it's `~/elisp`.
   ```
   $ mkdir ~/elisp
   $ git init ~/elisp
   ```
   This location will be referred to as `<user root>` from now on.
   
   **NOTE**: It is **strongly recommended** to keep your config under
   version control. It doesn't need to be git, but it should be a
   version control tool you're comfortable with.
3. Create an init file. It can be any of the [files Emacs will read on
   startup](https://www.gnu.org/software/emacs/manual/html_node/emacs/Init-File.html).
   Give it the following contents:
   ```
   (load "<mood root>/src/mood")
   ```
   * If you're using [Chemacs](https://github.com/plexus/chemacs),
     which is an excellent and recommended way to switch between
     configs and try out new frameworks for your Emacs, the init file
     **must be** `<user root>/init.el`, since Chemacs already resides
     in `~/.emacs`. Afterwards, create a new profile for Mood in
     `~/.emacs-profiles.el`:
     
     ```
     (("default" . ((user-emacs-directory . "~/.emacs.d")))
      ("mood" . ((user-emacs-directory . "~/elisp"))))
     ```

Usage
=====

_FIXME_

FAQ
===

_FIXME_
