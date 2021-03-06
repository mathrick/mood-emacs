> This is my config framework. There are many like it, but this one is mine.

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [Intro](#intro)
    - [Goals](#goals)
- [Usage](#usage)
    - [Before you start](#before-you-start)
        - [Windows-specific configuration](#windows-specific-configuration)
        - [Using Chemacs](#using-chemacs)
    - [Installation](#installation)
    - [Configuration](#configuration)
- [FAQ](#faq)

<!-- markdown-toc end -->


Intro
=====

This is Mood, a modular Emacs config inspired by Doom.

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

It makes some choices I find cumbersome (`bin/doom sync`, which also
makes it very tricky to install in non-standard locations), some that
are non-standard and confusing (popups library), and hardcoded in the
core config and impossible to disable (`winner.el`). It ships with
lots and lots of modules that are shiny, but also had some bad
interactions that made it literally impossible to use over TRAMP
because it'd freeze, and others that make buffer switching
unpredictable.

So I did what any self-respecting Lisp hacker would do, and figured
I'd have a crack at it myself. Mood is my attempt at stealing the best
pieces of Doom while fixing things that are problems to me.

Goals
-----

The goals of Mood are as follows, in rough order of importance:

1. Works for me and does what I want without undue pain. At the end of
   the day, it's my config and that's my primary goal. I try not to
   impose gratuitous decisions and make things as modular as I can,
   but if something is too hard to make generic, I might make
   decisions that suit me primarily.
2. Modular and with a minimum of hardcoded choices that can't be
   changed. As long as #1 can be satisfied, I try to keep unrelated
   concerns separate and independent.
3. Portable. It should be trivial to move to another machine and start
   using Emacs there simply by cloning your personal config repo. No
   changes should be necessary to make it work. It should also be easy
   to use it on Windows, although POSIX is the primary OS family
   targeted.
4. Easy to select reasonably broad chunks of configuration with sane
   defaults and functionality out of the box. Whilst occasionally
   certain things might require finer knobs to provide sufficient
   flexibility, in general modules should only need to be enabled to
   start working in a way the user expects.
5. Well-documented and readable. The project is extremely young at the
   moment, and still lacking in this regard, but good documentation
   and readable code are something to strive for.
6. Fast. Speed during normal use is more important than initalisation
   or bootstrap speed, but neither should be needlessly
   compromised. Things that might lead to unacceptable performance
   under specific circumstances (e.g. when using TRAMP) should be easy
   to disable.
7. Aesthetically pleasing and functionally modern. I'm not too worried
   about winning screenshot contests against Atom & friends, but
   there's no reason not to have a smart editor that can reasonably
   compete against IDEs in terms of functionality provided, and be
   pretty to look at

Usage
=====

Before you start
----------------

### Windows-specific configuration

_Note: Mood has been tested on Windows 7, using native builds from
GNU.org. Using Cygwin or Win10 with WSL will require adjustments_

* Set up your HOME, as the default location is rather silly. Right
  click on *Computer* → *Properties* → *Advanced system settings*

	Click on `New...` user variable. Call it `HOME`, and give it the
	value of `%USERPROFILE%`. This will make it point to your user
	folder (to access it through the file explorer, navigate to
	Desktop in the file explorer, then click the arrow in the address
	bar and select your user folder from the dropdown list).

	Below `~/` refers to the home directory you've just set up.


### Using Chemacs

[Chemacs](https://github.com/plexus/chemacs) is recommended. Chemacs
provides an easy way to switch between multiple profiles, and even if
you only use one profile, it makes it much easier to put Emacs config
files in an arbitrary location. If you decide to use Chemacs, follow
its installation instructions first. Afterwards, set up a profile
(either dedicated or `default`) in `~/.emacs-profiles.el` that points to
your `<user root>` from step 2 below:

```
(("default" . ((user-emacs-directory . "~/.emacs.d")))
 ("mood" . ((user-emacs-directory . "~/elisp"))))
```


Installation
------------

1. Clone this repository somewhere. For me, this is `~/Dev/mood-emacs/`:
   ```
   $ cd ~/Dev/
   $ git clone https://github.com/mathrick/mood-emacs
   ```
   This location will be referred to as `<mood root>` from now on.

   **NOTE**: This is _not_ the same as your Emacs config dir! See the
   next step.

2. Create a directory where your Mood-powered Emacs config will
   live. This can either be the standard location (`~/.emacs.d/`), or
   somewhere else. For me, it's `~/elisp`.

   ```
   $ mkdir ~/elisp
   $ cd ~/elisp; git init && git ignore mood.el # optional but recommended
   ```

   This location will be referred to as `<user root>` from now on.

   **NOTE**: It is **strongly recommended** to keep your config under
   version control. It doesn't need to be git, but it should be a
   version control tool you're comfortable with. After you initialise
   the repo, you should add the file `mood.el` to the ignore list.

3. Pick a location for your init file. It can be any of the [files
   Emacs will read on startup](https://www.gnu.org/software/emacs/manual/html_node/emacs/Init-File.html).
   Either symlink or copy `<mood root>/init.el` to it:

   ```
   $ ln -s ~/Dev/mood-emacs/init.el ~/elisp/
   ```

   or

   ```
   $ cp ~/Dev/mood-emacs/init.el ~/.emacs.d/
   ```

   * If you're using Chemacs, the init file **must be** `<user root>/init.el`, 
	 since Chemacs already resides in `~/.emacs`

   * On Windows, symlinks are not well-supported, so copying is necessary
	 

4. Start Emacs, optionally passing in `--with-mood <mood root>`. If
   using Chemacs, start it with the newly created profile:
   
   ``` 
   emacs --with-profile mood --with-mood ~/Dev/mood-emacs
   ``` 

   If you omit `--with-mood` from the command line, Emacs will instead
   prompt you to enter the location of Mood checkout (ie. `<mood
   root>`). That location will be used to create a loader stub under
   `<user root>/mood.el`. The stub makes it easier to make your config
   machine-independent, and should not be checked into your VCS.

   Next, it will bootstrap [`straight.el`](https://github.com/raxod502/straight.el) 
   and install the required libraries (Internet connection is
   necessary). Depending on your connection speed, it might take a
   while, but Mood configures `straight.el` to minimise the badwidth
   requirements, and for me, it takes only about 7 seconds.

   **NOTE**: The bootstrap will only happen the first time you run
   Mood Emacs, and after updates and changes to your
   config. Otherwise, it will be skipped and Emacs should start almost
   instantly.

5. At the end of the bootstrap, Mood will prompt to create the
   `config.el` file from template. Answer `y` to that, then edit the
   default config to your liking. When you're done, run `M-x
   mood-reload` or restart Emacs. There will be some delay as straight
   installs the packages you selected, but again, that will only
   happen once.

6. Congratulations! Your Emacs is now ready to use.

Configuration
-------------

_FIXME_

FAQ
===

_FIXME_
