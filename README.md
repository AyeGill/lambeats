Lambeats
========

Lambeats is a simple automated mpd client written in Common Lisp. It automatically fills your playlist to a certain number of songs at a specific interval(both configurable). It allows you to define categories of folders to pick songs from, and how the script should determine which category to use. 

To use it, you need quicklisp and asdf. You also need an internet connection first time you run it, so quicklisp can get the required systems.

Installation
------------
    git clone https://github.com/AyeGill/lambeats.git

Easy.

Configuration
-------------

The configuration is loaded from lambeats.conf(in the same directory as the script). The configuration data is continually updated as the program runs; therefore, the condiguration of the program can be changed without restarting it simply by editing this file.

The configuration consists of three sections. Each is contained in an s-expression, with the first element being the name of the section, and the rest being the options for that section. For example

    (:globals (fill-size 20)
    	      (refresh-time 30))
is the globals section, with the options (fill-size 20) and (refresh-time 30). The three sections are:

###globals.
In this section, you can set new values for various global variables in the script. Each option is a two-element list. The first element denotes the name of the global being set, the second the value. You can set:
* fill-size: the number of elements that the script will fill the playlist to. Defaults to 10
* host: the host at which the mpd server is located. You usually won't have to mess with this. Defaults to "localhost"
* port: the port of the mpd server. Again, you can usually leave this at default, which is 6600.
* refresh-time: the interval at which your playlist is refreshed, in seconds. Defaults to two minutes(120)

Any of these can be left out if you're fine with the defaults, and in fact the entire globals section is optional.

###categories.
In this section, you define the categories of songs(or song folders, rather) from which your music will be picked. Each option is a two-element list, which defines a category. The first element is the name of the category that you're defining. The second element is parsed according to the following syntax
* A list of strings is simply the category containing the folders named by those strings
* A list in which the first element is :and, is the category containing exactly those folders which appear in ALL of the categories denoted by the rest of the list (intersection, in set-theoretic terms)
* A list in which the first element is :or, is the category containing the folders which appear in any one of the categories denoted by the rest of the list (union).
* A list in which the first element is :except is the category containg the same folders as the second element, except those which appear in any of the other elements.
* A symbol is the category with that name that has already been defined.

This might be confusing: read lambeats.conf.example for an example of how this works.

###category-picks.
Configures how the script picks categories to add songs from. Each options has two elements. The first is a lisp expression, which is evaluated as such. The second is the name of a category. The options are iterated through in order. If any option returns a non-nil value, the corresponding category is used, and the remaining options are ignored. If you don't know how to use lisp, just use the symbol t, like in the example. It always passes, so you can use it to have a default value(although, if you don't know anything about lisp, maybe this isn't the right script for you).


Usage
-----

Go into the lambeats folder. Launch your favorite common lisp. If you're in some ancient implementation that doesn't have asdf prepackaged, make sure it's loaded.
Simply:

    (asdf:load-system :lambeats)
    (in-package :lambeats)
    (run)
    
Compilation
-----------

If you've got ECL, you can compile lisp programs into executables. This can make execution and especially load times faster. This is especially helpful if you're on a old or otherwise slow system (I run lambeats on my raspberry pi, and foung this very useful)
Simply append "(run)" (without the quotes) to lambeats.lisp, and do

    ecl -eval "(asdf:make-build :lambeats :type :program)"

in the lambeats directory. This will create the executable. In my experience, the executable is saved to some location in the ecl directory (/home/pi/.cache/common-lisp/ecl-11.1.1-unknown-linux-arm/home/pi/lambeats/). Thisisn't specified anywhere in the ECL documentation that I could find, but fortunately it's printed at the end of compilation.

