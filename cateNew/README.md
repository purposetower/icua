Setup
=====

* Install stack

* stack new <project_name> simple

* stack build

* stack exec <project_name>   --(check in .stack-work for binary)

-------------------------------------------------------------------------------

Display terminal size
=====

* Create a new TerminalSize module/file, add it to cate.cabal (other-modules:TerminalSize and add unix dependency)

* Foreign function call! Call some C code to get terminal size

-------------------------------------------------------------------------------

Display text to terminal
=====

Given some input text, we want to display a terminals worth of it. Given the
terminal's width and height we can work out how much text to display.

* Create module for working out text to display, the function should be something simple like:
getDisplayText :: Input -> Width -> Height -> String

* Only consider wrap mode(if text is too long we go to next line)

* Do some refactoring for terminal window size

-------------------------------------------------------------------------------

Test our code
=====

* Setup testing framework with different testing styles(unit and property based)

* Test DisplayText.hs using HUnit and QuickCheck

-------------------------------------------------------------------------------

Display contents of file
=====

* Pass in file path as program argument

* Display a terminals worth

-------------------------------------------------------------------------------

Seek around the file
=====

* We want to be able to handle huge files, so we can't read it all into memory.
  Manage the file handle ourselves, seeking around and putting the file pointer where we want

-------------------------------------------------------------------------------

Fix handle closing bug
=====

* hGetContents closes the handle, we want to seek around and read at leisure!

* Put file reading functionality in it's own module

-------------------------------------------------------------------------------

Handle terminal input
=====

* Capture keyboard events and print out byte value of keypress. We read from stdin and
execute a function on the input(just print out the byte array). We block until next input.
Notice that pressing a key doesn't do anything untill we press enter! This is because of the terminal
mode.

-------------------------------------------------------------------------------

Put terminal in raw mode
=====

* Let us handle the terminal!

* Exit when user hits Esc

* Make sure to restore terminal on exit even if there is an error(pressing a should do it)

-------------------------------------------------------------------------------

Move left and right
=====

* Handle moving left and right

* Pad the output so we dont clear and reprint as causes flickering

-------------------------------------------------------------------------------



get text with padding...
move up and down read file handle-no optimisations.

















Move up and down
=====

* Handle moving up and down



-- handle no wrap mode


-- terminal character width
-- utf8 char size



File Resources:

Display at position:
Server  <--- json rpc									<--- ->Plugins(only coms with server)
						|||||| changes, send hash ---> json rpc
						\/\/\/ annotations


^^^^^^^^^^^^^^^^^^^^^^^^^
|||||||||||||||||||||||||
----------___------------					

annotation overlay
change overlay
file handles



