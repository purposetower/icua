Setup
=====

* Install stack

* $> stack new <project_name> simple

* $> cd <project_name>

* $> stack build

* $> stack exec <project/binary_name>  --check in .stack-work directory for binary name

* create README.md :D

* add .gitignore

--------------------------------------------------------------------------------------

Display terminal size
=====

* Create a new TerminalSize module/file, add it to cate.cabal (other-modules:TerminalSize and add unix dependency)

* Foreign function call! Call some C code to get terminal size

--------------------------------------------------------------------------------------

Display text to terminal
=====

Given some input text, we want to display a terminals worth of it. Given the
terminal's width and height we can work out how much text to display.

* Create module for working out text to display, the function should be something simple like:
getDisplayText :: Input -> Width -> Height -> String

* Consider wrap mode(if text is too long we go to next line)

* Do some refactoring for terminal window size

-------------------------------------------------------------------------------

Test our code
=====

* Setup testing framework with different testing styles(unit and property based)

* Test DisplayText.hs using HUnit and QuickCheck
