# prolog-compiler
Currently the compiler can do the following (at least at a basic level):
Lexing
Parsing
CST
AST
Symbol Table
Semantic Analysis
Really Basic Code Generation

Problems: 
The compiler currently skips a character after reading in certain letters and not reading in the keywords that those letters start. As such, while testing my grammar, please avoid using b, f, t, p, w, i, or s as identifiers.
Strings don't work. They just don't. Don't even try them.

How to run my code-
Get yourself a copy of SWI-ProLog 6.6.6 and download compiler.pl. Simply trying to use the file should work as long as you aren't running Perl on your computer (in which case you'll need to choose to open the file with SWI-ProLog). These are Wandows instructions because I've never touched ProLog on a Mac or Linux machine.
Once you have started running compiler.pl, you can query it. Your best option is to do so by typing in:

?-lexAndParse(<filename>).

and don't forget the period, it is important. This function shows off the CST and AST, and will return true if the program is valid (in all ways except for the byte limit in code generation, as code generation hasn't been fully implemented yet), and will only return false if the code fails one of the checks. Eventually my compiler will give more helpful error messages, but I still have yet to learn how to do that with ProLog.

If you wish to check my code generation, use the command

?-generateCode(<filename>).

it does everything that lexAndParse do, but generates some really bad code.
