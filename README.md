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
The compiler currently skips a character after reading in certain letters and not reading in the keywords that those letters start. As such, while testing my grammar, please avoid using b, f, t, p, w, i, or s as identifiers. A single = will also need a space after it. Strings work now though

How to run my code-
Get yourself a copy of SWI-ProLog 7.2.0 and download compiler.pl. Simply trying to use the file should work as long as you aren't running Perl on your computer (in which case you'll need to choose to open the file with SWI-ProLog). These are Wandows instructions because I've never touched ProLog on a Mac or Linux machine.
Once you have started running compiler.pl, you can query it. Your best option is to do so by typing in:

?-lexAndParse(<filename>).

and don't forget the period, it is important. The function will say if Lex and Parse have succeeded, then print the CST and AST. It will then generate a symbol table that you can check via the following command, but it doesn't automatically print this table. Semantic analysis, however, is pretty borked right now, so expect the program to say false at the end because semantic analysis doesn't work.

?-listing(symbolTable/3)

If you wish to check my code generation, use the command

?-generateCode(<filename>).

it does everything that lexAndParse do, but generates some really bad code.
