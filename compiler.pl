% declaring tables and the like
:- dynamic scope/1. 		%holds the last created scope
:- dynamic currentScope/1. 	%holds the scope that we are currently in
:- dynamic child/2. 		%table listing (parent, child) for usage with scope
:- dynamic symbolTable/5. 	%table holding (scope, identifier, type, initialized flag, usage flag). created in semantic analysis, used in SA and code generation
:- dynamic temp/2. 			%table for holding temporary values- (number, value) numbers 0 and 2 will hold identifier, 1 and 3 hold types
:- dynamic generatedCode/1. %holds the string of generatedCode
:- dynamic heapString/1.	%holds the string containing the ascii values of strings used in the code
:- dynamic staticData/4.	%table listing (temporary code, variable, scope, and offset), for use in code generation.
:- dynamic jumpData/2.		%table listing (temporary code, distance) for use in code generation
:- dynamic heap/1.			%holds all of the strings made with heapString
:- dynamic tempVarTable/1.	%keeps track of which temporary variable is being made next
:- dynamic jumpVarTable/1.  %keeps track of which jump temporary variable is being made next

%main method. does everything except code generation
compile(Filename):-
	clearDictionaries,
	lex(Filename, X), !,
	write('Lex succeeded - '),
	writeln(X),
	programNT(X, []), !,
	program(CST, X, []), !,
	programAST(AST, X, []), !,
	writeln('Parse succeeded,'),
	write('CST - '),
	writeln(CST),
	write('AST - '),
	writeln(AST), !,
	assert(currentScope(0)),
	assert(scope(0)),
	assert(generatedCode("")),
	assert(heap("")),
	programST(X, []), !,
	writeln('Semantic Analysis succeeded,'),
	listing(symbolTable/5),
	initializeCheck,
	useCheck, !.

%clears all tables so that multiple things can be compiled
clearDictionaries:-retractall(symbolTable(_,_,_,_,_)),retractall(scope(_)),retractall(currentScope(_)),
retractall(child(_,_)),retractall(temp(_,_)), retractall(generatedCode(_)),retractall(heapString(_)),
retractall(staticData(_,_,_,_)),retractall(jumpData(_,_)), retractall(heap(_)),retractall(tempVarTable(_)),
retractall(jumpVarTable(_)).

%compiles and generates code
generateCode(Filename):-
	clearDictionaries,
	lex(Filename, X), !,
	write('Lex succeeded - '),
	writeln(X),
	programNT(X, []), !,
	program(CST, X, []), !,
	programAST(AST, X, []), !,
	writeln('Parse succeeded,'),
	write('CST - '),
	writeln(CST),
	write('AST - '),
	writeln(AST), !,
	assert(currentScope(0)),
	assert(scope(0)),
	assert(generatedCode("")),
	assert(heap("")),
	assert(heapString("")),
	programST(X, []), !,
	writeln('Semantic Analysis succeeded,'),
	listing(symbolTable/5),
	initializeCheck,
	useCheck, !,
	retractall(scope(_)),retractall(currentScope(_)),
	assert(currentScope(0)), assert(scope(0)),
	programCG(X, []), !,
	backpatch,
	write('Code- '),generatedCode(Y),!,
	writeln(Y).

%rule that takes in a file (Filename), and returns a list of tokens (X). It makes sure to close the file after reading it in.
lex(Filename, X):-
	open(Filename,read,Str),
	lexMain(Str,X),
	close(Str).

%helper rule to lex that takes in a stream of characters and returns a list of tokens.
lexMain(InStream, Tokens):-
	readWord(InStream, Token),
	readList(Token, Tokens, InStream).

%readList is a rule that takes in a token, returns a list of tokens, and takes in a stream of characters
%this particular iteration is the base case and reads in the end of file token. It also checks to see if
%there is any code after the program, and returns a warning if there is.
readList($,[$],InStream):- 
	peek_code(InStream,TestChar),
	afterProgramCodeChecker(TestChar), !.

%this iteration ignores an empty token (any whitespace), and continues reading
readList('',Tokens,InStream):-
	readWord(InStream,NextToken),
	readList(NextToken,Tokens,InStream).

%this iteration deals with quotes by taking in the quote and splitting it up into individual lexemes
readList(TestQuote,Tokens,InStream):-
	sub_string(TestQuote,0,1, _, '"'),
	string_chars(TestQuote,Quote),
	readWord(InStream,NextToken),
	readList(NextToken,NextTokens,InStream),
	append(Quote,NextTokens,Tokens).

%this iteration reads in any other token not dealt with yet, adds it to the list, and then continues reading
readList(Token,[Token|Tokens],InStream):-
	readWord(InStream,NextToken),
	readList(NextToken,Tokens,InStream).

%afterProgramCodeChecker is a rule that checks to make sure that the next token after the $ is the
%end_of_file character. If not, it issues a warning but allows the code to finish.
afterProgramCodeChecker(-1):- !.
afterProgramCodeChecker(end_of_file):- !.
afterProgramCodeChecker(_):- writeln('Warning- code exists after program ends.'), !.


%read-word is a helper rule that starts the process of reading in a token. it takes in a stream and returns a token (W)
readWord(InStream, W):-
	get_code(InStream,ThisChar),
	checkCharAndReadRest(ThisChar,Chars,InStream),
	atom_codes(W,Chars).

%checkCharAndReadRest is the rule that handles the bulk of the work in reading in a token
%it takes in a character, returns a list of characters, and takes in a stream. Each iteration
%of this rule handles a different character.

%These three handle different types of whitespace
checkCharAndReadRest(10,[],_):- !.
checkCharAndReadRest(32,[],_):- !.
checkCharAndReadRest(9,[],_):- !.

%these two handle if end of file is reached before reaching a $. A warning is issued in this case
%and a $ is added.
checkCharAndReadRest(-1,[$],_):- writeln('Warning- no $ included'), !.
checkCharAndReadRest(end_of_file,[$],_):- writeln('Warning- no $ included'), !.

%these two handle parenthesis
checkCharAndReadRest(40,[40],_):- !.
checkCharAndReadRest(41,[41],_):- !.

%these two handle curly braces
checkCharAndReadRest(123,[123],_):- !.
checkCharAndReadRest(125,[125],_):- !.

%this handles the dollar sign
checkCharAndReadRest(36,[36],_):- !.

%this set handles digits
checkCharAndReadRest(48,[48],_):- !.
checkCharAndReadRest(49,[49],_):- !.
checkCharAndReadRest(50,[50],_):- !.
checkCharAndReadRest(51,[51],_):- !.
checkCharAndReadRest(52,[52],_):- !.
checkCharAndReadRest(53,[53],_):- !.
checkCharAndReadRest(54,[54],_):- !.
checkCharAndReadRest(55,[55],_):- !.
checkCharAndReadRest(56,[56],_):- !.
checkCharAndReadRest(57,[57],_):- !.

%this handles the plus sign
checkCharAndReadRest(43,[43],_):- !.

%this handles the equals sign and continues to check if another equals sign is present
checkCharAndReadRest(61,[61|Chars],InStream):-
	peek_code(InStream,NextChar),
	checkBoolOp(NextChar,Chars,InStream).

%this handles the exclamation point and checks to see if an equals sign is present afterwards
checkCharAndReadRest(33,[33|Chars],InStream):-
	peek_code(InStream,NextChar),
	checkBoolOp(NextChar,Chars,InStream).

%the letter a
checkCharAndReadRest(97,[97],_):- !.

%the letter b. first checks to see if boolean can be formed. If not, simply returns a token of b.
checkCharAndReadRest(98,[98|Chars],InStream):- 
	peek_code(InStream,NextChar),
	checkBooleanOne(NextChar,Chars,InStream).	
checkCharAndReadRest(98,[98],_):- !.

%letters c-e
checkCharAndReadRest(99,[99],_):- !.
checkCharAndReadRest(100,[100],_):- !.
checkCharAndReadRest(101,[101],_):- !.

%letter f, checks for false first
checkCharAndReadRest(102,[102|Chars],InStream):- 
	peek_code(InStream,NextChar),
	checkFalseOne(NextChar,Chars,InStream).
checkCharAndReadRest(102,[102],_):- !.

%letters g and h
checkCharAndReadRest(103,[103],_):- !.
checkCharAndReadRest(104,[104],_):- !.

%letter i, checks for if and int first
checkCharAndReadRest(105,[105|Chars],InStream):- 
	peek_code(InStream,NextChar),
	checkIOne(NextChar,Chars,InStream).
checkCharAndReadRest(105,[105],_):- !.

%letters j-o
checkCharAndReadRest(106,[106],_):- !.
checkCharAndReadRest(107,[107],_):- !.
checkCharAndReadRest(108,[108],_):- !.
checkCharAndReadRest(109,[109],_):- !.
checkCharAndReadRest(110,[110],_):- !.
checkCharAndReadRest(111,[111],_):- !.

%letter p, checks for print first
checkCharAndReadRest(112,[112|Chars],InStream):- 
	peek_code(InStream,NextChar),
	checkPrintOne(NextChar,Chars,InStream).
checkCharAndReadRest(112,[112],_):- !.

%letters q and r
checkCharAndReadRest(113,[113],_):- !.
checkCharAndReadRest(114,[114],_):- !.

%letter s, checks for string first
checkCharAndReadRest(115,[115|Chars],InStream):- 
	peek_code(InStream,NextChar),
	checkStringOne(NextChar,Chars,InStream).
checkCharAndReadRest(115,[115],_):- !.

%letter t, checks for true first
checkCharAndReadRest(116,[116|Chars],InStream):- 
	peek_code(InStream,NextChar),
	checkTrueOne(NextChar,Chars,InStream).
checkCharAndReadRest(116,[116],_):- !.
	
%letters u and v
checkCharAndReadRest(117,[117],_):- !.
checkCharAndReadRest(118,[118],_):- !.

%letter w, checks for while first
checkCharAndReadRest(119,[119|Chars],InStream):-
	peek_code(InStream,NextChar),
	checkWhileOne(NextChar,Chars,InStream).
checkCharAndReadRest(119,[119],_):- !.

%letters x-z
checkCharAndReadRest(120,[120],_):- !.
checkCharAndReadRest(121,[121],_):- !.
checkCharAndReadRest(122,[122],_):- !.

%handles the first quotation mark and throws into quotationMode
checkCharAndReadRest(34,[34|Chars],InStream):-
	get_code(InStream,NextChar),
	quotationMode(NextChar,Chars,InStream).

%handles any other characters (such as upper case letters) and issues a lex error.
checkCharAndReadRest(X,[],_):-
	write('Invalid token- '),
	writeln(X), !, fail.

%checks for equals sign to conclude a boolean operator
checkBoolOp(61,[61],InStream):- get_code(InStream,_), !.

checkBoolOp(_,[],_):- !.

%handles the keyword boolean
checkBooleanOne(111,[111|Chars],InStream):- 
	get_code(InStream,_),
	get_code(InStream,NextChar),
	checkBooleanTwo(NextChar,Chars,InStream).

checkBooleanOne(_,[],_):- fail.

checkBooleanTwo(111,[111|Chars],InStream):- 
	get_code(InStream,NextChar),
	checkBooleanThree(NextChar,Chars,InStream).

checkBooleanTwo(_,[],_):- fail.

checkBooleanThree(108,[108|Chars],InStream):- 
	get_code(InStream,NextChar),
	checkBooleanFour(NextChar,Chars,InStream).

checkBooleanThree(_,[],_):- fail.

checkBooleanFour(101,[101|Chars],InStream):- 
	get_code(InStream,NextChar),
	checkBooleanFive(NextChar,Chars,InStream).

checkBooleanFour(_,[],_):- fail.

checkBooleanFive(97,[97|Chars],InStream):- 
	get_code(InStream,NextChar),
	checkBooleanSix(NextChar,Chars,InStream).

checkBooleanFive(_,[],_):- fail.

checkBooleanSix(110,[110],_):- !.

checkBooleanSix(_,[],_):- fail.

%handles the keyword false
checkFalseOne(97,[97|Chars],InStream):-
	get_code(InStream,_),
	get_code(InStream,NextChar),
	checkFalseTwo(NextChar,Chars,InStream).
	
checkFalseOne(_,[],_):- fail.

checkFalseTwo(108,[108|Chars],InStream):-
	get_code(InStream,NextChar),
	checkFalseThree(NextChar,Chars,InStream).
	
checkFalseTwo(_,[],_):- fail.

checkFalseThree(115,[115|Chars],InStream):-
	get_code(InStream,NextChar),
	checkFalseFour(NextChar,Chars,InStream).
	
checkFalseThree(_,[],_):- fail.

checkFalseFour(101,[101],_):- !.

checkFalseFour(_,[],_):- fail.

%handles the keywords if and int
checkIOne(102,[102],InStream):- 	
	get_code(InStream,_),!.

checkIOne(110,[110|Chars],InStream):-
	get_code(InStream,_),
	get_code(InStream,NextChar),
	checkITwo(NextChar,Chars,InStream).
	
checkIOne(_,[],_):- fail.

checkITwo(116,[116],_):- !.

checkITwo(_,[],_):- fail.

%handles the keyword print
checkPrintOne(114,[114|Chars],InStream):-
	get_code(InStream,_),
	get_code(InStream,NextChar),
	checkPrintTwo(NextChar,Chars,InStream).

checkPrintOne(_,[],_):- fail.

checkPrintTwo(105,[105|Chars],InStream):-
	get_code(InStream,NextChar),
	checkPrintThree(NextChar,Chars,InStream).

checkPrintTwo(_,[],_):- fail.

checkPrintThree(110,[110|Chars],InStream):-
	get_code(InStream,NextChar),
	checkPrintFour(NextChar,Chars,InStream).

checkPrintThree(_,[],_):- fail.

checkPrintFour(116,[116],_):- !.

checkPrintFour(_,[],_):- fail.

%handles the keyword string
checkStringOne(116,[116|Chars],InStream):-
	get_code(InStream,_),
	get_code(InStream,NextChar),
	checkStringTwo(NextChar,Chars,InStream).

checkStringOne(_,[],_):- fail.

checkStringTwo(114,[114|Chars],InStream):-
	get_code(InStream,NextChar),
	checkStringThree(NextChar,Chars,InStream).
	
checkStringTwo(_,[],_):- fail.

checkStringThree(105,[105|Chars],InStream):-
	get_code(InStream,NextChar),
	checkStringFour(NextChar,Chars,InStream).
	
checkStringThree(_,[],_):- fail.

checkStringFour(110,[110|Chars],InStream):-
	get_code(InStream,NextChar),
	checkStringFive(NextChar,Chars,InStream).

checkStringFour(_,[],_):- fail.

checkStringFive(103,[103],_):- !.

checkStringFive(_,[],_):- fail.

%handles the keyword true
checkTrueOne(114,[114|Chars],InStream):-
	get_code(InStream,_),
	get_code(InStream,NextChar),
	checkTrueTwo(NextChar,Chars,InStream).

checkTrueOne(_,[],_):- fail.

checkTrueTwo(117,[117|Chars],InStream):-
	get_code(InStream,NextChar),
	checkTrueThree(NextChar,Chars,InStream).

checkTrueTwo(_,[],_):- fail.

checkTrueThree(101,[101],_):- !.

checkTrueThree(_,[],_):- fail.

%handles the keyword while
checkWhileOne(104,[104|Chars],InStream):-
	get_code(InStream,_),
	get_code(InStream,NextChar),
	checkWhileTwo(NextChar,Chars,InStream).
	
checkWhileOne(_,[],_):- fail.

checkWhileTwo(105,[105|Chars],InStream):-
	get_code(InStream,NextChar),
	checkWhileThree(NextChar,Chars,InStream).

checkWhileTwo(_,[],__):- fail.

checkWhileThree(108,[108|Chars],InStream):-
	get_code(InStream,NextChar),
	checkWhileFour(NextChar,Chars,InStream).

checkWhileFour(101,[101],_):- !.

checkWhileFour(_,[],_):- fail.

%handles quotes
quotationMode(34,[34],_):- !.

quotationMode(X,[X|Chars],InStream):-
	get_code(InStream,NextChar),
	quotationMode(NextChar,Chars,InStream).

%error state in parse, can accept any character
errorHandling --> errorHandlingHelper.

errorHandlingHelper --> [].
errorHandlingHelper --> [_], errorHandlingHelper.

%a DCG that handles parsing and will show any errors that occur. It CAN get back on track after an error, but it isn't very good at it.

programNT --> blockNT, [$].

blockNT --> ['{'],  statementListNT, ['}'].
blockNT --> ['{'], statementListNT, errorHandling, {writeln('missing expected token }')}.

statementListNT --> statementNT, statementListNT.
statementListNT --> [].

statementNT --> printStatementNT.
statementNT --> assignmentStatementNT.
statementNT --> varDeclNT.
statementNT --> whileStatementNT.
statementNT --> ifStatementNT.
statementNT --> blockNT.

printStatementNT --> [print], ['('], exprNT, [')'].
printStatementNT --> [print], errorHandling, ['('], exprNT, [')'], {writeln('characters exist between print and (')}.
printStatementNT --> [print], errorHandling, [')'], {writeln('missing expected token ( in printStatement')}.
printStatementNT --> [print], ['('], exprNT, errorHandling, {writeln('missing expected token )')}.

assignmentStatementNT --> idNT, [=], exprNT.
assignmentStatementNT --> idNT, errorHandling, {writeln('missing expected token =')}.

varDeclNT --> typeNT, idNT.
varDeclNT --> typeNT, errorHandling, {writeln('missing expected identifier')}.

whileStatementNT --> [while], booleanExprNT, blockNT.
whileStatementNT --> [while], errorHandling, blockNT, {writeln('missing booleanExpr')}.

ifStatementNT --> [if], booleanExprNT, blockNT.
ifStatementNT --> [if], errorHandling, blockNT, {writeln('missing booleanExpr')}.

exprNT --> intExprNT.
exprNT --> stringExprNT.
exprNT --> booleanExprNT.
exprNT --> idNT.
exprNT --> errorHandling, {writeln('invalid expr type')}.

intExprNT --> digitNT, intopNT, exprNT.
intExprNT --> digitNT.
intExprNT --> digitNT, errorHandling, {writeln('invalid int op')}.

stringExprNT --> ['"'], charListNT, ['"'].
stringExprNT --> ['"'], errorHandling, ['"'], {writeln('invalid character in string')}.
stringExprNT --> ['"'], errorHandling, {writeln('missing expected token "')}.


booleanExprNT --> ['('], exprNT, boolopNT, exprNT, [')'].
booleanExprNT --> boolvalNT.
booleanExprNT --> ['('], exprNT, errorHandling, exprNT, [')'], {writeln('missing expected bool op')}.
booleanExprNT --> ['('], exprNT, boolopNT, exprNT, errorHandling, {writeln('missing expected token )')}.

idNT --> charNT.

charListNT --> charNT, charListNT.
charListNT --> spaceNT, charListNT.
charListNT --> [].

typeNT --> [int].
typeNT --> [string].
typeNT --> [boolean].

charNT --> [a].
charNT --> [b].
charNT --> [c].
charNT --> [d].
charNT --> [e].
charNT --> [f].
charNT --> [g].
charNT --> [h].
charNT --> [i].
charNT --> [j].
charNT --> [k].
charNT --> [l].
charNT --> [m].
charNT --> [n].
charNT --> [o].
charNT --> [p].
charNT --> [q].
charNT --> [r].
charNT --> [s].
charNT --> [t].
charNT --> [u].
charNT --> [v].
charNT --> [w].
charNT --> [x].
charNT --> [y].
charNT --> [z].

spaceNT --> [' '].

digitNT --> ['1'].
digitNT --> ['2'].
digitNT --> ['3'].
digitNT --> ['4'].
digitNT --> ['5'].
digitNT --> ['6'].
digitNT --> ['7'].
digitNT --> ['8'].
digitNT --> ['9'].
digitNT --> ['0'].

boolopNT --> ['=='].
boolopNT --> ['!='].

boolvalNT --> [false].
boolvalNT --> [true].

intopNT --> ['+'].

%rules for semantic analysis. They're set up to handle scope checking, type checking, and checking to see
%if a variable is declared twice. If any of these situations come up, the semantic analysis fails

typeCheck(0,_,_):- currentScope(X), scope(Y), write('type error in scope- '), writeln(X),
	write('last created scope- '), writeln(Y), abort.
typeCheck(X,Y,Z):- symbolTable(X,Y,S,_,_), S == Z.
typeCheck(X,Y,Z):- child(S,X), typeCheck(S,Y,Z).

scopeNoType(0,_,_):- currentScope(X), scope(Y), write('scope error in scope- '), writeln(X),
	write('last created scope- '), writeln(Y), abort.
scopeNoType(X,Y,Z):- symbolTable(X,Y,Z,_,_), !.
scopeNoType(X,Y,Z):- child(S,X), scopeNoType(S,Y,Z).

redeclareCheck(X,Y):- \+ symbolTable(X,Y,_,_,_).
redeclareCheck(_,_):-  currentScope(X), scope(Y), write('redeclared identifier in scope- '), writeln(X),
	write('last created scope- '), writeln(Y), abort.

%rules that act as modifier functions for initializing and using variables

initialize(0,_,_):- !.
initialize(X,Y,Z):-retract(symbolTable(X,Y,Z,0,U)), asserta(symbolTable(X,Y,Z,1,U)),!.
initialize(X,Y,Z):-child(S,X), initialize(S,Y,Z).

use(0,_,_):- !.
use(X,Y,Z):-retract(symbolTable(X,Y,Z,I,0)), asserta(symbolTable(X,Y,Z,I,1)),!.
use(X,Y,Z):-child(S,X), use(S,Y,Z).

%more semantic analysis rules. they check to see if a variable is initialized and unused or is declared but
%uninitialized. If so, they issue a warning and proceed on their merry way.

initializeCheck:- \+ symbolTable(_,_,_,0,_).
initializeCheck:- symbolTable(X,Y,Z,0,_), write('Warning- uninitialized variable exists in scope '),
	write(X), write(' with identifier '), write(Y),
	write(' and type '), writeln(Z), initialize(X,Y,Z), initializeCheck.

useCheck:- \+ symbolTable(_,_,_,_,0).
useCheck:- symbolTable(X,Y,Z,_,0), write('Warning- unused variable exists in scope '), write(X),
	write(' with identifier '), write(Y),
	write(' and type '), writeln(Z), use(X,Y,Z), useCheck.

%the actual semantic analysis DCG. type and scope checking is done at certain bottle-necks within the grammar.

programST --> blockST, [$].

blockST --> ['{'], {scope(X), Y is X + 1, currentScope(Z), assert(child(Z,Y)), asserta(scope(Y)), asserta(currentScope(Y))}, statementListST, closeBlockST.

closeBlockST --> ['}'], {retract(currentScope(_))}.

statementListST --> statementST, statementListST.
statementListST --> [].

statementST --> printStatementST.
statementST --> assignmentStatementST.
statementST --> varDeclST.
statementST --> whileStatementST.
statementST --> ifStatementST.
statementST --> blockST.

printStatementST --> [print], ['('], exprST, [')'].

assignmentStatementST --> idST, {temp(0,T), asserta(temp(2,T)), retract(temp(0,_))}, [=], exprST, {currentScope(X), temp(1,Z), temp(2,Y),scopeNoType(X,Y,_), typeCheck(X, Y, Z), initialize(X,Y,Z), retract(temp(_,_)) }.

varDeclST --> typeST, idST, {currentScope(X), retract(temp(0,Y)), retract(temp(1,Z)),redeclareCheck(X,Y), asserta(symbolTable(X,Y,Z,0,0))}.

whileStatementST --> [while], booleanExprST, blockST.

ifStatementST --> [if], booleanExprST, blockST.

exprST --> intExprST, {asserta(temp(1,'int'))}.
exprST --> stringExprST, {asserta(temp(1,'string'))}.
exprST --> booleanExprST, {asserta(temp(1,'boolean'))}.
exprST --> idST, {currentScope(X), temp(0,Y), scopeNoType(X,Y,Z), use(X,Y,Z), asserta(temp(1,Z))}.

intExprST --> digitST, intopST, exprST, intHelperST.
intExprST --> digitST.

intHelperST --> {temp(1,X), X == 'int'}.
intHelperST --> {currentScope(X), scope(Y), write('type mismatch in int expression in scope- '), writeln(X),
	write('last created scope- '), writeln(Y), abort}.

stringExprST --> ['"'], charListST, ['"'].

booleanExprST --> ['('], exprST, {temp(1,X), asserta(temp(3,X))}, boolopST, exprST, {temp(3,X), temp(1,Y), X == Y}, [')'].
booleanExprST --> ['('], {currentScope(X), scope(Y), write('type mismatch in boolean expression in scope- '), writeln(X),
	write('last created scope- '), writeln(Y), abort}.
booleanExprST --> boolvalST.

idST --> charST.

charListST --> charST, {retract(temp(_,_))}, charListST.
charListST --> spaceST, charListST.
charListST --> [].

typeST --> [int], {asserta(temp(1,'int'))}.
typeST --> [string], {asserta(temp(1,'string'))}.
typeST --> [boolean], {asserta(temp(1,'boolean'))}.

charST --> [a], {asserta(temp(0,a))}.
charST --> [b], {asserta(temp(0,b))}.
charST --> [c], {asserta(temp(0,c))}.
charST --> [d], {asserta(temp(0,d))}.
charST --> [e], {asserta(temp(0,e))}.
charST --> [f], {asserta(temp(0,f))}.
charST --> [g], {asserta(temp(0,g))}.
charST --> [h], {asserta(temp(0,h))}.
charST --> [i], {asserta(temp(0,i))}.
charST --> [j], {asserta(temp(0,j))}.
charST --> [k], {asserta(temp(0,k))}.
charST --> [l], {asserta(temp(0,l))}.
charST --> [m], {asserta(temp(0,m))}.
charST --> [n], {asserta(temp(0,n))}.
charST --> [o], {asserta(temp(0,o))}.
charST --> [p], {asserta(temp(0,p))}.
charST --> [q], {asserta(temp(0,q))}.
charST --> [r], {asserta(temp(0,r))}.
charST --> [s], {asserta(temp(0,s))}.
charST --> [t], {asserta(temp(0,t))}.
charST --> [u], {asserta(temp(0,u))}.
charST --> [v], {asserta(temp(0,v))}.
charST --> [w], {asserta(temp(0,w))}.
charST --> [x], {asserta(temp(0,x))}.
charST --> [y], {asserta(temp(0,y))}.
charST --> [z], {asserta(temp(0,z))}.

spaceST --> [' '].

digitST --> ['1'].
digitST --> ['2'].
digitST --> ['3'].
digitST --> ['4'].
digitST --> ['5'].
digitST --> ['6'].
digitST --> ['7'].
digitST --> ['8'].
digitST --> ['9'].
digitST --> ['0'].

boolopST --> ['=='].
boolopST --> ['!='].

boolvalST --> [false].
boolvalST --> [true].

intopST --> ['+'].

%DCG that produces a concrete syntax tree.

program(program(BLOCK, $)) --> block(BLOCK), [$].

block(block('{', STATEMENTLIST, '}')) --> ['{'],  statementList(STATEMENTLIST), ['}'].

statementList(statementList(STATEMENT, STATEMENTLIST)) --> statement(STATEMENT), statementList(STATEMENTLIST).
statementList(statementList([])) --> [].

statement(statement(PRINTSTATEMENT)) --> printStatement(PRINTSTATEMENT).
statement(statement(ASSIGNMENTSTATEMENT)) --> assignmentStatement(ASSIGNMENTSTATEMENT).
statement(statement(VARDECL)) --> varDecl(VARDECL).
statement(statement(WHILESTATEMENT)) --> whileStatement(WHILESTATEMENT).
statement(statement(IFSTATEMENT)) --> ifStatement(IFSTATEMENT).
statement(statement(BLOCK)) --> block(BLOCK).

printStatement(printStatement(print, '(', EXPR, ')')) --> [print], ['('], expr(EXPR), [')'].

assignmentStatement(assignmentStatement(ID, =, EXPR)) --> id(ID), [=], expr(EXPR).

varDecl(varDecl(TYPE, ID)) --> type(TYPE), id(ID).

whileStatement(whileStatement(while, BOOLEANEXPR, BLOCK)) --> [while], booleanExpr(BOOLEANEXPR), block(BLOCK).

ifStatement(ifStatement(if, BOOLEANEXPR, BLOCK)) --> [if], booleanExpr(BOOLEANEXPR), block(BLOCK).

expr(expr(INTEXPR)) --> intExpr(INTEXPR).
expr(expr(STRINGEXPR)) --> stringExpr(STRINGEXPR).
expr(expr(BOOLEANEXPR)) --> booleanExpr(BOOLEANEXPR).
expr(expr(ID)) --> id(ID).

intExpr(intExpr(DIGIT, INTOP, EXPR)) --> digit(DIGIT), intop(INTOP), expr(EXPR).
intExpr(intExpr(DIGIT)) --> digit(DIGIT).

stringExpr(stringExpr('"', CHARLIST, '"')) --> ['"'], charList(CHARLIST), ['"'].

booleanExpr(booleanExpr('(', EXPR, BOOLOP, EXPRPRIME, ')')) --> ['('], expr(EXPR), boolop(BOOLOP), expr(EXPRPRIME), [')'].
booleanExpr(booleanExpr(BOOLVAL)) --> boolval(BOOLVAL).

id(id(CHAR)) --> char(CHAR).

charList(charList(CHAR, CHARLIST)) --> char(CHAR), charList(CHARLIST).
charList(charList(SPACE, CHARLIST)) --> space(SPACE), charList(CHARLIST).
charList(charlist([])) --> [].

type(type(int)) --> [int].
type(type(string)) --> [string].
type(type(boolean)) --> [boolean].

char(char(a)) --> [a].
char(char(b)) --> [b].
char(char(c)) --> [c].
char(char(d)) --> [d].
char(char(e)) --> [e].
char(char(f)) --> [f].
char(char(g)) --> [g].
char(char(h)) --> [h].
char(char(i)) --> [i].
char(char(j)) --> [j].
char(char(k)) --> [k].
char(char(l)) --> [l].
char(char(m)) --> [m].
char(char(n)) --> [n].
char(char(o)) --> [o].
char(char(p)) --> [p].
char(char(q)) --> [q].
char(char(r)) --> [r].
char(char(s)) --> [s].
char(char(t)) --> [t].
char(char(u)) --> [u].
char(char(v)) --> [v].
char(char(w)) --> [w].
char(char(x)) --> [x].
char(char(y)) --> [y].
char(char(z)) --> [z].

space(space(' ')) --> [' '].

digit(digit(1)) --> ['1'].
digit(digit(2)) --> ['2'].
digit(digit(3)) --> ['3'].
digit(digit(4)) --> ['4'].
digit(digit(5)) --> ['5'].
digit(digit(6)) --> ['6'].
digit(digit(7)) --> ['7'].
digit(digit(8)) --> ['8'].
digit(digit(9)) --> ['9'].
digit(digit(0)) --> ['0'].

boolop(boolop('==')) --> ['=='].
boolop(boolop('!=')) --> ['!='].

boolval(boolval(false)) --> [false].
boolval(boolval(true)) --> [true].

intop(intop('+')) --> ['+'].

%DCG that produces an abstract syntax tree.

programAST((BLOCK, $)) --> blockAST(BLOCK), [$].

blockAST(('{', STATEMENTLIST, '}')) --> ['{'],  statementListAST(STATEMENTLIST), ['}'].

statementListAST((STATEMENT, STATEMENTLIST)) --> statementAST(STATEMENT), statementListAST(STATEMENTLIST).
statementListAST(([])) --> []. %Might be wrong

statementAST((PRINTSTATEMENT)) --> printStatementAST(PRINTSTATEMENT).
statementAST((ASSIGNMENTSTATEMENT)) --> assignmentStatementAST(ASSIGNMENTSTATEMENT).
statementAST((VARDECL)) --> varDeclAST(VARDECL).
statementAST((WHILESTATEMENT)) --> whileStatementAST(WHILESTATEMENT).
statementAST((IFSTATEMENT)) --> ifStatementAST(IFSTATEMENT).
statementAST((BLOCK)) --> blockAST(BLOCK).

printStatementAST((print, '(', EXPR, ')')) --> [print], ['('], exprAST(EXPR), [')'].

assignmentStatementAST((ID, =, EXPR)) --> idAST(ID), [=], exprAST(EXPR).

varDeclAST((TYPE, ID)) --> typeAST(TYPE), idAST(ID).

whileStatementAST((while, BOOLEANEXPR, BLOCK)) --> [while], booleanExprAST(BOOLEANEXPR), blockAST(BLOCK).

ifStatementAST((if, BOOLEANEXPR, BLOCK)) --> [if], booleanExprAST(BOOLEANEXPR), blockAST(BLOCK).

exprAST((INTEXPR)) --> intExprAST(INTEXPR).
exprAST((STRINGEXPR)) --> stringExprAST(STRINGEXPR).
exprAST((BOOLEANEXPR)) --> booleanExprAST(BOOLEANEXPR).
exprAST((ID)) --> idAST(ID).

intExprAST((DIGIT, INTOP, EXPR)) --> digitAST(DIGIT), intopAST(INTOP), exprAST(EXPR).
intExprAST((DIGIT)) --> digitAST(DIGIT).

stringExprAST(('"', CHARLIST, '"')) --> ['"'], charListAST(CHARLIST), ['"'].

booleanExprAST(('(', EXPR, BOOLOP, EXPRPRIME, ')')) --> ['('], exprAST(EXPR), boolopAST(BOOLOP), exprAST(EXPRPRIME), [')'].
booleanExprAST((BOOLVAL)) --> boolvalAST(BOOLVAL).

idAST((CHAR)) --> charAST(CHAR).

charListAST((CHAR, CHARLIST)) --> charAST(CHAR), charListAST(CHARLIST).
charListAST((SPACE, CHARLIST)) --> spaceAST(SPACE), charListAST(CHARLIST).
charListAST(([])) --> []. %could also be wrong

typeAST((int)) --> [int].
typeAST((string)) --> [string].
typeAST((boolean)) --> [boolean].

charAST((a)) --> [a].
charAST((b)) --> [b].
charAST((c)) --> [c].
charAST((d)) --> [d].
charAST((e)) --> [e].
charAST((f)) --> [f].
charAST((g)) --> [g].
charAST((h)) --> [h].
charAST((i)) --> [i].
charAST((j)) --> [j].
charAST((k)) --> [k].
charAST((l)) --> [l].
charAST((m)) --> [m].
charAST((n)) --> [n].
charAST((o)) --> [o].
charAST((p)) --> [p].
charAST((q)) --> [q].
charAST((r)) --> [r].
charAST((s)) --> [s].
charAST((t)) --> [t].
charAST((u)) --> [u].
charAST((v)) --> [v].
charAST((w)) --> [w].
charAST((x)) --> [x].
charAST((y)) --> [y].
charAST((z)) --> [z].

spaceAST((' ')) --> [' '].

digitAST((1)) --> ['1'].
digitAST((2)) --> ['2'].
digitAST((3)) --> ['3'].
digitAST((4)) --> ['4'].
digitAST((5)) --> ['5'].
digitAST((6)) --> ['6'].
digitAST((7)) --> ['7'].
digitAST((8)) --> ['8'].
digitAST((9)) --> ['9'].
digitAST((0)) --> ['0'].

boolopAST(('==')) --> ['=='].
boolopAST(('!=')) --> ['!='].

boolvalAST((false)) --> [false].
boolvalAST((true)) --> [true].

intopAST(('+')) --> ['+'].

tempVar(T,Y):- retract(tempVarTable(X)), Y is X + 1, asserta(tempVarTable(Y)), number_string(Y,S),
	string_concat("T", S, Z), string_concat(Z, "XX", T).
tempVar(T,0):- asserta(tempVarTable(0)), T = "T0XX".

jumpVar(J,Y):- retract(jumpVarTable(X)), Y is X + 1, asserta(jumpVarTable(Y)), number_string(Y,S),
	string_concat("J", S, Z), string_concat(Z, "XX", J).
jumpVar(J,0):- asserta(jumpVarTable(0)), J = "J0XX".

staticData(T,V,S,O):- child(P,S), staticData(T,V,P,O).

backpatch:- retract(staticData(T,V,S,O)),backpatchHelper(T,V,S,O), backpatch.
backpatch:- retract(jumpData(T,J)),backpatchHelper(T,J),backpatch.
backpatch:- tempVar(_,Offset), generatedCode(X), heap(Y), string_concat(X,Y,Z), string_length(Z, Length), Test is Length/2 + Offset, backpatchTest(Test), pad(Length).

backpatchTest(Test):- Test=<255.
backpatchTest(_):- writeln('Error: code too long'), abort.

pad(510):- retract(generatedCode(X)), retract(heap(Y)), string_concat(X,Y,Z), asserta(generatedCode(Z)).
pad(Length):- retract(generatedCode(X)), string_concat(X,"00",Z),asserta(generatedCode(Z)),NewLength is Length + 2, pad(NewLength).

backpatchHelper(T,_,_,O):- retract(generatedCode(X)),string_length(X,CodeLength),NumberLocation is (CodeLength/2 + O)*256,atomic_list_concat(L,T,X),format(string(Location),'~16R',NumberLocation),
	atomics_to_string(L,Location,Z), asserta(generatedCode(Z)).
backpatchHelper(T,J):- retract(generatedCode(X)),format(string(Jump),'~|~`0t~16R~2+',J),string_concat("D0",Jump,Y),atomic_list_concat(L,T,X),atomics_to_string(L,Y,Z),asserta(generatedCode(Z)).

lengthTest:- generatedCode(X), string_length(X,Length), backpatchTest(Length).

%DCG that generates code.
%TODO-boolean expressions need to be handled, while statements, if statements, and pad final product with 0's

programCG --> blockCG, [$], {retract(generatedCode(X)), string_concat(X,"00",Y), asserta(generatedCode(Y)),lengthTest}.

blockCG --> ['{'],{scope(X), Y is X + 1, currentScope(Z), assert(child(Z,Y)), asserta(scope(Y)), asserta(currentScope(Y))}, statementListCG, ['}'], {retract(currentScope(_))}.

statementListCG --> statementCG, statementListCG.
statementListCG --> [].

statementCG --> printStatementCG.
statementCG --> assignmentStatementCG.
statementCG --> varDeclCG.
statementCG --> whileStatementCG.
statementCG --> ifStatementCG.
statementCG --> blockCG.

printStatementCG --> [print], ['('], idCG, {temp(0,Identifier),currentScope(Scope),scopeNoType(Scope,Identifier,Type),retractall(temp(1,_)),asserta(temp(1,Type))}, printHelperCG, [')'].
printStatementCG --> [print], ['('], booleanExprCG, {retract(generatedCode(X)), string_concat(X,"8DFF00ACFF00A201FF",Z),asserta(generatedCode(Z)),lengthTest}, [')'].
printStatementCG --> [print], ['('], stringExprCG, {heap(Heap), string_length(Heap,Length), Position is 255 - Length/2, format(string(Location), '~|~`0t~16R~2+', Position), string_concat("A0", Location, W),
	string_concat(W,"A202FF",Y), retract(generatedCode(X)), string_concat(X,Y,Z), asserta(generatedCode(Z)),lengthTest}, [')'].
printStatementCG --> [print], ['('], {asserta(temp(intTest,'true'))},intExprCG, {retract(generatedCode(X)), string_concat(X,"8DFF00ACFF00A201FF",Z),asserta(generatedCode(Z)),lengthTest}, [')'].


printHelperCG --> {temp(1,Type), Type == 'string', currentScope(Scope),temp(0,Identifier),staticData(T,Identifier,Scope,_), string_concat("AC", T, V),
	string_concat(V, "A202FF", Y), retract(generatedCode(X)), string_concat(X, Y, Z), asserta(generatedCode(Z)),lengthTest}.
printHelperCG --> {temp(0,Identifier),currentScope(Scope),staticData(T,Identifier,Scope,_), string_concat("AC", T, V),
	string_concat(V, "A201FF", Y), retract(generatedCode(X)), string_concat(X, Y, Z), asserta(generatedCode(Z)),lengthTest}.

assignmentStatementCG --> idCG,{temp(0,Identifier),currentScope(Scope),scopeNoType(Scope,Identifier,Type),retractall(temp(1,_)), asserta(temp(1,Type))},assignmentHelperCG.

assignmentHelperCG --> {temp(1,Type), Type == 'string'},[=],stringAssignmentCG.
assignmentHelperCG --> [=],intAssignmentCG.


intAssignmentCG --> {temp(0,Identifier)},idCG,{temp(0,NewIdentifier),currentScope(Scope),staticData(T,Identifier,Scope,_),staticData(NewT,NewIdentifier,Scope,_), 
	string_concat("AD",NewT,V),string_concat(V,"8D",W),string_concat(W,T,Y), retract(generatedCode(X)), string_concat(X,Y,Z), asserta(generatedCode(Z)),lengthTest}.
intAssignmentCG --> {temp(0,Identifier),assert(temp('intTest','true'))},exprCG,{currentScope(Scope),staticData(T,Identifier,Scope,_),
	retract(generatedCode(X)), string_concat("8D",T,Y), string_concat(X,Y,Z), asserta(generatedCode(Z)),lengthTest}.
stringAssignmentCG --> {temp(0,Identifier)},idCG,{temp(0,NewIdentifier),currentScope(Scope),staticData(T,Identifier,Scope,_),staticData(NewT,NewIdentifier,Scope,_),
	string_concat("AD",NewT,V),string_concat(V,"8D",W),string_concat(W,T,Y), retract(generatedCode(X)), string_concat(X,Y,Z), asserta(generatedCode(Z)),lengthTest}.
stringAssignmentCG --> {temp(0,Identifier)},exprCG,{currentScope(Scope),heap(Heap), string_length(Heap,Length),Position is 255 - Length/2,
	format(string(Location), '~|~`0t~16R~2+', Position), string_concat("A9", Location, V), staticData(T, Identifier, Scope,_), string_concat(V, "8D", W),
	string_concat(W,T,Y), retract(generatedCode(X)), string_concat(X, Y, Z),asserta(generatedCode(Z)),lengthTest}.

varDeclCG --> [int], idCG, {generatedCode(X), string_concat(X,"A9008D",Y), tempVar(T, Offset), string_concat(Y, T, Z),asserta(generatedCode(Z)),lengthTest,temp(0,Identifier),currentScope(Scope),assert(staticData(T,Identifier,Scope,Offset))}.
varDeclCG --> [string], idCG, {tempVar(T, Offset),temp(0,Identifier),currentScope(Scope),assert(staticData(T,Identifier,Scope,Offset))}.
varDeclCG --> [boolean], idCG, {generatedCode(X), string_concat(X,"A9008D",Y), tempVar(T, Offset), string_concat(Y,T,Z),asserta(generatedCode(Z)),lengthTest,temp(0,Identifier),currentScope(Scope),assert(staticData(T,Identifier,Scope,Offset))}.

whileStatementCG --> [while],{generatedCode(L),string_length(L,Length),NewLength is Length/2}, booleanExprCG, {retract(generatedCode(X)),jumpVar(J,_),string_concat(X,J,Y),asserta(generatedCode(Y)),lengthTest}, blockCG,
	{retract(generatedCode(W)),string_length(W,JumpDistance),NewJump is JumpDistance/2, assert(jumpData(J,NewJump)),format(string(LengthString),'~|~`0t~16R~2+',NewLength),string_concat("A201ECFE00D0",
	LengthString,V),string_concat(W,V,Z), asserta(generatedCode(Z)),lengthTest}.

ifStatementCG --> [if], booleanExprCG, {retract(generatedCode(X)),jumpVar(J,_),string_concat(X,J,Y),asserta(generatedCode(Y)),lengthTest}, blockCG,
	{generatedCode(Z),string_length(Z,JumpDistance), NewJump is JumpDistance/2, assert(jumpData(J,NewJump))}.

exprCG --> intExprCG.
exprCG --> stringExprCG.
exprCG --> booleanExprCG.
exprCG --> idCG.

%slightly inconsistant handling of int expressions is necessary to ensure that things get added to the generated code properly and that the generated code doesn't get deleted.
intExprCG --> {retract(temp('intTest',Test)), Test == 'true', retractall('intTest')}, intHelperCG.
intExprCG --> digitCG, {temp(0,Digit), string_concat("A90", Digit, W), string_concat(W, "6DFF008DFF00", Y)}, intopCG, idCG,{retract(generatedCode(X)), string_concat(X,Y,V), temp(0,Identifier), currentScope(Scope), staticData(T,Identifier,Scope,_), string_concat("AD",T,U), string_concat(U,"6DFF00",S),string_concat(V,S,Z),asserta(generatedCode(Z)),lengthTest}.
intExprCG --> digitCG, intopCG, {temp(0,Digit), retract(generatedCode(X)), string_concat("A90", Digit, W), string_concat(W, "6DFF008DFF00", Y), string_concat(X,Y,Z), asserta(generatedCode(Z)),lengthTest}, intExprCG.
intExprCG --> digitCG, {temp(0,Digit), retract(generatedCode(X)), string_concat("A90", Digit, W), string_concat(W, "6DFF00",Y), string_concat(X,Y,Z), asserta(generatedCode(Z)),lengthTest}.

intHelperCG --> digitCG, {temp(0,Digit), string_concat("A90",Digit,W), string_concat(W,"8DFF00",Y)}, intopCG, idCG,{retract(generatedCode(X)), string_concat(X,Y,V), temp(0,Identifier), currentScope(Scope), staticData(T,Identifier,Scope,_), string_concat("AD",T,U), string_concat(U,"6DFF00",S),string_concat(V,S,Z),asserta(generatedCode(Z)),lengthTest}.
intHelperCG --> digitCG, intopCG, {temp(0,Digit), retract(generatedCode(X)), string_concat("A90",Digit,W), string_concat(W,"8DFF00",Y), string_concat(X,Y,Z), asserta(generatedCode(Z)),lengthTest}, intExprCG.
intHelperCG --> digitCG, {temp(0,Digit), retract(generatedCode(X)), string_concat("A90", Digit, Y), string_concat(X,Y,Z), asserta(generatedCode(Z)),lengthTest}.

stringExprCG --> ['"'], charListCG, {retract(heapString(W)),string_concat(W,"00",X), retract(heap(Y)), string_concat(X,Y,Z), asserta(heap(Z)),asserta(heapString(""))}, ['"'].

booleanExprCG --> ['('], {asserta(temp(intTest,'true'))},intExprCG,boolopCG, idCG,{retract(generatedCode(X)),temp(0,Identifier),currentScope(Scope),staticData(T,Identifier,Scope,_),string_concat(X,"8DFF00AEFF00EC",Y), 
	string_concat(Y,T,Z),asserta(generatedCode(Z)),lengthTest},booleanHelperCG,[')'].
booleanExprCG --> ['('], {asserta(temp(intTest,'true'))},intExprCG,{retract(generatedCode(X)),string_concat(X,"8DFF00AEFF00",W),asserta(generatedCode(W)),lengthTest}, boolopCG, {asserta(temp(intTest,'true'))},intExprCG, {retract(generatedCode(Y)),
	string_concat(Y,"8DFF00ECFF00",Z),asserta(generatedCode(Z)),lengthTest},booleanHelperCG, [')'].
booleanExprCG --> ['('], idCG,{temp(0,Identifier)}, boolopCG, idCG, {retract(generatedCode(X)),temp(0,NewIdentifier),currentScope(Scope),staticData(T,Identifier,Scope,_),
	string_concat(X,"AE",W),string_concat(W,T,V),staticData(NewT,NewIdentifier,Scope,_),string_concat(V,"EC",Y), string_concat(Y,NewT,Z),asserta(generatedCode(Z)),lengthTest}, booleanHelperCG, [')'].
booleanExprCG --> ['('], idCG,{temp(0,Identifier),retract(generatedCode(X)),string_concat(X,"AE",V),currentScope(Scope),staticData(T,Identifier,Scope,_),string_concat(V,T,W),asserta(generatedCode(W)),lengthTest},
	boolopCG,{asserta(temp(intTest,'true'))}, exprCG,{retract(generatedCode(Y)), string_concat(Y,"8DFF00ECFF00",Z), asserta(generatedCode(Z)),lengthTest}, booleanHelperCG, [')'].
booleanExprCG --> ['('], idCG, boolopCG, stringExprCG, booleanHelperCG, [')'].
booleanExprCG --> ['('], booleanExprCG, boolopCG, idCG,{retract(generatedCode(X)),temp(0,Identifier),currentScope(Scope),staticData(T,Identifier,Scope,_),string_concat(X,"8DFF00AEFF00EC",Y), 
	string_concat(Y,T,Z),asserta(generatedCode(Z)),lengthTest}, [')'].
booleanExprCG --> ['('], booleanExprCG,{retract(generatedCode(X)),string_concat(X,"8DFF00AEFF00",W),asserta(generatedCode(W)),lengthTest}, boolopCG, booleanExprCG, {retract(generatedCode(Y)),
	string_concat(Y,"8DFF00ECFF00",Z),asserta(generatedCode(Z)),lengthTest}, [')'].
booleanExprCG --> ['('], stringExprCG, boolopCG, stringExprCG, [')'].
booleanExprCG --> ['('], stringExprCG, boolopCG, idCG, [')'].
booleanExprCG --> boolvalCG.

booleanHelperCG --> {temp('boolop',Boolop), Boolop == "==", retract(generatedCode(X)), string_length(X, Length), NewLength is Length/2 + 6, format(string(LengthString),'~|~`0t~16R~2+',NewLength),
	string_concat("A900D0",LengthString,W),string_concat(W,"A901",Y),string_concat(X,Y,Z),asserta(generatedCode(Z)),lengthTest}.
booleanHelperCG --> {temp('boolop',Boolop), Boolop == "!=", retract(generatedCode(X)), string_length(X, Length), NewLength is Length/2 + 6, format(string(LengthString),'~|~`0t~16R~2+',NewLength),
	string_concat("A901D0",LengthString,W),string_concat(W,"A900",Y),string_concat(X,Y,Z),asserta(generatedCode(Z)),lengthTest}.

idCG --> charCG.

charListCG --> charCG, {retract(temp('ascii',CharCode)), retractall(temp(_,_)), retract(heapString(X)), string_concat(X,CharCode,Z), asserta(heapString(Z))}, charListCG.
charListCG --> spaceCG, {retract(heapString(X)), string_concat(X, "20", Z), asserta(heapString(Z))}, charListCG.
charListCG --> [].

charCG --> [a], {retractall(temp(0,_)),asserta(temp(0,a)),asserta(temp('ascii',"61"))}.
charCG --> [b], {retractall(temp(0,_)),asserta(temp(0,b)),asserta(temp('ascii',"62"))}.
charCG --> [c], {retractall(temp(0,_)),asserta(temp(0,c)),asserta(temp('ascii',"63"))}.
charCG --> [d], {retractall(temp(0,_)),asserta(temp(0,d)),asserta(temp('ascii',"64"))}.
charCG --> [e], {retractall(temp(0,_)),asserta(temp(0,e)),asserta(temp('ascii',"65"))}.
charCG --> [f], {retractall(temp(0,_)),asserta(temp(0,f)),asserta(temp('ascii',"66"))}.
charCG --> [g], {retractall(temp(0,_)),asserta(temp(0,g)),asserta(temp('ascii',"67"))}.
charCG --> [h], {retractall(temp(0,_)),asserta(temp(0,h)),asserta(temp('ascii',"68"))}.
charCG --> [i], {retractall(temp(0,_)),asserta(temp(0,i)),asserta(temp('ascii',"69"))}.
charCG --> [j], {retractall(temp(0,_)),asserta(temp(0,j)),asserta(temp('ascii',"6A"))}.
charCG --> [k], {retractall(temp(0,_)),asserta(temp(0,k)),asserta(temp('ascii',"6B"))}.
charCG --> [l], {retractall(temp(0,_)),asserta(temp(0,l)),asserta(temp('ascii',"6C"))}.
charCG --> [m], {retractall(temp(0,_)),asserta(temp(0,m)),asserta(temp('ascii',"6D"))}.
charCG --> [n], {retractall(temp(0,_)),asserta(temp(0,n)),asserta(temp('ascii',"6E"))}.
charCG --> [o], {retractall(temp(0,_)),asserta(temp(0,o)),asserta(temp('ascii',"6F"))}.
charCG --> [p], {retractall(temp(0,_)),asserta(temp(0,p)),asserta(temp('ascii',"70"))}.
charCG --> [q], {retractall(temp(0,_)),asserta(temp(0,q)),asserta(temp('ascii',"71"))}.
charCG --> [r], {retractall(temp(0,_)),asserta(temp(0,r)),asserta(temp('ascii',"72"))}.
charCG --> [s], {retractall(temp(0,_)),asserta(temp(0,s)),asserta(temp('ascii',"73"))}.
charCG --> [t], {retractall(temp(0,_)),asserta(temp(0,t)),asserta(temp('ascii',"74"))}.
charCG --> [u], {retractall(temp(0,_)),asserta(temp(0,u)),asserta(temp('ascii',"75"))}.
charCG --> [v], {retractall(temp(0,_)),asserta(temp(0,v)),asserta(temp('ascii',"76"))}.
charCG --> [w], {retractall(temp(0,_)),asserta(temp(0,w)),asserta(temp('ascii',"77"))}.
charCG --> [x], {retractall(temp(0,_)),asserta(temp(0,x)),asserta(temp('ascii',"78"))}.
charCG --> [y], {retractall(temp(0,_)),asserta(temp(0,y)),asserta(temp('ascii',"79"))}.
charCG --> [z], {retractall(temp(0,_)),asserta(temp(0,z)),asserta(temp('ascii',"7A"))}.

spaceCG --> [' '].

digitCG --> ['1'],{retractall(temp(0,_)),asserta(temp(0,"1"))}.
digitCG --> ['2'],{retractall(temp(0,_)),asserta(temp(0,"2"))}.
digitCG --> ['3'],{retractall(temp(0,_)),asserta(temp(0,"3"))}.
digitCG --> ['4'],{retractall(temp(0,_)),asserta(temp(0,"4"))}.
digitCG --> ['5'],{retractall(temp(0,_)),asserta(temp(0,"5"))}.
digitCG --> ['6'],{retractall(temp(0,_)),asserta(temp(0,"6"))}.
digitCG --> ['7'],{retractall(temp(0,_)),asserta(temp(0,"7"))}.
digitCG --> ['8'],{retractall(temp(0,_)),asserta(temp(0,"8"))}.
digitCG --> ['9'],{retractall(temp(0,_)),asserta(temp(0,"9"))}.
digitCG --> ['0'],{retractall(temp(0,_)),asserta(temp(0,"0"))}.

boolopCG --> ['=='], {retractall(temp('boolop',_)),asserta(temp('boolop',"=="))}.
boolopCG --> ['!='], {retractall(temp('boolop',_)),asserta(temp('boolop',"!="))}.

boolvalCG --> [false],{retract(generatedCode(X)), string_concat(X, "A900",Y), asserta(generatedCode(Y)),lengthTest}.
boolvalCG --> [true],{retract(generatedCode(X)), string_concat(X, "A901",Y), asserta(generatedCode(Y)),lengthTest}.

intopCG --> ['+'].
