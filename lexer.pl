
lex(InStream, Tokens):-
	readWord(InStream, Token),
	readList(Token, Tokens, InStream).

readList(Token,[Token|Tokens],InStream):-
	readWord(InStream,Token),
	readList(nextToken,Tokens,InStream).

readWord(InStream, W):-
	get_code(InStream,thisChar),
	checkCharAndReadRest(thisChar,Chars,InStream),
	atom_codes(W,Chars).

checkCharAndReadRest(10,[],_):- !.
checkCharAndReadRest(32,[],_):- !.
checkCharAndReadRest(-1,[],_):- !.

checkCharAndReadRest(40,[40],_):- !.
checkCharAndReadRest(41,[41],_):- !.

checkCharAndReadRest(123,[123],_):- !.
checkCharAndReadRest(125,[125],_):- !.

checkCharAndReadRest(36,[36],_):- !.

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

checkCharAndReadRest(97,[97],_):- !.

checkCharAndReadRest(98,[98|Chars],InStream):- 
	get_code(InStream,nextChar),
	checkBooleanOne(nextChar,Chars,InStream).	
checkCharAndReadRest(98,[98],_):- !.

checkCharAndReadRest(99,[99],_):- !.
checkCharAndReadRest(100,[100],_):- !.
checkCharAndReadRest(101,[101],_):- !.

checkCharAndReadRest(102,[102|Chars],InStream):- 
	get_code(InStream,nextChar),
	checkFalseOne(nextChar,Chars,InStream).
checkCharAndReadRest(102,[102],_):- !.

checkCharAndReadRest(103,[103],_):- !.
checkCharAndReadRest(104,[104],_):- !.

checkCharAndReadRest(105,[105|Chars],InStream):- 
	getCode(InStream,nextChar),
	checkIOne(nextChar,Chars,InStream).
checkCharAndReadRest(105,[105],_):- !.

checkCharAndReadRest(106,[106],_):- !.
checkCharAndReadRest(107,[107],_):- !.
checkCharAndReadRest(108,[108],_):- !.
checkCharAndReadRest(109,[109],_):- !.
checkCharAndReadRest(110,[110],_):- !.
checkCharAndReadRest(111,[111],_):- !.

checkCharAndReadRest(112,[112|Chars],InStream):- . %print

checkCharAndReadRest(113,[113],_):- !.
checkCharAndReadRest(114,[114],_):- !.

checkCharAndReadRest(115,[115|Chars],InStream):- . %string

checkCharAndReadRest(116,[116|Chars],InStream):- . %true

checkCharAndReadRest(117,[117],_):- !.
checkCharAndReadRest(118,[118],_):- !.

checkCharAndReadRest(119,[119|Chars],InStream):- . %while

checkCharAndReadRest(120,[120],_):- !.
checkCharAndReadRest(121,[121],_):- !.
checkCharAndReadRest(122,[122],_):- !.

checkBooleanOne(111,[111|Chars],InStream):- 
	get_code(InStream,nextChar),
	checkBooleanTwo(nextChar,Chars,InStream).

checkBooleanOne(_,[],_):- fail.

checkBooleanTwo(111,[111|Chars],InStream):- 
	get_code(InStream,nextChar),
	checkBooleanThree(nextChar,Chars,InStream).

checkBooleanTwo(_,[],_):- fail.

checkBooleanThree(108,[108|Chars],InStream):- 
	get_code(InStream,nextChar),
	checkBooleanFour(nextChar,Chars,InStream).

checkBooleanThree(_,[],_):- fail.

checkBooleanFour(101,[101|Chars],InStream):- 
	get_code(InStream,nextChar),
	checkBooleanFive(nextChar,Chars,InStream).

checkBooleanFour(_,[],_):- fail.

checkBooleanFive(97,[97|Chars],InStream):- 
	get_code(InStream,nextChar),
	checkBooleanSix(nextChar,Chars,InStream).

checkBooleanFive(_,[],_):- fail.

checkBooleanSix(110,[110],InStream):- !.

checkBooleanSix(_,[],_):- fail.

checkFalseOne(97,[97|Chars],InStream):-
	get_code(InStream,nextChar),
	checkFalseTwo(nextChar,Chars,InStream).
	
checkFalseOne(_,[],_):- fail.

checkFalseTwo(108,[108|Chars],InStream):-
	get_code(InStream,nextChar),
	checkFalseThree(nextChar,Chars,InStream).
	
checkFalseTwo(_,[],_):- fail.

checkFalseThree(115,[115|Chars],InStream):-
	get_code(InStream,nextChar),
	checkFalseFour(nextChar,Chars,InStream).
	
checkFalseThree(_,[],_):- fail.

checkFalseFour(101,[101],_):- !.

checkFalseFour(_,[],_):- fail.

checkIOne(102,[102],_):- !.

checkIOne(110,[110|Chars],InStream):-
	get_code(InStream,nextChar),
	checkITwo(nextChar,Chars,InStream).
	
checkIOne(_,[],_):- fail.

checkITwo(116,[116],_):- !.

checkITwo(_,[],_):- fail.

checkCharAndReadRest(34,[34|Chars],InStream):-
	get_code(InStream,nextChar),
	quotationMode(nextChar,Chars,InStream).

checkCharAndReadRest(thisChar,[thisChar|Chars],InStream):-
	get_code(InStream, nextChar),
	checkCharAndReadRest(nextChar,Chars,InStream).



program --> block, [$].

block --> [{],  statementList, [}].

statementList --> statement, statementList.
statementList --> [].

statement --> printStatement.
statement --> assignmentStatement.
statement --> varDecl.
statement --> whileStatement.
statement --> ifStatement.
statement --> block.

printStatement --> [print], [(], expr, [)].

assignmentStatement --> id, [=], expr.

varDecl --> type, id.

whileStatement --> [while], booleanExpr, block.

ifStatement --> [if], booleanExpr, block.

expr --> intExpr.
expr --> stringExpr.
expr --> booleanExpr.
expr --> id.

intExpr --> digit, intop, expr.
intExpr --> digit.

stringExpr --> ["], charList, ["].

booleanExpr --> [(], expr, boolop, expr, [)].
booleanExpr --> boolval.

id --> char.

charList --> char, charList.
charList --> space, charList.
charList --> [].

type --> [int].
type --> [string].
type --> [boolean].

char --> [a].
char --> [b].
char --> [c].
char --> [d].
char --> [e].
char --> [f].
char --> [g].
char --> [h].
char --> [i].
char --> [j].
char --> [k].
char --> [l].
char --> [m].
char --> [n].
char --> [o].
char --> [p].
char --> [q].
char --> [r].
char --> [s].
char --> [t].
char --> [u].
char --> [v].
char --> [w].
char --> [x].
char --> [y].
char --> [z].

space --> [ ].

digit --> [1].
digit --> [2].
digit --> [3].
digit --> [4].
digit --> [5].
digit --> [6].
digit --> [7].
digit --> [8].
digit --> [9].
digit --> [0].

boolop --> [==].
boolop --> [!=].

boolval --> [false].
boolval --> [true].

intop --> [+].
