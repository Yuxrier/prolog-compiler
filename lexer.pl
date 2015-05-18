lexAndParse(Filename):-
	lex(Filename, X), !,
	programNT(X, Y), !,
	program(CST, X, []), !,
	programAST(AST, X, []),
	write('CST - '),
	writeln(CST),
	write('AST - '),
	writeln(AST).

lex(Filename, X):-
	open(Filename,read,Str),
	lexMain(Str,X),
	close(Str).

lexMain(InStream, Tokens):-
	readWord(InStream, Token),
	readList(Token, Tokens, InStream).

readList($,[$],_):- !.

readList('',Tokens,InStream):-
	readWord(InStream,NextToken),
	readList(NextToken,Tokens,InStream).

readList(Token,[Token|Tokens],InStream):-
	readWord(InStream,NextToken),
	readList(NextToken,Tokens,InStream).

readWord(InStream, W):-
	get_code(InStream,ThisChar),
	checkCharAndReadRest(ThisChar,Chars,InStream),
	atom_codes(W,Chars).

checkCharAndReadRest(10,[],_):- !.
checkCharAndReadRest(32,[],_):- !.
checkCharAndReadRest(-1,[],_):- !.
checkCharAndReadRest(end_of_file,[],_):- !.

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

checkCharAndReadRest(43,[43],_):- !.

checkCharAndReadRest(61,[61|Chars],InStream):-
	get_code(InStream,NextChar),
	checkBoolOp(NextChar,Chars,InStream).

checkCharAndReadRest(33,[33|Chars],InStream):-
	get_code(InStream,NextChar),
	checkBoolOp(NextChar,Chars,InStream).

checkBoolOp(61,[61],_):- !.

checkBoolOp(_,[],_):- fail.

checkCharAndReadRest(97,[97],_):- !.

checkCharAndReadRest(98,[98|Chars],InStream):- 
	get_code(InStream,NextChar),
	checkBooleanOne(NextChar,Chars,InStream).	
checkCharAndReadRest(98,[98],_):- !.

checkCharAndReadRest(99,[99],_):- !.
checkCharAndReadRest(100,[100],_):- !.
checkCharAndReadRest(101,[101],_):- !.

checkCharAndReadRest(102,[102|Chars],InStream):- 
	get_code(InStream,NextChar),
	checkFalseOne(NextChar,Chars,InStream).
checkCharAndReadRest(102,[102],_):- !.

checkCharAndReadRest(103,[103],_):- !.
checkCharAndReadRest(104,[104],_):- !.

checkCharAndReadRest(105,[105|Chars],InStream):- 
	get_code(InStream,NextChar),
	checkIOne(NextChar,Chars,InStream).
checkCharAndReadRest(105,[105],_):- !.

checkCharAndReadRest(106,[106],_):- !.
checkCharAndReadRest(107,[107],_):- !.
checkCharAndReadRest(108,[108],_):- !.
checkCharAndReadRest(109,[109],_):- !.
checkCharAndReadRest(110,[110],_):- !.
checkCharAndReadRest(111,[111],_):- !.

checkCharAndReadRest(112,[112|Chars],InStream):- 
	get_code(InStream,NextChar),
	checkPrintOne(NextChar,Chars,InStream).
checkCharAndReadRest(112,[112],_):- !.

checkCharAndReadRest(113,[113],_):- !.
checkCharAndReadRest(114,[114],_):- !.

checkCharAndReadRest(115,[115|Chars],InStream):- 
	get_code(InStream,NextChar),
	checkStringOne(NextChar,Chars,InStream).
checkCharAndReadRest(115,[115],_):- !.

checkCharAndReadRest(116,[116|Chars],InStream):- 
	get_code(InStream,NextChar),
	checkTrueOne(NextChar,Chars,InStream).
checkCharAndReadRest(116,[116],_):- !.
	

checkCharAndReadRest(117,[117],_):- !.
checkCharAndReadRest(118,[118],_):- !.

checkCharAndReadRest(119,[119|Chars],InStream):-
	get_code(InStream,NextChar),
	checkWhileOne(NextChar,Chars,InStream).
checkCharAndReadRest(119,[119],_):- !.

checkCharAndReadRest(120,[120],_):- !.
checkCharAndReadRest(121,[121],_):- !.
checkCharAndReadRest(122,[122],_):- !.

checkBooleanOne(111,[111|Chars],InStream):- 
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

checkBooleanSix(110,[110],InStream):- !.

checkBooleanSix(_,[],_):- fail.

checkFalseOne(97,[97|Chars],InStream):-
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

checkIOne(102,[102],_):- !.

checkIOne(110,[110|Chars],InStream):-
	get_code(InStream,NextChar),
	checkITwo(NextChar,Chars,InStream).
	
checkIOne(_,[],_):- fail.

checkITwo(116,[116],_):- !.

checkITwo(_,[],_):- fail.

checkPrintOne(114,[114|Chars],InStream):-
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

checkStringOne(116,[116|Chars],InStream):-
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

checkTrueOne(114,[114|Chars],InStream):-
	get_code(InStream,NextChar),
	checkTrueTwo(NextChar,Chars,InStream).

checkTrueOne(_,[],_):- fail.

checkTrueTwo(117,[117|Chars],InStream):-
	get_code(InStream,NextChar),
	checkTrueThree(NextChar,Chars,InStream).

checkTrueTwo(_,[],_):- fail.

checkTrueThree(101,[101],_):- !.

checkTrueThree(_,[],_):- fail.

checkWhileOne(104,[104|Chars],InStream):-
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

checkCharAndReadRest(34,[34|Chars],InStream):-
	get_code(InStream,NextChar),
	quotationMode(NextChar,Chars,InStream).

quotationMode(34,[34],_):- !.

quotationMode(_,[_|Chars],InStream):-
	get_code(InStream,NextChar),
	quotationMode(NextChar,Chars,InStream).

programNT --> blockNT, [$].

blockNT --> ['{'],  statementListNT, ['}'].

statementListNT --> statementNT, statementListNT.
statementListNT --> [].

statementNT --> printStatementNT.
statementNT --> assignmentStatementNT.
statementNT --> varDeclNT.
statementNT --> whileStatementNT.
statementNT --> ifStatementNT.
statementNT --> blockNT.

printStatementNT --> [print], ['('], exprNT, [')'].

assignmentStatementNT --> idNT, ['='], exprNT.

varDeclNT --> typeNT, idNT.

whileStatementNT --> [while], booleanExprNT, blockNT.

ifStatementNT --> [if], booleanExprNT, blockNT.

exprNT --> intExprNT.
exprNT --> stringExprNT.
exprNT --> booleanExprNT.
exprNT --> idNT.

intExprNT --> digitNT, intopNT, exprNT.
intExprNT --> digitNT.

stringExprNT --> ['"'], charListNT, ['"'].

booleanExprNT --> ['('], exprNT, boolopNT, exprNT, [')'].
booleanExprNT --> boolvalNT.

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

digitNT --> [1].
digitNT --> [2].
digitNT --> [3].
digitNT --> [4].
digitNT --> [5].
digitNT --> [6].
digitNT --> [7].
digitNT --> [8].
digitNT --> [9].
digitNT --> [0].

boolopNT --> ['=='].
boolopNT --> ['!='].

boolvalNT --> [false].
boolvalNT --> [true].

intopNT --> ['+'].

program(program(BLOCK, $)) --> block(BLOCK), [$].

block(block('{', STATEMENTLIST, '}')) --> ['{'],  statementList(STATEMENTLIST), ['}'].

statementList(statementList(STATEMENT, STATEMENTLIST)) --> statement(STATEMENT), statementList(STATEMENTLIST).
statementList(statementList([])) --> []. %Might be wrong

statement(statement(PRINTSTATEMENT)) --> printStatement(PRINTSTATEMENT).
statement(statement(ASSIGNMENTSTATEMENT)) --> assignmentStatement(ASSIGNMENTSTATEMENT).
statement(statement(VARDECL)) --> varDecl(VARDECL).
statement(statement(WHILESTATEMENT)) --> whileStatement(WHILESTATEMENT).
statement(statement(IFSTATEMENT)) --> ifStatement(IFSTATEMENT).
statement(statement(BLOCK)) --> block(BLOCK).

printStatement(printStatement(print, '(', EXPR, ')')) --> [print], ['('], expr(EXPR), [')'].

assignmentStatement(assignmentStatement(ID, '=', EXPR)) --> id(ID), ['='], expr(EXPR).

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
charList(charlist([])) --> []. %could also be wrong

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

digit(digit(1)) --> [1].
digit(digit(2)) --> [2].
digit(digit(3)) --> [3].
digit(digit(4)) --> [4].
digit(digit(5)) --> [5].
digit(digit(6)) --> [6].
digit(digit(7)) --> [7].
digit(digit(8)) --> [8].
digit(digit(9)) --> [9].
digit(digit(0)) --> [0].

boolop(boolop('==')) --> ['=='].
boolop(boolop('!=')) --> ['!='].

boolval(boolval(false)) --> [false].
boolval(boolval(true)) --> [true].

intop(intop('+')) --> ['+'].

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

assignmentStatementAST((ID, '=', EXPR)) --> idAST(ID), ['='], exprAST(EXPR).

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

digitAST((1)) --> [1].
digitAST((2)) --> [2].
digitAST((3)) --> [3].
digitAST((4)) --> [4].
digitAST((5)) --> [5].
digitAST((6)) --> [6].
digitAST((7)) --> [7].
digitAST((8)) --> [8].
digitAST((9)) --> [9].
digitAST((0)) --> [0].

boolopAST(('==')) --> ['=='].
boolopAST(('!=')) --> ['!='].

boolvalAST((false)) --> [false].
boolvalAST((true)) --> [true].

intopAST(('+')) --> ['+'].
