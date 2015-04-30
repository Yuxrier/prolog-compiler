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
