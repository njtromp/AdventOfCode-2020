grammar Day18_2020;
prog:	(expr NEWLINE?)* ;
expr:	expr ('*'|'+') expr
    |	INT
    |	'(' expr ')'
    ;
prog2:	(expr2 NEWLINE?)* ;
expr2:	expr2 '+' expr2
    |   expr2 '*' expr2
    |	INT
    |	'(' expr2 ')'
    ;
WHITESPACE: [\t ] -> skip;
NEWLINE : [\r\n]+ ;
INT     : [0-9]+;
