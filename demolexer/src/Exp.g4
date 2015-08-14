lexer grammar Exp;

WS : [ \t\r\n]+ -> skip ;
INT: [0-9]+;
ID: [a-zA-Z][a-zA-Z0-9]*;
VAR: 'var';
ADD: '+';
SUB: '-';
MUL: '*';
DIV: '/';