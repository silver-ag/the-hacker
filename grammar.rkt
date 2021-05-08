#lang brag

line : let-statement | case-start | case-default | case-end | if-start | if-else | if-else-if | if-end | case-label | goto | expression | ()
let-statement : /LET NAMESTR /"=" expression
case-start : /CASE expression /":"
case-default: /DEFAULT /":"
case-end : /ENDCASE
if-start : /IF expression /":"
if-else : /ELSE ":"
if-else-if : /ELSEIF expression ":"
if-end : /ENDIF
case-label : (literal | list-literal)+ /":"
goto: /GOTO expression
@expression: delayed-variable | funcall | literal | list-literal | operation | /"(" expression /")"
funcall : NAMESTR /"(" expression? (/"," expression)* /")"
literal : NUMBER | STRING
list-literal: /"[" expression? (/"," expression)* /"]"
operation: expression OPERATOR expression
delayed-variable: DELAYEDVARIABLE