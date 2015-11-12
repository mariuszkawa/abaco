/*
 * Copyright (c) 2015, Codigeria <copyright@codigeria.com>
 *
 * Permission to use, copy, modify, and/or distribute this software for any
 * purpose with or without fee is hereby granted,   provided that the above
 * copyright notice and this permission notice appear in all copies.

 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD  TO THIS SOFTWARE  INCLUDING  ALL IMPLIED WARRANTIES OF MER-
 * CHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
 * SPECIAL,   DIRECT,   INDIRECT,   OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE,   DATA OR PROFITS,  WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,  ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */
 
/*
 * Author: Mariusz Kawa <mariusz.kawa@codigeria.com>
 */
grammar Abaco;

tokens { INDENT, DEDENT }

@lexer::members {
    private java.util.LinkedList<Token> tokens = new java.util.LinkedList<>();
    private java.util.Stack<Integer> indents = new java.util.Stack<>();

    private int opened = 0;

    private Token lastToken = null;

    @Override
    public void emit(Token t) {
        super.setToken(t);
        tokens.offer(t);
    }

    @Override
    public Token nextToken() {
        if (_input.LA(1) == EOF && !this.indents.isEmpty()) {
            for (int i = tokens.size() - 1; i >= 0; i--) {
                if (tokens.get(i).getType() == EOF) {
                    tokens.remove(i);
                }
            }
            this.emit(commonToken(AbacoParser.NEWLINE, "\n"));
            while (!indents.isEmpty()) {
                this.emit(createDedent());
                indents.pop();
            }
            this.emit(commonToken(AbacoParser.EOF, "<EOF>"));
        }
        Token next = super.nextToken();
        if (next.getChannel() == Token.DEFAULT_CHANNEL) {
            this.lastToken = next;
        }
        return tokens.isEmpty() ? next : tokens.poll();
    }

    private Token createDedent() {
        CommonToken dedent = commonToken(AbacoParser.DEDENT, "");
        dedent.setLine(this.lastToken.getLine());
        return dedent;
    }

    private CommonToken commonToken(int type, String text) {
        int stop = this.getCharIndex() - 1;
        int start = text.isEmpty() ? stop : stop - text.length() + 1;
        return new CommonToken(this._tokenFactorySourcePair, type, 
                               DEFAULT_TOKEN_CHANNEL, start, stop);
    }

    static int getIndentationCount(String spaces) {
        int count = 0;
        for (char ch : spaces.toCharArray()) {
            switch (ch) {
                case '\t':
                    count += 8 - (count % 8);
                    break;
                default:
                    count++;
            }
        }
        return count;
    }

    boolean atStartOfInput() {
        return super.getCharPositionInLine() == 0 && super.getLine() == 1;
    }
}

script
    : ( NEWLINE | statement )* EOF
    ;

statement
    : compoundStatement
    | simpleStatement NEWLINE
    ;

compoundStatement
    : conditionalStatement
    | forLoop
    | whileLoop
    | exceptionHandling
    | functionDefinition
    | anonymousFunctionAssignemnt
    ;

conditionalStatement
    : ifConditionalClause elseIfConditionalClause* elseConditionalClause?
    ;

ifConditionalClause
    : IF expression statementBlock
    ;

elseIfConditionalClause
    : ELSE ifConditionalClause
    ;

elseConditionalClause
    : ELSE statementBlock
    ;

forLoop
    : FOR ( Identifier COLON )? Identifier IN expression statementBlock
    ;

whileLoop
    : WHILE expression statementBlock
    ;

exceptionHandling
    : tryClause catchClause? finallyClause?
    ;

tryClause
    : TRY statementBlock
    ;

catchClause
    : CATCH Identifier statementBlock
    ;

finallyClause
    : FINALLY statementBlock
    ;

functionDefinition
    : FUNCTION Identifier functionArguments? statementBlock
    ;

functionArguments
    : LPAREN RPAREN
    | LPAREN Identifier ( COMMA Identifier )* COMMA? RPAREN
    ;

anonymousFunctionAssignemnt
    : leftHandSide EQ FUNCTION functionArguments? statementBlock
    ;

leftHandSide
    : Identifier ( ( DOT Identifier ) | ( LPAREN expression COMMA? RPAREN ) )?
    ;

statementBlock
    : NEWLINE ( INDENT statement ( NEWLINE | statement )* DEDENT )?
    ;

simpleStatement
    : throwStatement
    | assignment
    | expression
    ;

throwStatement
    : THROW expression
    ;

assignment
    : leftHandSide EQ expression
    ;

expression
    : orExpression
    ;

orExpression
    : andExpression ( OR andExpression )*
    ;

andExpression
    : notExpression ( AND notExpression )*
    ;

notExpression
    : NOT notExpression
    | comparisonExpression
    ;

comparisonExpression
    : arithmeticExpression ( comparisonOperator arithmeticExpression )*
    ;

comparisonOperator
    : NE
    | LE
    | GE
    | EQ
    | GT
    | LT
    ;

arithmeticExpression
    : term ( ( ADD | SUB ) term )*
    ;

term
    : factor ( ( MUL | DIV ) factor )*
    ;

factor
    : ( ADD | SUB ) factor
    | atom trailer*
    ;

trailer
    : functionCall
    | DOT Identifier
    ;
    
functionCall
    : LPAREN RPAREN
    | LPAREN expression ( COMMA expression )* COMMA? RPAREN
    ;

atom
    : LPAREN expression RPAREN
    | Identifier
    | simpleLiteral
    | structureLiteral
    | functionLiteral
    ;

simpleLiteral
    : NilLiteral
    | BooleanLiteral
    | NumberLiteral
    | StringLiteral
    ;

structureLiteral
    : LPAREN RPAREN
    | LPAREN expression COMMA RPAREN
    | LPAREN expression COLON expression COMMA? RPAREN
    | LPAREN structureElement ( COMMA structureElement )+ COMMA? RPAREN
    ;

structureElement
    : ( expression COLON )? expression
    ;

functionLiteral
    : ( Identifier | functionArguments ) ARROW expression
    ;

// Literals

NilLiteral
    : 'nil'
    ;

BooleanLiteral
    : 'false'
    | 'true'
    ;

NumberLiteral
    : PlainNotationDecimalNumber
    | ScientificNotationDecimalNumber
    | HexadecimalInteger
    ;

StringLiteral
    : '"' StringCharacter* '"'
    ;

// Keywords

// Boolean Operators
NOT : 'not' BR;
AND : 'and' BR;
OR : 'or' BR;

// Conditional Statement
IF : 'if' BR;
ELSE : 'else';

// Loop Statement
FOR : 'for' BR;
IN : 'in' BR;
WHILE : 'while' BR;

// Exception Handling
THROW : 'throw' BR;
TRY : 'try';
CATCH : 'catch' BR;
FINALLY : 'finally';

// Function Definition
FUNCTION : 'function' BR;

// New Line

NEWLINE
    : ( {atStartOfInput()}? WHITESPACE | ( '\r'? '\n' | '\r' ) WHITESPACE? )
    {
        String newLine = getText().replaceAll("[^\r\n]+", "");
        String spaces = getText().replaceAll("[\r\n]+", "");
        int next = _input.LA(1);
        if (opened > 0 || next == '\r' || next == '\n' || next == '\'') {
            skip();
        }
        else {
            emit(commonToken(NEWLINE, newLine));
            int indent = getIndentationCount(spaces);
            int previous = indents.isEmpty() ? 0 : indents.peek();
            if (indent == previous) {
                skip();
            }
            else if (indent > previous) {
                indents.push(indent);
                emit(commonToken(AbacoParser.INDENT, spaces));
            }
            else {
                while(!indents.isEmpty() && indents.peek() > indent) {
                    this.emit(createDedent());
                    indents.pop();
                }
            }
        }
    }
    ;

// Separators

ARROW : '=>' BR;
COMMA : ',' BR;
DOT : '.' BR;
LPAREN : '(' {opened++;};
RPAREN : ')' {opened--;};
COLON : ':' BR;

// Operators

// Relational
NE : '<>' BR;
LE : '<=' BR;
GE : '>=' BR;
EQ : '=' BR;
GT : '>' BR;
LT : '<' BR;

// Arithmetic
ADD : '+' BR;
SUB : '-' BR;
MUL : '*' BR;
DIV : '/' BR;

// Identifiers

Identifier
    : IdentifierLetter IdentifierLetterOrDigit*
    ;

// Other

SKIP
    : ( WHITESPACE | COMMENT ) -> skip
    ;

// Fragments

fragment PlainNotationDecimalNumber
    : DecimalDigit+ DecimalFraction?
    ;

fragment ScientificNotationDecimalNumber
    : Mantissa ExponentIndicator Exponent
    ;

fragment HexadecimalInteger
    : HexadecimalPrefix HexadecimalDigit+
    ;

fragment DecimalFraction
    : DecimalPoint DecimalDigit+
    ;

fragment Mantissa
    : DecimalDigitExcludingZero DecimalFraction?
    ;

fragment Exponent
    : Sign? DecimalDigit+
    ;

fragment Sign
    : [+-]
    ;

fragment Zero
    : '0'
    ;

fragment DecimalDigit
    : [0-9]
    ;

fragment DecimalDigitExcludingZero
    : [1-9]
    ;

fragment HexadecimalDigit
    : [0-9a-fA-F]
    ;

fragment DecimalPoint
    : '.'
    ;

fragment HexadecimalPrefix
    : Zero HexadecimalIndicator
    ;

fragment HexadecimalIndicator
    : [xX]
    ;

fragment ExponentIndicator
    : [eE]
    ;

fragment StringCharacter
    : ~["\\]
    | EscapeSequence
    ;

fragment EscapeSequence
    : '\\' [btnfr"\\]
    ;

fragment IdentifierLetter
    : [a-zA-Z_]
    ;

fragment IdentifierLetterOrDigit
    : [a-zA-Z_0-9]
    ;

fragment BR
    : LINE_BREAK*
    ;

fragment WHITESPACE
    : [ \t]+
    ;

fragment LINE_BREAK
    : WHITESPACE? ( '\r'? '\n' | '\r' )
    ;

fragment COMMENT
    : '\'' ~[\r\n]*
    ;
