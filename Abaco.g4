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
    : EOF
    ;

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

SKIP
    : ( WHITESPACE | COMMENT ) -> skip
    ;

fragment WHITESPACE
    : [ \t]+
    ;

fragment COMMENT
    : ';' ~[\r\n]*
    ;
