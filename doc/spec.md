# Paragon - A Macro Assembler For NES

## Table Of Contents

- [General Elements](#general-elements)
    - [Alphabet And Digit](#alphabet-and-digit)
- [Lexical Items](#lexical-items)
    - [Comment](#comment)
    - [Identifier](#identifier)
    - [Operators And Symbols](#operators-and-symbols)
    - [Keywords](#keywords)
    - [Integer Literal](#integer-literal)
- [Types And Type Casting](#types-and-type-casting)
- [Items In Source Code](#items-in-source-code)
    - [Global And Local Symbol](#global-and-local-symbol)
- [Expression](#expression)
- [Buildin Functions](#buildin-functions)
    - [typeof](#typeof)
- [Statements](#statements)
    - [Instruction Statement](#instruction-statement)
- [Pseudo Instructions](#pseudo-instructions)
    - [.inesmap](#inesmap)
    - [.inessmap](#inessmap)
    - [.inesprg](#inesprg)
    - [.ineschr](#ineschr)
    - [.bank](#bank)
    - [.equ](#equ)
    - [.org](#org)
    - [.ds](#ds)
    - [.db](#db)
    - [.dw](#dw)
    - [.include](#include)
    - [.incbin](#incbin)
    - [.macro](#macro)
    - [.endmacro](#endmacro)
    - [.func](#func)
    - [.endfunc](#endfunc)
    - [.if](#if)
    - [.ifdef](#ifdef)
    - [.ifndef](#ifndef)
    - [.elseif](#elseif)
    - [.elseifdef](#elseifdef)
    - [.elseifndef](#elseifndef)
    - [.else](#else)
    - [.endif](#endif)
    - [.error](#error)

## General Elements

### Alphabet And Digit

```
<alphabet>          ::= "a" .. "z" | "A" .. "Z"
<binary-digit>      ::= "0" | "1"
<octadecimal-digit> ::= "0" .. "7"
<decimal-digit>     ::= "0" .. "9"
<hexadecimal-digit> ::= "0" .. "9" | "a" .. "f" | "A" .. "F"
```

## Lexical Items

### Comment

There is two type of comment: range comment and line comment.

- Line comment start with `//` and valid in the line.
- Range comment start with `/*` and end with `*/`. This is valid through multiple lines.

```
// Line comment
This line is not a comment

/*
 * This is range comment
 */
```

### Identifier

An identifier contain ascii character, underline and number. Start with number is not
allowed on this specification.

```
<identifier>      ::= <identifier-head> { <identifier-rest> }
<identifier-head> ::= <alphabet> | "_"
<identifier-rest> ::= <alphabet> | "_" | <decimal-digit>
```

```
a
ident12
_ident12

12ident // start with numeric is not valid
```

### Operators And Symbols

Below is operators and symbols which the lexer recognize.

```
+ - * / % & | ^ << >> && || ! < > <= >= == != ~ =
```

```
( ) [ ] ,
```

### Keywords

These keyword is reserved and cannot be used as identifier.

```
adc	and	asl	bcc	bcs	beq	bit	bmi	bne	bpl	brk	bvc	bvs	clc
cld	cli	clv	cmp	cpx	cpy	dec	dex	dey	eor	inc	inx	iny	jmp
jsr	lda	ldx	ldy	lsr	nop	ora	pha	php	pla	plp	rol	ror	rti
rts	sbc	sec	sed	sei	sta	stx	sty	tax	tay	tsx	txa	txs	tya

equ ds db dw include incbin macro endmacro if ifdef ifndef
elseif elseifdef elseifndef else endif error
```

### Integer Literal

An integer is a list of alphabet or numeric. 
You can separate digit with `_`, but end with it is not allowed.

```
<integer-lit>        ::= <binary-lit> | <decimal-lit> | <octadecimal-lit> | <hexadecimal-lit>
<binary-lit>         ::= <binary-prefix> <binary-digits>
<decimal-lit>        ::= "0" | <decimal-digits>
<octadecimal-lit>    ::= <octadecimal-prefix> <octadecimal-digits>
<hexadecimal-lit>    ::= <hexadecimal-prefix> <hexadecimal-digits>
<binary-prefix>      ::= "%" | "0" ( "b" | "B" )
<octadecimal-prefix> ::= "0" ( "o" | "O" )
<hexadecimal-prefix> ::= "$" | "0" ( "x" | "X" )
<binary-digits>       ::= <binary-digit> { [ "_" ] <binary-digit> }
<decimal-digits>      ::= <decimal-digit> { [ "_" ] <decimal-digit> }
<octadecimal-digits>  ::= <octadecimal-digit> { [ "_" ] <octadecimal-digit> }
<hexadecimal-digits>  ::= <hexadecimal-digit> { [ "_" ] <hexadecimal-digit> }
```

```
0b0010_1101
30
0o76_13
0x0a23

42_ // end with _ is invalid
_42 // this is identifier, not integer literal
```

## Types And Type Casting

There is three types: `byte`, `word` and `string`

- byte: 8-bit integer
- word: 16-bit integer
- string: sequence of alphabet, digit and `_`

When a operator is applied to two expression with different types, or cast operator is applied to word, a type casting occure.

| ty1    | ty2       | operator               | result type | note                                              |
| ------ | --------- | ---------------------- | ----------- | ------------------------------------------------- |
| byte   | word      | \| ^ & << >> + - * / % | word        | byte will be extended to word with sign extension |
| string | word/byte | +                      | string      | ex. "hoge" + 10 => "hoge10"                       |
| word   | None      | < >                    | byte        | < take lsb and > take msb of the word value       |

## Items In Source Code

### Global And Local Symbol

There are two type of symbol: global symbol and local symbol.

- global symbol: just a identifier.
- local symbol: a identifier which start with `.`.

```
<symbol> ::= <global-symbol> | <local-symbol>
<global-symbol> ::= <identifier>
<local-symbol> ::= "." <identifier>
```

## Expression

```
<expression> ::= <cast-expression>
<cast-expression> ::= <logical-or-expression>
                    | ">" <cast-expression>
                    | "<" <cast-expression>
<logical-or-expression> ::= <logical-and-expression>
                          | <logical-and-expression> "||" <logical-or-expression>
<logical-and-expression> ::= <bitwise-or-expression>
                           | <bitwise-or-expression> "&&" <logical-and-expression>
<bitwise-or-expression> ::= <bitwise-xor-expression>
                          | <bitwise-xor-expression> "|" <bitwise-or-expression>
<bitwise-xor-expression> ::= <bitwise-and-expression>
                           | <bitwise-and-expression> "^" <bitwise-xor-expression>
<bitwise-and-expression> ::= <equality-expression>
                           | <equality-expression> "&" <bitwise-and-expression>
<equality-expression> ::= <relational-expression>
                        | <relational-expression> "==" <equality-expression>
                        | <relational-expression> "!=" <equality-expression>
<relational-expression> ::= <shift-expression>
                          | <shift-expression> "<" <relational-expression>
                          | <shift-expression> ">" <relational-expression>
                          | <shift-expression> "<=" <relational-expression>
                          | <shift-expression> ">=" <relational-expression>
<shift-expression> ::= <additive-expression>
                     | <additive-expression> "<<" <shift-expression>
                     | <additive-expression> ">>" <shift-expression>
<additive-expression> ::= <multiplicative-expression>
                        | <multiplicative-expression> "+" <additive-expression>
                        | <multiplicative-expression> "-" <additive-expression>
<multiplicative-expression> ::= <unary-expression>
                              | <unary-expression> "*" <multiplicative-expression>
                              | <unary-expression> "/" <multiplicative-expression>
                              | <unary-expression> "%" <multiplicative-expression>
<unary-expression> ::= <primary-expression>
                     | "~" <unary-expression>
                     | "-" <unary-expression>
                     | "!" <unary-expression>
<primary-expression> ::= <integer-lit>
                       | <symbol>
                       | <function-call>
                       | "(" <expression> )"
<function-call> ::= <identifier> "(" [ <function-call-params> ] ")"
<function-call-params> ::= <function-call-param> [ "," <function-call-params> ]
<function-call-param> ::= <expression>
```

## Buildin Functions

### typeof

The `typeof` function take a expression and return the type of expression by string.
Currently, possible return value is one of `"byte"`, `"word"` and `"string"`.

```
typeof(10) // => "word"
typeof(<10) // => "byte"
typeof("hello") // => "string"
```

## Statements

```
<statement> ::= <instruction-statement>
```

### Instruction Statement

```
<instruction-statement> ::= [ <symbol> ":" ]     <actual-inst-name> [ <actual-inst-params> ] <newline>
                          | [ <symbol> ":" ] "." <pseudo-inst-name> [ <pseudo-inst-params> ] <newline>

<actual-inst-name> ::= <identifier> | /* valid 6502 opcode names */
<pseudo-inst-name> ::= /* valid pseudo instruction names */

<actual-inst-params> ::= <actual-inst-param> [ <actual-inst-params> ]
<actual-inst-param> ::= <accumulator>
                      | <absolute-or-zeropage>
                      | <relative>
                      | <indirect>
<accumulator> ::= "a"
<absolute-or-zeropage> ::= <expression>
                         | <expression> "," "x"
                         | <expression> "," "y"
<relative> ::= <identifier>
<indirect> ::= "[" <expression> [ "," "x" ] "]" [ "," "y" ]

<pseudo-inst-params> ::= <pseudo-inst-param> [ <pseudo-inst-params> ]
<pseudo-inst-param> ::= <expression>
```

```
    .macro hoge p1 p2 p3
        lda p1
        lda p2
        lda p3
    .endmacro

    lda #$00
    hoge 10, x [20, x] #30 // passing three argument
```

## Pseudo Instructions

### .inesmap

SYNTAX: `.inesmap expr`

The `.inesmap` instruction specify the mapper number to be used in nes header.

```
    .inesmap 10
```

### .inessmap

SYNTAX: `.inessmap expr`

The `.inessmap` instruction specify the submapper number to be used in nes header.

```
    .inessmap 10
```

### .inesprg

SYNTAX: `.inesprg expr [, expr]`

The `.inesprg` instruction specify the number of program bank.
You can also specify the size of each bank in KB unit. The default bank size is 16KB.

```
    .inesprg 10    // the number of program bank is 10 and each size is 16KB.
    .inesprg 10, 8 // the number of program bank is 10 and each size is 8KB.
    .inesprg 10, 3 // each bank size is 3KB, but may not be useful.
```

### .ineschr

SYNTAX: `.ineschr expr [, expr]`

The `.ineschr` instruction specify the number of character bank.
You can also specify the size of each bank in KB unit. The default bank size is 4KB.

```
    .ineschr 10    // the number of character bank is 10 and each size is 4KB
    .ineschr 10, 8 // the number of character bank is 10 and each size is 8KB
```

### .bank

SYNTAX: `.bank expr`

The `.bank` instruction switch current bank to specified bank number.
This instruction will guess the start address of the bank by using `.org` instruction
called after this instruction.

```
    .inesprg 2
    .ineschr 2

    .bank 0
    .org $9000 // The size of bank 0 is 16KB and the range is $8000 ~ $BFFF

    .bank 1
    .org $FFFA // The size of bank 1 is 16KB and the range is $C000 ~ $FFFF

    .bank 2
    .org $0000 // The size of bank 2 is 4KB and the range is $0000 ~ $0FFF

    .bank 3
    .org $1000 // The size of bank 3 is 4KB and the range is $1000 ~ $1FFF
```

### .equ

SYNTAX: `symbol .equ expr` or `.symbol .equ expr`

The `.equ` (or `=`) instruction assign given expression to the symbol.

```
hoge: .equ 10 + 20
.fuga: .equ 2

hoge: = 2 // equal to hoge .equ 2
```

### .org

SYNTAX: `.org expr`

The `.org` instruction change current program counter to given integer expression.
If specified value exceed the address range of current bank, cause error.

```
    .inesprg 1

    .bank 0
    .org $8000 // change to $8000
    lda #$00
    .org $9000 // change to $9000
    lda #$10
    .org $C000 // ERROR: the range of this bank is $8000 ~ $BFFF, but $C000 exceed the range.
```

### .ds

SYNTAX: `.ds expr`

The `.ds` instruction reserve spaces as amount of specified bytes in the place.

```
    .ds 10 // reserve 10 byte
```

### .db

SYNTAX: `.db expr1 expr2 ... exprn`

The `.db` instruction place given bytes in the place. If `expri` exceed the range of byte,
cause error.

```
    .db 1 2 3
```

### .dw

SYNTAX: `.dw expr1 expr2 ... exprn`

The `.dw` instruction place given words in the place as little endian.

```
    .dw 1 2 3 // => .db 1 0 2 0 3 0
```

### .include

SYNTAX: `.include "filename"`

The `.include` instruction read a file and paste the contents in the place.

```
    .include "hoge.asm"
```

### .incbin

SYNTAX: `.incbin "filename"`

The `.incbin` instruction read a file as byte sequence and write bytes in the place.

```
    .incbin "hoge.bin"
```

### .macro

SYNTAX: `.macro name sym1 sym2 ... symn`

The `.macro` instruction define a macro. TODO

```
    .macro hoge p1 p2 p3
        lda #p1
        sta <p2
        sta <p3
    .endmacro
```

### .endmacro

SYNTAX: `.endmacro`

The `.endmacro` indicate the end of macro definition. This instruction must be placed under
`.macro` instruciton.

```
    .macro hoge
    .endmacro // correspond to .macro hoge
```

### .func

SYNTAX: `.func name sym1 sym2 ... symn`

The `.func` instruction define a function which accept several expression and return a value by using
these given expression. Currently, this `.func` accept a expression using given expressions.

```
    .func add p1 p2
        p1 + p2
    .endfunc
```

### .endfunc

SYNTAX: `.endfunc`

The `.endfunc` indicate the end of function definition. This instruction must be placed under
`.func` instruction

```
    .func id p
        p
    .endfunc // correspond to .func id p
```

### .if

SYNTAX: `.if expr`

The `.if` instruction take a expression. If given expression is zero, skip statements under
this instruction until `.else*` or `.endif`.

```
    .if 1
        lda #0x01
    .else
        .error "unreachable"
    .endif
```

### .ifdef

SYNTAX: `.ifdef symbol` or `.ifdef .symbol`

The `.ifdef` instruction take a symbol. If given symbol is not defined skip statemsnts under
this instruction until `.else*` or `.endif`.

```
hoge = 10
    .ifdef hoge
        // do something
    .else
        .error "unreachable"
    .endif
```

### .ifndef

SYNTAX: `.ifndef symbol` or `.ifndef .symbol`

The `.ifndef` instruction take a symbol. If given symbol is defined skip statemsnts under
this instruction until `.else*` or `.endif`.

```
hoge = 10
    .ifndef hoge
        .error "unreachable"
    .else
        // do something
    .endif
```

### .elseif

SYNTAX `.elseif expr`

The `.elseif` instruction take a expression.
If given expression is zero or above `.if*` or `.else*`'s condition doesn't fulfilled,
skip statemsnts under this instruction until `.else*` or `.endif`.
This instruction must be placed under `.if*` or `.else*`.

```
    .if 0
        .error "unreachable"
    .elseif 1
        // do something
    .endif
```

### .elseifdef

SYNTAX `.elseifdef symbol` or `.elseifdef .symbol`

The `.elseifdef` instruction take a symbol.
If given symbol is not defined or above `.if*` or `.else*`'s condition doesn't fulfilled,
skip statemsnts under this instruction until `.else*` or `.endif`.
This instruction must be placed under `.if*` or `.else*`.

```
hoge = 10
    .if 0
        .error "unreachable"
    .elseifdef hoge
        // do something
    .endif
```

### .elseifndef

SYNTAX `.elseifndef symbol` or `.elseifndef .symbol`

The `.elseifndef` instruction take a symbol.
If given symbol is defined or above `.if*` or `.else*`'s condition doesn't fulfilled,
skip statemsnts under this instruction until `.else*` or `.endif`.
This instruction must be placed under `.if*` or `.else*`.

```
    .if 0
        .error "unreachable"
    .elseifdef hoge
        // do something
    .endif
```

### .else

SYNTAX `.else`

The `.else` instruction skip statements under this instruction if above `.if*` or `.else*`'s
condition doesn't fulfilled.
This instruction must be placed under `.if*` or `.else*`.

```
    .if 0
        .error "unreachable"
    .else
        // do something
    .endif
```

### .endif

SYNTAX: `.endif`

The `.endif` instruction indicate the end of `.if*` and must be placed under `.if*` or `.else*`.

```
    .if 0
    .endif // correspond to .if 0
```

### .error

SYNTAX: `.error [ "error message" ]`

The `.error` instruction may take a string and cause error using given message if message given.

```
    .error "error here"
    .error // cause error with default error message
```

