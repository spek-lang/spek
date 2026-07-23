lexer grammar SpekLexer;

// ─── Keywords ────────────────────────────────────────────────────────────────
// Must appear before IDENTIFIER so ANTLR's lexer priority resolves them first.

ABSTRACT    : 'abstract';
ACTOR       : 'actor';
ANY         : 'any';
BASE        : 'base';
BECOME      : 'become';
BEHAVIOR    : 'behavior';
CHANNEL     : 'channel';
CLASS       : 'class';
ELSE        : 'else';
EMITS       : 'emits';
ENUM        : 'enum';
FALSE       : 'false';
FOR         : 'for';
IF          : 'if';
INIT        : 'init';
INTERFACE   : 'interface';
INTERNAL    : 'internal';
MESSAGE     : 'message';
NAMESPACE   : 'namespace';
NEW         : 'new';
NULL        : 'null';
ON          : 'on';
OVERRIDE    : 'override';
PASSIVATE   : 'passivate';
PERSIST     : 'persist';
PRIVATE     : 'private';
PROGRAM     : 'program';
PROTECTED   : 'protected';
PUBLIC      : 'public';
RESTORE     : 'Restore';
PRESTART    : 'PreStart';
POSTSTOP    : 'PostStop';
RETURN      : 'return';
SELF        : 'self';
SENDER      : 'sender';
SPAWN       : 'spawn';
SUPERVISE   : 'supervise';
TRUE        : 'true';
USING       : 'using';
INTEROP     : 'interop';
READER      : 'reader';
WRITER      : 'writer';
TRY         : 'try';
CATCH       : 'catch';
FINALLY     : 'finally';
THROW       : 'throw';
WHEN        : 'when';
WHERE       : 'where';
SWITCH      : 'switch';
CASE        : 'case';
DEFAULT     : 'default';
// Pattern keywords. Reserved everywhere (hard keywords) for the same
// reason WHEN and SWITCH are: simpler ANTLR grammar than contextual
// keywords. If we ever need `not`/`and`/`or` as identifiers we can
// migrate to context-sensitive lexing later. They have distinct token
// names from AND (`&&`) and OR (`||`), which remain expression-level
// operators.
KW_NOT      : 'not';
KW_AND      : 'and';
KW_OR       : 'or';
EVENT       : 'event';
MODULE      : 'module';
// parameter-passing modifiers. Identical to C#'s semantics
// (`in` = readonly reference, `ref` = aliased reference, `out` =
// caller-bound output binding). They're hard keywords because making
// them context-sensitive would complicate every `param`-bearing rule.
IN_KW       : 'in';
REF_KW      : 'ref';
OUT_KW      : 'out';
SHARED      : 'shared';
TERM        : 'term';
TRANSIENT   : 'transient';
DEPRECATED  : 'deprecated';
RETIRED     : 'retired';
USE         : 'use';
VAR         : 'var';
VOID        : 'void';
WHILE       : 'while';
BREAK       : 'break';
CONTINUE    : 'continue';
DO          : 'do';
FOREACH     : 'foreach';
IS          : 'is';
AS          : 'as';
AFTER       : 'after';
FLAGS       : 'flags';
STRATEGY    : 'strategy';
FAILURE     : 'Failure';
RESTART     : 'Restart';
STOP        : 'Stop';
ESCALATE    : 'Escalate';
RESUME      : 'Resume';
ONE_FOR_ONE : 'OneForOne';
ALL_FOR_ONE : 'AllForOne';

// ─── Identifier ──────────────────────────────────────────────────────────────

IDENTIFIER  : [a-zA-Z_][a-zA-Z0-9_]*;

// ─── Literals ────────────────────────────────────────────────────────────────

// Numeric and character literals follow C#'s lead: digit separators (`_`),
// hex (`0x`) and binary (`0b`) integers, integer suffixes (`u`/`l` combos),
// real suffixes (`f`/`d`/`m`), and exponents. They are lexed permissively
// and emitted verbatim — the generated C# is the final arbiter of the exact
// value and type, so Spek never silently re-formats or re-types a literal.
// A bare integer (no `.`, exponent, or real suffix) is an INTEGER_LITERAL;
// anything fractional / exponential / real-suffixed is a DECIMAL_LITERAL.
DECIMAL_LITERAL
    : Digits '.' Digits Exponent? RealSuffix?
    | Digits Exponent RealSuffix?
    | Digits RealSuffix
    ;
INTEGER_LITERAL
    : '0' [xX] HexDigits IntSuffix?
    | '0' [bB] [01] [01_]* IntSuffix?
    | Digits IntSuffix?
    ;
CHAR_LITERAL    : '\'' ( CharEscape | ~['\\\r\n] ) '\'';
// Raw string (C# 11), common case: content may contain quotes and newlines
// but not the triple-quote fence itself (documented limitation — longer
// fences and embedded `"""` aren't supported yet).
RAW_STRING      : '"""' .*? '"""';
// Verbatim string: @"...", where `""` is an escaped quote and newlines are
// allowed (no backslash escape processing).
VERBATIM_STRING : '@"' ( '""' | ~["] )* '"';
// Verbatim-interpolated: $@"..." or @$"...".
VERBATIM_INTERP : ('$@' | '@$') '"' ( InterpHole | '""' | ~["{] )* '"';
STRING_LITERAL  : '"' ( '\\' ~[\r\n] | ~["\\\r\n] )* '"';
INTERP_STRING   : '$"' ( InterpHole | '\\' ~[\r\n] | ~["{\\\r\n] )* '"';

// An interpolation hole. `EmbeddedStr` absorbs string literals inside the
// hole so their quotes (and any `}` they contain) don't end the hole or the
// outer string early. Nested braces close the hole at the first top-level
// `}` here, but that's only the token boundary — the AST builder re-splits
// holes with full brace balancing. (Limitation: `{{`/`}}` literal-brace
// escapes aren't recognised yet.)
fragment InterpHole  : '{' ( EmbeddedStr | ~["}] )* '}';
fragment EmbeddedStr : '"' ( '\\' ~[\r\n] | ~["\\\r\n] )* '"';
fragment Digits      : [0-9] [0-9_]*;
fragment HexDigits   : [0-9a-fA-F] [0-9a-fA-F_]*;
fragment Exponent    : [eE] [+-]? [0-9] [0-9_]*;
fragment RealSuffix  : [fFdDmM];
fragment IntSuffix   : [lL] [uU]? | [uU] [lL]?;
fragment HexDigit    : [0-9a-fA-F];
fragment CharEscape
    : '\\' ( 'u' HexDigit HexDigit HexDigit HexDigit
           | 'U' HexDigit HexDigit HexDigit HexDigit HexDigit HexDigit HexDigit HexDigit
           | 'x' HexDigit HexDigit? HexDigit? HexDigit?
           | . );

// ─── Operators and punctuation ───────────────────────────────────────────────

ARROW       : '=>';
DOT         : '.';
COMMA       : ',';
SEMICOLON   : ';';
COLON       : ':';
LPAREN      : '(';
RPAREN      : ')';
LBRACE      : '{';
RBRACE      : '}';
LBRACKET    : '[';
RBRACKET    : ']';
LT          : '<';
GT          : '>';
ASSIGN      : '=';
PLUS_ASSIGN : '+=';
MINUS_ASSIGN: '-=';
STAR_ASSIGN : '*=';
SLASH_ASSIGN: '/=';
PERCENT_ASSIGN : '%=';
AMP_ASSIGN     : '&=';
PIPE_ASSIGN    : '|=';
CARET_ASSIGN   : '^=';
QQ_ASSIGN      : '??=';
EQ          : '==';
NEQ         : '!=';
LTE         : '<=';
GTE         : '>=';
AND         : '&&';
OR          : '||';
BANG        : '!';
PLUS        : '+';
MINUS       : '-';
STAR        : '*';
SLASH       : '/';
PERCENT     : '%';
QUESTION    : '?';
QQ          : '??';
AMP         : '&';
PIPE        : '|';
CARET       : '^';
TILDE       : '~';
// NOTE: no SHL/SHR tokens — '>>' must stay two GT tokens so nested generics
// (List<List<int>>) keep parsing. Shift operators are matched in the parser
// as adjacent LT LT / GT GT (see shiftExpr).

// ─── Whitespace and comments (skip) ──────────────────────────────────────────

WHITESPACE   : [ \t\r\n]+       -> skip;
// Comments routed to the hidden channel (not skipped) so the
// formatter can re-attach them by token-position when re-emitting
// canonical Spek source. The parser ignores hidden-channel tokens
// the same way `skip` would, so existing parse paths are unaffected.
// Trailing newline is optional so a comment at end-of-file (no
// trailing \n) still tokenizes as one LINE_COMMENT instead of
// degenerating to two SLASH tokens.
LINE_COMMENT : '//' ~[\n]*       -> channel(HIDDEN);
BLOCK_COMMENT: '/*' .*? '*/'     -> channel(HIDDEN);
