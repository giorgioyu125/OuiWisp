#ifndef LEXER_H
#define LEXER_H

#include <stddef.h>

typedef struct safe_str {
    size_t size;
    char   string[];
} safe_str;

typedef enum {
    LEFT_PAREN,
    RIGHT_PAREN,
    DIV_OP,
    MUL_OP,
    MINUS_OP,
    PLUS_OP,
	QUOTE_OP,
    WORD,
    UNKNOWN
} kinds;

typedef struct token {
    kinds     kind;
    safe_str* lexeme;
    size_t    span;    /* position or offset position form orgin */
    size_t    depth;   /* nesting depth */
    size_t    sexprid; /* current s-expr unique id */
} token;

token* lexer(const char* program_text, size_t program_length, size_t* out_count);
void free_tokens(token* toks, size_t count);

#endif /* OUIWISP_LEXER_H */
