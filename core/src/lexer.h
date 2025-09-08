#ifndef OUIWISP_LEXER_H
#define OUIWISP_LEXER_H

#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

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
    WORD,
    UNKNOWN
} kinds;

typedef struct token {
    kinds     kind;
    safe_str* lexeme;
    size_t    span;    /* posizione/offset nel sorgente */
    size_t    depth;   /* profondità di annidamento */
    size_t    sexprid; /* id della s-expr corrente */
} token;

token* lexer(const char* program_text, size_t program_length, size_t* out_count);
void free_tokens(token* toks, size_t count);

#ifdef __cplusplus
}
#endif

#endif /* OUIWISP_LEXER_H */
