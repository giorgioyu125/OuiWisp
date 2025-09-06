#include <stdint.h>
#include <stddef.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

// ========= Types =========

typedef struct safe_str {
    size_t size;
    char string[];
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
    size_t    span;
    size_t    depth;
    size_t    sexprid;
} token;


// ========= Helpers =========

static safe_str* make_safe_str(const char* start, size_t len) {
    safe_str* s = (safe_str*)malloc(sizeof(safe_str) + len + 1);
    if (!s) return NULL;
    s->size = len;
    if (len) memcpy(s->string, start, len);
    s->string[len] = '\0';
    return s;
}

void free_tokens(token* toks, size_t count) {
    if (!toks) return;
    for (size_t i = 0; i < count; ++i) {
        free(toks[i].lexeme);
    }
    free(toks);
}

static kinds classify_token(const safe_str* s) {
    if (!s || s->size == 0) return UNKNOWN;

    if (s->size == 1) {
        switch (s->string[0]) {
            case '(' : return LEFT_PAREN;
            case ')' : return RIGHT_PAREN;
            case '/' : return DIV_OP;
            case '*' : return MUL_OP;
            case '-' : return MINUS_OP;
            case '+' : return PLUS_OP;
            default  : break;
        }
    }

    return UNKNOWN;
}


// ========= Lexer =========
//
// Returns a heap-allocated array of tokens and writes the number of tokens
// to out_count. Caller must free with free_tokens(tokens, count).
//
token* lexer(const char* program_text, size_t program_length, size_t* out_count) {
    if (!program_text) {
        if (out_count) *out_count = 0;
        return NULL;
    }

    token* tokens = (token*)malloc(sizeof(token) * (program_length ? program_length : 1));
    if (!tokens) {
        if (out_count) *out_count = 0;
        return NULL;
    }

    size_t count = 0;
    size_t depth = 0;

    size_t stack_cap = 16;
    size_t stack_len = 0;
    size_t* sexpr_stack = (size_t*)malloc(sizeof(size_t) * stack_cap);
    if (!sexpr_stack) {
        free(tokens);
        if (out_count) *out_count = 0;
        return NULL;
    }
    size_t next_sexpr_id = 1;

    #define TOP_SEXPR() (stack_len ? sexpr_stack[stack_len - 1] : 0)
    #define PUSH_SEXPR(id) do { \
        if (stack_len == stack_cap) { \
            stack_cap = stack_cap ? stack_cap * 2 : 16; \
            size_t* tmp = (size_t*)realloc(sexpr_stack, sizeof(size_t) * stack_cap); \
            if (!tmp) { goto oom; } \
            sexpr_stack = tmp; \
        } \
        sexpr_stack[stack_len++] = (id); \
    } while (0)
    #define POP_SEXPR() do { if (stack_len) stack_len--; } while (0)

    size_t i = 0;
    while (i < program_length) {
        unsigned char c = (unsigned char)program_text[i];

        if (isspace(c)) { i++; continue; }

        size_t start = i;
        safe_str* lex = NULL;
        kinds kind = UNKNOWN;
        size_t sexprid = TOP_SEXPR();

        if (c == '(') {
            lex = make_safe_str(program_text + i, 1);
            if (!lex) goto oom;

            kind = LEFT_PAREN;

            tokens[count++] = (token){ kind, lex, start, depth, sexprid };

            PUSH_SEXPR(next_sexpr_id++);
            depth++;
            i++;
            continue;
        }

        if (c == ')') {
            if (depth > 0) depth--;
            size_t closing_id = TOP_SEXPR();
            if (stack_len) POP_SEXPR();

            lex = make_safe_str(program_text + i, 1);
            if (!lex) goto oom;

            kind = RIGHT_PAREN;
            tokens[count++] = (token){ kind, lex, start, depth, closing_id };
            i++;
            continue;
        }

        if (c == '+' || c == '-' || c == '*' || c == '/') {
            lex = make_safe_str(program_text + i, 1);
            if (!lex) goto oom;

            kind = classify_token(lex);
            tokens[count++] = (token){ kind, lex, start, depth, sexprid };
            i++;
            continue;
        }

        // WORD: alphabetic run
        if (isalpha(c)) {
            size_t j = i + 1;
            while (j < program_length && isalpha((unsigned char)program_text[j])) {
                j++;
            }
            lex = make_safe_str(program_text + i, j - i);
            if (!lex) goto oom;

            kind = WORD;
            tokens[count++] = (token){ kind, lex, start, depth, sexprid };
            i = j;
            continue;
        }

        lex = make_safe_str(program_text + i, 1);
        if (!lex) goto oom;

        kind = UNKNOWN;
        tokens[count++] = (token){ kind, lex, start, depth, sexprid };
        i++;
    }

    free(sexpr_stack);
    if (out_count) *out_count = count;
    return tokens;

oom:
    for (size_t k = 0; k < count; ++k) free(tokens[k].lexeme);
    free(tokens);
    free(sexpr_stack);
    if (out_count) *out_count = 0;
    return NULL;

    #undef TOP_SEXPR
    #undef PUSH_SEXPR
    #undef POP_SEXPR
}

static const char* kind_name(kinds k) {
    switch (k) {
        case LEFT_PAREN:  return "LEFT_PAREN";
        case RIGHT_PAREN: return "RIGHT_PAREN";
        case DIV_OP:      return "DIV_OP";
        case MUL_OP:      return "MUL_OP";
        case MINUS_OP:    return "MINUS_OP";
        case PLUS_OP:     return "PLUS_OP";
        case WORD:        return "WORD";
        default:          return "UNKNOWN";
    }
}


// lexer tests
// int main(void) {
//    const char* path = "./test.lisp";
//    FILE* f = fopen(path, "rb");
//    if (!f) {
//        perror("fopen");
//        return 1;
//    }
//
//    if (fseek(f, 0, SEEK_END) != 0) {
//        perror("fseek");
//        fclose(f);
//        return 1;
//    }
//
//    long len = ftell(f);
//    if (len < 0) {
//        perror("ftell");
//        fclose(f);
//        return 1;
//    }
//    if (fseek(f, 0, SEEK_SET) != 0) {
//        perror("fseek");
//        fclose(f);
//        return 1;
//    }
//
//    size_t size = (size_t)len;
//    char* code = (char*)malloc(size + 1);
//    if (!code) {
//        perror("malloc");
//        fclose(f);
//        return 1;
//    }
//
//    size_t nread = fread(code, 1, size, f);
//    if (nread != size) {
//        if (ferror(f)) {
//            perror("fread");
//            free(code);
//            fclose(f);
//            return 1;
//        }
//    }
//    fclose(f);
//
//    code[nread] = '\0';
//
//    size_t count = 0;
//    token* toks = lexer(code, nread, &count);
//
//    for (size_t i = 0; i < count; ++i) {
//        printf("%zu: %-12s  lex=\"%s\"  span=%zu  depth=%zu  sexprid=%zu\n",
//               i, kind_name(toks[i].kind), toks[i].lexeme->string,
//               toks[i].span, toks[i].depth, toks[i].sexprid);
//    }
//
//    free_tokens(toks, count);
//    free(code);
//    return 0;
//}
