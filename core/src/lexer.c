#include <stdint.h>
#include <stddef.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

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
	QUOTE_OP,
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

/**
 * @brief Classify a one char str into a know member of the kinds enum.
 * 
 * @details this function analyze the given string and transforms it into the
 *			corrisponding operator or separator.
 *
 * @param[in] s Un puntatore a una struttura `safe_str` che contiene la stringa
 *              da classificare. La funzione è sicura per puntatori NULL e
 *              stringhe vuote.
 * 
 * @return one of the enum `kinds` rapresent the token value.
 * @retval LEFT_PAREN  "(".
 * @retval RIGHT_PAREN  ")".
 * @retval DIV_OP  "/".
 * @retval MUL_OP  "*".
 * @retval MINUS_OP "-".
 * @retval PLUS_OP "+".
 * @retval UNKNOWN  NULL.
 *
 * @note This function manages only single char tokens because all the function
 *		 recognition and type system will be build in the semantic part of the 
 *       program to allow a more dynamic nature, for creating more optimized
 *       config files.
 *       
 * 
 * @see tokenize()
 * @see kinds
 */
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


/**
 * @brief Performs lexical analysis on a source code string, breaking it into a sequence of tokens.
 *
 * @details This function iterates through the input character stream (`program_text`)
 *          and transforms it into an array of `token` structures. It recognizes basic
 *          Lisp-like elements such as parentheses, simple operators, and words (symbols).
 *          Whitespace is ignored. Each generated token is tagged with metadata, including
 *          its position, nesting depth, and the ID of its parent S-expression.
 *
 * @param[in]  program_text        The raw source code to be tokenized.
 * @param[in]  program_length      The length of the `program_text` string.
 * @param[out] **out_count**       A pointer to a size_t variable that will be populated with the
 *                                 number of tokens generated. On success, this is set to the token count.
 *                                 On failure, it is set to 0.
 *
 * @return On success, returns a pointer to a dynamically allocated array of `token` structures.
 *         On failure (e.g., NULL input or out of memory), returns `NULL`.
 *
 * @warning The caller is responsible for freeing the memory allocated for the returned token array.
 *          Furthermore, the `lexeme` field of each `token` in the array (a `safe_str*`) must also
 *          be individually freed to prevent memory leaks.
 *
 * @note This function handles out-of-memory conditions gracefully by cleaning up any partially
 *       allocated resources before returning `NULL`. The internal S-expression stack management
 *       (PUSH_SEXPR, POP_SEXPR) is an implementation detail for associating tokens with their
 *       containing list structure.
 *
 * @see token
 * @see classify_token()
 * @see make_safe_str()
 */
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

		if (c == '\'') {
			lex = make_safe_str(program_text + i, 1);
    		if (!lex) goto oom;

    		kind = QUOTE_OP;
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

// const char* kind_to_string(kinds k) {
//     switch (k) {
//         case LEFT_PAREN:  return "LEFT_PAREN";
//         case RIGHT_PAREN: return "RIGHT_PAREN";
//         case DIV_OP:      return "DIV_OP";
//         case MUL_OP:      return "MUL_OP";
//         case MINUS_OP:    return "MINUS_OP";
//         case PLUS_OP:     return "PLUS_OP";
//         case WORD:        return "WORD";
//         case UNKNOWN:     return "UNKNOWN";
//         default:          return "INVALID_KIND";
//     }
// }
// 
// 
// // --- TEST PRINCIPALE ---
// int main (void){
//     // 1. Definisci il programma di test
// 	const char* program = "(+ foo '(* bar baz))";
//     printf("Tokenizing: \"%s\"\n\n", program);
// 
//     // 2. Esegui il lexer
//     size_t token_count = 0;
//     token* tokens = lexer(program, strlen(program), &token_count);
// 
//     // 3. Controlla se il lexer ha avuto successo
//     if (!tokens) {
//         fprintf(stderr, "Lexer failed to process the input.\n");
//         return 1;
//     }
// 
//     // 4. Stampa i token in una tabella
//     printf("Found %zu tokens:\n", token_count);
//     printf("----------------------------------------------------------\n");
//     printf("%-15s | %-10s | span | depth | sexprid\n", "Kind", "Lexeme");
//     printf("----------------------------------------------------------\n");
// 
//     for (size_t i = 0; i < token_count; ++i) {
//         token t = tokens[i];
//         printf("%-15s | %-10s | %-4zu | %-5zu | %-7zu\n",
//                kind_to_string(t.kind),
//                t.lexeme->string,
//                t.span,
//                t.depth,
//                t.sexprid);
//     }
//     printf("----------------------------------------------------------\n\n");
// 
//     // 5. Libera la memoria
//     printf("Cleaning up memory...\n");
//     free_tokens(tokens, token_count);
//     printf("Done.\n");
// 
// 	return 0;
// }
