/**
 * @file parser.c
 */
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <stdbool.h>
#include <ctype.h>
#include <stdarg.h>
#include "lexer.h"
#include "parser.h"


/* -------------------- Hashing and Symbol Allocation -------------------- */

void symtab_init(SymTab* st) {
	st->data = NULL; 
	st->len = 0;
	st->cap = 0;
}

void symtab_grow(SymTab* st, size_t mincap) {
	if (st->cap >= mincap) return;
	size_t cap = st->cap ? st->cap * 2 : 8;
	while (cap < mincap) cap *= 2;
	void* p = realloc(st->data, cap * sizeof(Symbol*));
	if (!p) abort();
	st->data = (Symbol**)p; st->cap = cap;
}

void sym_reset_meta(Symbol* sym) {
	sym->type = T_SYMBOL;
	sym->eval = EK_DATA;
	sym->flags = 0;
	sym->arity = 0;
	sym->value_ptr = NULL;
	sym->entry_ptr = NULL;
	sym->env_ptr = NULL;
	sym->stack_slot = -1;
	sym->gcinfo = gcinfo_pack(0,0,0);
	sym->size_bytes = 0;
}

void free_symtab(SymTab* symtab) {
    if (!symtab || !symtab->data) {
        return;
    }

    for (size_t i = 0; i < symtab->len; ++i) {
        Symbol* sym = symtab->data[i];
        if (sym) {
            free((void*)sym->name);
            free(sym);
        }
    }

    free(symtab->data);

    symtab->data = NULL;
    symtab->len = 0;
    symtab->cap = 0;
}

Symbol* symbol_new(const char* s, size_t n, uint32_t h) {
	Symbol* sym = (Symbol*)malloc(sizeof(Symbol));
	if (!sym) abort();
	char* name = (char*)malloc(n + 1);
	if (!name) abort();

	memcpy(name, s, n); 
	name[n] = '\0';

	sym->name = name; 
	sym->len = n; 
	sym->hash = h;

	sym_reset_meta(sym);
	return sym;
}

/* lookup/intern  (dynamic vector) */

Symbol* symtab_intern(SymTab* st, const char* s, size_t n) {
	uint32_t h = hash(s, n);

	for (size_t i = 0; i < st->len; ++i) {
		Symbol* sym = st->data[i];
		if (sym->hash == h 
			&& sym->len == n 
			&& memcmp(sym->name, s, n) == 0)
		return sym;
	}

	Symbol* sym = symbol_new(s, n, h);

	if (st->len == st->cap) symtab_grow(st, st->len + 1);
	st->data[st->len++] = sym;

	return sym;
}



static Node s_nil = { .tag = N_NIL };
Node* g_nil = &s_nil;

static token peek(Parser* p){
	return *(p->current);
}

static token next(Parser* p) {
	token current_token = *p->current;
	p->current++;
	return current_token;
}

static inline bool at_end(Parser* p) { return p->current >= p->end; }

static void parser_set_error(Parser* p, const char* msg) {
	if (!p || !p->out) return;
	p->out->has_error = true;
	p->out->error_message = msg;
	p->out->error_line = (p->current < p->end) ? (int)p->current->span : -1;
}

static Node* parse_form(Parser* p);
static Node* parse_list(Parser* p);
static Node* parse_atom(Parser* p);
static Node* parse_quote_like(Parser* p, const char* sym, size_t len);

// static Node* parse_expr(Parser* p) {
// 	if (!p || !p->st) abort();
// 	return parse_form(p);
// }

// Core Dispatcher (Don't call from parse_list -> parse_expr, use this)
static Node* parse_form(Parser* p) {
	if (at_end(p)) return g_nil;

	token t = peek(p);
	switch (t.kind) {
		case LEFT_PAREN:
			return parse_list(p);

		case RIGHT_PAREN:
			parser_set_error(p, "Unexpected ')'");
			next(p); // consuma per evitare loop
			return g_nil;

		case QUOTE_OP:
			// 'x -> (quote x)
			return parse_quote_like(p, "quote", 5);

		case WORD:
		case PLUS_OP:
		case MINUS_OP:
		case MUL_OP:
		case DIV_OP:
		case UNKNOWN:
			return parse_atom(p);

		default:
			parser_set_error(p, "Unknown token");
			next(p);
			return g_nil;
	}
}

static Node* parse_list(Parser* p) {
	if (at_end(p)) {
		parser_set_error(p, "Unexpected end of input before '('");
		return g_nil;
	}

	token open = next(p); // consuma '('
	Node* head = g_nil;
	Node** cur = &head;

	while (p->current < p->end) {
		token t = peek(p);
		if (t.kind == RIGHT_PAREN) {
			next(p); // consuma ')'
			return head;
		}

		Node* element = parse_form(p);
		Node* cell = cons(element, g_nil);
		*cur = cell;
		cur = &cell->v.cons.cdr;
	}

	parser_set_error(p, "Unclosed list '('");
	if (p->out) p->out->error_line = (int)open.span;
	return g_nil;
}

static bool is_pascal_ident(const char* s, size_t n) {
    if (!s || n == 0) {
        return false;
    }
    if (!isupper((unsigned char)s[0])) {
        return false;
    }
    for (size_t i = 1; i < n; ++i) {
        if (!isalnum((unsigned char)s[i])) {
            return false;
        }
    }
    return true;
}

static Node* parse_atom(Parser* p) {
	if (!p || !p->st) abort();

	if (p->current >= p->end) {
		if (p->out) {
			p->out->has_error = true;
			p->out->error_message = "Unexpected end of input while reading atom";
			p->out->error_line = -1;
		}
		return g_nil;
	}

	token t = next(p);

	switch (t.kind) {
		case PLUS_OP:
		case MINUS_OP:
		case MUL_OP:
		case DIV_OP:
		case UNKNOWN: {
			const char* s = t.lexeme ? t.lexeme->string : "";
			size_t len = t.lexeme ? t.lexeme->size : 0;
			return make_symbol(p->st, s, len);
		}


		case WORD: {
			const char* s = t.lexeme ? t.lexeme->string : "";
			size_t len = t.lexeme ? t.lexeme->size : 0;

			if (!is_pascal_ident(s, len)) {
				parser_set_error(p, "Identifier must be PascalCase (e.g., FooBar)");
				return g_nil;
			}
			return make_symbol(p->st, s, len);
		}

		case LEFT_PAREN:
		case RIGHT_PAREN:
		case QUOTE_OP:
		default: {
			if (p->out) {
				p->out->has_error = true;
				p->out->error_message = "Token is not an atom";
				p->out->error_line = (int)t.span;
			}
			return g_nil;
		}
	}
}

static Node* parse_quote_like(Parser* p, const char* sym, size_t len) {
	if (at_end(p)) {
		parser_set_error(p, "Unexpected end of input after quote");
		return g_nil;
	}

	// Consume the quote (')
	next(p);

	if (at_end(p)) {
		parser_set_error(p, "Quote without following expression");
		return g_nil;
	}

	Node* datum = parse_form(p);
	Node* sym_node = make_symbol(p->st, sym, len);
	return cons(sym_node, cons(datum, g_nil));
}

Node* parse(Parser* p) {
	if (!p || !p->st) abort();

	Node* head = g_nil;
	Node** tail_ref = &head;

	while (!at_end(p)) {
		if (peek(p).kind == RIGHT_PAREN) {
			parser_set_error(p, "Unmatched ')'");
			next(p);
			continue;
		}

		Node* e = parse_form(p);
		Node* cell = cons(e, g_nil);
		*tail_ref = cell;
		tail_ref = &cell->v.cons.cdr;

		if (p->out && p->out->has_error) break;
	}

	if (p->out) p->out->head = head;
	return head;
}


//// TESTING (dummy main function)
//
//#include <stdio.h>
//
//void print_node(Node* node);
//void free_ast(Node* node);
//
///**
// * @brief Recursively prints the contents of a list, starting from the 'cdr'.
// * @details This helper is used by print_node to correctly format lists,
// *          including proper spacing and handling of the closing parenthesis
// *          and dotted pairs.
// * @param node The current cons cell (or terminal node) in the list.
// */
//void print_list_contents(Node* node) {
//    if (node == g_nil) {
//        printf(")"); // End of a proper list
//        return;
//    }
//    if (node->tag == N_CONS) {
//        printf(" ");
//        print_node(node->v.cons.car);
//        print_list_contents(node->v.cons.cdr);
//    } else {
//        // This is a dotted pair, e.g., (a . b)
//        printf(" . ");
//        print_node(node);
//        printf(")");
//    }
//}
//
///**
// * @brief Prints a single AST Node and its children recursively.
// * @param node The root of the AST (or sub-tree) to print.
// */
//void print_node(Node* node) {
//    if (!node) {
//        printf("<null node>");
//        return;
//    }
//    switch (node->tag) {
//        case N_NIL:
//            printf("()");
//            break;
//        case N_CONS:
//            printf("(");
//            print_node(node->v.cons.car);
//            print_list_contents(node->v.cons.cdr);
//            break;
//        case N_SYMBOL:
//            printf("%s", node->v.sym->name);
//            break;
//        case N_INT:
//            // Note: The current parser does not generate N_INT nodes, but we handle it for completeness.
//            printf("%lld", (long long)node->v.i);
//            break;
//        case N_STRING:
//            // Note: The current parser does not generate N_STRING nodes.
//            printf("\"%s\"", node->v.str.ptr);
//            break;
//        default:
//            printf("<unknown node tag: %d>", node->tag);
//            break;
//    }
//}
//
///**
// * @brief Recursively frees all memory associated with an AST.
// * @details Traverses the tree and frees each allocated Node. It correctly
// *          handles shared data (like symbols owned by the SymTab) and
// *          statically allocated nodes (g_nil).
// * @param node The root of the AST to free.
// */
//void free_ast(Node* node) {
//    if (!node || node == g_nil) {
//        return; // Do not free the global nil object
//    }
//
//    switch (node->tag) {
//        case N_CONS:
//            free_ast(node->v.cons.car);
//            free_ast(node->v.cons.cdr);
//            break;
//        case N_STRING:
//            // The string's data was copied, so it must be freed.
//            free((void*)node->v.str.ptr);
//            break;
//        case N_SYMBOL:
//        case N_INT:
//        case N_NIL:
//            // Symbols are owned by the SymTab, which is freed separately.
//            // Integers are part of the node. g_nil is handled above.
//            // Nothing extra to free here besides the node itself below.
//            break;
//    }
//    free(node);
//}
//
///**
// * @brief A full test runner for the lexer and parser.
// * @details This function takes a source string, runs it through the entire
// *          parsing pipeline, prints the result or error, and then performs
// *          all necessary memory cleanup.
// * @param test_name A descriptive name for the test case.
// * @param source The Lisp source code string to parse.
// */
//void run_parser_test(const char* test_name, const char* source) {
//    printf("--- Running Test: %s ---\n", test_name);
//    printf("Input: \"%s\"\n", source);
//
//    // 1. Lexing Stage
//    size_t token_count = 0;
//    token* tokens = lexer(source, strlen(source), &token_count);
//    if (!tokens && strlen(source) > 0) {
//        fprintf(stderr, "Lexer failed to allocate memory!\n\n");
//        return;
//    }
//
//    // 2. Parser Setup
//    SymTab st;
//    symtab_init(&st);
//
//    ast result_ast = {0}; // Zero-initialize the ast struct
//    result_ast.lexer_tokens = tokens;
//    result_ast.source_name = test_name;
//
//    Parser p = {
//        .st = &st,
//        .out = &result_ast,
//        .current = tokens,
//        .end = tokens + token_count
//    };
//
//    // 3. Parsing Stage
//    parse(&p);
//
//    // 4. Print Results
//    if (result_ast.has_error) {
//        printf("Parse Error: %s at span ~%d\n", result_ast.error_message, result_ast.error_line);
//    } else {
//        printf("Parse Success. AST:\n");
//        // The top level is a list of expressions. Iterate and print each one.
//        Node* current_expr_cell = result_ast.head;
//        while (current_expr_cell != g_nil) {
//            print_node(current_expr_cell->v.cons.car);
//            printf("\n");
//            current_expr_cell = current_expr_cell->v.cons.cdr;
//        }
//        if (result_ast.head == g_nil) {
//             printf("(No expressions parsed)\n");
//        }
//    }
//
//    // 5. Cleanup Stage
//    printf("Cleaning up resources...\n");
//    free_ast(result_ast.head);
//    free_symtab(&st);
//    free_tokens(tokens, token_count);
//    printf("--- Test Complete ---\n\n");
//}
//
//
//int main(void) {
//    printf("==========================================\n");
//    printf("           Lisp Parser Test               \n");
//    printf("==========================================\n");
//
//    // --- Casi di Successo Convoluti ---
//    run_parser_test("Deeply Nested List", "((((((((((10))))))))))");
//    run_parser_test("Mixed Whitespace Hell", "(\t+\n1\r\n\t( - 5 2   ) )");
//    run_parser_test("Quote Mania", "'('('('a)))");
//    run_parser_test("Adjacent Parentheses and Atoms", "(a)(b c() d) 'e'f(g)");
//    run_parser_test("Symbols that look like operators", "(define + 10) (let ((* 5)) +)");
//    run_parser_test("Empty lists at various positions", "(() (a ()) () (b c ()))");
//    run_parser_test("Quote an empty list", "'()");
//
//    // --- Edge Cases Estremi ---
//    run_parser_test("Only operators", "+ - * /");
//    run_parser_test("Quote a quote", "''a");
//    run_parser_test("List starting with a number", "(1 2 3)"); // Valido in Lisp, ma a volte problematico per i parser semplici
//    run_parser_test("Extremely long symbol", "(this-is-a-very-very-very-long-symbol-name-that-tests-allocations)");
//    run_parser_test("Combination of quotes and lists", "(list 'a '(b c) ''d)");
//
//	printf("ERRORS CHECK BELOW\n");
//
//    // --- Casi di Errore "Bastardi" e Sottili ---
//    run_parser_test("Error: Mismatched Parentheses Interleaved", "(()");
//    run_parser_test("Error: Mismatched Parentheses Interleaved 2", "(())())");
//    run_parser_test("Error: Atom directly after quote without space", "'(a'b)"); // Questo è sintatticamente ambiguo/errato
//    run_parser_test("Error: Input ends mid-list after a quote", "(a '");
//    run_parser_test("Error: Unclosed list within a quoted list", "'(a (b c)");
//    run_parser_test("Error: Operator as a list head but with unclosed list", "(+ 1 2");
//    run_parser_test("Error: Stray quote at the end", "()'");
//    run_parser_test("Error: Stray characters after a full expression", "(a b) c)");
//    
//    printf("====================================================\n");
//    printf("                Test Suite Complete\n");
//    printf("====================================================\n\n");
//
//    return 0;
//}
