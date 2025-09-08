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

/* -------------------- base AST -------------------- */
/*
 * @struct This struct is the first component of the below tagged union named "Node".
 */
typedef enum NodeTag{
	N_NIL,
	N_CONS, 
	N_SYMBOL,
	N_INT, 
	N_STRING
} NodeTag;

/*
 *	@struct This is a tagged union just to make a versatile Node type for the AST
 */
typedef struct Node {
	NodeTag tag;
	union {
		struct { struct Node* car; struct Node* cdr; } cons;
		int64_t i;
		struct { const char* ptr; size_t len; } str;
		struct Symbol* sym;
	} v;
}Node;

/* -------------------- Meta-info for object and symbol -------------------- */

typedef enum TypeTag{
	T_NIL,
	T_CONS,
	T_SYMBOL,
	T_INT,
	T_STRING,
	T_FLOAT,
	T_BOOL,
	T_VECTOR,
	T_FUNCTION,
	T_MACRO,
	T_SPECIAL_FORM
} TypeTag;

typedef enum EvalKind{ 
	EK_DATA, 
	EK_FUNCTION, 
	EK_MACRO, 
	EK_SPECIAL 
} EvalKind;

enum {
	F_IMMUTABLE     = 1u << 0,
	F_PURE          = 1u << 1,
	F_PRIMITIVE     = 1u << 2,
	F_VARIADIC      = 1u << 3,
	F_PINNED        = 1u << 4,
	F_EXTERNAL      = 1u << 5,
	F_CONST_BINDING = 1u << 6
};

typedef struct Env Env;					/* forward decl for closures/enviroment */
typedef Node* (*NativeFn)(Node*, Env*);  /* native entrypoint  (primitives) */

typedef void (*generic_fn)(void); 

/* pack arity: low16=min, high16=max; 0xFFFF = unbounded */
static inline uint32_t arity_pack(uint16_t min, uint16_t max) {
	return ((uint32_t)max << 16) | (uint32_t)min;
}
static inline uint16_t arity_min(uint32_t a) { return (uint16_t)(a & 0xFFFFu); }
static inline uint16_t arity_max(uint32_t a) { return (uint16_t)(a >> 16); }

/* GCInfo pack: [resvd:8 | color:8 | age:8 | gen:8] */
static inline uint32_t gcinfo_pack(uint8_t gen, uint8_t age, uint8_t color) {
	return ((uint32_t)color << 16) | ((uint32_t)age << 8) | (uint32_t)gen;
}
static inline uint8_t gcinfo_gen(uint32_t g)  { return (uint8_t)(g & 0xFFu); }
static inline uint8_t gcinfo_age(uint32_t g)  { return (uint8_t)((g >> 8) & 0xFFu); }
static inline uint8_t gcinfo_color(uint32_t g){ return (uint8_t)((g >>16) & 0xFFu); }

/* @brief Flags packer via va_list.
 * @param[in] Accepts a variable number of args that will overlap in the flags count
 *			  via a or mask in the functions. This can be considered "like" a utility macro.
*/
static inline uint32_t flags_pack(uint32_t first, ...) {
	va_list ap;
	uint32_t flags = 0;
	va_start(ap, first);
	uint32_t f = first;
	while (f) {
		flags |= f;
		f = (uint32_t)va_arg(ap, unsigned int); /* default promotions */
	}
	va_end(ap);
	return flags;
}

/**
 * @brief An enum defining a set of bit flags.
 *
 * @details These flags describe various properties of an object, function, or
 *          binding. They can be combined using the bitwise OR operator (`|`).
 *
 *          To check if a particular flag is set, use the bitwise AND 
 *          operator (`&`).
 *
 * @code
 * // Example: Create a flags variable for an immutable and primitive object
 * flags my_object_flags = immutable | primitive;
 *
 * // Check if the object is immutable
 * if (my_object_flags & immutable) {
 *     printf("The object is immutable.\n");
 * }
 * @endcode
 */
typedef enum flags {
	/** @brief No special flags are set.
	 *  It's good practice to have a value for the zero/default state.
	 */
	NO_FLAGS      = 0,

	/** @brief The object cannot be modified after its creation. */
	immutable     = 1u << 0,

	/** @brief Indicates that a function is pure, i.e., it has no side effects
	 *         and its output depends solely on its inputs.
	 */
	pure          = 1u << 1,

	/** @brief The object represents a primitive data type or function(e.g., integer, boolean). */
	primitive     = 1u << 2,

	/** @brief The function accepts a variable number of arguments (like printf). */
	variadic      = 1u << 3,

	/** @brief The object is "pinned" in memory and cannot be moved or reallocated
	 *         by a memory manager (e.g., a garbage collector).
	 */
	pinned        = 1u << 4,

	/** @brief The object or function is defined in another compilation unit
	 *         or external library.
	 */
	external      = 1u << 5,

	/** @brief The variable's binding is constant and cannot be reassigned. 
	 *         Similar to the `const` keyword in C.
	 */
	const_binding = 1u << 6
} flags;

/** @brief Check if a flag is identical to the bit mask. */
static inline int  tst_flag(uint32_t flags, uint32_t bit) { return (flags & bit) != 0; }
/** @brief Add a specified number of flags via the bit list. */
static inline void add_flag(uint32_t* flags, uint32_t bit){ *flags |= bit; }
/** @brief Removes the flag set to 1 in bit in the flags param with a mask. */
static inline void clr_flag(uint32_t* flags, uint32_t bit){ *flags &= ~bit; }

/* -------------------- Symbol, SymTab, and Helpers -------------------- */

/**
 * @brief Represents a symbol (identifier) in the language.
 * @details A symbol is a unique object that represents a name. It holds
 *          all the metadata associated with that name, such as its type,
 *          value, arity (if it's a function), flags, and GC information.
 */
typedef struct Symbol {
	/* Core identification data */
	const char* name;   
	/* @brief The name of the symbol (as a string). */
	size_t len;         
	/* @brief The length of the name. */
	uint32_t hash;      
	/* @brief Pre-calculated hash of the name for fast lookups. */

	/* Semantic metadata */
	TypeTag  type;      
	/* @brief The high-level type of the symbol. @see TypeTag */
	EvalKind eval;      
	/* @brief How the evaluator should handle this symbol. @see EvalKind */
	uint32_t flags;     
	/* @brief A bitmask of additional properties. @see flags */
	uint32_t arity;     
	/* @brief Packed min/max arity (for functions/macros). */

	/* Pointers to data and code */
	void*    value_ptr; 
	/* @brief Pointer to the value/payload (usually on the heap).
	 * @warning if you want to evaluate this function you NEED to cast it first 
	 *			to the desired pointer function type. If is_native is true use 
	 *			(*NativeFn) as cast and you are done!*/
	generic_fn* entry_ptr;
	bool is_native;
	/* @brief Pointer to executable code (NativeFn, AST, or bytecode). */
	Env*     env_ptr;   
	/* @brief Pointer to the closure environment (for functions). */

	/* Runtime metadata */
	int32_t  stack_slot;
	/* @brief Index on the local stack (-1 if not a local variable). */
	uint32_t gcinfo;    
	/* @brief Packed information for the Garbage Collector. */
	size_t   size_bytes;
	/* @brief Size of the payload in bytes (for the GC). */
} Symbol;

/**
 * @brief A table that stores every unique symbol, ensuring that each symbol name 
 *		  exists only once. This process is known as interning.
 */
typedef struct SymTab {
  Symbol** data; ///< Array of symbols.
  size_t len;    ///< Number of symbol in the table.
  size_t cap;    ///< Capacity allocated in the table.
} SymTab;

/* -------------------- Hashing and Symbol Allocation -------------------- */

#define __HASH_STD_SEED 5
static inline uint32_t hash(const char* s, size_t n) {
	uint32_t h = 2166136261u;
	for (size_t i = 0; i < n; ++i) {
		h ^= (uint8_t)s[i]; 
		h *= 16777619u; 
	}
	return h;
}

/**
 * @brief Initializes a symbol table to an empty state.
 * @param st Pointer to the symbol table to initialize.
 */
static inline void symtab_init(SymTab* st) {
	st->data = NULL; 
	st->len = 0;
	st->cap = 0;
}

/**
 * @brief Increases the storage capacity of a symbol table if needed.
 * @details The capacity is grown exponentially (by doubling) to amortize reallocation costs.
 * @param st Pointer to the symbol table to grow.
 * @param mincap The minimum required capacity.
 */
static inline void symtab_grow(SymTab* st, size_t mincap) {
	if (st->cap >= mincap) return;
	size_t cap = st->cap ? st->cap * 2 : 8;
	while (cap < mincap) cap *= 2;
	void* p = realloc(st->data, cap * sizeof(Symbol*));
	if (!p) abort();
	st->data = (Symbol**)p; st->cap = cap;
}

/**
 * @brief Resets a symbol's metadata to its default values.
 * @param sym Pointer to the symbol to reset.
 */
static inline void sym_reset_meta(Symbol* sym) {
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

/**
 * @brief Frees all memory associated with a symbol table.
 * @details Deallocates each symbol, the name of each symbol, and the table's internal array.
 * @param symtab Pointer to the symbol table to free.
 */
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

/**
 * @brief Allocates and initializes a new symbol.
 * @details The symbol's name is copied into a new memory location.
 * @param s Pointer to the character string for the symbol's name.
 * @param n The length of the name.
 * @param h The pre-calculated hash of the name.
 * @return A pointer to the newly created symbol.
 */
static inline Symbol* symbol_new(const char* s, size_t n, uint32_t h) {
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

/* lookup/intern lineare (vettore dinamico) */

static inline Symbol* symtab_intern(SymTab* st, const char* s, size_t n) {
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

/*
 * @brief Allocates a node and returns the corrisponding cons-cell.
 *
 * @warning This memory needs to be freed by the caller (the GC probably).
 *			Memory leak are possible.
 *
 * @note The functions below called make_int, make_string_copy, make_symbol are all the "same" as this
 *		 just for different data types.
 */
static inline Node* cons(Node* car, Node* cdr) {
	Node* n = (Node*)malloc(sizeof(Node)); if (!n) abort(); ///> Node allocation
	n->tag = N_CONS; n->v.cons.car = car; n->v.cons.cdr = cdr; return n; ///> Node initialization
}

static inline Node* make_int(int64_t i) {
	Node* n = (Node*)malloc(sizeof(Node)); if (!n) abort();
	n->tag = N_INT; n->v.i = i; return n;
}

static inline Node* make_string_copy(const char* s, size_t len) {
	Node* n = (Node*)malloc(sizeof(Node)); if (!n) abort();
	char* copy = (char*)malloc(len + 1); if (!copy) abort();
	memcpy(copy, s, len); copy[len] = '\0';
	n->tag = N_STRING; n->v.str.ptr = copy; n->v.str.len = len; return n;
}

static inline Node* make_symbol(SymTab* st, const char* s, size_t len) {
	Node* n = (Node*)malloc(sizeof(Node)); if (!n) abort();
	Symbol* sym = symtab_intern(st, s, len);
	n->tag = N_SYMBOL; n->v.sym = sym; return n;
}

/* -------------------- Compact Setter for columns -------------------- */

/**
 * @brief Configures a symbol to represent a data value.
 * @param s   Pointer to the Symbol to configure.
 * @param t   The high-level type of the data (e.g., T_INT).
 * @param fl  Bitmask of flags to apply (e.g., F_IMMUTABLE).
 * @param val Pointer to the data's payload.
 * @param sz  Size of the payload in bytes (for GC).
 */
static inline void sym_set_data(Symbol* s, TypeTag t, uint32_t fl, void* val, size_t sz) {
	s->type = t; 
	s->eval = EK_DATA; s->flags = fl;
	s->arity = 0; s->value_ptr = val; s->size_bytes = sz;
	s->entry_ptr = NULL; s->env_ptr = NULL;
}

/**
 * @brief Configures a symbol as a primitive (native C) function.
 * @param s   Pointer to the Symbol to configure.
 * @param fn  Pointer to the native C function implementation.
 * @param min Minimum number of arguments (arity).
 * @param max Maximum number of arguments (arity).
 * @param fl  Additional bitmask of flags to apply.
 */
static inline void sym_set_primitive(Symbol* s, NativeFn fn, uint16_t min, uint16_t max, uint32_t fl) {
	s->type = T_FUNCTION; s->eval = EK_FUNCTION;
	s->flags = fl | F_PRIMITIVE;
	s->arity = arity_pack(min, max);
	s->entry_ptr = (generic_fn*)fn; 
	s->env_ptr = NULL;
}

// ../src/parser.c: In function ‘sym_set_primitive’:
// ../src/parser.c:407:24: warning: ISO C forbids conversion of function pointer to object pointer type [-Wpedantic]
//   407 |         s->entry_ptr = (void*)fn; s->env_ptr = NULL;
//       |                        ^
// ../src/parser.c: In function ‘sym_set_macro’:
// ../src/parser.c:422:24: warning: ISO C forbids conversion of function pointer to object pointer type [-Wpedantic]
//   422 |         s->entry_ptr = (void*)expander; s->env_ptr = NULL;
//       |                        ^
// ../src/parser.c: In function ‘sym_set_special’:
// ../src/parser.c:437:24: warning: ISO C forbids conversion of function pointer to object pointer type [-Wpedantic]
//   437 |         s->entry_ptr = (void*)sf; s->env_ptr = NULL;
//       |                        ^


/**
 * @brief Configures a symbol as a primitive (native C) macro.
 * @param s        Pointer to the Symbol to configure.
 * @param expander Pointer to the native C macro expander function.
 * @param min      Minimum number of arguments (arity).
 * @param max      Maximum number of arguments (arity).
 * @param fl       Additional bitmask of flags to apply.
 */
static inline void sym_set_macro(Symbol* s, NativeFn expander, uint16_t min, uint16_t max, uint32_t fl) {
	s->type = T_MACRO; s->eval = EK_MACRO;
	s->flags = fl | F_PRIMITIVE;
	s->arity = arity_pack(min, max);
	s->entry_ptr = (generic_fn*)expander; 
	s->env_ptr = NULL;
}

/**
 * @brief Configures a symbol as a special form implemented in C.
 * @param s   Pointer to the Symbol to configure.
 * @param sf  Pointer to the native C function implementing the special form.
 * @param min Minimum number of arguments (arity).
 * @param max Maximum number of arguments (arity).
 * @param fl  Additional bitmask of flags to apply.
 */
static inline void sym_set_special(Symbol* s, NativeFn sf, uint16_t min, uint16_t max, uint32_t fl) {
	s->type = T_SPECIAL_FORM; s->eval = EK_SPECIAL;
	s->flags = fl | F_PRIMITIVE;
	s->arity = arity_pack(min, max);
	s->entry_ptr = (generic_fn*)sf; 
	s->env_ptr = NULL;
}

/**
 * @brief Configures a symbol as a user-defined function (AST or bytecode).
 * @param s               Pointer to the Symbol to configure.
 * @param ast_or_bytecode Pointer to the function's body (AST root or bytecode).
 * @param env             Pointer to the function's closure environment.
 * @param min             Minimum number of arguments (arity).
 * @param max             Maximum number of arguments (arity).
 * @param fl              Bitmask of flags to apply.
 */
static inline void sym_set_function_ast(Symbol* s, void* ast_or_bytecode, Env* env,
                                        uint16_t min, uint16_t max, uint32_t fl) {
	s->type = T_FUNCTION; s->eval = EK_FUNCTION;
	s->flags = fl; s->arity = arity_pack(min, max);
	s->entry_ptr = ast_or_bytecode; s->env_ptr = env;
}

/** @brief Sets the Garbage Collector information for a symbol. */
static inline void sym_set_gc(Symbol* s, uint8_t gen, uint8_t age, uint8_t color) {
	s->gcinfo = gcinfo_pack(gen, age, color);
}

/** @brief Sets the local stack slot index for a symbol. */
static inline void sym_set_stack_slot(Symbol* s, int32_t slot) { s->stack_slot = slot; }

/** @brief Sets the payload size in bytes for a symbol. */
static inline void sym_set_size(Symbol* s, size_t sz) { s->size_bytes = sz; }

/** @brief Adds one or more flags to a symbol's flag mask. */
static inline void sym_add_flags(Symbol* s, uint32_t fl) { s->flags |= fl; }

/** @brief Removes (clears) one or more flags from a symbol's flag mask. */
static inline void sym_clear_flags(Symbol* s, uint32_t fl) { s->flags &= ~fl; }

/* ------------------------- Global AST --------------------------- */

static Node s_nil = { .tag = N_NIL };
Node* g_nil = &s_nil;

typedef struct ast{
	token* lexer_tokens;
    const char* source_name;
	
	Node* head;
    
    bool has_error;
    const char* error_message;
    int error_line;
} ast;

typedef struct Parser { 
	SymTab* st;
	ast* out;	

	token* current;
	token* end; 
} Parser;

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


// TESTING (dummy main function)

#include <stdio.h>

void print_node(Node* node);
void free_ast(Node* node);

/**
 * @brief Recursively prints the contents of a list, starting from the 'cdr'.
 * @details This helper is used by print_node to correctly format lists,
 *          including proper spacing and handling of the closing parenthesis
 *          and dotted pairs.
 * @param node The current cons cell (or terminal node) in the list.
 */
void print_list_contents(Node* node) {
    if (node == g_nil) {
        printf(")"); // End of a proper list
        return;
    }
    if (node->tag == N_CONS) {
        printf(" ");
        print_node(node->v.cons.car);
        print_list_contents(node->v.cons.cdr);
    } else {
        // This is a dotted pair, e.g., (a . b)
        printf(" . ");
        print_node(node);
        printf(")");
    }
}

/**
 * @brief Prints a single AST Node and its children recursively.
 * @param node The root of the AST (or sub-tree) to print.
 */
void print_node(Node* node) {
    if (!node) {
        printf("<null node>");
        return;
    }
    switch (node->tag) {
        case N_NIL:
            printf("()");
            break;
        case N_CONS:
            printf("(");
            print_node(node->v.cons.car);
            print_list_contents(node->v.cons.cdr);
            break;
        case N_SYMBOL:
            printf("%s", node->v.sym->name);
            break;
        case N_INT:
            // Note: The current parser does not generate N_INT nodes, but we handle it for completeness.
            printf("%lld", (long long)node->v.i);
            break;
        case N_STRING:
            // Note: The current parser does not generate N_STRING nodes.
            printf("\"%s\"", node->v.str.ptr);
            break;
        default:
            printf("<unknown node tag: %d>", node->tag);
            break;
    }
}

/**
 * @brief Recursively frees all memory associated with an AST.
 * @details Traverses the tree and frees each allocated Node. It correctly
 *          handles shared data (like symbols owned by the SymTab) and
 *          statically allocated nodes (g_nil).
 * @param node The root of the AST to free.
 */
void free_ast(Node* node) {
    if (!node || node == g_nil) {
        return; // Do not free the global nil object
    }

    switch (node->tag) {
        case N_CONS:
            free_ast(node->v.cons.car);
            free_ast(node->v.cons.cdr);
            break;
        case N_STRING:
            // The string's data was copied, so it must be freed.
            free((void*)node->v.str.ptr);
            break;
        case N_SYMBOL:
        case N_INT:
        case N_NIL:
            // Symbols are owned by the SymTab, which is freed separately.
            // Integers are part of the node. g_nil is handled above.
            // Nothing extra to free here besides the node itself below.
            break;
    }
    free(node);
}

/**
 * @brief A full test runner for the lexer and parser.
 * @details This function takes a source string, runs it through the entire
 *          parsing pipeline, prints the result or error, and then performs
 *          all necessary memory cleanup.
 * @param test_name A descriptive name for the test case.
 * @param source The Lisp source code string to parse.
 */
void run_parser_test(const char* test_name, const char* source) {
    printf("--- Running Test: %s ---\n", test_name);
    printf("Input: \"%s\"\n", source);

    // 1. Lexing Stage
    size_t token_count = 0;
    token* tokens = lexer(source, strlen(source), &token_count);
    if (!tokens && strlen(source) > 0) {
        fprintf(stderr, "Lexer failed to allocate memory!\n\n");
        return;
    }

    // 2. Parser Setup
    SymTab st;
    symtab_init(&st);

    ast result_ast = {0}; // Zero-initialize the ast struct
    result_ast.lexer_tokens = tokens;
    result_ast.source_name = test_name;

    Parser p = {
        .st = &st,
        .out = &result_ast,
        .current = tokens,
        .end = tokens + token_count
    };

    // 3. Parsing Stage
    parse(&p);

    // 4. Print Results
    if (result_ast.has_error) {
        printf("Parse Error: %s at span ~%d\n", result_ast.error_message, result_ast.error_line);
    } else {
        printf("Parse Success. AST:\n");
        // The top level is a list of expressions. Iterate and print each one.
        Node* current_expr_cell = result_ast.head;
        while (current_expr_cell != g_nil) {
            print_node(current_expr_cell->v.cons.car);
            printf("\n");
            current_expr_cell = current_expr_cell->v.cons.cdr;
        }
        if (result_ast.head == g_nil) {
             printf("(No expressions parsed)\n");
        }
    }

    // 5. Cleanup Stage
    printf("Cleaning up resources...\n");
    free_ast(result_ast.head);
    free_symtab(&st);
    free_tokens(tokens, token_count);
    printf("--- Test Complete ---\n\n");
}


int main(void) {
    printf("==========================================\n");
    printf("           Lisp Parser Test               \n");
    printf("==========================================\n");

    // --- Casi di Successo Convoluti ---
    run_parser_test("Deeply Nested List", "((((((((((10))))))))))");
    run_parser_test("Mixed Whitespace Hell", "(\t+\n1\r\n\t( - 5 2   ) )");
    run_parser_test("Quote Mania", "'('('('a)))");
    run_parser_test("Adjacent Parentheses and Atoms", "(a)(b c() d) 'e'f(g)");
    run_parser_test("Symbols that look like operators", "(define + 10) (let ((* 5)) +)");
    run_parser_test("Empty lists at various positions", "(() (a ()) () (b c ()))");
    run_parser_test("Quote an empty list", "'()");

    // --- Edge Cases Estremi ---
    run_parser_test("Only operators", "+ - * /");
    run_parser_test("Quote a quote", "''a");
    run_parser_test("List starting with a number", "(1 2 3)"); // Valido in Lisp, ma a volte problematico per i parser semplici
    run_parser_test("Extremely long symbol", "(this-is-a-very-very-very-long-symbol-name-that-tests-allocations)");
    run_parser_test("Combination of quotes and lists", "(list 'a '(b c) ''d)");

	printf("ERRORS CHECK BELOW\n");

    // --- Casi di Errore "Bastardi" e Sottili ---
    run_parser_test("Error: Mismatched Parentheses Interleaved", "(()");
    run_parser_test("Error: Mismatched Parentheses Interleaved 2", "(())())");
    run_parser_test("Error: Atom directly after quote without space", "'(a'b)"); // Questo è sintatticamente ambiguo/errato
    run_parser_test("Error: Input ends mid-list after a quote", "(a '");
    run_parser_test("Error: Unclosed list within a quoted list", "'(a (b c)");
    run_parser_test("Error: Operator as a list head but with unclosed list", "(+ 1 2");
    run_parser_test("Error: Stray quote at the end", "()'");
    run_parser_test("Error: Stray characters after a full expression", "(a b) c)");
    
    printf("====================================================\n");
    printf("                Test Suite Complete\n");
    printf("====================================================\n\n");

    return 0;
}
