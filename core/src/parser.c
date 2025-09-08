#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <stdbool.h>
#include <stdarg.h>
#include "lexer.h"

/* -------------------- AST base (immutato/esteso dove serve) -------------------- */

typedef enum NodeTag{
  N_NIL, N_CONS, N_SYMBOL, N_INT, N_STRING
} NodeTag;

typedef struct node Node;
struct node {
	NodeTag tag;
	union {
		struct { Node* car; Node* cdr; } cons;
		int64_t i;
		struct { const char* ptr; size_t len; } str;
		struct Symbol* sym;
	} v;
};

/* -------------------- Meta-informazioni per oggetti/simboli -------------------- */

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

typedef struct Env Env;            /* forward decl for closures */
typedef Node* (*NativeFn)(Node*, Env*);  /* native entrypoint  (primitives) */

/* pack arity: low16=min, high16=max; 0xFFFF = unbounded */
inline uint32_t arity_pack(uint16_t min, uint16_t max) {
	return ((uint32_t)max << 16) | (uint32_t)min;
}
inline uint16_t arity_min(uint32_t a) { return (uint16_t)(a & 0xFFFFu); }
inline uint16_t arity_max(uint32_t a) { return (uint16_t)(a >> 16); }

/* GCInfo pack: [resvd:8 | color:8 | age:8 | gen:8] */
inline uint32_t gcinfo_pack(uint8_t gen, uint8_t age, uint8_t color) {
	return ((uint32_t)color << 16) | ((uint32_t)age << 8) | (uint32_t)gen;
}
inline uint8_t gcinfo_gen(uint32_t g)  { return (uint8_t)(g & 0xFFu); }
inline uint8_t gcinfo_age(uint32_t g)  { return (uint8_t)((g >> 8) & 0xFFu); }
inline uint8_t gcinfo_color(uint32_t g){ return (uint8_t)((g >>16) & 0xFFu); }

/* flags packer via va_list */
inline uint32_t flags_pack(uint32_t first, ...) {
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

typedef enum flags {
	immutable     = 1u << 0,
	pure          = 1u << 1,
	primitive     = 1u << 2,
	variadic      = 1u << 3,
	pinned        = 1u << 4,
	external      = 1u << 5,
	const_binding = 1u << 6
} flags;

/* test/add/clear bit: micro-helper */
inline int  tst_flag(uint32_t flags, uint32_t bit) { return (flags & bit) != 0; }
inline void add_flag(uint32_t* flags, uint32_t bit){ *flags |= bit; }
inline void clr_flag(uint32_t* flags, uint32_t bit){ *flags &= ~bit; }


/* -------------------- Symbol, SymTab e helper -------------------- */

typedef struct Symbol {
	const char* name;
	size_t len;
	uint32_t hash;

	/* helper values */
	TypeTag  type;
	EvalKind eval;
	uint32_t flags;
	uint32_t arity;      /* pack min/max; 0 per dati */
	void*    value_ptr;  /* payload heap o immediato (se lo adotterai) */
	void*    entry_ptr;  /* codice: NativeFn o puntatore AST/bytecode */
	Env*     env_ptr;    /* chiusura */
	int32_t  stack_slot; /* root index nella tua pila; -1 se non root */
	uint32_t gcinfo;     /* gen/age/color */
	size_t   size_bytes; /* dimensione payload (copying GC), 0 se immediato */
} Symbol;

typedef struct SymTab {
  Symbol** data;
  size_t len;
  size_t cap;
} SymTab;

/* -------------------- Hash e allocazione simboli -------------------- */

inline uint32_t hash(const char* s, size_t n) {
	uint32_t h = 2166136261u;
	for (size_t i = 0; i < n; ++i) {
		h ^= (uint8_t)s[i]; h *= 16777619u; 
	}
	return h;
}

inline void symtab_init(SymTab* st) {
	st->data = NULL; 
	st->len = 0;
	st->cap = 0;
}

inline void symtab_grow(SymTab* st, size_t mincap) {
	if (st->cap >= mincap) return;
	size_t cap = st->cap ? st->cap * 2 : 8;
	while (cap < mincap) cap *= 2;
	void* p = realloc(st->data, cap * sizeof(Symbol*));
	if (!p) abort();
	st->data = (Symbol**)p; st->cap = cap;
}

inline void sym_reset_meta(Symbol* sym) {
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

inline Symbol* symbol_new(const char* s, size_t n, uint32_t h) {
	Symbol* sym = (Symbol*)malloc(sizeof(Symbol));
	if (!sym) abort();
	char* name = (char*)malloc(n + 1);
	if (!name) abort();
	memcpy(name, s, n); name[n] = '\0';
	sym->name = name; sym->len = n; sym->hash = h;
	sym_reset_meta(sym);
	return sym;
}

/* lookup/intern lineare (vettore dinamico) */
inline Symbol* symtab_intern(SymTab* st, const char* s, size_t n) {
	uint32_t h = hash(s, n);
	for (size_t i = 0; i < st->len; ++i) {
		Symbol* sym = st->data[i];
		if (sym->hash == h && sym->len == n && memcmp(sym->name, s, n) == 0)
		return sym;
	}
	Symbol* sym = symbol_new(s, n, h);
	if (st->len == st->cap) symtab_grow(st, st->len + 1);
	st->data[st->len++] = sym;
	return sym;
}

inline Node* cons(Node* car, Node* cdr) {
	Node* n = (Node*)malloc(sizeof(Node)); if (!n) abort();
	n->tag = N_CONS; n->v.cons.car = car; n->v.cons.cdr = cdr; return n;
}

inline Node* make_int(int64_t i) {
	Node* n = (Node*)malloc(sizeof(Node)); if (!n) abort();
	n->tag = N_INT; n->v.i = i; return n;
}

inline Node* make_string_copy(const char* s, size_t len) {
	Node* n = (Node*)malloc(sizeof(Node)); if (!n) abort();
	char* copy = (char*)malloc(len + 1); if (!copy) abort();
	memcpy(copy, s, len); copy[len] = '\0';
	n->tag = N_STRING; n->v.str.ptr = copy; n->v.str.len = len; return n;
}

inline Node* make_symbol(SymTab* st, const char* s, size_t len) {
	Node* n = (Node*)malloc(sizeof(Node)); if (!n) abort();
	Symbol* sym = symtab_intern(st, s, len);
	n->tag = N_SYMBOL; n->v.sym = sym; return n;
}

/* -------------------- Setter compatti per le colonne -------------------- */

inline void sym_set_data(Symbol* s, TypeTag t, uint32_t fl, void* val, size_t sz) {
	s->type = t; 
	s->eval = EK_DATA; s->flags = fl;
	s->arity = 0; s->value_ptr = val; s->size_bytes = sz;
	s->entry_ptr = NULL; s->env_ptr = NULL;
}

inline void sym_set_primitive(Symbol* s, NativeFn fn, uint16_t min, uint16_t max, uint32_t fl) {
	s->type = T_FUNCTION; s->eval = EK_FUNCTION;
	s->flags = fl | F_PRIMITIVE;
	s->arity = arity_pack(min, max);
	s->entry_ptr = (void*)fn; s->env_ptr = NULL;
}

inline void sym_set_macro(Symbol* s, NativeFn expander, uint16_t min, uint16_t max, uint32_t fl) {
	s->type = T_MACRO; s->eval = EK_MACRO;
	s->flags = fl | F_PRIMITIVE;
	s->arity = arity_pack(min, max);
	s->entry_ptr = (void*)expander; s->env_ptr = NULL;
}

inline void sym_set_special(Symbol* s, NativeFn sf, uint16_t min, uint16_t max, uint32_t fl) {
	s->type = T_SPECIAL_FORM; s->eval = EK_SPECIAL;
	s->flags = fl | F_PRIMITIVE;
	s->arity = arity_pack(min, max);
	s->entry_ptr = (void*)sf; s->env_ptr = NULL;
}

inline void sym_set_function_ast(Symbol* s, void* ast_or_bytecode, Env* env,
                                        uint16_t min, uint16_t max, uint32_t fl) {
	s->type = T_FUNCTION; s->eval = EK_FUNCTION;
	s->flags = fl; s->arity = arity_pack(min, max);
	s->entry_ptr = ast_or_bytecode; s->env_ptr = env;
}

inline void sym_set_gc(Symbol* s, uint8_t gen, uint8_t age, uint8_t color) {
	s->gcinfo = gcinfo_pack(gen, age, color);
}

inline void sym_set_stack_slot(Symbol* s, int32_t slot) { s->stack_slot = slot; }
inline void sym_set_size(Symbol* s, size_t sz) { s->size_bytes = sz; }
inline void sym_add_flags(Symbol* s, uint32_t fl) { s->flags |= fl; }
inline void sym_clear_flags(Symbol* s, uint32_t fl) { s->flags &= ~fl; }

/* -------------------- Globali/minimi AST -------------------- */

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
} Parser;

static token peek(Parser* p){

}

static token next(Parser* p){

}

static Node* parse_expr(Parser* p){

}

static Node* parse_list(Parser* p){

}

static Node* parse_atom(Parser* p){

}

static Node* parse_quote_like(Parser* p, const char* sym, size_t len){

}

Node* parse_toplevel(Parser* p){

}

