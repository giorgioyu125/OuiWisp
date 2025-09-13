/**
 * @file parser.h
 * @brief Helpful Parser Interface for a Lisp-like language
 */
#ifndef PARSER_H
#define PARSER_H

#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <stdbool.h>
#include <stdarg.h>
#include "lexer.h"
#include "ggc.h"

extern Node* g_nil;

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
		struct { size_t len; const char* ptr; } str;
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

typedef enum gc_colors{
	GC_COLOR_WHITE,
	GC_COLOR_GRAY,
	GC_COLOR_BLACK
} gc_colors;

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

	void*    value_ptr; 
	/* @brief Pointers to data and code */

	generic_fn* entry_ptr;
	/* @brief Pointer to the value/payload (usually on the heap).
	 * @warning if you want to evaluate this function you NEED to cast it first 
	 *			to the desired pointer function type. If is_native is true use 
	 *			(*NativeFn) as cast and you are done!*/

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
	
	Gc* gc;
} SymTab;

static inline Node* make_int(Gc* gc, SymTab* symtab, int64_t i) {
	Node* n = (Node*)ggc_alloc(gc, symtab, sizeof(Node));
	if (!n) return g_nil;
	n->tag = N_INT; n->v.i = i; return n;
}

static inline Node* make_string(Gc* gc,  SymTab* symtab, const char* s, size_t len) {
    size_t total_size = sizeof(Node) + len + 1;
    Node* n = (Node*)ggc_alloc(gc, symtab, total_size);
    if (!n) return g_nil;
    
    n->tag = N_STRING;
    
    char* copy = (char*)n + sizeof(Node);
    memcpy(copy, s, len);
    copy[len] = '\0';
    
    n->v.str.ptr = copy;
    n->v.str.len = len;
    return n;
}

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
void symtab_init(SymTab* st);

/**
 * @brief Increases the storage capacity of a symbol table if needed.
 * @details The capacity is grown exponentially (by doubling) to amortize reallocation costs.
 * @param st Pointer to the symbol table to grow.
 * @param mincap The minimum required capacity.
 */
void symtab_grow(SymTab* st, size_t mincap);

/**
 * @brief Resets a symbol's metadata to its default values.
 * @param sym Pointer to the symbol to reset.
 */
void sym_reset_meta(Symbol* sym);

/**
 * @brief Frees all memory associated with a symbol table.
 * @details Deallocates each symbol, the name of each symbol, and the table's internal array.
 * @param symtab Pointer to the symbol table to free.
 */
void free_symtab(SymTab* symtab);

/**
 * @brief Allocates and initializes a new symbol using GC.
 * @details The symbol's name is copied into old generation memory.
 * @param gc Garbage collector context
 * @param s Pointer to the character string for the symbol's name
 * @param n The length of the name
 * @param h The pre-calculated hash of the name
 * @return A pointer to the newly created symbol, or NULL on failure
 */
Symbol* symbol_new(Gc* gc, const char* s, size_t n, uint32_t h);

/**
 * @brief Looks up or interns a symbol in the symbol table.
 * @details If the symbol exists, returns it. Otherwise creates a new one.
 * @param gc Garbage collector context
 * @param st Pointer to the symbol table
 * @param s Pointer to the character string for the symbol's name
 * @param n The length of the name
 * @return A pointer to the symbol (existing or newly created)
 */
Symbol* symtab_intern(Gc* gc, SymTab* st, const char* s, size_t n);


Node* cons(Gc* gc, SymTab* symtab, Node* car, Node* cdr);
Node* make_symbol(Gc* gc, SymTab* symtab, const char* s, size_t len);


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

	Gc* gc;
} Parser;

/* ----------------------- MAIN API ------------------------ */ 

Node* parse(Parser* p);

#endif /* PARSER_H */
