/**
 * @file eval.h
 * @brief Eval interface for breaking down the whole AST.
 */
#ifndef EVAL_H
#define EVAL_H

#include "ggc.h"
#include "parser.h"

/* ------------------- ENVIRONMENT MANAGEMENT ------------------- */

/**
 * @brief Environment frame containing local bindings
 */
typedef struct EnvFrame {
    SymTab* locals;           ///< Local symbol table
    struct EnvFrame* parent;  ///< Parent environment (for scoping)
} EnvFrame;

/**
 * @brief Initialize a new environment frame
 * @param parent Parent environment (NULL for global scope)
 * @return New environment frame
 */
EnvFrame* env_frame_new(Parser* parser, EnvFrame* parent);

/**
 * @brief Destroy an environment frame and its contents
 * @param env Environment to destroy
 */
void env_frame_free(Parser* parser, EnvFrame* env);

/**
 * @brief Bind a value to a symbol in the current environment
 * @param env Environment to bind in
 * @param sym Symbol to bind
 * @param value Value to associate with symbol
 */
int env_bind(Parser* parser, EnvFrame* env, Symbol* sym, Node* value);

/**
 * @brief Look up a symbol's value in the environment chain
 * @param env Starting environment
 * @param sym Symbol to find
 * @return Value bound to symbol, or NULL if not found
 */
Node* env_lookup(EnvFrame* env, Symbol* sym);

/* ------------------- CORE EVALUATION ------------------- */

/**
 * @brief Evaluate a single AST node
 * @param node Node to evaluate
 * @param env Current environment
 * @return Evaluated result
 */
Node* eval_node(Node* node, EnvFrame* env);

/**
 * @brief Evaluate a list form (special forms and function calls)
 * @param list Cons cell representing the form
 * @param env Current environment
 * @return Evaluated result
 */
Node* eval_list(Node* list, EnvFrame* env);

/**
 * @brief Apply a function to arguments
 * @param func Function node to apply
 * @param args Arguments list
 * @param env Environment
 * @return Application result
 */
Node* apply(Node* func, Node* args, EnvFrame* env);

/* ------------------- BUILT-IN FUNCTIONS ------------------- */

/**
 * @brief Define built-in functions
 * @param env Global environment to populate
 */
void define_builtins(EnvFrame* env);


/* ---------------------- MAIN API ----------------- */

int eval_ast(Parser* parsed);

#endif
