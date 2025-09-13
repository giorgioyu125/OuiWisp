/**
 * @file eval.c
 * @brief Implementation of the eval.h file.
 */
#include "eval.h"
#include "src/ggc.h"
#include "src/parser.h"



/**
 * @brief
 */
EnvFrame* env_frame_new(Parser* parser, EnvFrame* parent) {
    size_t frame_size = sizeof(EnvFrame);
    
    EnvFrame* frame = (EnvFrame*)ggc_alloc(parser->gc, parser->st, frame_size);
    if (!frame) return NULL;
    
    frame->locals = NULL;
    frame->parent = parent;
    
    return frame;
}


/**
 * @brief
 */
void env_frame_free(Parser* parser, EnvFrame* env) {
	ggc_remove_root(parser->gc, (void*)env);
}

/**
 * @brief
 */
int env_bind(Parser* parser, EnvFrame* env, Symbol* sym, Node* value) {
    if (!parser->gc) return -1;
    
    if (!env->locals) {
        env->locals = (SymTab*)ggc_alloc(parser->gc, env->locals, sizeof(SymTab));
        if (!env->locals) return -1;
        symtab_init(env->locals);
    }
    
    Symbol* interned_sym = symtab_intern(parser->gc, env->locals, sym->name, sym->len);
    if (!interned_sym) return -1;
    
    interned_sym->value_ptr = value;

	return 0;
}


/**
 * @brief
 */
Node* env_lookup(EnvFrame* env, Symbol* sym) {
    EnvFrame* current_env = env;
    while (current_env != NULL) {
        if (current_env->locals != NULL) {
            for (size_t i = 0; i < current_env->locals->len; i++) {
                Symbol* candidate = current_env->locals->data[i];
                
                if (candidate->len == sym->len &&
                    memcmp(candidate->name, sym->name, sym->len) == 0) {
                    
                    return (Node*)candidate->value_ptr;
                }
            }
        }
        
        current_env = current_env->parent;
    }
    
    return NULL;
}
