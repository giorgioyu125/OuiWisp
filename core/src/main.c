#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "ggc.h"
#include "parser.h"
#include "lexer.h"

/* ------------------- LISP FILE ------------------ */ 

char* lisp_file = "(Defun Factorial (N)\n"
                  "  \"Calculate the factorial of N\"\n"
                  "  (if (<= N 1)\n"
                  "      1\n"
                  "      (* N (Factorial (- N 1)))))\n"
                  "\n"
                  "(Defun Fibonacci (N)\n"
                  "  \"Calculate the nth Fibonacci number\"\n"
                  "  (Cond ((= N 0) 0)\n"
                  "        ((= N 1) 1)\n"
                  "        (t (+ (Fibonacci (- N 1))\n"
                  "              (Fibonacci (- N 2))))))\n"
                  "\n"
                  ";; Example usage\n"
                  "(Print (Factorial 5))  ; Output: 120\n"
                  "(Print (Fibonacci 7))  ; Output: 13\n";


/* ------------------     END    ------------------ */

void print_ast(Node* ast_list);
void print_node(Node* node);

/**
 * @brief A helper function to print the current memory usage of the heap.
 * @param gc The garbage collector context.
 * @param title A title to print for the stats block.
 */
void print_heap_stats(Gc* gc, const char* title) {
    Heap* h = gc->heap;
    size_t eden_used = (size_t)(h->nursery.bump_ptr - h->nursery.eden.start);
    size_t eden_size = memregion_size(&h->nursery.eden);
    size_t old_used = (size_t)(h->old_gen.bump_ptr - h->old_gen.region.start);
    size_t old_size = memregion_size(&h->old_gen.region);

    printf("\n--- Heap Stats: %s ---\n", title);
    printf("Total Heap Size: %zu bytes\n", h->heap_memory_size);
    printf("Eden Space:      %6zu / %6zu bytes (%2.0f%% full)\n",
           eden_used, eden_size, eden_size > 0 ? (double)eden_used / eden_size * 100.0 : 0.0);
    printf("Survivor 0 Size: %6zu bytes\n", memregion_size(&h->nursery.s0));
    printf("Survivor 1 Size: %6zu bytes\n", memregion_size(&h->nursery.s1));
    printf("Old Generation:  %6zu / %6zu bytes (%2.0f%% full)\n",
           old_used, old_size, old_size > 0 ? (double)old_used / old_size * 100.0 : 0.0);
    printf("---------------------------------------\n");
}


void print_list_contents(Node* node) {
    if (node == g_nil) {
        printf(")");
        return;
    }
    if (node->tag == N_CONS) {
        printf(" ");
        print_node(node->v.cons.car);
        print_list_contents(node->v.cons.cdr);
    } else {
        printf(" . ");
        print_node(node);
        printf(")");
    }
}

void print_node(Node* node) {
    if (!node) {
        printf("<null-node>");
        return;
    }
    switch (node->tag) {
        case N_NIL:
            printf("NIL");
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
            printf("%lld", (long long)node->v.i);
            break;
        case N_STRING:
            printf("\"%s\"", node->v.str.ptr);
            break;
        default:
            printf("<unknown-node-tag:%d>", node->tag);
            break;
    }
}

void print_ast(Node* ast_list) {
    if (ast_list == g_nil) {
        printf("()\n");
        return;
    }

    Node* current = ast_list;
    while (current != g_nil && current->tag == N_CONS) {
        print_node(current->v.cons.car);
        printf("\n");
        current = current->v.cons.cdr;
    }
}


int main(void) {
    printf("==========================================\n");
    printf("     Generational GC and Parser Test      \n");
    printf("==========================================\n\n");

    const size_t EDEN_SIZE = 1024 * 4000;      // 4 MB
    const size_t SURVIVOR_SIZE = 1024 * 2000;  // 2 MB
    const size_t OLD_GEN_SIZE = 1024 * 2000;   // 2 MB

    Heap heap;
    if (heap_init(&heap, EDEN_SIZE, SURVIVOR_SIZE, OLD_GEN_SIZE, 0) != 0) {
        fprintf(stderr, "Failed to initialize heap.\n");
        return 1;
    }

    Gc gc;
    gc.heap = &heap;
    gc.vm_stack = vec_new(16, sizeof(Node*)); // Stack for GC roots
    if (!gc.vm_stack) {
        fprintf(stderr, "Failed to create VM stack for roots.\n");
        heap_destroy(&heap);
        return 1;
    }
    gc.collection_in_progress = false;

    SymTab symtab;
    symtab_init(&symtab);

    printf("GC Heap and Parser initialized successfully.\n");
    print_heap_stats(&gc, "Initial State");

    const char* lisp_code = lisp_file; // Original S-Expr
    printf("\nParsing LISP code in a loop to cause allocations: \"%s\"\n", lisp_code);

    Node* last_successful_ast = g_nil;

    ggc_add_root(&gc, &last_successful_ast);

    int allocations = 0;
    while (1) {
        size_t eden_free = (size_t)(heap.nursery.eden.end - heap.nursery.bump_ptr);
        if (eden_free < (EDEN_SIZE - 512)) { // Expected Happy Path
            perror("Eden space is nearly full. Stopping allocations.\n");
            break;
        }

        size_t token_count = 0;
        token* tokens = lexer(lisp_code, strlen(lisp_code), &token_count);
        if (!tokens) {
            fprintf(stderr, "Lexer failed on iteration %d\n", allocations + 1);
            break;
        }

        ast result_ast = {0};
        Parser p = {
            .st = &symtab,
            .out = &result_ast,
            .current = tokens, // Sets the lexed tokens in the parser
            .end = tokens + token_count,
            .gc = &gc
        };

        parse(&p);

        if (result_ast.has_error) {
            fprintf(stderr, "Parser error: %s\n", result_ast.error_message);
            free_tokens(tokens, token_count);
            break;
        }

        last_successful_ast = result_ast.head;

        free_tokens(tokens, token_count);
        allocations++;
    }

    printf("\nCompleted %d successful allocations.\n", allocations);
    if (last_successful_ast != g_nil) {
        printf("The last created AST is being kept alive as a root:\n");
        print_ast(last_successful_ast);
        printf("\n");
    }
    print_heap_stats(&gc, "After Allocation Loop");

    printf("\n>>> Triggering Minor GC (Nursery Collection)...\n");
    ggc_minor_collect(&gc, &symtab);
    printf("Minor GC complete.\n");

    print_heap_stats(&gc, "After Minor GC");
    printf("Note: Live objects from Eden should now be in a survivor space.\n");
    printf("The root AST should still be valid:\n");
    print_ast(last_successful_ast);
    printf("\n");

    printf("\n>>> Triggering Major GC (Old Generation Collection)...\n");
    ggc_major_collect(&gc, &symtab); // Still not sure if old unused objects 
	                                 // disapper from the heap (Not tested)
    printf("Major GC complete.\n");
    
    print_heap_stats(&gc, "After Major GC");
    printf("Note: Live symbols in the Old Gen should have been compacted.\n");


    printf("\nCleaning up all resources...\n");

    ggc_remove_root(&gc, &last_successful_ast);

    free(gc.vm_stack);

    if (symtab.data) {
        free(symtab.data);
    }

    heap_destroy(&heap);

    printf("\n==========================================\n");
    printf("               Test Complete              \n");
    printf("==========================================\n");

    return 0;
}



