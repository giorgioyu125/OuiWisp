#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "ggc.h"
#include "parser.h"
#include "lexer.h"

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


int main(void) {
    printf("==========================================\n");
    printf("     Generational GC and Parser Test      \n");
    printf("==========================================\n\n");

    // --- 1. SETUP ---
    // Initialize the core GC and Parser components.

    // Define heap sizes. We use small sizes to easily trigger GC.
    const size_t EDEN_SIZE = 1024 * 4;      // 4 KB
    const size_t SURVIVOR_SIZE = 1024 * 2;  // 2 KB
    const size_t OLD_GEN_SIZE = 1024 * 8;   // 8 KB

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

    // --- 2. ALLOCATION PHASE ---
    // We will parse a string repeatedly to create objects (Nodes, Symbols)
    // and fill up the Eden space.
    
    const char* lisp_code = "(Define MyVar (Add One Two))";
    printf("\nParsing LISP code in a loop to cause allocations: \"%s\"\n", lisp_code);

    Node* last_successful_ast = g_nil; // Keep one AST alive as a root

    // Add our root variable to the GC's root set
    ggc_add_root(&gc, &last_successful_ast);

    int allocations = 0;
    while (1) {
        size_t eden_free = (size_t)(heap.nursery.eden.end - heap.nursery.bump_ptr);
        // Stop when Eden is nearly full
        if (eden_free < 512) {
            printf("Eden space is nearly full. Stopping allocations.\n");
            break;
        }

        // Lex the source code
        size_t token_count = 0;
        token* tokens = lexer(lisp_code, strlen(lisp_code), &token_count);
        if (!tokens) {
            fprintf(stderr, "Lexer failed on iteration %d\n", allocations + 1);
            break;
        }

        // Setup the parser
        ast result_ast = {0};
        Parser p = {
            .st = &symtab,
            .out = &result_ast,
            .current = tokens,
            .end = tokens + token_count,
            .gc = &gc
        };

        // Parse the code, which will allocate nodes via ggc_alloc
        parse(&p);

        if (result_ast.has_error) {
            fprintf(stderr, "Parser error: %s\n", result_ast.error_message);
            free_tokens(tokens, token_count);
            break;
        }

        // Update our root pointer to the latest parsed AST.
        // The old one is now garbage (unless referenced elsewhere).
        last_successful_ast = result_ast.head;

        free_tokens(tokens, token_count); // Clean up lexer resources
        allocations++;
    }

    printf("\nCompleted %d successful allocations.\n", allocations);
    if (last_successful_ast != g_nil) {
        printf("The last created AST is being kept alive as a root:\n");
        print_ast(last_successful_ast);
        printf("\n");
    }
    print_heap_stats(&gc, "After Allocation Loop");

    // --- 3. MINOR GC PHASE ---
    printf("\n>>> Triggering Minor GC (Nursery Collection)...\n");
    ggc_minor_collect(&gc, &symtab);
    printf("Minor GC complete.\n");

    print_heap_stats(&gc, "After Minor GC");
    printf("Note: Live objects from Eden should now be in a survivor space.\n");
    printf("The root AST should still be valid:\n");
    print_ast(last_successful_ast);
    printf("\n");

    // --- 4. MAJOR GC PHASE ---
    printf("\n>>> Triggering Major GC (Old Generation Collection)...\n");
    // This will mainly compact symbols in the Old Gen.
    ggc_major_collect(&gc, &symtab);
    printf("Major GC complete.\n");
    
    print_heap_stats(&gc, "After Major GC");
    printf("Note: Live symbols in the Old Gen should have been compacted.\n");


    // --- 5. CLEANUP ---
    printf("\nCleaning up all resources...\n");

    // Remove the root before destroying the stack
    ggc_remove_root(&gc, &last_successful_ast);

    // Free the root stack vector
    free(gc.vm_stack);

    // Free the symbol table's internal data array.
    // NOTE: We DO NOT free the individual symbols. They live on the GC heap,
    // which will be freed all at once by heap_destroy.
    // The provided `free_symtab` function is incorrect for a GC environment
    // as it calls `free()` on memory managed by the GC.
    if (symtab.data) {
        free(symtab.data);
    }

    // Finally, destroy the entire heap, freeing the large memory block.
    heap_destroy(&heap);

    printf("\n==========================================\n");
    printf("               Test Complete              \n");
    printf("==========================================\n");

    return 0;
}


// --- Helper functions for printing the AST ---

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
