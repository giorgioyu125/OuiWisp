/*
 * @file ggc.c (Generational Garbage Collector)
 * @brief Implementazione completa di un GGC con integrazione per un parser LISP.
*/

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>

#include "parser.h"
#include "ggc.h"

/* -------------- CONFIGURATION ------------- */ 

#define GGC_ZERO_MEMORY 1

/* --------------     STACK     ------------- */

/**
 * @brief This is the fondamental struct for the stack 
 *		  of the ggc
 * @param initial_cap is the starting size of the vec 
 * @param elem_size is the size of *every* single element 
 *		  and i think i will use only 8byte pointers.
 */
Vec* vec_new(size_t initial_cap, size_t elem_size) {
    size_t total_size = sizeof(Vec) + initial_cap * elem_size;
    Vec* result = (Vec*)malloc(total_size);
    if (!result) return NULL;
    result->data = (char*)result + sizeof(Vec);
    result->max_cap = total_size;
    result->elem_count = 0;
    result->elem_size = elem_size;
    return result;
}

/**
 * @brief Appends to end of the vec and doesent need much documentation, 
 *		  is very very basic.
 */
void vec_add(Vec** v_ptr, void* value) {
    Vec* v = *v_ptr;
    if (sizeof(Vec) + (v->elem_count + 1) * v->elem_size > v->max_cap) {
        size_t new_cap = v->max_cap * 2;
        Vec* new_v = (Vec*)realloc(v, new_cap);
        if (!new_v) { perror("realloc fallita"); return; }
        *v_ptr = new_v;
        v = new_v;
        v->max_cap = new_cap;
        v->data = (char*)v + sizeof(Vec);
    }
    memcpy((char*)v->data + v->elem_count * v->elem_size,
		   value,
		   v->elem_size);
    v->elem_count++;
}

/**
 * @brief Searches for and removes the first occurrence of a value from a vector.
 * @param v_ptr A pointer to a pointer to the vector. The double pointer signature
 *              is used to maintain consistency with the `vec_add` API,
 *              even though shrinking reallocation is not implemented here.
 * @param value A pointer to the value to search for and remove. The function
 *              will compare the memory contents, not the pointers.
 */
int vec_del(Vec** v_ptr, void* value) {
    if (!v_ptr || !*v_ptr || (*v_ptr)->elem_count == 0 || !value) {
        return -1;
    }

    Vec* v = *v_ptr;
    char* data_ptr = (char*)v->data;
    size_t elem_size = v->elem_size;

    for (size_t i = 0; i < v->elem_count; ++i) {
        if (memcmp(data_ptr + i * elem_size, value, elem_size) == 0) {
            if (i < v->elem_count - 1) {
                memmove(
                    data_ptr + i * elem_size,
                    data_ptr + (i + 1) * elem_size,
                    (v->elem_count - i - 1) * elem_size
                );
            }

            v->elem_count--;
            break;
        }
    }

	return 0;
}


/**
 * @brief Removes a value in particular index.
 * @param Pointer to the pointer of v_ptr, 
 *		  to allow in-place modifications.
 * @param The Index that needs to be remove.
 */
int vec_rem(Vec** v_ptr, size_t index) {
    if (!v_ptr || !*v_ptr || index >= (*v_ptr)->elem_count) {
        return -1;
    }

    Vec* v = *v_ptr;
    char* data_ptr = (char*)v->data;
    size_t elem_size = v->elem_size;
    size_t elem_num = v->elem_count;

    memmove(data_ptr + index * elem_size,
            data_ptr + elem_size * (index + 1), 
            (elem_num - index - 1) * elem_size);

    v->elem_count--;
    return 0;
}


/**
 * @brief Initializes a new GGC heap.
 * @details Allocates a single large memory block and logically divides it
 *          among the Eden, survivor spaces, and the Old Generation.
 *
 * @param h Pointer to the Heap to be initialized.
 * @param eden_bytes The desired size for the Eden space.
 * @param survivor_bytes The size for *each* of the two survivor spaces.
 * @param old_bytes The size for the Old Generation.
 * @param align The required alignment for the start of the heap (e.g., 16).
 * @return `true` on success, `false` if memory could not be allocated.
 *
 * @note the define down below is the default as intended.
 */
#define DEFAULT_ALIGN 16
int heap_init(Heap* h, size_t eden_bytes, size_t survivor_bytes, size_t old_bytes, size_t align) {
    if (!h) return -1;

    if (align == 0) align = DEFAULT_ALIGN;
    if ((align & (align - 1)) != 0) align = DEFAULT_ALIGN;

    size_t total_size = eden_bytes + (2 * survivor_bytes) + old_bytes + align;

    void* raw = malloc(total_size);
    if (!raw) return -1;

    h->heap_memory_block = raw;
    h->heap_memory_size  = total_size;

    uintptr_t mask = (uintptr_t)(align - 1);
    char* p = (char*)(((uintptr_t)raw + mask) & ~mask);

    /* Nursery - Eden */
    h->nursery.eden.start = p;
    h->nursery.eden.end   = p + eden_bytes;  p += eden_bytes;

    /* Nursery - Survivor 0 */
    h->nursery.s0.start = p;
    h->nursery.s0.end   = p + survivor_bytes; p += survivor_bytes;

    /* Nursery - Survivor 1 */
    h->nursery.s1.start = p;
    h->nursery.s1.end   = p + survivor_bytes; p += survivor_bytes;

    /* Old Generation */
    h->old_gen.region.start = p;
    h->old_gen.region.end   = p + old_bytes;

    /* bump pointers e stato iniziale */
    h->nursery.bump_ptr     = h->nursery.eden.start;
    h->nursery.to_space_is_s0 = true;
    h->old_gen.bump_ptr     = h->old_gen.region.start;

    return 0;
}

void heap_destroy(Heap* h) {
    if (!h) return;

    if (h->heap_memory_block) {
        free(h->heap_memory_block);
    }

    h->heap_memory_block = NULL;
    h->heap_memory_size  = 0;

    h->nursery.eden.start = h->nursery.eden.end = NULL;
    h->nursery.s0.start   = h->nursery.s0.end   = NULL;
    h->nursery.s1.start   = h->nursery.s1.end   = NULL;
    h->nursery.bump_ptr   = NULL;
    h->nursery.to_space_is_s0 = false;

    h->old_gen.region.start = h->old_gen.region.end = NULL;
    h->old_gen.bump_ptr     = NULL;
}

// --- GC core functions ---

void* ggc_alloc_old(OldGen* old_gen,  size_t size) {
    if (old_gen->bump_ptr + size <= old_gen->region.end) {
        void* result = old_gen->bump_ptr;
        old_gen->bump_ptr += size;
        return result;
    }
    
    return NULL;
}


/**
 * @brief Checks if a pointer points to an object in the nursery.
 * @param gc Garbage collector context
 * @param ptr Pointer to check
 * @return true if pointer is in nursery (Eden or survivor spaces)
 */
bool is_in_nursery(Gc* gc, void* ptr) {
    if (!ptr) return false;
    
    Nursery* n = &gc->heap->nursery;
    char* p = (char*)ptr;
    
    bool in_eden = (p >= n->eden.start && p < n->eden.end);
    bool in_s0 = (p >= n->s0.start && p < n->s0.end);
    bool in_s1 = (p >= n->s1.start && p < n->s1.end);
    
    return in_eden || in_s0 || in_s1;
}

/**
 * @brief Registers a root pointer with the garbage collector.
 * @param gc Garbage collector context
 * @param root Pointer to the root object pointer
 */
void ggc_add_root(Gc* gc, Node** root) {
    if (!gc || !gc->vm_stack || !root) return;
    vec_add(&gc->vm_stack, &root);
}

/**
 * @brief Unregisters a root pointer from the garbage collector.
 * @param gc Garbage collector context  
 * @param root Pointer to the root object pointer
 */

void ggc_remove_root(Gc* gc, void* root_void) {
    if (!gc || !gc->vm_stack || !root_void) return;
    Node** root = (Node**)root_void;
    vec_del(&gc->vm_stack, root);
}


/**
 * @brief Determines the size of an object in bytes.
 * @details Calculates the memory footprint of an object based on its type.
 *          Includes proper alignment for the target architecture.
 * 
 * @param obj Object to measure (must not be NULL)
 * @return Size of the object in bytes, aligned to 8-byte boundary
 */
static size_t get_object_size(Node* obj) {
    if (!obj) return 0;
    
    size_t base_size;
    
    switch (obj->tag) {
        case N_CONS:
        case N_SYMBOL:
        case N_INT:
        case N_NIL:
            base_size = sizeof(Node);
            break;
            
        case N_STRING:
            base_size = sizeof(Node) + obj->v.str.len + 1;
            break;
            
        default:
            base_size = sizeof(Node);
            break;
    }
    
    return (base_size + 7) & ~7;
}


/**
 * @brief Copies an object from the nursery to the survivor space.
 * @details Implements object copying with forwarding pointer support.
 *          If the object has already been copied, returns the forwarding address.
 * 
 * @param gc Garbage collector context
 * @param obj Object to copy (may be NULL or g_nil)
 * @param to_ptr Pointer to current allocation position in to-space
 * @param to_end End of to-space region
 * @return Pointer to the copied object, or NULL if copy failed
 */
static Node* copy_object_to_survivor(Gc* gc, Node* obj, char** to_ptr, char* to_end) {
    if (!obj || obj == g_nil) {
        return obj;
    }
    
    Nursery* nursery = &gc->heap->nursery;
    bool in_eden = (char*)obj >= nursery->eden.start && (char*)obj < nursery->eden.end;
	bool in_s0 = (char*)obj >= nursery->s0.start && (char*)obj < nursery->s0.end;
	bool in_s1 = (char*)obj >= nursery->s1.start && (char*)obj < nursery->s1.end;
	bool in_survivor = in_s0 || in_s1;
    
    if (!in_eden && !in_survivor) {
        return obj;
    }
    
    if (obj->tag == FORWARDED_TAG) {
        return (Node*)obj->v.cons.car;
    }
    
    size_t obj_size = get_object_size(obj);
    
    if (*to_ptr + obj_size > to_end) {
        return NULL;
    }
    
    Node* new_obj = (Node*)*to_ptr;
    memcpy(new_obj, obj, obj_size);
    *to_ptr += obj_size;
    
    NodeTag original_tag = obj->tag;
    obj->tag = FORWARDED_TAG;
    obj->v.cons.car = new_obj;
    
    new_obj->tag = original_tag;
    
    return new_obj;
}

/**
 * @brief Scans an object's children and copies them if necessary.
 * @details Examines all pointer fields in an object and recursively copies
 *          any child objects that need to be moved to the survivor space.
 * 
 * @param gc Garbage collector context
 * @param obj Object to scan (assumed to be in to-space)
 * @param to_ptr Current allocation pointer in to-space
 * @param to_end End boundary of to-space
 */
static void scan_and_copy_children(Gc* gc, Node* obj, char** to_ptr, char* to_end) {
    if (!obj || obj == g_nil) {
        return;
    }
    
    switch (obj->tag) {
        case N_CONS:
            obj->v.cons.car = copy_object_to_survivor(gc, obj->v.cons.car, to_ptr, to_end);
            obj->v.cons.cdr = copy_object_to_survivor(gc, obj->v.cons.cdr, to_ptr, to_end);
            break;
            
        case N_SYMBOL:
            break;
            
        case N_STRING:
            break;
            
        case N_INT:
        case N_NIL:
        default:
            break;
    }
}

/**
 * @brief Performs a major garbage collection on the old generation.
 * @details Implements a robust and simple mark-and-compact algorithm.
 *          1. MARK: Finds all live symbols by scanning all root sources: the
 *             symbol table, the VM stack, and the nursery. Live symbols are
 *             marked by setting their GC color to BLACK.
 *          2. COMPACT: Live symbols are moved to the beginning of the old gen heap.
 *             This involves three sub-steps:
 *             a. Calculate new locations (forwarding addresses).
 *             b. Update ALL pointers (in symtab, vm_stack, nursery) to point
 *                to the new locations.
 *             c. Physically move the symbol data.
 *          3. CLEANUP: The old generation's allocation pointer is updated.
 *
 * @param gc     Pointer to the garbage collector context.
 * @param symtab Pointer to the symbol table, used as a primary source of roots.
 */
void ggc_major_collect(Gc* gc, SymTab* symtab) {
    if (!gc || !gc->heap || !symtab || gc->collection_in_progress) {
        return;
    }
    gc->collection_in_progress = true;

    OldGen* old_gen = &gc->heap->old_gen;
    Nursery* nursery = &gc->heap->nursery;

    char* current = old_gen->region.start;
    while (current < old_gen->bump_ptr) {
        Symbol* sym = (Symbol*)current;
        uint8_t gen = gcinfo_gen(sym->gcinfo);
        uint8_t age = gcinfo_age(sym->gcinfo);
        sym->gcinfo = gcinfo_pack(gen, age, GC_COLOR_WHITE);
        current += sizeof(Symbol) + sym->len + 1;
    }

    for (size_t i = 0; i < symtab->len; i++) {
        Symbol* sym = symtab->data[i];
        if (sym) {
            uint8_t gen = gcinfo_gen(sym->gcinfo);
            uint8_t age = gcinfo_age(sym->gcinfo);
            sym->gcinfo = gcinfo_pack(gen, age, GC_COLOR_BLACK);
        }
    }

    if (gc->vm_stack && gc->vm_stack->data) {
        for (size_t i = 0; i < gc->vm_stack->elem_count; i++) {
            Node* node = *(Node**)((char*)gc->vm_stack->data + i * sizeof(Node*));
            if (node && node->tag == N_SYMBOL && node->v.sym) {
                Symbol* sym = node->v.sym;
                uint8_t gen = gcinfo_gen(sym->gcinfo);
                uint8_t age = gcinfo_age(sym->gcinfo);
                sym->gcinfo = gcinfo_pack(gen, age, GC_COLOR_BLACK);
            }
        }
    }
    
    void* nursery_regions[] = { nursery->eden.start, nursery->s0.start, nursery->s1.start };
    size_t nursery_sizes[] = {
        (size_t)(nursery->bump_ptr - nursery->eden.start),
        memregion_size(&nursery->s0),
        memregion_size(&nursery->s1)
    };
    for (int i = 0; i < 3; i++) {
        char* scan_ptr = (char*)nursery_regions[i];
        char* end_ptr = scan_ptr + nursery_sizes[i];
        while (scan_ptr < end_ptr) {
            Node* obj = (Node*)scan_ptr;
            if (obj->tag == N_SYMBOL && obj->v.sym && (char*)obj->v.sym >= old_gen->region.start) {
                Symbol* sym = obj->v.sym;
                uint8_t gen = gcinfo_gen(sym->gcinfo);
                uint8_t age = gcinfo_age(sym->gcinfo);
                sym->gcinfo = gcinfo_pack(gen, age, GC_COLOR_BLACK);
            }
            scan_ptr += get_object_size(obj);
        }
    }

    char* new_location = old_gen->region.start;
    current = old_gen->region.start;
    while (current < old_gen->bump_ptr) {
        Symbol* sym = (Symbol*)current;
        if (gcinfo_color(sym->gcinfo) == GC_COLOR_BLACK) { // Se è vivo...
            sym->value_ptr = new_location;
            new_location += sizeof(Symbol) + sym->len + 1;
        }
        current += sizeof(Symbol) + sym->len + 1;
    }

    for (size_t i = 0; i < symtab->len; i++) {
        Symbol* sym = symtab->data[i];
        if (sym && gcinfo_color(sym->gcinfo) == GC_COLOR_BLACK) {
            symtab->data[i] = (Symbol*)sym->value_ptr;
        }
    }

    if (gc->vm_stack && gc->vm_stack->data) {
        for (size_t i = 0; i < gc->vm_stack->elem_count; i++) {
            Node* node = *(Node**)((char*)gc->vm_stack->data + i * sizeof(Node*));
            if (node && node->tag == N_SYMBOL && node->v.sym && gcinfo_color(node->v.sym->gcinfo) == GC_COLOR_BLACK) {
                node->v.sym = (Symbol*)node->v.sym->value_ptr;
            }
        }
    }

    for (int i = 0; i < 3; i++) {
        char* scan_ptr = (char*)nursery_regions[i];
        char* end_ptr = scan_ptr + nursery_sizes[i];
        while (scan_ptr < end_ptr) {
            Node* obj = (Node*)scan_ptr;
            if (obj->tag == N_SYMBOL && obj->v.sym && gcinfo_color(obj->v.sym->gcinfo) == GC_COLOR_BLACK) {
                obj->v.sym = (Symbol*)obj->v.sym->value_ptr;
            }
            scan_ptr += get_object_size(obj);
        }
    }

    current = old_gen->region.start;
    while (current < old_gen->bump_ptr) {
        Symbol* sym = (Symbol*)current;
        size_t total_size = sizeof(Symbol) + sym->len + 1;
        if (gcinfo_color(sym->gcinfo) == GC_COLOR_BLACK) {
            Symbol* new_addr = (Symbol*)sym->value_ptr;
            if (new_addr != sym) { // Sposta solo se l'indirizzo cambia
                memmove(new_addr, sym, total_size);
            }
            new_addr->name = (char*)new_addr + sizeof(Symbol);
            new_addr->value_ptr = NULL; 
        }
        current += total_size;
    }

    old_gen->bump_ptr = new_location;
    gc->collection_in_progress = false;
}

/**
 * @brief Performs a minor garbage collection on the nursery generation.
 * @details Implements a copying garbage collector using Cheney's algorithm.
 *          Collects garbage from Eden space and the from-space survivor,
 *          copying live objects to the to-space survivor. Objects that have
 *          survived multiple collections are promoted to the old generation.
 * 
 * @param gc Pointer to the garbage collector context
 * 
 * @note This function assumes that all roots are registered in gc->vm_stack.
 *       Objects in the old generation are not collected during minor GC.
 *       The function swaps survivor spaces after collection.
 * 
 * @warning This function may modify object memory layout and invalidate
 *          pointers. All references should go through registered roots.
 */
void ggc_minor_collect(Gc* gc, SymTab* symtab) {
    if (!gc || !gc->heap || gc->collection_in_progress) {
        return;
    }
    
    gc->collection_in_progress = true;
    Nursery* nursery = &gc->heap->nursery;
    
    MemRegion* to_space = nursery->to_space_is_s0 ? &nursery->s0 : &nursery->s1;
    
    char* to_ptr = to_space->start;
    char* scan_ptr = to_space->start;
    
    if (gc->vm_stack && gc->vm_stack->data) {
        for (size_t i = 0; i < gc->vm_stack->elem_count; i++) {
            Node** root_ptr = (Node**)((char*)gc->vm_stack->data + i * gc->vm_stack->elem_size);
            *root_ptr = copy_object_to_survivor(gc, *root_ptr, &to_ptr, to_space->end);
        }
    }
    
    while (scan_ptr < to_ptr) {
        Node* obj = (Node*)scan_ptr;
        
        scan_and_copy_children(gc, obj, &to_ptr, to_space->end);
        
        scan_ptr += get_object_size(obj);
    }
    
    if (to_ptr > to_space->end) {
        gc->collection_in_progress = false;
        ggc_major_collect(gc, symtab);
        return;
    }
    
    nursery->bump_ptr = nursery->eden.start;
    nursery->to_space_is_s0 = !nursery->to_space_is_s0;
    
    gc->collection_in_progress = false;
}


/* Main API */
void* ggc_alloc(Gc* gc, SymTab* symtab, size_t size) {
    size = (size + 7) & ~7;
    
    Nursery* nursery = &gc->heap->nursery;
    
    if (nursery->bump_ptr + size > nursery->eden.end) {
        ggc_minor_collect(gc, symtab);
        
        if (nursery->bump_ptr + size > nursery->eden.end) {
            return NULL;
        }
    }
    
    void* result = nursery->bump_ptr;
    nursery->bump_ptr += size;
    
    #ifdef GGC_ZERO_MEMORY
        memset(result, 0, size);
    #endif
    
    return result;
}
