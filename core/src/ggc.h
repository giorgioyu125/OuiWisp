/**
 * @file ggc.h
 * @brief Generational Garbage Collector for LISP DSL
 * @details Implements a copying GC for nursery and mark-compact for old generation
 */
#ifndef GGC_H
#define GGC_H

#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <stdbool.h>

typedef enum NodeTag NodeTag;
typedef struct Node Node;
typedef struct SymTab SymTab;
typedef enum gc_colors gc_colors;


/* -------------------- Constants and Configuration -------------------- */

#define DEFAULT_ALIGN 16
#define FORWARDED_TAG 255

/* -------------------- Memory Management Structures -------------------- */

/**
 * @brief Represents a contiguous memory region.
 */
typedef struct MemRegion {
    char* start;    ///< Start of the memory region
    char* end;      ///< End of the memory region (exclusive)
} MemRegion;

/**
 * @brief Nursery generation with Eden and two survivor spaces.
 * @details Implements a copying garbage collector using semi-space algorithm.
 */
typedef struct Nursery {
    MemRegion eden;    ///< Eden space for new allocations
    MemRegion s0;      ///< Survivor space 0
    MemRegion s1;      ///< Survivor space 1
    
    bool to_space_is_s0;  ///< true => s0 is to-space, s1 is from-space
    char* bump_ptr;       ///< Current allocation pointer (Eden during mutation, to-space during GC)
} Nursery;

/**
 * @brief Old generation using bump-pointer allocation.
 * @details Long-lived objects promoted from nursery.
 */
typedef struct OldGen {
    MemRegion region;   ///< Memory region for old generation
    char* bump_ptr;     ///< Current allocation pointer
} OldGen;

/**
 * @brief Complete heap containing all memory spaces.
 */
typedef struct Heap {
    Nursery nursery;    ///< Young generation
    OldGen old_gen;     ///< Old generation
    
    void* heap_memory_block;   ///< Underlying memory block (from malloc)
    size_t heap_memory_size;   ///< Total size of memory block
} Heap;

/**
 * @brief Dynamic vector for storing GC roots.
 */
typedef struct Vec {
    void* data;         ///< Pointer to data array
    size_t elem_size;   ///< Size of each element in bytes
    size_t elem_count;  ///< Number of elements currently stored
    size_t max_cap;     ///< Maximum capacity in bytes
} Vec;

/**
 * @brief Main garbage collector context.
 */
typedef struct Gc {
    Heap* heap;         ///< Managed heap
    Vec* vm_stack;      ///< Root stack for GC scanning
    
    void* native_stack_base;   ///< Optional: base of native stack for conservative scanning
    
    /* Thresholds and heuristics */
    size_t nursery_alloc_threshold;    ///< Threshold to trigger minor GC
    size_t old_gen_alloc_threshold;    ///< Threshold to trigger major GC
    
    uint8_t promotion_age_threshold;   ///< Age threshold for promotion to old gen
    bool collection_in_progress;       ///< Flag to prevent recursive collection
} Gc;


/* -------------------- Vector Functions -------------------- */

/**
 * @brief Creates a new dynamic vector.
 * @param initial_cap Initial capacity in number of elements
 * @param elem_size Size of each element in bytes
 * @return Pointer to new vector, or NULL on failure
 */
Vec* vec_new(size_t initial_cap, size_t elem_size);

/**
 * @brief Adds an element to the vector.
 * @param v_ptr Pointer to vector pointer (may be reallocated)
 * @param value Pointer to value to add
 */
void vec_add(Vec** v_ptr, void* value);

/**
 * @brief Removes the first occurrence of a value from the vector.
 * @param v_ptr Pointer to vector pointer
 * @param value Pointer to value to remove
 * @return 0 on success, -1 on error
 */
int vec_del(Vec** v_ptr, void* value);

/**
 * @brief Removes element at specified index.
 * @param v_ptr Pointer to vector pointer
 * @param index Index of element to remove
 * @return 0 on success, -1 on error
 */
int vec_rem(Vec** v_ptr, size_t index);

/* -------------------- Heap Management -------------------- */

/**
 * @brief Initializes a new GC heap.
 * @param h Pointer to heap structure to initialize
 * @param eden_bytes Size of Eden space
 * @param survivor_bytes Size of each survivor space
 * @param old_bytes Size of old generation
 * @param align Memory alignment (0 = use default)
 * @return 0 on success, -1 on failure
 */
int heap_init(Heap* h, size_t eden_bytes, size_t survivor_bytes, size_t old_bytes, size_t align);

/**
 * @brief Destroys a heap and frees all memory.
 * @param h Pointer to heap to destroy
 */
void heap_destroy(Heap* h);

/** 
 * @brief This functions tells if somethig is in the nursery or not.
 *		  Is especially useful if you dont want to use the symbol table for 
 *		  this kind of lookups.
 */
bool is_in_nursery(Gc* gc, void* ptr);


/* -------------------- Garbage Collection Functions -------------------- */

/**
 * @brief Performs a minor garbage collection on the nursery.
 * @param gc Garbage collector context
 * @param The symtab positioned in the parser
 */
void ggc_minor_collect(Gc* gc, SymTab* symtab);

/**
 * @brief Performs a major garbage collection on the old generation.
 * @param gc Garbage collector context
 * @param symtab of the evaluator
 */
void ggc_major_collect(Gc* gc, SymTab* symtab);


/* -------------------- Allocation Functions -------------------- */

/**
 * @brief Main allocation function for nursery objects.
 * @param gc Garbage collector context
 * @param size Size of object to allocate
 * @param type Node type (for future optimizations)
 * @return Pointer to allocated memory, or NULL on failure
 */
void* ggc_alloc(Gc* gc, SymTab* symtab, size_t size);

/**
 * @brief Allocation function for old generation objects.
 * @param old_gen Old generation context
 * @param size Size of object to allocate
 * @return Pointer to allocated memory, or NULL on failure
 */
void* ggc_alloc_old(OldGen* old_gen, size_t size);


/* -------------------- Root Management -------------------- */

/**
 * @brief Registers a root pointer with the garbage collector.
 * @param gc Garbage collector context
 * @param root Pointer to the root object pointer
 */
void ggc_add_root(Gc* gc, Node** root);

/**
 * @brief Unregisters a root pointer from the garbage collector.
 * @param gc Garbage collector context
 * @param root Pointer to the root object pointer
 */
void ggc_remove_root(Gc* gc, void* root_void);


/* -------------------- Utility Functions -------------------- */

/**
 * @brief Aligns a pointer up to the specified alignment.
 * @param p Pointer to align
 * @param a Alignment (must be power of 2)
 * @return Aligned pointer
 */
static inline char* align_up_ptr(char* p, size_t a) {
    uintptr_t x = (uintptr_t)p;
    return (char*)((x + (a - 1)) & ~(a - 1));
}

/**
 * @brief Sets up a memory region.
 * @param r Pointer to memory region
 * @param base Base address
 * @param size Size in bytes
 */
static inline void memregion_set(MemRegion* r, char* base, size_t size) {
    r->start = base;
    r->end = base + size;
}

/**
 * @brief Gets the size of a memory region.
 * @param r Pointer to memory region
 * @return Size in bytes
 */
static inline size_t memregion_size(const MemRegion* r) {
    return (size_t)(r->end - r->start);
}

#endif /* GGC_H */
