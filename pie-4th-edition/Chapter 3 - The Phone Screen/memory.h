/* PROBLEM: How do you allocate memory in C? */

/* This header contains the function signitures below as well `size_t` type. */
#include <stdlib.h>

/* The trailing `_` is to avoid conflicts with definitions from `stdlib.h`. */

/* Allocates `size` bytes of uninitialized memory. Returns pointer to the
   allocated block or NULL if the allocation fails. The returned pointer have to
   be deallocated with `realloc` or `free` to avoid memory leaks. */
void* _malloc(size_t size); 

/* Allocates memory for an array of `num` objects, each of them with size
   `size`. Initializes all allocated bytes with zeroes. Returns pointer to the
   first of the newly allocated objects or NUll on failure. The returned pointer
   have to be deallocated with `realloc` or `free` to avoid memory leaks.*/
void* _calloc(size_t num, size_t size);

/* Reallocates memory pointed by `ptr` by shrinking or extending the block to
   `size` bytes if possible or allocates completely new block of `size` bytes
   and copies the old block pointed by `ptr` to it. Returns pointer to the first
   byte of the newly allocated block or NULL on failure. The returned pointer
   have to be deallocated with `realloc` or `free` to avoid memory leaks.*/
void* _realloc(void* ptr, size_t size);

/* Frees previously allocated by `malloc`, `calloc` or `realloc` memory pointed
   by `ptr`. If `ptr` is a null pointer, the function does nothing. */
void _free(void* ptr);

#include <alloca.h>

/* Non-standard extension. Returns a pointer to allocated stack memory.
   Dangerous to use because can easily cause stack overflow. */
void* alloca(size_t size);
