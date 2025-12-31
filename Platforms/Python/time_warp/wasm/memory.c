/**
 * WASM Memory Management
 * Efficient heap allocation and garbage collection for WASM
 */

#include <stdlib.h>
#include <string.h>
#include "common.h"

// Memory pool constants
#define MEMORY_POOL_SIZE (16 * 1024 * 1024)  // 16 MB
#define SMALL_BLOCK_SIZE 64
#define MEDIUM_BLOCK_SIZE 256
#define LARGE_BLOCK_SIZE 1024

// Memory allocation header
typedef struct {
  size_t size;
  int is_free;
  struct MemHeader* next;
  struct MemHeader* prev;
} MemHeader;

// Global memory state
static struct {
  uint8_t* pool;
  MemHeader* free_list;
  size_t total_allocated;
  size_t peak_allocated;
  int initialized;
} mem_state;

/**
 * Initialize memory pool
 */
void memory_init() {
  if (mem_state.initialized) return;

  mem_state.pool = (uint8_t*)malloc(MEMORY_POOL_SIZE);
  mem_state.free_list = (MemHeader*)mem_state.pool;
  mem_state.total_allocated = 0;
  mem_state.peak_allocated = 0;
  mem_state.initialized = 1;

  // Initialize free list
  mem_state.free_list->size = MEMORY_POOL_SIZE - sizeof(MemHeader);
  mem_state.free_list->is_free = 1;
  mem_state.free_list->next = NULL;
  mem_state.free_list->prev = NULL;
}

/**
 * Allocate memory
 */
void* memory_allocate(size_t size) {
  if (!mem_state.initialized) {
    memory_init();
  }

  if (size == 0) return NULL;

  size = (size + sizeof(MemHeader) + 15) & ~15; // Align to 16 bytes

  // Find free block
  MemHeader* current = mem_state.free_list;
  while (current) {
    if (current->is_free && current->size >= size) {
      // Split block if needed
      if (current->size > size + sizeof(MemHeader)) {
        MemHeader* new_block = (MemHeader*)((uint8_t*)current + size);
        new_block->size = current->size - size;
        new_block->is_free = 1;
        new_block->next = current->next;
        new_block->prev = current;

        if (current->next) {
          current->next->prev = new_block;
        }
        current->next = new_block;
      }

      current->is_free = 0;
      current->size = size;
      mem_state.total_allocated += size;

      if (mem_state.total_allocated > mem_state.peak_allocated) {
        mem_state.peak_allocated = mem_state.total_allocated;
      }

      return (void*)((uint8_t*)current + sizeof(MemHeader));
    }
    current = current->next;
  }

  return NULL; // Out of memory
}

/**
 * Deallocate memory
 */
void memory_deallocate(void* ptr) {
  if (!ptr) return;

  MemHeader* header = (MemHeader*)ptr - 1;
  header->is_free = 1;
  mem_state.total_allocated -= header->size;

  // Coalesce with next block if free
  if (header->next && header->next->is_free) {
    header->size += header->next->size;
    header->next = header->next->next;
    if (header->next) {
      header->next->prev = header;
    }
  }

  // Coalesce with previous block if free
  if (header->prev && header->prev->is_free) {
    header->prev->size += header->size;
    header->prev->next = header->next;
    if (header->next) {
      header->next->prev = header->prev;
    }
  }
}

/**
 * Reallocate memory
 */
void* memory_reallocate(void* ptr, size_t new_size) {
  if (!ptr) {
    return memory_allocate(new_size);
  }

  if (new_size == 0) {
    memory_deallocate(ptr);
    return NULL;
  }

  MemHeader* header = (MemHeader*)ptr - 1;

  // If new size fits in current block
  if (header->size >= new_size) {
    return ptr;
  }

  // Allocate new block and copy
  void* new_ptr = memory_allocate(new_size);
  if (new_ptr) {
    memcpy(new_ptr, ptr, header->size);
    memory_deallocate(ptr);
  }

  return new_ptr;
}

/**
 * Get memory statistics
 */
typedef struct {
  size_t total_allocated;
  size_t peak_allocated;
  size_t available;
} MemStats;

MemStats memory_get_stats() {
  MemStats stats = {
    mem_state.total_allocated,
    mem_state.peak_allocated,
    MEMORY_POOL_SIZE - mem_state.total_allocated
  };
  return stats;
}

/**
 * Cleanup memory
 */
void memory_cleanup() {
  if (mem_state.initialized && mem_state.pool) {
    free(mem_state.pool);
    mem_state.pool = NULL;
    mem_state.free_list = NULL;
    mem_state.initialized = 0;
  }
}

/**
 * String utilities
 */

char* string_create(const char* str) {
  if (!str) return NULL;
  size_t len = strlen(str) + 1;
  char* new_str = (char*)memory_allocate(len);
  if (new_str) {
    strcpy(new_str, str);
  }
  return new_str;
}

char* string_concat(const char* s1, const char* s2) {
  if (!s1 || !s2) return NULL;
  size_t len = strlen(s1) + strlen(s2) + 1;
  char* result = (char*)memory_allocate(len);
  if (result) {
    strcpy(result, s1);
    strcat(result, s2);
  }
  return result;
}

char* string_substring(const char* str, int start, int length) {
  if (!str || start < 0 || length <= 0) return NULL;
  char* result = (char*)memory_allocate(length + 1);
  if (result) {
    strncpy(result, str + start, length);
    result[length] = '\0';
  }
  return result;
}

int string_index(const char* haystack, const char* needle) {
  if (!haystack || !needle) return -1;
  for (int i = 0; haystack[i]; i++) {
    if (strncmp(&haystack[i], needle, strlen(needle)) == 0) {
      return i;
    }
  }
  return -1;
}

/**
 * Array utilities
 */

typedef struct {
  void** elements;
  size_t size;
  size_t capacity;
  size_t element_size;
} Array;

Array* array_create(size_t initial_capacity, size_t element_size) {
  Array* arr = (Array*)memory_allocate(sizeof(Array));
  if (!arr) return NULL;

  arr->elements = (void**)memory_allocate(initial_capacity * sizeof(void*));
  if (!arr->elements) {
    memory_deallocate(arr);
    return NULL;
  }

  arr->size = 0;
  arr->capacity = initial_capacity;
  arr->element_size = element_size;
  return arr;
}

void array_push(Array* arr, void* element) {
  if (!arr) return;

  if (arr->size >= arr->capacity) {
    arr->capacity *= 2;
    arr->elements = (void**)memory_reallocate(
      arr->elements,
      arr->capacity * sizeof(void*)
    );
  }

  arr->elements[arr->size++] = element;
}

void* array_get(Array* arr, size_t index) {
  if (!arr || index >= arr->size) return NULL;
  return arr->elements[index];
}

void array_set(Array* arr, size_t index, void* element) {
  if (!arr || index >= arr->size) return;
  arr->elements[index] = element;
}

size_t array_size(Array* arr) {
  return arr ? arr->size : 0;
}

void array_destroy(Array* arr) {
  if (arr) {
    if (arr->elements) {
      memory_deallocate(arr->elements);
    }
    memory_deallocate(arr);
  }
}
