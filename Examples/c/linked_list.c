/*=================================================================
 * linked_list.c  — Doubly Linked List with Full CRUD Operations
 * Demonstrates: structs, pointers, dynamic memory, typedef,
 * function pointers, generic void*, malloc/free, and sorting.
 *=================================================================*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

/* ──────────────────────────────────────────────────────────────
 *  NODE AND LIST TYPES
 * ──────────────────────────────────────────────────────────────*/

typedef struct Node {
    int          val;
    struct Node *prev;
    struct Node *next;
} Node;

typedef struct {
    Node  *head;
    Node  *tail;
    size_t size;
} DoublyLinkedList;

/* ──────────────────────────────────────────────────────────────
 *  LIFECYCLE
 * ──────────────────────────────────────────────────────────────*/

/** Allocate and initialise an empty list. Caller must call dll_free(). */
DoublyLinkedList *dll_create(void) {
    DoublyLinkedList *list = calloc(1, sizeof *list);
    if (!list) { perror("dll_create"); exit(EXIT_FAILURE); }
    return list;
}

/** Create a single node. Returns NULL on allocation failure. */
static Node *make_node(int val) {
    Node *n = calloc(1, sizeof *n);
    if (!n) { perror("make_node"); return NULL; }
    n->val = val;
    return n;
}

/** Free all nodes and the list itself. */
void dll_free(DoublyLinkedList *list) {
    if (!list) return;
    Node *cur = list->head;
    while (cur) {
        Node *next = cur->next;
        free(cur);
        cur = next;
    }
    free(list);
}

/* ──────────────────────────────────────────────────────────────
 *  INSERTION
 * ──────────────────────────────────────────────────────────────*/

/** Prepend value at front. O(1). */
void dll_push_front(DoublyLinkedList *list, int val) {
    Node *n = make_node(val);
    n->next = list->head;
    if (list->head) list->head->prev = n;
    list->head = n;
    if (!list->tail) list->tail = n;
    list->size++;
}

/** Append value at back. O(1). */
void dll_push_back(DoublyLinkedList *list, int val) {
    Node *n = make_node(val);
    n->prev = list->tail;
    if (list->tail) list->tail->next = n;
    list->tail = n;
    if (!list->head) list->head = n;
    list->size++;
}

/** Insert after the k-th node (0-indexed). O(k). */
int dll_insert_after(DoublyLinkedList *list, size_t k, int val) {
    if (k >= list->size) return -1;  /* out of range */
    Node *cur = list->head;
    for (size_t i = 0; i < k; i++) cur = cur->next;
    Node *n = make_node(val);
    n->next = cur->next;
    n->prev = cur;
    if (cur->next) cur->next->prev = n;
    else           list->tail = n;
    cur->next = n;
    list->size++;
    return 0;
}

/** Insert in sorted order (ascending). O(n). */
void dll_insert_sorted(DoublyLinkedList *list, int val) {
    Node *cur = list->head;
    while (cur && cur->val < val) cur = cur->next;
    if (!cur) {
        dll_push_back(list, val);
    } else if (!cur->prev) {
        dll_push_front(list, val);
    } else {
        Node *n = make_node(val);
        n->next = cur;
        n->prev = cur->prev;
        cur->prev->next = n;
        cur->prev = n;
        list->size++;
    }
}

/* ──────────────────────────────────────────────────────────────
 *  DELETION
 * ──────────────────────────────────────────────────────────────*/

/** Remove and return front value. Returns INT_MIN if empty. */
int dll_pop_front(DoublyLinkedList *list) {
    if (!list->head) return ~0U >> 1;  /* INT_MAX sentinel */
    Node *n   = list->head;
    int   val = n->val;
    list->head = n->next;
    if (list->head) list->head->prev = NULL;
    else            list->tail = NULL;
    free(n);
    list->size--;
    return val;
}

/** Remove and return back value. */
int dll_pop_back(DoublyLinkedList *list) {
    if (!list->tail) return ~0U >> 1;
    Node *n   = list->tail;
    int   val = n->val;
    list->tail = n->prev;
    if (list->tail) list->tail->next = NULL;
    else            list->head = NULL;
    free(n);
    list->size--;
    return val;
}

/** Delete first occurrence of value. Returns 0 on success, -1 if not found. */
int dll_delete_val(DoublyLinkedList *list, int val) {
    Node *cur = list->head;
    while (cur) {
        if (cur->val == val) {
            if (cur->prev) cur->prev->next = cur->next;
            else           list->head = cur->next;
            if (cur->next) cur->next->prev = cur->prev;
            else           list->tail = cur->prev;
            free(cur);
            list->size--;
            return 0;
        }
        cur = cur->next;
    }
    return -1;
}

/** Delete all occurrences of value. Returns count deleted. */
int dll_delete_all(DoublyLinkedList *list, int val) {
    int count = 0;
    while (dll_delete_val(list, val) == 0) count++;
    return count;
}

/* ──────────────────────────────────────────────────────────────
 *  SEARCH
 * ──────────────────────────────────────────────────────────────*/

/** Linear search. Returns pointer to first matching node or NULL. */
Node *dll_find(DoublyLinkedList *list, int val) {
    Node *cur = list->head;
    while (cur) {
        if (cur->val == val) return cur;
        cur = cur->next;
    }
    return NULL;
}

/** Count occurrences of val. */
size_t dll_count(DoublyLinkedList *list, int val) {
    size_t n = 0;
    Node *cur = list->head;
    while (cur) { if (cur->val == val) n++; cur = cur->next; }
    return n;
}

/* ──────────────────────────────────────────────────────────────
 *  TRANSFORMATIONS
 * ──────────────────────────────────────────────────────────────*/

/** Reverse the list in-place. O(n). */
void dll_reverse(DoublyLinkedList *list) {
    Node *cur = list->head;
    Node *tmp;
    while (cur) {
        tmp      = cur->prev;
        cur->prev = cur->next;
        cur->next = tmp;
        cur       = cur->prev;  /* was cur->next, now reversed */
    }
    tmp        = list->head;
    list->head = list->tail;
    list->tail = tmp;
}

/** Copy a list. Caller must dll_free() the copy. */
DoublyLinkedList *dll_copy(DoublyLinkedList *src) {
    DoublyLinkedList *dst = dll_create();
    Node *cur = src->head;
    while (cur) { dll_push_back(dst, cur->val); cur = cur->next; }
    return dst;
}

/** Compare function type for dll_sort */
typedef int (*CmpFn)(int a, int b);

static int cmp_asc(int a, int b)  { return (a > b) - (a < b); }
static int cmp_desc(int a, int b) { return (b > a) - (b < a); }

/** Merge-sort the list using cmp comparison function. O(n log n). */
static Node *merge_sorted(Node *left, Node *right, CmpFn cmp) {
    if (!left)  return right;
    if (!right) return left;
    if (cmp(left->val, right->val) <= 0) {
        left->next = merge_sorted(left->next, right, cmp);
        if (left->next) left->next->prev = left;
        left->prev = NULL;
        return left;
    } else {
        right->next = merge_sorted(left, right->next, cmp);
        if (right->next) right->next->prev = right;
        right->prev = NULL;
        return right;
    }
}

static Node *split_half(Node *head) {
    Node *slow = head, *fast = head ? head->next : NULL;
    while (fast && fast->next) {
        slow = slow->next;
        fast = fast->next->next;
    }
    Node *second = slow ? slow->next : NULL;
    if (slow) slow->next = NULL;
    return second;
}

static Node *merge_sort_nodes(Node *head, CmpFn cmp) {
    if (!head || !head->next) return head;
    Node *second = split_half(head);
    head   = merge_sort_nodes(head, cmp);
    second = merge_sort_nodes(second, cmp);
    return merge_sorted(head, second, cmp);
}

void dll_sort(DoublyLinkedList *list, CmpFn cmp) {
    if (list->size <= 1) return;
    list->head = merge_sort_nodes(list->head, cmp);
    /* Repair tail pointer and prev links */
    Node *cur = list->head;
    Node *prev = NULL;
    while (cur) {
        cur->prev = prev;
        if (!cur->next) list->tail = cur;
        prev = cur;
        cur  = cur->next;
    }
}

/* ──────────────────────────────────────────────────────────────
 *  UTILITY / DISPLAY
 * ──────────────────────────────────────────────────────────────*/

void dll_print_fwd(DoublyLinkedList *list, const char *label) {
    printf("  %s [size=%zu]: HEAD ↔ ", label, list->size);
    Node *cur = list->head;
    while (cur) {
        printf("%d", cur->val);
        if (cur->next) printf(" ↔ ");
        cur = cur->next;
    }
    printf(" ↔ NULL\n");
}

void dll_print_bwd(DoublyLinkedList *list) {
    printf("  Backward:          TAIL ↔ ");
    Node *cur = list->tail;
    while (cur) {
        printf("%d", cur->val);
        if (cur->prev) printf(" ↔ ");
        cur = cur->prev;
    }
    printf(" ↔ NULL\n");
}

/** Validate internal consistency (prev/next symmetry). */
static void dll_validate(DoublyLinkedList *list) {
    size_t fwd = 0;
    Node *cur = list->head;
    while (cur) {
        assert(cur->next == NULL || cur->next->prev == cur);
        fwd++;
        cur = cur->next;
    }
    assert(fwd == list->size);
}

/* ──────────────────────────────────────────────────────────────
 *  DEMO / MAIN
 * ──────────────────────────────────────────────────────────────*/

int main(void) {
    printf("╔══════════════════════════════════════════════════════════╗\n");
    printf("║       DOUBLY LINKED LIST — Full Operations Demo          ║\n");
    printf("╚══════════════════════════════════════════════════════════╝\n\n");

    DoublyLinkedList *list = dll_create();

    /* Build list: push front and back */
    printf("── Construction ─────────────────────────────────────────\n");
    dll_push_back(list,  10);
    dll_push_back(list,  20);
    dll_push_back(list,  30);
    dll_push_front(list,  5);
    dll_push_front(list,  1);
    dll_insert_after(list, 2, 15);   /* insert 15 after index 2 */
    dll_print_fwd(list, "After inserts");
    dll_print_bwd(list);

    /* Search */
    printf("\n── Search ───────────────────────────────────────────────\n");
    Node *found = dll_find(list, 15);
    printf("  Find 15: %s\n", found ? "FOUND" : "NOT FOUND");
    printf("  Count of 20: %zu\n", dll_count(list, 20));
    printf("  Count of 99: %zu\n", dll_count(list, 99));

    /* Deletion */
    printf("\n── Deletion ─────────────────────────────────────────────\n");
    dll_delete_val(list, 15);
    printf("  pop_front=%d  pop_back=%d\n",
           dll_pop_front(list), dll_pop_back(list));
    dll_print_fwd(list, "After deletions");

    /* Reverse */
    printf("\n── Reverse ──────────────────────────────────────────────\n");
    dll_reverse(list);
    dll_print_fwd(list, "Reversed");
    dll_reverse(list);
    dll_print_fwd(list, "Reversed back");

    /* Sort */
    printf("\n── Sort ─────────────────────────────────────────────────\n");
    dll_push_back(list, 7);
    dll_push_front(list, 25);
    dll_print_fwd(list, "Before sort");
    dll_sort(list, cmp_asc);
    dll_print_fwd(list, "Sorted asc");
    dll_sort(list, cmp_desc);
    dll_print_fwd(list, "Sorted desc");

    /* Insert sorted */
    printf("\n── Insert Sorted ────────────────────────────────────────\n");
    DoublyLinkedList *sorted = dll_create();
    int vals[] = {50, 10, 30, 20, 40, 5, 60};
    for (int i = 0; i < 7; i++) dll_insert_sorted(sorted, vals[i]);
    dll_print_fwd(sorted, "Sorted inserts");

    /* Copy */
    printf("\n── Copy ─────────────────────────────────────────────────\n");
    DoublyLinkedList *copy = dll_copy(sorted);
    dll_push_front(copy, 999);
    dll_print_fwd(sorted, "Original");
    dll_print_fwd(copy,   "Copy (with 999 prepended)");

    /* Validate */
    dll_validate(list);
    dll_validate(sorted);
    dll_validate(copy);
    printf("\n  ✓ All internal consistency checks passed.\n");

    /* Cleanup */
    dll_free(list);
    dll_free(sorted);
    dll_free(copy);

    printf("\n  ✓ All memory freed. Done.\n");
    return 0;
}
