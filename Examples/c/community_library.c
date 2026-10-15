/*
 * COMMUNITY LIBRARY CIRCULATION DESK
 * Inventory, checkouts, returns, search, and service metrics.
 */
#include <stdio.h>

int main() {
    int ids[6] = {101, 102, 103, 104, 105, 106};
    int copies[6] = {4, 2, 5, 1, 3, 2};
    int borrowed[6] = {1, 2, 0, 1, 0, 1};
    int checkouts[6] = {18, 25, 9, 14, 7, 12};
    int i, total_copies, total_borrowed, total_available;
    int busiest_index, target_id, found_index;

    printf("============================================================\n");
    printf("  COMMUNITY LIBRARY | CIRCULATION DESK\n");
    printf("  Inventory  |  Loans  |  Returns  |  Service report\n");
    printf("============================================================\n\n");

    printf("OPENING CATALOG\n");
    printf("ID     TITLE CODE     AVAILABLE  ON LOAN  CHECKOUTS\n");
    total_copies = 0;
    total_borrowed = 0;
    total_available = 0;
    busiest_index = 0;
    for (i = 0; i < 6; i++) {
        printf("%d     COLLECTION-%d       %d         %d        %d\n",
               ids[i], i + 1, copies[i], borrowed[i], checkouts[i]);
        total_copies = total_copies + copies[i] + borrowed[i];
        total_borrowed = total_borrowed + borrowed[i];
        total_available = total_available + copies[i];
        if (checkouts[i] > checkouts[busiest_index]) {
            busiest_index = i;
        }
    }

    printf("\nTODAY'S SERVICE TRANSACTIONS\n");
    printf("Checkout request: item 101\n");
    if (copies[0] > 0) {
        copies[0] = copies[0] - 1;
        borrowed[0] = borrowed[0] + 1;
        checkouts[0] = checkouts[0] + 1;
        printf("  Approved. One copy is now on loan.\n");
    } else {
        printf("  Wait-list required; no copies are available.\n");
    }

    printf("Return received: item 104\n");
    if (borrowed[3] > 0) {
        borrowed[3] = borrowed[3] - 1;
        copies[3] = copies[3] + 1;
        printf("  Return processed. Item 104 is available again.\n");
    }

    target_id = 105;
    found_index = -1;
    for (i = 0; i < 6; i++) {
        if (ids[i] == target_id) {
            found_index = i;
        }
    }
    printf("\nCATALOG SEARCH | ID %d\n", target_id);
    if (found_index >= 0) {
        printf("  Collection code: %d\n", found_index + 1);
        printf("  Available now: %d\n", copies[found_index]);
        printf("  Total checkouts: %d\n", checkouts[found_index]);
    } else {
        printf("  No matching catalog record.\n");
    }

    total_borrowed = 0;
    total_available = 0;
    for (i = 0; i < 6; i++) {
        total_borrowed = total_borrowed + borrowed[i];
        total_available = total_available + copies[i];
    }
    printf("\nEND OF DAY REPORT\n");
    printf("  Catalog copies: %d\n", total_copies);
    printf("  Available: %d | On loan: %d\n", total_available, total_borrowed);
    printf("  Most requested item: %d (%d checkouts)\n",
           ids[busiest_index], checkouts[busiest_index]);
    if (total_available < 8) {
        printf("  Action: review acquisitions for low-stock collections.\n");
    } else {
        printf("  Action: inventory levels are within the service target.\n");
    }
    printf("\nDesk session complete. All changes were kept in memory.\n");
    return 0;
}
