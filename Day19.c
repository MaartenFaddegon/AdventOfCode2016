#include <stdlib.h>
#include <stdio.h>

typedef struct node {
    int val;
    struct node* next;
} node;

node* mkNodes(int max) {
  node* head;
  node* cursor;
  int i;

  head = malloc(sizeof(node));
  cursor = head;
  for (i=1; i<=max; i++) {
    cursor->val = i;
    if (i==max) {
      cursor->next = head;
    } else {
      cursor->next = malloc(sizeof(node));
      cursor = cursor->next;
    }
  }
  return head;
}

node* next(node* cursor, int n) {
  for (int i=0; i<n; i++) {
    cursor = cursor->next;
  }
  return cursor;
}

void delete(node* n) {
  node* next = n->next;
  n->val = next->val;
  n->next = next->next;
  free(next);
}

int main() {
  int m = 3014603;
  node* cursor = mkNodes(m);

  do {
    delete(next(cursor, m/2));
    cursor = cursor->next;
    m--;
  } while (cursor != cursor->next);

  printf("%i\n", cursor->val);
}
