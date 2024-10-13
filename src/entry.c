#include "entry.h"

#include <assert.h>
#include <stdlib.h>

struct entries *entries_create(size_t len)
{
    struct entries *ret = calloc(1, sizeof(*ret) + (sizeof(struct entry) * len));
    if (ret != NULL) {
        ret->len = len;
    }
    return ret;
}

void entries_destroy(struct entries *es)
{
    if (es == NULL) {
        return;
    };
    struct entry curr = {0};
    for (size_t i = 0; i < es->len; ++i) {
        curr = es->data[i];
        free(curr.id);
        free(curr.timestamp);
        free(curr.key_id);
        free(curr.description);
        free(curr.identity);
        free(curr.ciphertext);
        free(curr.metadata);
    }
    free(es);
}
