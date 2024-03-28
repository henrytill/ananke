#ifndef ANANKE_INCLUDE_ENTRY_H
#define ANANKE_INCLUDE_ENTRY_H

#include <stddef.h>

struct entry {
    char *id;
    char *timestamp;
    char *key_id;
    char *description;
    char *identity;
    char *ciphertext;
    char *metadata;
};

struct entries {
    size_t len;
    struct entry data[];
};

struct entries *entries_create(size_t len);

void entries_destroy(struct entries *es);

#endif // ANANKE_INCLUDE_ENTRY_H
