#ifndef ANANKE_INCLUDE_ENTRY_H
#define ANANKE_INCLUDE_ENTRY_H

struct entry {
    const char *id;
    const char *timestamp;
    const char *key_id;
    const char *description;
    const char *identity;
    const char *ciphertext;
    const char *metadata;
};

#endif // ANANKE_INCLUDE_ENTRY_H
