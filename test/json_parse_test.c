#include <assert.h>
#include <stdlib.h>

#include "entry.h"
#include "yyjson.h"

const char *const DATA_JSON = "./example/db/data.json";

static struct entry make_entry(yyjson_val *entry_val)
{
    assert(yyjson_get_type(entry_val) == YYJSON_TYPE_OBJ);
    yyjson_val *id = yyjson_obj_get(entry_val, "id");
    yyjson_val *timestamp = yyjson_obj_get(entry_val, "timestamp");
    yyjson_val *key_id = yyjson_obj_get(entry_val, "keyId");
    yyjson_val *description = yyjson_obj_get(entry_val, "description");
    yyjson_val *identity = yyjson_obj_get(entry_val, "identity");
    yyjson_val *ciphertext = yyjson_obj_get(entry_val, "ciphertext");
    yyjson_val *metadata = yyjson_obj_get(entry_val, "meta");
    struct entry ret = {
        .id = yyjson_get_str(id),
        .timestamp = yyjson_get_str(timestamp),
        .key_id = yyjson_get_str(key_id),
        .description = yyjson_get_str(description),
        .identity = identity ? yyjson_get_str(identity) : NULL,
        .ciphertext = yyjson_get_str(ciphertext),
        .metadata = metadata ? yyjson_get_str(metadata) : NULL,
    };
    return ret;
}

static void print_entry(struct entry *e)
{
    printf("id=%s\n", e->id);
    printf("    timestamp=%s\n", e->timestamp);
    printf("    key_id=%s\n", e->key_id);
    printf("    description=%s\n", e->description);
    printf("    identity=%s\n", e->identity);
    printf("    ciphertext=%s\n", e->ciphertext);
    printf("    metadata=%s\n", e->metadata);
}

int main(void)
{
    yyjson_read_err err;
    yyjson_doc *doc = yyjson_read_file(DATA_JSON, 0, NULL, &err);
    assert(doc != NULL);
    yyjson_val *root = yyjson_doc_get_root(doc);
    assert(root != NULL);
    yyjson_type root_type = yyjson_get_type(root);
    assert(root_type == YYJSON_TYPE_ARR);
    size_t arr_size = yyjson_arr_size(root);
    assert(arr_size == 3);

    struct entry *entries = calloc(arr_size, sizeof(*entries));

    yyjson_val *entry_val = NULL;
    for (size_t i = 0; i < arr_size; ++i) {
        entry_val = yyjson_arr_get(root, i);
        assert(entry_val != NULL);
        entries[i] = make_entry(entry_val);
        print_entry(&entries[i]);
        printf("\n");
    }

    free(entries);
    yyjson_doc_free(doc);
    return EXIT_SUCCESS;
}
