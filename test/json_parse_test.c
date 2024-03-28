#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

#include "entry.h"
#include "yyjson.h"

const char *const DATA_JSON = "./example/db/data.json";

struct error {
    int rc;
    const char *msg;
};

enum {
    ALLOC = 1,
    JSON = 2,
    MISSING_KEY = 3,
    WRONG_TYPE = 4,
};

#define MSG_MISSING_KEY "missing key: "
#define MSG_WRONG_TYPE  "wrong type for key: "

#ifdef PRINT
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
#endif

#ifdef PRINT
static inline void print_entries(struct entries *es)
{
    for (size_t i = 0; i < es->len; ++i) {
        print_entry(&es->data[i]);
    };
}
#else
static inline void print_entries(struct entries *es)
{
    (void)es;
}
#endif

static int make_entry(yyjson_val *entry_val, struct entry *out, struct error *err)
{
    assert(yyjson_get_type(entry_val) == YYJSON_TYPE_OBJ);

    yyjson_val *val = NULL;
    const char *str = NULL;

#define REQUIRED(name, dest)                     \
    do {                                         \
        val = yyjson_obj_get(entry_val, (name)); \
        if (val == NULL) {                       \
            err->rc = -MISSING_KEY;              \
            err->msg = MSG_MISSING_KEY name;     \
            return -MISSING_KEY;                 \
        };                                       \
        str = yyjson_get_str(val);               \
        if (str == NULL) {                       \
            err->rc = -WRONG_TYPE;               \
            err->msg = MSG_WRONG_TYPE name;      \
            return -WRONG_TYPE;                  \
        };                                       \
        (dest) = calloc(1, strlen(str) + 1);     \
        if ((dest) == NULL) {                    \
            err->rc = -ALLOC;                    \
            err->msg = "calloc failed";          \
            return -ALLOC;                       \
        }                                        \
        strcpy((dest), str);                     \
    } while (0)

#define OPTIONAL(name, dest)                     \
    do {                                         \
        val = yyjson_obj_get(entry_val, (name)); \
        if (val == NULL) {                       \
            break;                               \
        };                                       \
        str = yyjson_get_str(val);               \
        if (str == NULL) {                       \
            err->rc = -WRONG_TYPE;               \
            err->msg = MSG_WRONG_TYPE name;      \
            return -WRONG_TYPE;                  \
        };                                       \
        (dest) = calloc(1, strlen(str) + 1);     \
        if ((dest) == NULL) {                    \
            err->rc = -ALLOC;                    \
            err->msg = "calloc failed";          \
            return -ALLOC;                       \
        }                                        \
        strcpy((dest), str);                     \
    } while (0)

    REQUIRED("id", out->id);
    REQUIRED("timestamp", out->timestamp);
    REQUIRED("keyId", out->key_id);
    REQUIRED("description", out->description);
    REQUIRED("ciphertext", out->ciphertext);

    OPTIONAL("identity", out->identity);
    OPTIONAL("meta", out->metadata);

#undef REQUIRED
#undef OPTIONAL

    assert(err->rc == 0);
    assert(err->msg == NULL);
    return 0;
}

static struct entries *deserialize(size_t buf_len, char buf[buf_len], struct error *err)
{
    struct entries *ret = NULL;

    yyjson_doc *doc = yyjson_read(buf, buf_len, 0);
    if (doc == NULL) {
        err->rc = -JSON;
        err->msg = "yyjson_read failed";
        return ret;
    }

    yyjson_val *root = yyjson_doc_get_root(doc);
    if (root == NULL) {
        err->rc = -JSON;
        err->msg = "root should be non-NULL";
        goto out_free_doc;
    }

    yyjson_type root_type = yyjson_get_type(root);
    if (root_type != YYJSON_TYPE_ARR) {
        err->rc = -JSON;
        err->msg = "root_type should be YYJSON_TYPE_ARR";
        goto out_free_doc;
    }

    size_t arr_size = yyjson_arr_size(root);
    struct entries *tmp = entries_create(arr_size);
    if (tmp == NULL) {
        err->rc = -ALLOC;
        err->msg = "entries_create failed";
        goto out_free_doc;
    }

    yyjson_val *val = NULL;
    int rc = 0;
    for (size_t i = 0; i < tmp->len; ++i) {
        val = yyjson_arr_get(root, i);
        assert(val != NULL);
        rc = make_entry(val, &tmp->data[i], err);
        if (rc != 0) {
            entries_destroy(tmp);
            assert(err->rc != 0);
            assert(err->msg != NULL);
            assert(ret == NULL);
            goto out_free_doc;
        }
    }

    assert(err->rc == 0);
    assert(err->msg == NULL);

    ret = tmp;
out_free_doc:
    yyjson_doc_free(doc);
    return ret;
}

static void make_obj(struct entry *in, yyjson_mut_doc *doc, yyjson_mut_val *out)
{
    yyjson_mut_val *key = NULL;
    yyjson_mut_val *val = NULL;

#define REQUIRED(name, in)                 \
    do {                                   \
        key = yyjson_mut_str(doc, (name)); \
        val = yyjson_mut_str(doc, (in));   \
        yyjson_mut_obj_add(out, key, val); \
    } while (0)

#define OPTIONAL(name, in)                     \
    do {                                       \
        if ((in) != NULL) {                    \
            key = yyjson_mut_str(doc, (name)); \
            val = yyjson_mut_str(doc, (in));   \
            yyjson_mut_obj_add(out, key, val); \
        };                                     \
    } while (0)

    REQUIRED("timestamp", in->timestamp);
    REQUIRED("id", in->id);
    REQUIRED("keyId", in->key_id);
    REQUIRED("description", in->description);
    OPTIONAL("identity", in->identity);
    REQUIRED("ciphertext", in->ciphertext);
    OPTIONAL("meta", in->metadata);

#undef REQUIRED
#undef OPTIONAL
}

static char *serialize(struct entries *es, struct error *err)
{
    yyjson_mut_doc *doc = yyjson_mut_doc_new(NULL);
    yyjson_mut_val *arr = yyjson_mut_arr(doc);

    for (size_t i = 0; i < es->len; ++i) {
        yyjson_mut_val *obj = yyjson_mut_obj(doc);
        make_obj(&es->data[i], doc, obj);
        yyjson_mut_arr_append(arr, obj);
    }

    yyjson_mut_doc_set_root(doc, arr);
    char *json = yyjson_mut_write(doc, YYJSON_WRITE_PRETTY, NULL);
    char *ret = calloc(1, strlen(json) + 1);
    if (ret == NULL) {
        err->rc = -ALLOC;
        err->msg = "calloc failed";
        return NULL;
    }
    strcpy(ret, json);
    free(json);
    yyjson_mut_doc_free(doc);
    return ret;
}

int main(int argc, char *argv[])
{
    int ret = EXIT_FAILURE;

    const char *file = NULL;

    if (argc == 1) {
        file = DATA_JSON;
    } else if (argc == 2) {
        file = argv[1];
    } else {
        (void)fprintf(stderr, "Usage: %s <FILE>\n", argv[0]);
        return EXIT_FAILURE;
    }

    FILE *fp = fopen(file, "r");
    fseek(fp, 0, SEEK_END);
    const size_t fsize = (size_t)ftell(fp);
    fseek(fp, 0, SEEK_SET);

    char *buf = malloc(fsize);
    assert(buf != NULL);
    fread(buf, fsize, 1, fp);
    fclose(fp);

    struct error err = {0};
    struct entries *es = deserialize(fsize, buf, &err);
    if (es == NULL || err.rc != 0) {
        (void)fprintf(stderr, "deserialize failed: %s\n", err.msg);
        goto out_free_buf;
    };

    char *json = serialize(es, &err);
    if (json == NULL || err.rc != 0) {
        (void)fprintf(stderr, "serialize failed: %s\n", err.msg);
        goto out_destroy_es;
    }

    print_entries(es);

    buf[fsize - 1] = '\000'; // remove final newline
    if (strcmp(json, buf) == 0) {
        printf("PASS\n");
        ret = EXIT_SUCCESS;
    } else {
        ret = EXIT_FAILURE;
    }

    assert(err.rc == 0);
    assert(err.msg == NULL);

    free(json);
out_destroy_es:
    entries_destroy(es);
out_free_buf:
    free(buf);
    return ret;
}
