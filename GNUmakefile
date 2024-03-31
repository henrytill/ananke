CFLAGS = -g -std=c11 -Iinclude -Wall -Wextra -Wpedantic -Wconversion -Wsign-conversion
BINOUT = _bin

HEADERS =
HEADERS += include/entry.h
HEADERS += include/yyjson.h

OBJECTS =
OBJECTS += src/entry.o
OBJECTS += src/yyjson.o

TEST_OBJECTS =
TEST_OBJECTS += src/entry.o
TEST_OBJECTS += src/yyjson.o
TEST_OBJECTS += test/json_parse_test.o

.PHONY: all
all: $(BINOUT)/json_parse_test

test/json_parse_test.o: include/entry.h include/yyjson.h

$(BINOUT)/json_parse_test: $(TEST_OBJECTS)
	@mkdir -p -- $(BINOUT)
	$(CC) $(LDFLAGS) -o $@ $(TEST_OBJECTS) $(LDLIBS)

.PHONY: check
check: $(BINOUT)/json_parse_test
	$(BINOUT)/json_parse_test

.PHONY: clean
clean:
	rm -rf -- $(BINOUT) $(OBJECTS) $(TEST_OBJECTS)
