# ------------------------------------------------------------
#  configure for lib/
# ------------------------------------------------------------
# SML files
MODULES = $(wildcard lib/*/*.sml)
C_MODULES = $(wildcard lib/*/*.c)

# ------------------------------------------------------------
#  configure for test
# ------------------------------------------------------------
TEST_MODULES = $(wildcard test/*/*.sml) test/Main.sml
TEST_TARGET = testRunner

# ------------------------------------------------------------
#  configure for test
# ------------------------------------------------------------
EXAMPLE_MODULES = $(wildcard example/*.sml)
EXAMPLE_MAIN = hello
TARGET = example/hello

# ------------------------------------------------------------
#  Flags
# ------------------------------------------------------------
SMLSHARP = smlsharp
SMLSHARP_FLAGS = -I lib
SMLSHARP_CFLAGS = -O2
SMLSHARP_LDFLAGS =
CFLAGS += -m32 -Iinclude

# ------------------------------------------------------------
#  prepare variables
# ------------------------------------------------------------
sources := $(MODULES)
objects := $(sources:.sml=.o)

c_sources = $(C_MODULES)
c_objects := $(c_sources:.c=.o)

test_sources := $(TEST_MODULES)
test_objects := $(test_sources:.sml=.o)

example_sources := $(EXAMPLE_MODULES)
example_objects := $(example_sources:.sml=.o)

# ------------------------------------------------------------
#  Build target
# ------------------------------------------------------------
.PHONY: all clean check
all: $(TARGET)

$(TARGET): $(example_objects) $(objects) $(c_objects)
	$(SMLSHARP) $(SMLSHARP_LDFLAGS) $(SMLSHARP_FLAGS) -o $@ example/$(EXAMPLE_MAIN).smi $(c_objects)

$(TEST_TARGET): $(objects) $(c_objects) $(lib_objects) $(test_objects)
	$(SMLSHARP) $(SMLSHARP_LDFLAGS) $(SMLSHARP_FLAGS) -o $@ test/Main.smi $(c_objects)

check: $(TEST_TARGET)
	./$(TEST_TARGET)

clean:
	find . -name '*.o' | xargs rm -rf
	rm -rf $(TARGET) $(TEST_TARGET)

# ------------------------------------------------------------
#  Build rules
# ------------------------------------------------------------
%.o: %.sml
	$(SMLSHARP) $(SMLSHARP_CFLAGS) $(SMLSHARP_FLAGS) -c -o $@ $<

%.o: %.c
	$(CC) $(CFLAGS) $(DEFS) $(CPPFLAGS) -c -o $@ $<

# ------------------------------------------------------------
#  Dependencies
# ------------------------------------------------------------
.PHONY: depend

depend:
	$(SMLSHARP) $(SMLSHARP_FLAGS) -MM $(sources) $(test_sources) $(lib_sources) > .depend
-include .depend
