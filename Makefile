# ------------------------------------------------------------
#  configure for lib/
# ------------------------------------------------------------
# SML files
MODULES = \
	ext/Std \
	ext/Set \
	parser/Parser

# C files
C_MODULES = \

# ------------------------------------------------------------
#  configure for test
# ------------------------------------------------------------
#  (TBD)
TEST_MODULES = \
	Main \
	ext/StdTest \
	ext/SetTest \
	parser/ParserTest

TEST_TARGET = testRunner

# ------------------------------------------------------------
#  configure for test
# ------------------------------------------------------------
EXAMPLE_MODULES = \
	hello

EXAMPLE_MAIN = hello

TARGET = example/hello

# ------------------------------------------------------------
#  Flags
# ------------------------------------------------------------
SMLSHARP = smlsharp
SMLSHARP_FLAGS = -I lib
SMLSHARP_CFLAGS = -O2
SMLSHARP_LDFLAGS =
CFLAGS += -m32

# ------------------------------------------------------------
#  prepare variables
# ------------------------------------------------------------
sources := $(addprefix lib/,$(MODULES:=.sml))
objects := $(sources:.sml=.o)

c_sources = $(addprefix lib/,$(C_MODULES:=.c))
c_objects := $(c_sources:.c=.o)

test_sources := $(addprefix test/, $(TEST_MODULES:=.sml))
test_objects := $(test_sources:.sml=.o)

example_sources := $(addprefix example/, $(EXAMPLE_MODULES:=.sml))
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
