# The directories containing the source files, separated by ':'
VPATH=.

CC=gcc

# To make "debug" the default configuration if invoked with just "make":
#
# The source files: regardless of where they reside in the source tree,
# VPATH will locate them...
Group0_SRC = $(shell echo *.c)

# Build a Dependency list and an Object list, by replacing the .c
# extension to .d for dependency files, and .o for object files.
Group0_OBJ = $(patsubst %.c, %.o, ${Group0_SRC})

# Your final binary
TARGET=mainprogram

# What include flags to pass to the compiler
INCLUDEFLAGS= -I .

# -Wall -Wextra -pedantic -Wcast-qual -Wformat=2 -Wshadow -Wundef -Wno-unused -Wuninitialized -fdiagnostics-show-option
# Separate compile options per configuration
CFLAGS += -g -Wall -Werror -Wextra -Wuninitialized -Wcast-qual -Wshadow -Wundef -Wno-unused-parameter -Wuninitialized -fdiagnostics-show-option -D_DEBUG   ${INCLUDEFLAGS} -O0 -fprofile-arcs -ftest-coverage
LDFLAGS += -g -ftest-coverage -fprofile-arcs

# A common link flag for all configurations
LDFLAGS +=

all:	${TARGET}

${TARGET}: ${Group0_OBJ}
	@mkdir -p $(dir $@)
	$(CC) -g -o $@ $^ ${LDFLAGS}

%.o: %.c
	@mkdir -p $(dir $@)
	$(CC) -c $(CFLAGS) -o $@ $<

coverage:
	$(MAKE) && ./$(TARGET) && \
	    gcov a.c

clean:
	@rm -rf *.o $(TARGET) *.gcda *.gcno *.gcov