.SUFFIXES: .f90
UNAME_S := $(shell uname -s)
UNAME_M := $(shell uname -m)

ifeq ($(UNAME_S),Linux)
	ifneq ($(UNAME_M),aarch64)
		BITS=-m64
	endif
	CWARNING=-Werror
	CFLAGS=$(BITS) -fPIC -g -std=gnu99 -I../../heclib/heclib_c/src/headers -I. $(CWARNING)  -D__linux__ -fcommon
	FWARNINGS=-Werror -Warray-bounds -Wc-binding-type -Wcharacter-truncation
	FDEBUG=-fcheck=all,no-recursion
	LDFLAGS = -lz -lrt -lm
	MAKE=make
        F90=gfortran
endif
ifeq ($(UNAME_S),SunOS)
	CFLAGS=-m64 -I../../heclib/heclib_c/src/headers -I. -mt -xcode=pic32 -xstrconst -xc99=all -xCC -g -xs 
	LDFLAGS = -m64 -lz -lrt -lm -g -xs -lsunmath
	MAKE=gmake
endif
ifeq ($(UNAME_S),Darwin)
    LDFLAGS=-lz -lm -lc
	CWARNING=-Werror=implicit-function-declaration
    CFLAGS=-m64 -dynamiclib -fPIC -g -std=gnu99 -I../../heclib/heclib_c/src/headers -I. $(CWARNING)
    MAKE=make
endif

TARGET := Dss-C
BUILD_DIR := ./Output
SRC_DIRS := .
CC=cc
HLIBS=../../heclib/Output/heclib.a

SRCS := $(shell find $(SRC_DIRS) -name '*.c' )

OBJS := $(SRCS:%=$(BUILD_DIR)/%.o)

$(BUILD_DIR)/$(TARGET): $(OBJS) $(HLIBS)
	mkdir -p $(BUILD_DIR)
	$(CC)  -o $@ $^ $(HLIBS) $(HLIBS) $(LDFLAGS)

test:$(BUILD_DIR)/$(TARGET)
	mkdir -p $(BUILD_DIR)
	rm -f $(BUILD_DIR)/*.dss
	cp ../../dss-test-data/*.dss $(BUILD_DIR)
	(cd $(BUILD_DIR); ./Dss-C test)

#valgrind  --leak-check=full --show-leak-kinds=all --read-inline-info=yes --track-origins=yes  --read-var-info=yes --exit-on-first-error=yes --error-exitcode=-1 --leak-check=full --log-file=vg.txt
valgrind:$(BUILD_DIR)/$(TARGET)
	mkdir -p $(BUILD_DIR)
	rm -f $(BUILD_DIR)/*.dss
	cp ../../dss-test-data/*.dss $(BUILD_DIR)
	(cd $(BUILD_DIR); valgrind   --read-inline-info=yes --track-origins=yes  --read-var-info=yes --error-exitcode=-1 --leak-check=full --log-file=vg.txt ./Dss-C test)

$(BUILD_DIR)/%.c.o: %.c
	mkdir -p $(dir $@)
	$(CC) $(CFLAGS) -c $< -o $@

$(BUILD_DIR)/%.f90.o: %.f90
	mkdir -p $(dir $@)
	$(F90) $(FFLAGS) -c $< -o $@

.PHONY: clean
clean:
	rm -r $(BUILD_DIR)
