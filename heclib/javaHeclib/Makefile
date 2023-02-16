# based on examples at https://makefiletutorial.com/
UNAME_S := $(shell uname -s)
UNAME_M := $(shell uname -m)

ifeq ($(UNAME_S),Linux)
	CWARNING=-Werror
	FDEBUG=-fcheck=all,no-recursion
	ifeq ($(UNAME_M),aarch64)
		LDFLAGS1=-Wl,-z,defs
	else
		LDFLAGS1=-Wl,-z,defs,--wrap=powf
		BITS=-m64
	endif
	CFLAGS=$(BITS) -fPIC -g -std=gnu99 $(CWARNING) $(INC_DIRS) -D__linux__ -fcommon
	LDFLAGS=$(LDFLAGS1) -lz -lrt -lgfortran -lm
endif
ifeq ($(UNAME_S),SunOS)
	CC=cc
	CFLAGS=-m64 -mt -xmemalign=2i -xcode=pic32 -xstrconst -xc99=all -xCC -g -xs -I"${JAVA_HOME}/include" -I"${JAVA_HOME}/include/solaris" -I../heclib_c/src/headers
	LDFLAGS=-lrt -lz -lfsu 
endif
ifeq ($(UNAME_S),Darwin)
	CWARNING=-Werror=implicit-function-declaration
    FDEBUG=-fcheck=all,no-recursion
    CFLAGS=-m64 -dynamiclib -fPIC -g -std=gnu99 $(CWARNING) $(INC_DIRS_MACOS)
    LDFLAGS=-L${GFORTRAN_PATH} -lz -lgfortran -lm
endif

TARGET_LIB := libjavaHeclib.so
ifeq ($(UNAME_S),Darwin)
	TARGET_LIB := libjavaHeclib.dylib
endif
BUILD_DIR := ./Output
SRC_DIRS := ./src
INC_DIRS=-Isrc -I../heclib_c/src/headers -I${JAVA_HOME}/include -I${JAVA_HOME}/include/linux
INC_DIRS_MACOS=-Isrc -I../heclib_c/src/headers -I${JAVA_HOME}/include -I${JAVA_HOME}/include/darwin
HLIBS=../heclib_f/Output/libhec_f.a ../heclib_c/Output/libhec_c.a 

SRCS := $(shell find $(SRC_DIRS) -name '*.c' )


OBJS := $(SRCS:%=$(BUILD_DIR)/%.o)

$(BUILD_DIR)/$(TARGET_LIB): $(OBJS)
	$(CC) $(CFLAGS) -shared  -o $@  $(OBJS) $(HLIBS) $(HLIBS)  $(LDFLAGS)
	ls -la $(BUILD_DIR)/lib*

$(BUILD_DIR)/%.c.o: %.c
	mkdir -p $(dir $@)
	$(CC) $(CFLAGS) -c $< -o $@

.PHONY: clean
clean:
	rm -fr $(BUILD_DIR)
