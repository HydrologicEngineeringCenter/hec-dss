# based on examples at https://makefiletutorial.com/
UNAME_S := $(shell uname -s)

ifeq ($(UNAME_S),Linux)
	CWARNING=-Werror
	FDEBUG=-fcheck=all,no-recursion
	CFLAGS=-m64 -fPIC -g -std=gnu99 $(CWARNING) $(INC_DIRS) -D__linux__
	LDFLAGS=-Wl,-z,defs,--wrap=powf -lz -lrt -lgfortran -lm
endif
ifeq ($(UNAME_S),SunOS)
	CC=cc
	CFLAGS=-m64 -mt -xmemalign=2i -xcode=pic32 -xstrconst -xc99=all -xCC -g -xs -I"${JAVA_HOME}/include" -I"${JAVA_HOME}/include/solaris" -I../heclib_c/src/headers
	LDFLAGS=-lrt -lz -lfsu 
endif

TARGET_LIB := libjavaHeclib.so
BUILD_DIR := ./Output
SRC_DIRS := ./src
INC_DIRS=-Isrc -I../heclib_c/src/headers -I${JAVA_HOME}/include -I${JAVA_HOME}/include/linux
HLIBS=../heclib_f/Output/libhec_f.a ../heclib_c/Output/libhec_c.a

SRCS := $(shell find $(SRC_DIRS) -name '*.c' )


OBJS := $(SRCS:%=$(BUILD_DIR)/%.o)

$(BUILD_DIR)/$(TARGET_LIB): $(OBJS)
	$(CC) $(CFLAGS) -shared  -o $@  $(OBJS) ../../nws_shef/Output/*.o $(HLIBS) $(HLIBS)  $(LDFLAGS)
	ls -la $(BUILD_DIR)/lib*

$(BUILD_DIR)/%.c.o: %.c
	mkdir -p $(dir $@)
	$(CC) $(CFLAGS) -c $< -o $@

.PHONY: clean
clean:
	rm -r $(BUILD_DIR)
