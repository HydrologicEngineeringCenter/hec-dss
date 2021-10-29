UNAME_S := $(shell uname -s)

ifeq ($(UNAME_S),Linux)
	LDFLAGS = -lz -lrt -lgfortran -lm
	CWARNING=-Werror=implicit-function-declaration
	CFLAGS=-m64 -fPIC -g -std=gnu99 -I../../heclib/heclib_c/src/headers -I. $(CWARNING)  -D__linux__
	MAKE=make
endif
ifeq ($(UNAME_S),SunOS)
	CFLAGS=-m64 -I../../heclib/heclib_c/src/headers -I. -mt -xcode=pic32 -xstrconst -xc99=all -xCC -g -xs 
	LDFLAGS = -lz -lrt -lm -lfsu -g -xs -lsunmath
	MAKE=gmake
endif

TARGET := Dss-C
BUILD_DIR := ./Output
SRC_DIRS := .
CC=cc
HLIBS=../../heclib/heclib_f/Output/libhec_f.a ../../heclib/heclib_c/Output/libhec_c.a

SRCS := $(shell find $(SRC_DIRS) -name '*.c' )

OBJS := $(SRCS:%=$(BUILD_DIR)/%.o)

$(BUILD_DIR)/$(TARGET): $(OBJS)
	mkdir -p $(BUILD_DIR)
	$(CC) $(CFLAGS) -o $@ $^ $(HLIBS) $(HLIBS)  $(HLIBS) $(LDFLAGS)

test:$(BUILD_DIR)/$(TARGET)
	mkdir -p $(BUILD_DIR)
	rm -f $(BUILD_DIR)/*.dss
	cp ../../dss-test-data/*.dss $(BUILD_DIR)
	(cd $(BUILD_DIR); ./Dss-C test)


$(BUILD_DIR)/%.c.o: %.c
	mkdir -p $(dir $@)
	$(CC) $(CFLAGS) -c $< -o $@

.PHONY: clean
clean:
	rm -r $(BUILD_DIR)
