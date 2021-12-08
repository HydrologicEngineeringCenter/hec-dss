# based on examples at https://makefiletutorial.com/
UNAME_S := $(shell uname -s)

ifeq ($(UNAME_S),Linux)
	CWARNING=-Werror=implicit-function-declaration
	CFLAGS=-m64 -c -fPIC -g -std=gnu99 -Isrc/headers $(CWARNING) -D__linux__
endif
ifeq ($(UNAME_S),SunOS)
	CFLAGS=-m64 -xmemalign=2i -xcode=pic32 -c -Isrc/headers -mt -xcode=abs64 -xstrconst -xc99=all -xCC -g -xs
endif
ifeq ($(UNAME_S),Darwin)
	CWARNING=-Werror=implicit-function-declaration
    CFLAGS=-m64 -c -fPIC -g -std=gnu99 -Isrc/headers $(CWARNING)
endif

TARGET_LIB := libhec_c.a
BUILD_DIR := ./Output
SRC_DIRS := ./src
CC=cc

FDEBUG=-fcheck=all,no-recursion
SRCS := $(shell find $(SRC_DIRS) -name '*.c' )

OBJS := $(SRCS:%=$(BUILD_DIR)/%.o)

$(BUILD_DIR)/$(TARGET_LIB): $(OBJS)
	$(AR) -r $@  $(OBJS)  

$(BUILD_DIR)/%.c.o: %.c
	mkdir -p $(dir $@)
	$(CC) $(CFLAGS) -c $< -o $@

.PHONY: clean
clean:
	rm -fr $(BUILD_DIR)

