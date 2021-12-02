.SUFFIXES: .f90
UNAME_S := $(shell uname -s)

ifeq ($(UNAME_S),Linux)
	CWARNING=-Werror
	CFLAGS=-m64 -fPIC -g -std=gnu99 -I../../heclib/heclib_c/src/headers -I. $(CWARNING)  -D__linux__
	FWARNINGS=-Werror -Warray-bounds -Wc-binding-type -Wcharacter-truncation
	FDEBUG=-fcheck=all,no-recursion
	FFLAGS=-c -g -cpp -m64 -fPIC $(FWARNINGS) $(FDEBUG) -I../../heclib/heclib_f/src/headers
	LDFLAGS = -lz -lrt -lgfortran -lm
	MAKE=make
endif
ifeq ($(UNAME_S),SunOS)
	CFLAGS=-m64 -I../../heclib/heclib_c/src/headers -I. -mt -xcode=pic32 -xstrconst -xc99=all -xCC -g -xs
	LDFLAGS = -lz -lrt -lm -lfsu -g -xs -lsunmath
	MAKE=gmake
endif

TARGET := Dss-Fortran
BUILD_DIR := ./Output
SRC_DIRS := .
CC=cc
F90=gfortran
HLIBS=../../heclib/heclib_f/Output/libhec_f.a ../../heclib/heclib_c/Output/libhec_c.a

SRCS := $(shell find $(SRC_DIRS) -name '*.c' -o -name '*.f90' )

OBJS := $(SRCS:%=$(BUILD_DIR)/%.o)

$(BUILD_DIR)/$(TARGET): $(OBJS)
	mkdir -p $(BUILD_DIR)
	$(F90) $(CFLAGS) -o $@ $^ $(HLIBS) $(HLIBS)  $(HLIBS) $(LDFLAGS)

test:$(BUILD_DIR)/$(TARGET)
	mkdir -p $(BUILD_DIR)
	rm -f $(BUILD_DIR)/*.dss
	cp ../../dss-test-data/*.dss $(BUILD_DIR)
	(cd $(BUILD_DIR); ./Dss-Fortran test)


$(BUILD_DIR)/%.c.o: %.c
	mkdir -p $(dir $@)
	$(CC) $(CFLAGS) -c $< -o $@

$(BUILD_DIR)/%.f90.o: %.f90
	mkdir -p $(dir $@)
	$(F90) $(FFLAGS) -c $< -o $@

.PHONY: clean
clean:
	rm -r $(BUILD_DIR)
	rm -r $(shell find $(SRC_DIRS) -name '*.mod')
