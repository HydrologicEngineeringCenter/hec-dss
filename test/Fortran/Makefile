.SUFFIXES: .f90
UNAME_S := $(shell uname -s)
UNAME_M := $(shell uname -m)
FOR_VER := $(shell gfortran -dumpversion | cut -c1,2)
F_GE_10 := $(shell if [ "$(FOR_VER)" -ge "10" ]; then echo 1; else echo 0; fi; )

ifeq ($(UNAME_S),Linux)
	ifneq ($(UNAME_M),aarch64)
		BITS=-m64
	endif
	CWARNING=-Werror
	CFLAGS=$(BITS) -fPIC -g -std=gnu99 -I../../heclib/heclib_c/src/headers -I. $(CWARNING)  -D__linux__
	FWARNINGS= -Warray-bounds -Wc-binding-type -Wcharacter-truncation
	ifeq "$(F_GE_10)" "1"
	        FWARNINGS += -fallow-argument-mismatch
	endif

	FDEBUG=-fcheck=all,no-recursion
	FFLAGS=-c $(BITS) -g -cpp -fPIC $(FWARNINGS) $(FDEBUG) -I../../heclib/heclib_f/src/headers
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
HLIBS=../../heclib/Output/heclib.a

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
