LIB := libhec_f.a 
BUILD_DIR := ./Output
SRC_DIRS := ./src


UNAME_S := $(shell uname -s)
UNAME_M := $(shell uname -m)
FOR_VER := $(shell gfortran -dumpversion | cut -c1,2)
F_GE_10 := $(shell if [ "$(FOR_VER)" -ge "10" ]; then echo 1; else echo 0; fi; )

ifeq ($(UNAME_S),Linux)
	FWARNINGS=-Warray-bounds -Wc-binding-type -Wcharacter-truncation
	ifeq "$(F_GE_10)" "1"
		FWARNINGS += -fallow-argument-mismatch
	endif
	FDEBUG=-fcheck=all,no-recursion
	FRELEASE=-O3 -march=native -funroll-loops -ftree-vectorize \
                -flto -ffast-math
	FORTRAN=gfortran
	ifneq ($(UNAME_M),aarch64)
		BITS=-m64
	endif
	FDEBUG=
	FFLAGS=-c $(BITS) -cpp -fPIC $(FWARNINGS) $(FRELEASE) -Isrc/headers  
	FIND=find
endif
ifeq ($(UNAME_S),SunOS)
	FFLAGS= -m64 -c -fpp -mt -xmemalign=2i -xcode=pic32 -xpagesize=4M -Isrc/headers  -O 
	FORTRAN=f95
	FIND=gfind
endif
ifeq ($(UNAME_S),Darwin)
	FWARNINGS=-Warray-bounds -Wc-binding-type -Wcharacter-truncation -fallow-argument-mismatch
	FDEBUG=-fcheck=all,no-recursion
	FORTRAN=gfortran
	FFLAGS=-c -g -cpp -m64 -fPIC $(FWARNINGS) $(FDEBUG) -Isrc/headers
	FIND=find
endif



SRCS := $(shell $(FIND) ./src -name '*.f' -or -name '*.f90' -or -name '*.for')
OBJS := $(SRCS:%=$(BUILD_DIR)/%.o)

$(BUILD_DIR)/$(LIB): $(OBJS)
	$(AR) -r $@  $(OBJS) 

$(BUILD_DIR)/%.f.o: %.f
	mkdir -p $(dir $@)
	$(FORTRAN) $(FFLAGS)  -c $< -o $@

$(BUILD_DIR)/%.f90.o: %.f90
	mkdir -p $(dir $@)
	$(FORTRAN) $(FFLAGS)  -c $< -o $@

$(BUILD_DIR)/%.for.o: %.for
	mkdir -p $(dir $@)
	$(FORTRAN) $(FFLAGS) -c $< -o $@


.PHONY: clean
clean:
	rm -fr $(BUILD_DIR)





