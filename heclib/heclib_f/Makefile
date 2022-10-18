LIB := libhec_f.a 
BUILD_DIR := ./Output
SRC_DIRS := ./src


UNAME_S := $(shell uname -s)

ifeq ($(UNAME_S),Linux)
	FWARNINGS=-fallow-argument-mismatch -Warray-bounds -Wc-binding-type -Wcharacter-truncation
	FDEBUG=-fcheck=all,no-recursion
	FORTRAN=gfortran
	FFLAGS=-c -g -cpp -m64 -fPIC $(FWARNINGS) $(FDEBUG) -Isrc/headers  
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





