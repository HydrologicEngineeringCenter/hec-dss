TARGET := Dss-Fortran
BUILD_DIR := ./Output


UNAME_S := $(shell uname -s)

ifeq ($(UNAME_S),Linux)
	FWARNINGS=-Warray-bounds -Wc-binding-type -Wcharacter-truncation
	FDEBUG=-fcheck=all,no-recursion
	FORTRAN=gfortran
	FFLAGS=-c -g -cpp -m64 -fPIC $(FWARNINGS) $(FDEBUG)
	FIND=find
	LDFLAGS=-lz -lrt -lgfortran -lm
endif
ifeq ($(UNAME_S),SunOS)
	FFLAGS= -m64 -c -fpp -mt -xmemalign=2i -xcode=pic32 -xpagesize=4M -O 
	FORTRAN=f95
	FIND=gfind
endif

HLIBS=../../heclib/heclib_f/Output/libhec_f.a ../../heclib/heclib_c/Output/libhec_c.a

SRCS := $(shell $(FIND) ./source -name '*.f' -or -name '*.f90' -or -name '*.for')
OBJS := $(SRCS:%=$(BUILD_DIR)/%.o)


test:$(BUILD_DIR)/$(TARGET)
	rm -f Output/*.dss
	cp ../../dss-test-data/*.dss ./Output/
	(cd Output; ./Dss-Fortran)


$(BUILD_DIR)/$(TARGET): $(OBJS)
	mkdir -p $(BUILD_DIR)
	$(FORTRAN) -m64 -fPIC -o $@ $^ $(HLIBS) $(HLIBS)  $(HLIBS) $(LDFLAGS)


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






