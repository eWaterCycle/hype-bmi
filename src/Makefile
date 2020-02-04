# Call options: comp=ifort or gfortran config=debug or release
comp_ifort = ifort
comp_gfortran = gfortran
config_debug = debug
config_release = release

# Set compiler and switches; gfortran release is default
f90comp = $(comp_$(comp))
f90config = $(config_$(config))
ifeq ($(f90comp),)
	f90comp = $(comp_gfortran)
endif
ifeq ($(f90comp),$(comp_gfortran))
	switch = -O2 -fbacktrace -ffree-line-length-0 -Wall -fno-automatic -cpp
	ifeq ($(f90config),$(config_debug))
		switch = -O0 -g -fbounds-check -fbacktrace -ffree-line-length-0 -Wall -fno-automatic -cpp
	endif
endif
ifeq ($(f90comp),$(comp_ifort))
	switch = -auto-scalar -heap-arrays 1024 -fpp
	ifeq ($(f90config),$(config_debug))
		switch = -warn unused -warn uncalled -warn interfaces -check bounds -traceback -O0 -auto-scalar -heap-arrays 1024 -fpp
	endif
endif

BUILD_HYPE ?= 0

# Makefile
all:        	hype hype_bmi_server hype_bmi_test

ifndef SKIP_HYPE_BUILD
hype:	    	hypecode
	cd hypecode && make comp=$(comp) config=$(config)

hypecode:
	wget https://sourceforge.net/projects/hype/files/release_hype_5_6_2/hype_5_6_2_src.tgz
	tar -xvzf hype_5_6_2_src.tgz
	mv src hypecode
	rm -f hypecode/*.mod && rm -f hypecode/*.obj && chmod u-x hypecode/*
	rm hype_5_6_2_src.tgz
else
hype:	    	hypecode
	@echo "Skipping hype source build"

hypecode:
	@echo "Skipping hype source download"
endif

hype_bmi_server:	hypelib.o hype_bmi.o run-bmi-server.o
	$(CXX) $(LDFLAGS) -o hype_bmi_server hypelib.o run-bmi-server.o hype_bmi.o $(shell find ./hypecode/ -name "*.o" ! -name "main.o") -lgfortran `pkg-config --libs protobuf grpc++ grpc` -Wl,--no-as-needed -lgrpc++_reflection -Wl,--as-needed -ldl -lgrpc4bmi -lbmic

hype_bmi_test:		hypelib.o mainbmi.o hype_bmi.o
	$(CXX) $(LDFLAGS) -o hype_bmi_test hypelib.o mainbmi.o hype_bmi.o $(shell find ./hypecode/ -name "*.o" ! -name "main.o") -lgfortran

hypelib.o:			hypecode hypelib.f90
	$(f90comp) -I hypecode -c $(switch) hypelib.f90

run-bmi-server.o:	run-bmi-server.cc
	$(CXX) $(CPPFLAGS) -c run-bmi-server.cc

mainbmi.o:			mainbmi.c
	$(CXX) $(CPPFLAGS) -c mainbmi.c

hype_bmi.o:			hype_bmi.cc hype_bmi.h
	$(CXX) $(CPPFLAGS) -c hype_bmi.cc

%.mod: 				%.f90
	$(f90comp) -c $(switch) $<

.PHONY : clean
clean:
	rm -f *.o *.mod
	rm -rf hypecode
	rm hype_bmi_server hype_bmi_test
