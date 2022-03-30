# Makefile to compile conversion utilities. You need to set include and library paths for NETCDF
#

PLATFORM := $(shell uname -s)

# uncomment line for your platform or add your own:

#---------------------------------------------------------
# AIX 

ifeq ($(PLATFORM),AIX)

LIB_NETCDF = /usr/local/xlf/lib
INC_NETCDF = /usr/local/xlf/include

FF = xlf90_r -qfixed -I$(INC_NETCDF) 
LDFLAGS = -L$(LIB_NETCDF) -lnetcdf

endif
#---------------------------------------------------------
# SGI IRIX64


ifeq ($(PLATFORM),IRIX64)


LIB_NETCDF = /usr/local/lib

FF = f90 -fixedform -extend_source -I$(INC_NETCDF) 
LDFLAGS = -L$(LIB_NETCDF) -lnetcdf

endif
#---------------------------------------------------------
# Mac OS X 


ifeq ($(PLATFORM),Darwin)


INC_NETCDF      := /usr/local/netcdf/include
LIB_NETCDF       := /usr/local/netcdf/lib

FF = mpif90 -fixed -O3 -pad -extend_source -I$(INC_NETCDF)

#FF = mpif90 -fixed -extend_source -I$(INC_NETCDF)

#FF = xlf90 -qfixed -I$(INC_NETCDF) 

#FF = f95 -qfixed -kind=byte -gline -I$(INC_NETCDF) 

#FF = gfortran  -O3 -ffixed-form -ffixed-line-length-0 -I$(INC_NETCDF)

LDFLAGS = -L${LIB_NETCDF} -lnetcdf


endif
#---------------------------------------------------------
#
#ifeq ($(PLATFORM),Linux)

#SDSC BlueGene

#INC_NETCDF   := /apps/nco/netcdf-final/netcdf-3.6.2/include
#LIB_NETCDF   := /apps/nco/netcdf-final/netcdf-3.6.2/lib

#INC_NETCDF   := /gpfs/home2/marat/netcdf-vis-3.6.2/include
#LIB_NETCDF   := /gpfs/home2/marat/netcdf-vis-3.6.2/lib

#FF = /opt/ibmcmp/xlf/bg/10.1/bin/xlf90 -qsuffix=f=f -qfixed=132 -I$(INC_NETCDF) 
#DFLAGS = -L${LIB_NETCDF} -lnetcdf 
#LDFLAGS = -L${LIB_NETCDF} -L/usr/lib -lnetcdf -lpthread
#---------------------------------------------------------
#
# Linux Cluster (seawulf@sunysb)
#

#INC_NETCDF = /nfs/user04/marat/local/include
#LIB_NETCDF = /nfs/user04/marat/local/lib


#FF = ifort -O3 -fixed -extend_source -I${INC_NETCDF}
#LDFLAGS = -L${LIB_NETCDF} -lnetcdf

#---------------------------------------------------------
#
# UW Milwaukee Linux machines
#
#INC_NETCDF = /usr/local/netcdf-g95/include
#LIB_NETCDF = /usr/local/netcdf-g95/lib
#FF = g95 -g -ftrace=full -fbounds-check -msse2 -O3 -I${INC_NETCDF}
#LDFLAGS = -L${LIB_NETCDF} -lnetcdf


### MWSWONG: on NERSC/edison
ifeq ($(PLATFORM),Linux)
#ifeq ($(PLATFORM),NOTNOT)
INC_NETCDF = ${NETCDF_DIR}/include 
LIB_NETCDF = ${NETCDF_DIR}/lib 
FF = ftn -C -I${INC_NETCDF} -DUWM_STATS -DPNNL_STATS#sunf95 -g -C -fpp -xO3 -xvector=simd -ftrap=common -I${INC_NETCDF} -DUWM_STATS
LDFLAGS = -L${LIB_NETCDF} -lnetcdf  

endif

# UWM's HPC Cluster
#ifeq ($(PLATFORM),Linux)
ifeq ($(PLATFORM),NOTNOT)

INTEL_MPI = /sharedapps/ICC/11.1/openmpi/1.6.5

LIB_MPI = $(INTEL_MPI)/lib
INC_MPI = $(INTEL_MPI)/include

LIB_NETCDF = /sharedapps/ICC/11.1/netcdf/4.3.0/lib
INC_NETCDF = /sharedapps/ICC/11.1/netcdf/4.3.0/include

LIB_ZLIB = /sharedapps/ICC/11.1/zlib/1.2.8/lib
INC_ZLIB = /sharedapps/ICC/11.1/zlib/1.2.8/include

LIB_HDF5 = /sharedapps/ICC/11.1/hdf5/1.8.12/lib
INC_HDF5 = /sharedapps/ICC/11.1/hdf5/1.8.12/include

LIB_LAPACK = -mkl=sequential

FF = ifort -g -C -fpp -I${INC_NETCDF} -I${INC_MPI} -I${INC_NETCDF} -I${INC_ZLIB} -I${INC_HDF5} -DUWM_STATS -DNO_READ
LDFLAGS = -L${LIB_MPI} -L${LIB_LAPACK} -L${LIB_ZLIB} -L${LIB_HDF5} -L${LIB_NETCDF} -lnetcdf -lnetcdff  

endif
#---------------------------------------------------------

VPATH = ./SRC

all: bin2D2nc bin3D2nc 2Dbin2nc 2Dbin2nc_mean bin3D2nc_mean com3D2bin 2Dcom2nc 2Dcom2nc_mean com3D2nc com3D2nc_mean com2D2nc stat2nc isccp2nc com3D2nc_sep 2Dbin2nc_sep 2Dcom_sep2one 2Dbin_sep2one com3D_sep2one bin3D_sep2one glue_movie_raw

.F:   
	$(FF) -o $@ -I./SRC $< ./SRC/hbuf_lib.F ./SRC/cape.f ./SRC/cin.f $(LDFLAGS) 

.f:   
	$(FF) -o $@ -I./SRC $< ./SRC/hbuf_lib.F ./SRC/cape.f ./SRC/cin.f $(LDFLAGS) 

clean: 
	rm bin* com* stat* 2* isccp* *.o 
