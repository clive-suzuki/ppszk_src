#FC = gfortran
FC = ifort

TARGET = ${HOME}/bin/ppszk
OBJECTS = mod_VTK_IO.o mod_szk.o mod_globals.o prc_searcbShock.o main.o
MOD_FILES = mod_VTK_IO.mod mod_szk.mod mod_globals.mod prc_searcbShock.mod

FFLAGS =
LDFLAGS =


ifeq (${FC},gfortran)
	FFLAGS += -fimplicit-none
	FFLAGS += -fopenmp
endif

ifeq (${FC},ifort)
	FFLAGS += -parallel -qopenmp
#	FFLAGS += -mcmodel=large -shared-intel
#	FFLAGS += -ipo -inline-level=2 -inline-forceinline
	FFLAGS += -fpp
endif

.SUFFIXES : .o .f90
.f90.o:
	${FC} -c -fpp $<
${TARGET} : ${OBJECTS}
	${FC} -o $@ ${OBJECTS} ${LDFLAGS} ${FFLAGS}

.PHONY: clean
clean:
	${RM} ${TARGET} ${OBJECTS} ${MOD_FILES}
