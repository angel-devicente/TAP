# -*- makefile -*-
################################################################
####
#### This makefile is part of `Parallel Programming in MPI and OpenMP'
#### by Victor Eijkhout, copyright 2013-9
#### eijkhout@tacc.utexas.edu
####
#### Makefile for MPI course exercises in C++
####
################################################################

info ::

COURSE = mpi
MODE = mpi

info ::
	@echo "make EXERCISE (where EXERCISE=${EXERCISES})"
EXERCISES = hello \
    commrank prime randommax jordan pingpong sendrecv stridesend \
    rightsend bucketblock bucketpipe bucketpipenonblock \
    isendirecv isendirecvarray procgrid procgridnonblock \
    rightput randomput onesidedbuild countdown \
    blockwrite scatterwrite cubegather \
    ibarrierupdate
info ::
	@echo "    [ OPTS=... ] (for instance: OPTS=\"-DN 100\")"
	@echo "    support options:"
	@echo "      bucketblock : N"

include ../course-support/Make.template
include ../course-support/Make.inc

.SECONDEXPANSION:
OPTS =
${EXERCISES} : $$@.cxx
	${MPI_CXX_COMPILER} \
	  ${CXXFLAGS} ${OPTS} \
	  -o $@ $^

clean ::
	@/bin/rm -f *.out[0-9]*

####
#### Regression tests
####

include ../course-support/Make.mpi_regress
