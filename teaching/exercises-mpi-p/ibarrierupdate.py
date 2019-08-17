#!/usr/bin/env python

################################################################
################################################################
####
#### This program file is part of the book and course
#### "Parallel Computing"
#### by Victor Eijkhout, copyright 2013-8
####
#### ibarrierupdate.py : exercise for ibarrier testing
####
################################################################
################################################################

from mpi4py import MPI
import numpy as np
import random
import sys

comm = MPI.COMM_WORLD

nprocs = comm.Get_size()
procno = comm.Get_rank()

## Start the random number generator
random.seed(procno)

##
## How many processes are we going to send to?
##
n_destinations = int( random.random() * nprocs )
send_data = np.zeros(n_destinations,dtype=np.float32)
receive_data = np.zeros(1,dtype=np.float32)
send_requests = [ None ] * n_destinations

##
## Pick random processes to send to, and post Isend
##
for idestination in range(n_destinations):
    destination = int( random.random() * nprocs )
    print("[%d] send to %d" % (procno,destination))
    send_requests[idestination] \
        = comm.Isend( send_data[idestination], destination )

##
## When you're done sending, post a non-blocking barrier
print("[%d] posting barrier" % procno)
final_barrier = [ None ]
final_barrier = comm.Ibarrier()

step = 0
while True:
    ##
    ## Exercise part 1:
    ## -- use MPI_Test to determine when the barrier is completed;
    ##    in that case you can quite
    ##
    all_done_flag = False
#### your code here ####
    if all_done_flag:
        break

    ##
    ## Exercise part 2:
    ## -- use MPI_Iprobe to test if there is a message
    ## -- the message can come from anywhere, so
    ##    you need to inspect the status to find the source and tag
    ## -- if there is no message, skip to the next iteration
    ##
    message_flag = False
    status=MPI.Status()
    message_flag = comm.Iprobe(source=MPI.ANY_SOURCE,tag=MPI.ANY_TAG,status=status)
    if not message_flag:
        continue
    source = status.source
    comm.Recv(receive_data,source)
    print("[%d] received from %d" % (procno,source))

if procno==0:
    print("Finished")

