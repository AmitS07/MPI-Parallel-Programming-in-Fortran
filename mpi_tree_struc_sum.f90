! The following code is to parctice paraller coumputing from book "An Introduction to Parallel Programming"-2nd Edition by Peter S Pacheco and Mathhew Malensek
! Chapter-3: Distributed memory programming with MPI
! Date: 23rd September 2026
! Author: Amit (amitdravid@outlook.com, amitdravid@ymail.com), The University of Alabama, AL, USA
!===============================================================================================================================================================
! Program exercise 3.3: Tree-structure global sum
program mpi_tree_sum_anysize
    use mpi
    implicit none
    ! Defining variables
    integer :: ierr, my_rank, comm_sz
    integer :: local_value, global_sum, received_value
    integer :: step, source, dest, tag
    integer :: status(MPI_STATUS_SIZE)
    logical :: active
    ! Calling MPI
    call MPI_Init(ierr)
    call MPI_Comm_rank(MPI_COMM_WORLD, my_rank, ierr)
    call MPI_Comm_size(MPI_COMM_WORLD, comm_sz, ierr)
    ! Each rank contribution
    local_value = my_rank + 1
    global_sum = local_value 
    ! Tree-structured reduction
    step = 1
    active = .true.
    tag = 0

    do while (step<comm_sz .and. active)
        if (mod(my_rank, 2*step) == 0) then  !The receiver condition
            source = my_rank + step ! The receivers
            if (source<comm_sz) then 
                call MPI_Recv(received_value, 1, MPI_INTEGER, source, tag, MPI_COMM_WORLD, status, ierr)
                global_sum = global_sum +received_value 
            end if 
        else if (mod(my_rank, 2*step) == step) then  !The sender condition
            dest = my_rank - step ! The Senders
            call MPI_Send(global_sum, 1, MPI_INTEGER, dest, tag, MPI_COMM_WORLD, ierr)
            active = .false.
        end if 
        step = 2*step
    end do 

    if (my_rank == 0) then 
        print*, "Tree-Structured global sum = ", global_sum 
        print*, "Expected Sum               = ", comm_sz*(comm_sz+1)/2
    end if 

    call MPI_Finalize(ierr)
end program mpi_tree_sum_anysize