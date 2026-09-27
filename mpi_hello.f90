! The following code/s are as parctice paraller coumputing from book "An Introduction to Parallel Programming"-2nd Edition by Peter S Pacheco and Mathhew Malensek
! Chapter-3: Distributed memory programming with MPI
! Date: 26th May 2026
! Author: Amit (amitdravid@outlook.com, amitdravid@ymail.com), The University of Alabama, AL, USA
!================================================================================================================================================================
!Program: 3.1
program mpi_hello
    ! Importing MIP library
    use mpi
    implicit none
    ! Defining variables
    integer :: ierr, rank, nprocs, i
    integer, parameter :: max_string = 50
    character (len=max_string):: Greeting
    integer :: status(MPI_STATUS_SIZE)
    ! Calling MPI Communicator aka MPI_COMM_WORLD
    call MPI_init(ierr)
    call MPI_comm_rank(MPI_COMM_WORLD, rank, ierr)
    call MPI_comm_size(MPI_COMM_WORLD, nprocs, ierr)
    !
    write(Greeting, '(A, I0, A, I0)') 'Greetings from processor ', rank, 'of', nprocs

    if (rank /=0) then
        call MPI_Send(Greeting, max_string, MPI_CHAR, 0, 0, MPI_COMM_WORLD, ierr)
    else 
        print*, trim(Greeting)
        do i = 1, nprocs-1
            call MPI_Recv(Greeting, max_string, MPI_CHAR, i, 0, MPI_COMM_WORLD, status, ierr)
            print*, trim(Greeting)
        end do
    end if
    call MPI_finalize(ierr)
end program mpi_hello