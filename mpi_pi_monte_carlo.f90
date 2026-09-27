! The following code is to parctice paraller coumputing from book "An Introduction to Parallel Programming"-2nd Edition by Peter S Pacheco and Matthew Malensek
! Chapter-3: Distributed memory programming with MPI
! Date: 26rd September 2026
! Author: Amit (amitdravid@outlook.com, amitdravid@ymail.com), The University of Alabama, AL, USA
!===============================================================================================================================================================
! Program Exercise 3.2: Calculating "pi" value using Monte-Carlo approach in Darts game
program mpi_monte_carlo_pi
    use mpi
    implicit none 
    !!! 1. Defining variables
    ! MPI related variables
    integer :: ierr, my_rank, comm_sz 
    integer :: status(MPI_STATUS_SIZE)
    ! Defining variables for computation, Note: "kind=8" make the compiler to use larger interger numbers  in int64 digit
    integer(kind=8) :: total_tosses, local_tosses
    integer(kind=8) :: total_in_circle, local_in_circle
    integer(kind=8) :: toss, base_tosses, remainder
    integer :: seed_size, i 
    integer, allocatable :: seed(:)
    real(kind=8) :: x,y 
    real(kind=8) :: pi_estimate 
    real(kind=8) :: start_time, finish_time, elapsed_time 
    !!! 2. Calling MPI functions !!!
    call MPI_Init(ierr)

    call MPI_Comm_rank(MPI_COMM_WORLD, my_rank, ierr)
    call MPI_Comm_size(MPI_COMM_WORLD, comm_sz, ierr)
    ! Rank 0 reads the total number of random dart tosses
    if (my_rank == 0) then 
        print*, "Enter total number of tosses:"
        read*, total_tosses
    end if 
    ! Boarcasting this to all processors or ranks
    call MPI_Bcast(total_tosses, 1, MPI_INTEGER8, 0, MPI_COMM_WORLD, ierr)

    !!! 3. Divide tosses across proceses/ranks
    base_tosses = total_tosses/comm_sz 
    remainder = mod(total_tosses, comm_sz)
    local_tosses = base_tosses
    if (my_rank < remainder) then
        local_tosses = local_tosses + 1     !Note:: 1_8 meaning that 1 is considered in int64
    end if 

    !!! 4. Giving every processor/rank or CPU a distinct random-number seed 
    call random_seed(size = seed_size)
    allocate(seed(seed_size))
    do i = 1, seed_size
        seed(i) = 100000*(my_rank + 1) + 7000*i 
    end do
    call random_seed(put=seed)
    deallocate(seed) 

    !!! 5. Each process simulate local_tosses darts 
    local_in_circle = 0 
    call MPI_Barrier(MPI_COMM_WORLD, ierr)
    start_time = MPI_Wtime()
    do toss = 1, local_tosses 
        call random_number(x)
        call random_number(y)
        x = 2.0d0 * x - 1.0d0
        y = 2.0d0 * y - 1.0d0

        if (x*x + y*y <= 1.0d0) then 
            local_in_circle = local_in_circle + 1
        end if 
    end do 

    finish_time = MPI_Wtime()
    elapsed_time = finish_time - start_time

    !!! 6. Adding all local hit counts on to rank 0
    call MPI_Reduce(local_in_circle, total_in_circle, 1, MPI_INTEGER8, MPI_SUM, 0, MPI_COMM_WORLD, ierr)

    !!! 7. Estimating pi value 
    if (my_rank == 0) then
        pi_estimate = 4.0d0*real(total_in_circle, kind=8)/real(total_tosses, kind=8)
        print *
        print *, "Total tosses         = ", total_tosses
        print *, "Hits inside circle   = ", total_in_circle
        print *, "Estimated pi         = ", pi_estimate
        print *, "Maximum elapsed time = ", finish_time, " seconds"
    end if 
    call MPI_Finalize(ierr)
end program mpi_monte_carlo_pi
