! The following code/s are as parctice paraller coumputing from book "An Introduction to Parallel Programming"-2nd Edition by Peter S Pacheco and Mathhew Malensek
! Chapter-3: Distributed memory programming with MPI
! Date: 2nd September 2026
! Author: Amit (amitdravid@outlook.com, amitdravid@ymail.com), The University of Alabama, AL, USA
!=======================================================================================================================================================================
! Exercise 3.11: Prefix sum using MPI
! Prefix sum is a generalization of global sum, rather than simply finding the sum of n values.
program mpi_prefix_sum
    use mpi ! MPI called
    implicit none
    ! Defining variables
    integer, parameter :: dp =kind(1.0d0) ! dp is a varaible which is constant, it stad for double-precision
    integer, parameter :: count = 5 ! elements per process
    integer :: ierr, my_rank, comm_sz, i 
    real(dp) :: x(count), y(count)
    real(dp) ::local_total, offset, scan_result
    real(dp), allocatable :: all_x(:), all_y(:)
    integer ::seed_size
    integer, allocatable :: seed(:)
    ! Calling MPI functions
    call MPI_init(ierr)
    call MPI_Comm_rank(MPI_COMM_WORLD, my_rank, ierr)
    call MPI_comm_size(MPI_COMM_WORLD, comm_sz, ierr)
    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! Actual code start
    ! Generating seeds
    call random_seed(size=seed_size)
    allocate(seed(seed_size))
    seed  = 99999+my_rank*5555+[(i, i = 1, seed_size)]
    call random_seed(put = seed)
    call random_number(x)
    x = x*10.0_dp ! Note: Appending _dp to a number tells the compiler to treat that specific number with double precision, preventing any accidental loss of accuracy
    !(a) Serial-Program: prefix sum of this process's local array
    y(1) = x(1)
    do i = 2, count
        y(i) = y(i - 1) + x(i)
    end do
    local_total = y(count)
    ! (d) Prarallel-Program Using MPI_Scan:Inclusive scan of the per-process totals 
    call MPI_Scan(local_total, scan_result, 1, MPI_DOUBLE_PRECISION, MPI_SUM, MPI_COMM_WORLD, ierr)
    offset = scan_result - local_total   ! sum of all earlier processes
    y = y + offset                       ! promote local -> global prefix sums
    ! Gathering everthing to the rank "0" for tidy, ordered printing
    if (my_rank == 0) then
        allocate(all_x(count*comm_sz), all_y(count*comm_sz))
    else
        allocate(all_x(1), all_y(1))
    end if
 
    call MPI_Gather(x, count, MPI_DOUBLE_PRECISION, &
                     all_x, count, MPI_DOUBLE_PRECISION, &
                     0, MPI_COMM_WORLD, ierr)
    call MPI_Gather(y, count, MPI_DOUBLE_PRECISION, &
                     all_y, count, MPI_DOUBLE_PRECISION, &
                     0, MPI_COMM_WORLD, ierr)
 
    if (my_rank == 0) then
        print '(A,I0,A,I0,A)', 'Global array has ', comm_sz, &
              ' processes x ', count, ' elements each'
        print '(A)', ''
        print '(A)', 'x  (original values):'
        print '(*(F9.3))', all_x
        print '(A)', ''
        print '(A)', 'y  (prefix sums via MPI_Scan):'
        print '(*(F9.3))', all_y
    end if
    deallocate(all_x, all_y, seed)
    call MPI_Finalize(ierr)
end program mpi_prefix_sum
