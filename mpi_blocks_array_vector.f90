! The following code/s are as parctice paraller coumputing from book "An Introduction to Parallel Programming"-2nd Edition by Peter S Pacheco and Mathhew Malensek
! Chapter-3: Distributed memory programming with MPI
! Date: 2nd September 2026
! Author: Amit (amitdravid@outlook.com, amitdravid@ymail.com), The University of Alabama, AL, USA
!=======================================================================================================================================================================
! Exercise 3.18: Derived data type MPI_Type_vector
! Global vector of N doubles is distributed block-cyclically
! across comm_sz processes with block size BS.
!  MPI_Type_vector(local_n_blocks, BS, comm_sz*BS, MPI_DOUBLE, ...)
program mpi_block_cycle_vector
    use mpi
    implicit none
 
    integer, parameter :: dp = kind(1.0d0)
 
    integer :: my_rank, comm_sz, ierr
    integer :: n, bs, local_n, local_n_blocks
    integer :: vect_mpi_t
    real(dp), allocatable :: local_x(:)
 
    call MPI_Init(ierr)
    call MPI_Comm_rank(MPI_COMM_WORLD, my_rank, ierr)
    call MPI_Comm_size(MPI_COMM_WORLD, comm_sz, ierr)
 
    ! ---- problem size: matches the example in the exercise ----
    n  = 18
    bs = 2
    local_n        = n / comm_sz
    local_n_blocks = local_n / bs
 
    allocate(local_x(local_n))
 
    ! ---- build the derived datatype (process 0 only needs it,
    !      but it's cheap/harmless to build everywhere) ----
    call MPI_Type_vector(local_n_blocks, bs, comm_sz * bs, &
                          MPI_DOUBLE_PRECISION, vect_mpi_t, ierr)
    call MPI_Type_commit(vect_mpi_t, ierr)
 
    call Read_vector(local_x, local_n, n, bs, local_n_blocks, &
                      my_rank, comm_sz, vect_mpi_t, MPI_COMM_WORLD)
 
    call Print_vector(local_x, local_n, n, bs, local_n_blocks, &
                       my_rank, comm_sz, vect_mpi_t, MPI_COMM_WORLD, &
                       'The vector read back is')
 
    call MPI_Type_free(vect_mpi_t, ierr)
    deallocate(local_x)
    call MPI_Finalize(ierr)

contains
    subroutine Read_vector(local_x, local_n, n, bs, local_n_blocks, &
        my_rank, comm_sz, vect_mpi_t, comm)
    integer, intent(in)  :: local_n, n, bs, local_n_blocks
    integer, intent(in)  :: my_rank, comm_sz, vect_mpi_t, comm
    real(dp), intent(out) :: local_x(local_n)

    real(dp), allocatable :: global_x(:)
    integer :: q, blk, src_start, dst_start, ierr

    if (my_rank == 0) then
    allocate(global_x(n))

    print '(A,I0,A)', 'Enter the ', n, ' elements of the vector:'
    read *, global_x

    ! process 0's own scattered chunk -> contiguous local_x
    do blk = 0, local_n_blocks - 1
    src_start = blk * comm_sz * bs + 1   ! rank 0 offset = 0
    dst_start = blk * bs + 1
    local_x(dst_start:dst_start + bs - 1) = &
    global_x(src_start:src_start + bs - 1)
    end do

    ! one send per OTHER process, using the derived datatype;
    ! starting address = global_x(q*bs + 1) gives the offset,
    ! the datatype itself already describes the whole strided
    ! pattern of blocks belonging to process q
    do q = 1, comm_sz - 1
    call MPI_Send(global_x(q * bs + 1), 1, vect_mpi_t, &
            q, 0, comm, ierr)
    end do

    deallocate(global_x)
    else
    ! a single contiguous receive - this process only ever
    ! sees its own local_n values, laid out contiguously
    call MPI_Recv(local_x, local_n, MPI_DOUBLE_PRECISION, &
        0, 0, comm, MPI_STATUS_IGNORE, ierr)
    end if
    end subroutine Read_vector


    ! --------------------------------------------------------
    ! Print_vector: mirror image of Read_vector.
    !   - every other process q : ONE MPI_Send (contiguous)
    !   - process 0 : ONE MPI_Recv per other process, using the
    !                 derived datatype, straight into the right
    !                 strided slot of the global array; its own
    !                 chunk is placed back with a manual loop
    ! --------------------------------------------------------
    subroutine Print_vector(local_x, local_n, n, bs, local_n_blocks, &
            my_rank, comm_sz, vect_mpi_t, comm, title)
    integer, intent(in)  :: local_n, n, bs, local_n_blocks
    integer, intent(in)  :: my_rank, comm_sz, vect_mpi_t, comm
    real(dp), intent(in) :: local_x(local_n)
    character(len=*), intent(in) :: title

    real(dp), allocatable :: global_x(:)
    integer :: q, blk, src_start, dst_start, ierr

    if (my_rank == 0) then
    allocate(global_x(n))

    ! process 0's own contiguous local_x -> scattered global_x
    do blk = 0, local_n_blocks - 1
    dst_start = blk * comm_sz * bs + 1
    src_start = blk * bs + 1
    global_x(dst_start:dst_start + bs - 1) = &
    local_x(src_start:src_start + bs - 1)
    end do

    ! one receive per OTHER process, using the derived datatype
    do q = 1, comm_sz - 1
    call MPI_Recv(global_x(q * bs + 1), 1, vect_mpi_t, &
            q, 0, comm, MPI_STATUS_IGNORE, ierr)
    end do

    print '(A)', title
    print '(*(F8.2))', global_x

    deallocate(global_x)
    else
    ! a single contiguous send of this process's local data
    call MPI_Send(local_x, local_n, MPI_DOUBLE_PRECISION, &
        0, 0, comm, ierr)
    end if
    end subroutine Print_vector
end program mpi_block_cycle_vector
