! The following code is to parctice paraller coumputing from book "An Introduction to Parallel Programming"-2nd Edition by Peter S Pacheco and Mathhew Malensek
! Chapter-3: Distributed memory programming with MPI
! Date: 25rd September 2026
! Author: Amit (amitdravid@outlook.com, amitdravid@ymail.com), The University of Alabama, AL, USA
!===============================================================================================================================================================
! Program Exercise: 3.5 Matrix-vector Multipication using a block-column distribution
program mpi_mat_vec_block_col
    use mpi
    implicit none
    ! Defining variables, CPU, and loop related
    integer :: ierr, my_rank, comm_sz
    integer :: n, local_n 
    integer :: i, j, dest 
    integer :: status(MPI_STATUS_SIZE)
    ! Defining variables, matrix related
    real, allocatable :: a(:,:) ! matrix -a(m, n)
    real, allocatable :: x(:) ! vector -x(n)
    real, allocatable :: y(:) ! vector-y(m)
    real, allocatable :: local_a(:,:) ! per CPU matrix -a(m, n)
    real, allocatable :: local_x(:) ! per CPU vector -x(n)
    real, allocatable :: local_y(:) ! per CPU vector-y(m)
    ! Calling MPI
    call MPI_Init(ierr)
    call MPI_Comm_rank(MPI_COMM_WORLD, my_rank, ierr)
    call MPI_Comm_size(MPI_COMM_WORLD,comm_sz,ierr)
    ! Input matrix  dimension and sending this info to all machine (processors)
    if (my_rank == 0) then
        print*, "Enter the sqaure matrix dimension (n):"
        read (*,*) n
    end if
    call MPI_Bcast(n, 1,MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)
    ! Checking whether matrix-dimension is divisible by "n" or not
    if (mod(n, comm_sz) /= 0) then
        if (my_rank == 0) then
            print *, "Error: n must be evenly divisible by comm_sz."
            print *, "n = ", n, ", comm_sz = ", comm_sz
        end if
        call MPI_Finalize(ierr)
        stop
    end if
    ! Dimension to each processor receive
    local_n = n/comm_sz
    !Alloacting the dimension to each local cpu
    allocate(local_a(n, local_n))
    allocate(local_x(local_n))
    allocate(local_y(n))
    ! Machine/Process rank "0" read matrix A and vector x, then distributes block columns of A and matching elements of x to other 
    if (my_rank == 0) then
        ! Allocating the matrix, vectors 
        allocate(a(n,n))
        allocate(x(n))
        allocate(y(n))
        ! Input matrix elements
        print*, "Enter matrix elements row-wise-row:"
        do i = 1,n 
            read*, a(i, 1:n) ! This make simple entry in row wise fashion
        end do
        ! Input vector elements
        print*, "Enter vector elements:"
        read*, x ! This make simple entry in row wise fashion
        ! Rank-0 cPU keep the 1st local_n columns of a, and vector x
        local_a = a(:, 1:local_n)
        local_x = x(1:local_n)
        ! Now send the other block-column of matrix (a) to other CPUs and corresponding vector x
        do dest = 1, comm_sz-1
            call MPI_Send(a(1,dest*local_n+1), n*local_n, MPI_REAL, dest, 0,MPI_COMM_WORLD, ierr) ! Sending to each CPU, the matrix column
            call MPI_Send(x(dest*local_n+1), local_n, MPI_REAL, dest, 1, MPI_COMM_WORLD, ierr) ! Sending to each CPU, the fulll vector
        end do
    else 
        call MPI_Recv(local_a, n*local_n, MPI_REAL, 0, 0, MPI_COMM_WORLD, status, ierr) ! for matrix (a) column
        call MPI_Recv(local_x, local_n, MPI_REAL, 0, 1, MPI_COMM_WORLD, status, ierr)
    end if 
    ! Computation of y vector
    ! local_y computation per CPUs
    do j = 1, local_n
        do i = 1, n 
            local_y(i) = local_y(i) + local_a(i,j)*local_x(j)
        end do 
    end do
    ! Total y vector, sum of all local_y, by summed by my_rank = 0, so 1st bring all local_ys of each CPU my_rank = 0
    call MPI_Reduce(local_y, y, n, MPI_REAL, MPI_SUM, 0, MPI_COMM_WORLD, ierr)

    if (my_rank == 0) then
        print*, "y = Ax:"
        do i = 1,n 
            write (*, '(F20.5)') y(i)
        end do
        deallocate(a)
        deallocate(x)
        deallocate(y)
    end if 
    deallocate(local_a)
    deallocate(local_x)
    deallocate(local_y)
    call MPI_Finalize(ierr)
end program mpi_mat_vec_block_col
