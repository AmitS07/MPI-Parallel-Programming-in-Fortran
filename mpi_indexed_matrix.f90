! The following code is to parctice paraller coumputing from book "An Introduction to Parallel Programming"-2nd Edition by Peter S Pacheco and Mathhew Malensek
! Chapter-3: Distributed memory programming with MPI
! Date: 23rd September 2026
! Author: Amit (amitdravid@outlook.com, amitdravid@ymail.com), The University of Alabama, AL, USA
!===============================================================================================================================================================
! Exercise-3.19: Derived data type from arbitrary array using "MPI_Type_indexed"
! Here, displacements are measured in units of old_mpi_t---not bytes
program mpi_indexed_upper_triangle
    use mpi
    implicit none

    integer :: ierr, my_rank, comm_sz !comm_sz is the number of processes in MPI_COMM_WORLD
    integer :: n, i, j, total_elements
    integer :: upper_type
    integer :: status(MPI_STATUS_SIZE)
    integer, allocatable :: matrix(:)
    integer, allocatable :: blocklengths(:)
    integer, allocatable :: displacements(:)
    integer, allocatable :: upper_triangle(:)

    call MPI_Init(ierr) ! It is called to start the MPI
    call MPI_Comm_rank(MPI_COMM_WORLD, my_rank, ierr)
    call MPI_Comm_size(MPI_COMM_WORLD, comm_sz, ierr)

    if (comm_sz /= 2) then
        if (my_rank == 0) then
            print *, "Run this program using exactly 2 processes."
            print *, "Example: mpirun -np 2 ./indexed_upper_triangle"
        end if
        call MPI_Finalize(ierr) ! it is called to exit the MPI
        stop
    end if

    ! Rank 0 reads n and the n*n values as a 1-D row-major array.
    if (my_rank == 0) then
        print *, "Enter matrix dimension n:"
        read (*, *) n
    end if

    call MPI_Bcast(n, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)

    total_elements = n * (n + 1) / 2

    if (my_rank == 0) then
        allocate(matrix(n * n))
        allocate(blocklengths(n))
        allocate(displacements(n))

        print *, "Enter ", n*n, " matrix elements in row-major order:"
        read (*, *) matrix
        ! MPI displacements here start from 0, even though Fortran arrays
        ! normally start from index 1.
        ! Define one contiguous block for the upper portion of each row.
        do i = 0, n - 1
            blocklengths(i + 1) = n - i
            displacements(i + 1) = i * n + i
        end do

        call MPI_Type_indexed(n, blocklengths, displacements, MPI_INTEGER, upper_type, ierr)
        call MPI_Type_commit(upper_type, ierr)

        ! One send: MPI gathers the selected, non-contiguous matrix values.
        call MPI_Send(matrix, 1, upper_type, 1, 0, MPI_COMM_WORLD, ierr)

        call MPI_Type_free(upper_type, ierr)

        deallocate(matrix)
        deallocate(blocklengths)
        deallocate(displacements)

    else if (my_rank == 1) then
        allocate(upper_triangle(total_elements))

        ! Receive the extracted values as ordinary contiguous integers.
        call MPI_Recv(upper_triangle, total_elements, MPI_INTEGER, &
                      0, 0, MPI_COMM_WORLD, status, ierr)

        print *, "Rank 1 received upper-triangular values:"
        write (*, '(100(I6,1X))') upper_triangle

        print *, "Upper triangle displayed in matrix form:"
        j = 1
        do i = 1, n
            write (*, '(A, 100(I6,1X))') repeat(' ', 7 * (i - 1)), upper_triangle(j : j + n - i)
            j = j + n - i + 1
        end do

        deallocate(upper_triangle)
    end if

    call MPI_Finalize(ierr) ! it is called to exit the MPI
end program mpi_indexed_upper_triangle