! The following code/s are as parctice paraller coumputing from book "An Introduction to Parallel Programming"-2nd Edition by Peter S Pacheco and Mathhew Malensek
! Chapter-3: Distributed memory programming with MPI
! Date: 11th July 2026
! Author: Amit (amitdravid@outlook.com, amitdravid@ymail.com), The University of Alabama, AL, USA
!=======================================================================================================================================================================
!Program 3.2: Trapezoidal rule using MPI-version1 (Trapz-V1)
!Note: The effort has been made to put all the inputs or all inputs call via same processesor or core. To do that get_input function is written
module trap_module
    implicit none
  contains
  
    real function f(x) result(val)
      implicit none
      real, intent(in) :: x
      val = x*x
    end function f
  
    real function trapz(left_endpt, right_endpt, trap_count, base_len) result(estimate)
      implicit none
      real, intent(in) :: left_endpt, right_endpt, base_len
      integer, intent(in) :: trap_count
      real :: x
      integer :: i
  
      estimate = (f(left_endpt) + f(right_endpt)) / 2.0
  
      do i = 1, trap_count - 1
        x = left_endpt + i * base_len
        estimate = estimate + f(x)
      end do
  
      estimate = estimate * base_len
    end function trapz
  
  end module trap_module

  program mpi_trapz
    use mpi
    use trap_module
    implicit none
  
    integer :: ierr, my_rank, comm_sz
    integer :: n_p, local_n, i
    real :: a_p, b_p, h
    real :: local_a, local_b, local_int, total_int
  
    call MPI_Init(ierr)
    call MPI_Comm_rank(MPI_COMM_WORLD, my_rank, ierr)
    call MPI_Comm_size(MPI_COMM_WORLD, comm_sz, ierr)
  
    if (my_rank == 0) then
       print *, "Enter left endpoint, right endpoint, and number of trapezoids:"
       read(*,*) a_p, b_p, n_p
    end if
  
    call MPI_Bcast(a_p, 1, MPI_REAL, 0, MPI_COMM_WORLD, ierr)
    call MPI_Bcast(b_p, 1, MPI_REAL, 0, MPI_COMM_WORLD, ierr)
    call MPI_Bcast(n_p, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)
  
    h = (b_p - a_p) / real(n_p)
    local_n = n_p / comm_sz
  
    local_a = a_p + my_rank * local_n * h
    local_b = local_a + local_n * h
  
    local_int = trapz(local_a, local_b, local_n, h)

    call MPI_Reduce(local_int, total_int, 1, MPI_REAL, MPI_SUM, 0, MPI_COMM_WORLD, ierr)

    if (my_rank == 0) then
      print *, "With n = ", n_p, " trapezoids,"
      print *, "estimate of the integral from ", a_p, " to ", b_p, " = ", total_int
    end if
  
    call MPI_Finalize(ierr)
  end program mpi_trapz
  !! Alaternative of line 69 to 47
  !if (my_rank /= 0) then
  !  call MPI_Send(local_int, 1, MPI_REAL, 0, 0, MPI_COMM_WORLD, ierr)
 !else
  !  total_int = local_int
   ! do i = 1, comm_sz - 1
  !     call MPI_Recv(local_int, 1, MPI_REAL, i, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE, ierr)
  !     total_int = total_int + local_int
  !  end do
 
   ! print *, "With n = ", n_p, " trapezoids,"
  !  print *, "estimate of the integral from ", a_p, " to ", b_p, " = ", total_int
 !end if