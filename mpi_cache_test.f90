! The following code/s are as parctice paraller coumputing from book "An Introduction to Parallel Programming"-2nd Edition by Peter S Pacheco and Mathhew Malensek
! Chapter-2: Parallel Hardware and Parallel Software 
! Date: 13th Febuary 2026
! Author: Amit (amitdravid@outlook.com, amitdravid@ymail.com), The University of Alabama, AL, USA
!================================================================================================================================================================

! Example-1: Testing use of Machine's Cache
program cache_test
    implicit none
    ! Defining variable
    real:: tik,tok, t1
    integer, allocatable:: A(:,:), x(:), y(:)
    integer:: m, n, i, j, k,t
    ! Input
    print*, "Enter the size of the A-matrix, m and n:"
    read*, m, n 
    allocate(A(m,n), x(n), y(m))
    print*, "Enter the number of iterations:"
    read*, t
    ! Initialize arrays
    A = 1.0
    x = 1.0
    y = 0.0
    ! ================= ROW-MAJOR ACCESS (GOOD CACHE) =================
    call cpu_time(tik) ! To start-CPU time
    
    do k=1,t
        do i=1,m 
            do j=1,n
                y(i) = y(i) + A(i,j) * x(j)
            end do 
        end do 
    end do 
   
    call cpu_time(tok) ! To finish-CPU tim
    t1 = tok - tik
    print '(A, F10.5)', "Row-wise time(s): ", t1
    
    ! ================= COLUMN-MAJOR ACCESS (BAD CACHE) =================
    ! y = 0.0
    ! call cpu_time(tik) ! To start-CPU time
    
    ! do k=1,t
    !     do i=1,n
    !         do j=1,m
    !             y(i) = y(i) + A(i,j) * x(j)
    !         end do 
    !     end do 
    ! end do 
   
    ! call cpu_time(tok)
    ! t2 = tok - tik
    ! print '(A, F10.5)', "Column-wise time(s): ", t2

    ! !--------------------------------------------------

    ! print '(A, F10.5)',"Fraction difference in time:", (abs(t1-t2)/t1)
end program cache_test

