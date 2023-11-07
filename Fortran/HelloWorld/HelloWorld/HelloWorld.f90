!  HelloWorld.f90 
!
!  FUNCTIONS:
!  HelloWorld - Entry point of console application.
!

!****************************************************************************
!
!  PROGRAM: HelloWorld
!
!  PURPOSE:  Entry point for the console application.
!
!****************************************************************************

    program HelloWorld

    use omp_lib
    implicit none

    ! Variables
    integer :: num

    call omp_set_num_threads(2)
    ! Body of HelloWorld
    !$omp parallel default(private)
    num = omp_get_thread_num()
    print '(a, i2, a)', 'Hello (', num, ') '
    print '(a, i2, a)', 'World (', num, ')!'
    !$omp end parallel

    end program HelloWorld

! Expected Output
! ===============
! Hello ( 0)
! World ( 0)!
! Hello ( 1)
! World ( 1)!