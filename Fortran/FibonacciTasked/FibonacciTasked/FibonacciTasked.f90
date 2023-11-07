!  FibonacciTasked.f90 
!
!  FUNCTIONS:
!  FibonacciTasked - Entry point of console application.
!

!****************************************************************************
!
!  PROGRAM: FibonacciTasked
!
!  PURPOSE:  Entry point for the console application.
!
!****************************************************************************

    
recursive integer(kind=C_INT) function fib(num)
    use, intrinsic :: ISO_C_BINDING ! Declares C kinds
    use omp_lib
    implicit none
    integer(kind=C_INT), intent(in)  :: num
    integer(kind=C_INT) :: fib_m1, fib_m2
        
    if (num < 2) then
        fib = num
    else
        !$omp task shared(fib_m1)
        fib_m1 = fib(num - 1)
        !$omp end task
        
        !$omp task shared(fib_m2)
        fib_m2 = fib(num -2)
        !$omp end task
        
        !$omp taskwait
        fib = fib_m1 + fib_m2
    endif
end function fib
    
    
program FibonacciTasked
    
    use, intrinsic :: ISO_C_BINDING ! Declares C kinds 
    use omp_lib
    implicit none
    
    ! Variables
    integer(kind=C_INT) :: fib, fib_num, num = 34
    real(kind=C_DOUBLE) :: start_time, end_time, delta_time
    
    ! Body of FibonacciTasked
    call omp_set_num_threads(4)
    start_time = omp_get_wtime()
    !$omp parallel 
    !$omp single
    fib_num = fib(num)
    !$omp end single
    !$omp end parallel
    end_time = omp_get_wtime()
    
    delta_time = end_time - start_time
    print *, 'Fn(', num, ') = ', fib_num, ', time delta = ', delta_time
    
end program FibonacciTasked
