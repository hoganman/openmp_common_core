!  CalcPiTasked.f90 
!
!  FUNCTIONS:
!  CalcPiTasked - Entry point of console application.
!

!****************************************************************************
!
!  PROGRAM: CalcPiTasked
!
!  PURPOSE:  Entry point for the console application.
!
!****************************************************************************

    recursive real(kind=C_DOUBLE) function integrant(start_step, end_step, step_size, min_steps)
    
        use, intrinsic :: ISO_C_BINDING ! Declares C kinds
        use omp_lib
        implicit none
        
        integer(kind=C_LONG), intent(in) :: start_step, end_step, min_steps
        real(kind=C_DOUBLE), intent(in) :: step_size
        integer(kind=C_LONG) :: step_index, index_block
        real(kind=C_DOUBLE) :: temp, partial_sum, partial_sum_1, partial_sum_2
        
        if ((end_step - start_step) / 2 < min_steps) then
            partial_sum = 0.
            do step_index = start_step, end_step
                temp = step_size * (step_index + 0.5)
                partial_sum = partial_sum + 4. / (1. + temp * temp)
            end do
        else 
            index_block = end_step - start_step
            !$omp task shared(partial_sum_1)
            partial_sum_1 = integrant(start_step, end_step - index_block / 2, step_size, min_steps)
            !$omp end task
            !$omp task shared(partial_sum_2)
            partial_sum_2 = integrant(end_step - index_block / 2 + 1, end_step, step_size, min_steps)
            !$omp end task
            !$omp taskwait
            partial_sum = partial_sum_1 + partial_sum_2
        end if
        integrant = partial_sum
        
    end function integrant
    
    program CalcPiTasked

    use, intrinsic :: ISO_C_BINDING ! Declares C kinds
    use omp_lib
    implicit none
    
    ! Variables
    integer(kind=C_LONG), parameter  :: NUM_STEPS = 1024*1024*1024
    integer(kind=C_LONG)   :: NUM_THREADS = 4, START_INDEX = 1, MIN_STEPS = 256 * 1024
    real   (kind=C_DOUBLE) :: STEP_SIZE = 1. / NUM_STEPS
    real   (kind=C_DOUBLE) :: temp_term, partial_sum = 0., total_sum
    real   (kind=C_DOUBLE) :: start_time, end_time, total_time, integrant
    
    call omp_set_num_threads(NUM_THREADS)
    start_time = omp_get_wtime()
    !$omp parallel shared(partial_sum)
    !$omp single
    partial_sum = integrant(START_INDEX, NUM_STEPS, STEP_SIZE, MIN_STEPS)
    !$omp end single
    !$omp end parallel
    end_time = omp_get_wtime()
    
    total_sum = partial_sum * STEP_SIZE
    total_time = end_time - start_time
    print '(a, f, a, f)', 'The value of pi is ', total_sum, ' with runtime ', total_time, '!'
    !**************
    ! End 
    !**************

    end program CalcPiTasked

