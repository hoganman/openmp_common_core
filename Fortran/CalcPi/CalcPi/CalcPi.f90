!  CalcPi.f90 
!
!  FUNCTIONS:
!  CalcPi - Entry point of console application.
!

!****************************************************************************
!
!  PROGRAM: CalcPi
!
!  PURPOSE:  Entry point for the console application.
!
!****************************************************************************

    program CalcPi

    !**************
    ! Serial verion
    !**************
    !USE, INTRINSIC :: ISO_C_BINDING ! Declares C kinds
    !implicit none
    !
    !! Variables
    !integer(kind=C_LONG)  :: NUM_STEPS = 100000
    !integer(kind=C_LONG)  :: step_index
    !real   (kind=C_DOUBLE):: STEP_SIZE
    !real   (kind=C_DOUBLE):: temp_term
    !real   (kind=C_DOUBLE):: sum = 0.
    !
    !! Body of CalcPi
    !STEP_SIZE = 1./NUM_STEPS
    !
    !do step_index = 1, NUM_STEPS
    !    temp_term = (step_index + 0.5) * STEP_SIZE
    !    sum = sum + 4.0/(1. + temp_term * temp_term)
    !end do
    !sum = sum * STEP_SIZE
    !
    !print '(a, f10.7, a)', 'The value of pi is', sum, '!'
    !**************
    ! End serial
    !**************
    
    
    !**************
    ! Parallel with false sharing
    !**************
    !use, intrinsic :: ISO_C_BINDING ! Declares C kinds
    !use omp_lib
    !implicit none
    !
    !! Variables
    !integer(kind=C_LONG), parameter  :: NUM_STEPS = 100000
    !integer(kind=C_LONG), parameter  :: NUM_THREADS = 1
    !integer(kind=C_LONG), parameter  :: PAD = 8
    !integer(kind=C_LONG)             :: step_index
    !integer(kind=C_LONG)             :: array_index
    !integer(kind=C_INT)              :: num_threads_shared = 0
    !integer(kind=C_INT)              :: num_threads_local
    !integer(kind=C_INT)              :: thread_id_local
    !
    !real   (kind=C_DOUBLE), parameter :: STEP_SIZE = 1. / NUM_STEPS
    !real   (kind=C_DOUBLE) :: temp_term
    !real   (kind=C_DOUBLE) :: total_sum = 0.
    !real   (kind=C_DOUBLE) :: sum_per_thread(NUM_THREADS, PAD)
    !
    !do array_index = 1, NUM_THREADS
    !    sum_per_thread(array_index, 1) = 0.
    !end do
    !call omp_set_num_threads(NUM_THREADS)
    !
    !
    !!$omp parallel default(private) shared(sum_per_thread, num_threads_shared)
    !thread_id_local = omp_get_thread_num()
    !num_threads_local = omp_get_num_threads()
    !if (thread_id_local .eq. 0) then
    !    num_threads_shared = num_threads_local
    !end if
    !do step_index = thread_id_local + 1, NUM_STEPS, num_threads_local
    !    temp_term = (step_index + 0.5) * STEP_SIZE
    !    sum_per_thread(thread_id_local + 1, 1) = &
    !        sum_per_thread(thread_id_local + 1, 1) + &
    !        4.0/(1. + temp_term * temp_term)
    !end do
    !!$omp end parallel
    !
    !do array_index = 1, num_threads_shared
    !    total_sum = total_sum + sum_per_thread(array_index, 1)
    !end do
    !total_sum = total_sum * STEP_SIZE
    !
    !print '(a, f, a)', 'The value of pi is ', total_sum, '!'
    !**************
    ! End Parallel with false sharing
    !**************

    !**************
    ! Parallel with critical
    !**************
    !use, intrinsic :: ISO_C_BINDING ! Declares C kinds
    !use omp_lib
    !implicit none
    !
    !! Variables
    !integer(kind=C_LONG), parameter  :: NUM_STEPS = 100000
    !integer(kind=C_LONG)             :: step_index
    !integer(kind=C_INT), parameter  :: NUM_THREADS = 4
    !integer(kind=C_INT)              :: num_threads_local
    !integer(kind=C_INT)              :: thread_id_local
    !
    !real   (kind=C_DOUBLE), parameter :: STEP_SIZE = 1. / NUM_STEPS
    !real   (kind=C_DOUBLE) :: temp_term, partial_sum
    !real   (kind=C_DOUBLE) :: total_sum = 0.
    !
    !call omp_set_num_threads(NUM_THREADS)
    !
    !!$omp parallel default(private) shared(total_sum)
    !thread_id_local = omp_get_thread_num()
    !num_threads_local = omp_get_num_threads()
    !partial_sum = 0.
    !do step_index = thread_id_local + 1, NUM_STEPS, num_threads_local
    !    temp_term = (step_index + 0.5) * STEP_SIZE
    !    partial_sum = partial_sum + 4.0/(1. + temp_term * temp_term)
    !end do
    !!$omp critical 
    !    total_sum = total_sum + partial_sum
    !!$omp end critical
    !!$omp end parallel
    !
    !total_sum = total_sum * STEP_SIZE
    !print '(a, f, a)', 'The value of pi is ', total_sum, '!'
    !**************
    ! End Parallel with critical
    !**************
    
    !**************
    ! Parallel worksharing
    !**************
    use, intrinsic :: ISO_C_BINDING ! Declares C kinds
    use omp_lib
    implicit none
    
    ! Variables
    integer(kind=C_LONG), parameter  :: NUM_STEPS = 100000
    integer(kind=C_LONG)             :: step_index
    integer(kind=C_INT), parameter   :: NUM_THREADS = 4
    real   (kind=C_DOUBLE), parameter :: STEP_SIZE = 1. / NUM_STEPS
    real   (kind=C_DOUBLE) :: temp_term, partial_sum = 0., total_sum
    real   (kind=C_DOUBLE) :: start_time, end_time, total_time
    
    call omp_set_num_threads(NUM_THREADS)
    start_time = omp_get_wtime()
    !$omp parallel do reduction(+:partial_sum)
    do step_index = 1, NUM_STEPS
        temp_term = (step_index + 0.5) * STEP_SIZE
        partial_sum = partial_sum + 4.0/(1. + temp_term * temp_term)
    end do 
    !$omp end parallel do
    end_time = omp_get_wtime()
    
    total_sum = partial_sum * STEP_SIZE
    total_time = end_time - start_time
    print '(a, f, a, f)', 'The value of pi is ', total_sum, ' with runtime ', total_time, '!'
    !**************
    ! End Parallel worksharing
    !**************
    
    end program CalcPi

