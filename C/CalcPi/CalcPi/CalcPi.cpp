// CalcPi.cpp : This file contains the 'main' function. Program execution begins and ends there.
//

//#include <iostream>
#include <stdio.h>
#include "omp.h"

#define NUM_THREADS 4

static long const NUM_STEPS = 100000;
static double const STEP_SIZE = 1. / NUM_STEPS;


int main()
{
    //int num_threads_shared = 0;
    double total_sum = 0.;
    omp_set_num_threads(NUM_THREADS);
    #pragma omp parallel shared(total_sum, STEP_SIZE)
    {
        int const num_threads = omp_get_num_threads();
        int const thread_id = omp_get_thread_num();
        //if (thread_id == 0) num_threads_shared = num_threads;
        for (int step_index = thread_id; step_index < NUM_STEPS; step_index = step_index + NUM_THREADS)
        {
            double temp = (step_index + 0.5) * STEP_SIZE;
            #pragma omp critical
            total_sum += 4.0 / (1.0 + temp * temp);
        }
    }//end omp parallel
    total_sum *= STEP_SIZE;
    
    printf("The value of Pi is %f!", total_sum);
}

// Run program: Ctrl + F5 or Debug > Start Without Debugging menu
// Debug program: F5 or Debug > Start Debugging menu

// Tips for Getting Started: 
//   1. Use the Solution Explorer window to add/manage files
//   2. Use the Team Explorer window to connect to source control
//   3. Use the Output window to see build output and other messages
//   4. Use the Error List window to view errors
//   5. Go to Project > Add New Item to create new code files, or Project > Add Existing Item to add existing code files to the project
//   6. In the future, to open this project again, go to File > Open > Project and select the .sln file
