#ifndef TIMER
#define TIMER

#include <math.h>
#include <stdio.h>
#include <stdlib.h>

#include <sys/time.h>
#include <time.h> 

int timeval_subtract(struct timeval *result, struct timeval *t2, struct timeval *t1)
{
    unsigned int resolution=1000000;
    long int diff = (t2->tv_usec + resolution * t2->tv_sec) - (t1->tv_usec + resolution * t1->tv_sec);
    result->tv_sec = diff / resolution;
    result->tv_usec = diff % resolution;
    return (diff<0);
}


void randomInit(float* data, int size) {
   for (int i = 0; i < size; ++i) {
     data[i] = rand() / (float)RAND_MAX;
    }
    printf("\n");
}

template<class T>
bool validate(float* A,float* B, unsigned int sizeAB){
    for (int i = 0; i < sizeAB; i++) {
      if (fabs(A[i] - B[i]) > 0.0005){
        printf("INVALID RESULT %d %f %f\n", i, A[i], B[i]);
        return false;
      }
    }
    printf("VALID RESULT!\n");
    return true;
}

#endif