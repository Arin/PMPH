#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <cuda_runtime.h>
// Setup for measuing time
#include <sys/time.h>
#include <time.h>

int timeval_subtract(struct timeval* result,
                     struct timeval* t2,
                     struct timeval* t1) {
  unsigned int resolution=1000000;
  long int diff = (t2->tv_usec + resolution * t2->tv_sec) -
                  (t1->tv_usec + resolution * t1->tv_sec);
  result->tv_sec  = diff / resolution;
  result->tv_usec = diff % resolution;
  return (diff<0);
}

void seqFunct(float* m_out) {
  for (int i = 1; i <= 753411; i++) {
    m_out[i-1] = pow(i/(i-2.3), 3);
  }
}

__global__ void parFunct(float *d_in, float *d_out, int limit) {
  const unsigned int lid = threadIdx.x; //local id inside a block
  const unsigned int gid = blockIdx.x*blockDim.x + lid; // global id
  float x;
  if (gid <= limit) {
    x = d_in[gid];
    d_out[gid] = pow((x/(x-2.3)),3); // Compute
  }
}

int main(int argc, char** argv) {
  unsigned int num_threads = 753411;
  unsigned int mem_size    = num_threads*sizeof(float);
  
  // Initialize vars for measuring time
  unsigned long int par_elapsed;
  unsigned long int seq_elapsed;
  struct timeval p_start, p_end, p_diff;
  struct timeval s_start, s_end, s_diff;

  // alocate host memory
  float* h_in  = (float*) malloc(mem_size);
  float* h_out = (float*) malloc(mem_size);
  float* m_out = (float*) malloc(mem_size);

  // iniitalize the memory
  for(unsigned int i=1; i<=num_threads; ++i) {
    h_in[i-1] = (float)(i);
  }

  // allocate device memory
  float* d_in;
  float* d_out;
  cudaMalloc((void**)&d_in, mem_size);
  cudaMalloc((void**)&d_out, mem_size);

  // copy host memory to device
  cudaMemcpy(d_in, h_in, mem_size, cudaMemcpyHostToDevice);

  // Start timer
  gettimeofday(&p_start, NULL);

  // execute the kernel
  parFunct<<< ceil(num_threads/1024.0), 1024>>>(d_in, d_out, num_threads);
  cudaThreadSynchronize();

  // calc elapsed time
  gettimeofday(&p_end, NULL);
  timeval_subtract(&p_diff, &p_end, &p_start);
  par_elapsed = p_diff.tv_sec*1e6+p_diff.tv_usec;

  cudaMemcpy(h_out, d_out, mem_size, cudaMemcpyDeviceToHost);
  
  gettimeofday(&s_start, NULL);

  // execute the kernel
  seqFunct(m_out);

  // Print elapsed time
  gettimeofday(&s_end, NULL);
  timeval_subtract(&s_diff, &s_end, &s_start);
  seq_elapsed = s_diff.tv_sec*1e6+s_diff.tv_usec;
  //printf("Took %d microseconds (%.2fms)\n", elapsed, elapsed/1000.0);

  bool isValid = true;
  for (int i = 0; i < num_threads; i++) {
    if (abs(m_out[i] - h_out[i]) > 0.000001) {
      isValid = false;
      break;
    }
  }
  if (isValid) {
    printf("VALID\n");
  }
  else {
    printf("INVALID\n");
  }

  printf("CPU Time: %d microseconds (%.fms)\n", seq_elapsed, seq_elapsed/1000.0);
  printf("GPU Time: %d microseconds (%.fms)\n", par_elapsed, par_elapsed/1000.0);

  // clean-up memory
  free(h_in);     free(h_out);
  free(m_out);
  cudaFree(d_in); cudaFree(d_out);
}
