#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include "Timer.h"
//#include "ScanTrans.cu.h"

#define T 64

// Transpose matrix sequentially
int seqTrans(float** h_in, float** h_out, int x_size, int y_size) {
  for (int y = 0; y < y_size; y++) {
    for (int x = 0; x < x_size; x++) {
      h_out[x][y] = h_in[y][x];
    }
  }
  return 1;
}

__global__ void naiveParTrans(float* d_in, float* d_out, int x_size, int y_size) {
  int gidx = blockIdx.x * blockDim.x + threadIdx.x;
  int gidy = blockIdx.y * blockDim.y + threadIdx.y;

  if (gidx < x_size && gidy < y_size) {
    d_out[gidx*y_size+gidy] = d_in[gidy*x_size + gidx];
  }
}

__global__ void matTranspose(float* d_in, float* d_out, 
                             int x_size,  int y_size) {
  __shared__ float tile[T][T+1];
  int tidx = threadIdx.x;
  int tidy = threadIdx.y;
  int j    = blockIdx.x*T + tidx;
  int i    = blockIdx.y*T + tidy;
  if (j < y_size && i < x_size) {
    tile[tidy][tidx] = d_in[i*y_size+j];
  }
  __syncthreads();
  i = blockIdx.y*T + threadIdx.x;
  j = blockIdx.x*T + threadIdx.y;
  if (j < y_size && i < x_size) {
    d_out[j*x_size+i] = tile[tidx][tidy];
  }
}

// Helper function for deallocating main memory matrices
int deallocMatr(float** matr, int y_size) {
  for (int i = 0; i < y_size; i++) {
    free(matr[i]);
  }
  free(matr);
  return 1;
}

int main() {
  int dim_x = 2048;
  int dim_y = 2048;
  dim3 block (T, T, 1);
  dim3 grid (dim_x, dim_y, 1);
  int num_threads = dim_x*dim_y;
  size_t size     = num_threads*sizeof(float);
  // Allocate space for h_in and d_in, and initialize it
  float** h_in    = (float**) malloc(dim_y*sizeof(float*));
  float* d_in_tmp = (float*) malloc(size);
  float* d_naiv_in;
  float* d_tiled_in;
  int position;
  cudaMalloc((void**)&d_naiv_in,  size);
  cudaMalloc((void**)&d_tiled_in, size);
  for (int i = 0; i < dim_y; i++) {
    float* tmp_h = (float*) malloc(dim_x*sizeof(float));
    for (int j = 0; j < dim_x; j++) {
      tmp_h[j] = (float) j;
      position = (i*dim_x+j);
      d_in_tmp[position] = (float) j;
    }
    h_in[i] = tmp_h;
  }

  cudaMemcpy(d_naiv_in,  d_in_tmp, size, cudaMemcpyHostToDevice);
  cudaMemcpy(d_tiled_in, d_in_tmp, size, cudaMemcpyHostToDevice);
  float* d_out_tmp = (float*) malloc(size);
  // Allocate space for h_out and d_out.
  float** h_out = (float**) malloc(dim_x*sizeof(float*));
  float* d_naiv_out; 
  float* d_tiled_out;
  cudaMalloc((void**)&d_naiv_out, size);
  cudaMalloc((void**)&d_tiled_out, size);
  for (int i = 0; i < dim_x; i++) {
    float* tmp_h = (float*) malloc(dim_y*sizeof(float));
    for (int j = 0; j < dim_y; j++) {
      tmp_h[j] = 0.0;
      position = (i*dim_x+j);
      d_out_tmp[position] = (float) 0;
    } 
    h_out[i] = tmp_h;
  }
  cudaMemcpy(d_naiv_out, d_out_tmp, size, cudaMemcpyHostToDevice);
  cudaMemcpy(d_tiled_out, d_out_tmp, size, cudaMemcpyHostToDevice);

  // Create timers for sequential run
  unsigned long int t_elapsed;
  struct timeval t_start, t_end, t_diff;
  gettimeofday(&t_start, NULL);

  // Run sequentially
  seqTrans(h_in, h_out, dim_x, dim_y);

  gettimeofday(&t_end, NULL);
  timeval_subtract(&t_diff, &t_end, &t_start);
  t_elapsed = (t_diff.tv_sec*1e6+t_diff.tv_usec);
  printf("CPU Transpositioning done in: %lu microsecs\n", t_elapsed);


  gettimeofday(&t_start, NULL);

  // Run naive parallel
  naiveParTrans<<< grid, block  >>>(d_naiv_in, d_naiv_out, dim_x, dim_y);
  cudaThreadSynchronize();

  gettimeofday(&t_end, NULL);
  timeval_subtract(&t_diff, &t_end, &t_start);
  t_elapsed = (t_diff.tv_sec*1e6+t_diff.tv_usec);
  printf("Naive GPU Transpositioning done in: %lu microsecs\n", t_elapsed);

  float* d_naiv_res = (float*) malloc(size);
  cudaMemcpy(d_naiv_res, d_naiv_out, size, cudaMemcpyDeviceToHost);

  gettimeofday(&t_start, NULL);

  matTranspose<<< grid, block >>>(d_tiled_in, d_tiled_out, dim_x, dim_y);
  cudaThreadSynchronize();

  gettimeofday(&t_end, NULL);
  timeval_subtract(&t_diff, &t_end, &t_start);
  t_elapsed = (t_diff.tv_sec*1e6+t_diff.tv_usec);
  printf("Memory Coalesced GPU Transpositioning done in: %lu microsecs\n", t_elapsed);
  float* d_tiled_res = (float*) malloc(size);
  cudaMemcpy(d_tiled_res, d_tiled_out, size, cudaMemcpyDeviceToHost);

  bool isEqual = true;

  // Validate:
  for (int x = 0; x < dim_x; x++) {
    for (int y = 0; y < dim_y; y++) {
      if (abs(h_out[x][y] - d_naiv_res[x*dim_y+y]) > 0.00005 &&
          abs(d_naiv_res[x*dim_y+y]-d_tiled_res[x*dim_y+y]) > 0.00005 && isEqual) {
        printf("INVALID: %.1f != %.1f != %.1f\n", h_out[x][y],
                                                  d_naiv_res[x*dim_y+y],
                                                  d_tiled_res[x*dim_y+y]);
        isEqual = false;
      }
    }
  }
  if (isEqual) {
    printf("Valid\n");
  }
  deallocMatr(h_out, dim_x);
  deallocMatr(h_in, dim_y);
  free(d_naiv_res);
  free(d_tiled_res);
  free(d_out_tmp);
  free(d_in_tmp);
  cudaFree(d_naiv_out);
  cudaFree(d_naiv_in);
  cudaFree(d_tiled_out);
  cudaFree(d_tiled_in);
  return 1;
}
