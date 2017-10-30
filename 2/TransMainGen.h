#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include "Timer.h"
//#include "ScanTrans.cu.h"

#define T 32

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

