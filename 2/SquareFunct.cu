#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include "TransMainGen.h"
//#include "ScanTrans.cu.h"

__global__ void squareFunction(float* d_in, float* d_out, int y_size) {
  int gid = blockIdx.x * blockDim.x + threadIdx.x;
  int i = gid * y_size;
  float accum = d_in[i]*d_in[i];
  float tmpA;
  d_out[i] = accum;
  if (i < y_size) {
    for (int j = 1; j < 64; j++) {
      tmpA  = d_in[i+j];
      accum = sqrt(accum) + tmpA*tmpA;
      d_out[i+j] = accum;
    }
  }
}

// Input:  Transposed matrix A'
// Output: Transposed matrix B'
__global__ void squareCoalesced(float* d_in, float* d_out, int x_size) {
  int i = blockIdx.x * blockDim.x + threadIdx.x;
  if (i < x_size) {
    float accum = d_in[i]*d_in[i];
    float tmpA;
    d_out[i] = accum;
    for (int j = 1; j < 64; j++) {
      tmpA  = d_in[j*x_size + i];
      accum = sqrt(accum) + tmpA*tmpA;
      d_out[j*x_size + i] = accum;
    }
  }
}

int main() {
  int dim_x = 64;
  int dim_y = 10;
  unsigned int block = 256;
  unsigned int grid = (dim_x/64)*dim_y;
  int num_threads = dim_x*dim_y;
  size_t size     = num_threads*sizeof(float);
  // Allocate space for h_in and d_in, and initialize it
  float** h_in    = (float**) malloc(dim_y*sizeof(float*));
  float* d_in_tmp = (float*) malloc(size);
  float* d_naiv_in;
  float* d_coalesced_in;
  int position;
  cudaMalloc((void**)&d_naiv_in,  size);
  cudaMalloc((void**)&d_coalesced_in, size);
  for (int i = 0; i < dim_y; i++) {
    float* tmp_h = (float*) malloc(dim_x*sizeof(float));
    for (int j = 0; j < dim_x; j++) {
      tmp_h[j] = (float) j;
      position = (i*dim_x+j);
      d_in_tmp[position] = (float) j;
    }
    h_in[i] = tmp_h;
  }
  cudaMemcpy(d_naiv_in,      d_in_tmp, size, cudaMemcpyHostToDevice);
  cudaMemcpy(d_coalesced_in, d_in_tmp, size, cudaMemcpyHostToDevice);
  float* d_out_tmp = (float*) malloc(size);
  // Allocate space 
  float** h_out = (float**) malloc(dim_x*sizeof(float*));
  float* d_naiv_out; 
  float* d_coalesced_out1;
  float* d_coalesced_out2;
  float* d_coalesced_out3;
  cudaMalloc((void**)&d_naiv_out,       size);
  cudaMalloc((void**)&d_coalesced_out1, size);
  cudaMalloc((void**)&d_coalesced_out2, size);
  cudaMalloc((void**)&d_coalesced_out3, size);
  for (int i = 0; i < dim_x; i++) {
    float* tmp_h = (float*) malloc(dim_y*sizeof(float));
    for (int j = 0; j < dim_y; j++) {
      tmp_h[j]            = 0.0;
      position            = (i*dim_x+j);
      d_out_tmp[position] = (float) 0;
    } 
    h_out[i] = tmp_h;
  }
  cudaMemcpy(d_naiv_out,       d_out_tmp, size, cudaMemcpyHostToDevice);
  cudaMemcpy(d_coalesced_out1, d_out_tmp, size, cudaMemcpyHostToDevice);
  cudaMemcpy(d_coalesced_out2, d_out_tmp, size, cudaMemcpyHostToDevice);
  cudaMemcpy(d_coalesced_out3, d_out_tmp, size, cudaMemcpyHostToDevice);

  // Run naive parallel
  squareFunction<<< grid, block >>>(d_naiv_in, d_naiv_out, dim_y);
  cudaThreadSynchronize();

  float* d_naiv_res = (float*) malloc(size);
  cudaMemcpy(d_naiv_res, d_naiv_out, size, cudaMemcpyDeviceToHost);
  dim3 transBlock (T,T,1);
  dim3 transGrid (dim_x,dim_y,1);
  matTranspose<<< transGrid, transBlock >>> (d_coalesced_in,
                                             d_coalesced_out1,
                                             dim_x,
                                             dim_y);
  cudaThreadSynchronize();
  squareCoalesced<<< grid, block >>>(d_coalesced_out1, 
                                     d_coalesced_out2, 
                                     dim_y);
  cudaThreadSynchronize();
  dim3 transGrid1 (dim_y,dim_x,1);
  matTranspose<<< transGrid1, transBlock >>> (d_coalesced_out2,
                                              d_coalesced_out3,
                                              dim_y,
                                              dim_x);
  cudaThreadSynchronize();
  float* d_coalesced_res = (float*) malloc(size);
  cudaMemcpy(d_coalesced_res, d_coalesced_out3, size, cudaMemcpyDeviceToHost);

  bool isEqual = true;
  for (int y = 0; y < dim_y; y++) {
   for (int x = 0; x < dim_x; x++) {
      if (isEqual && fabs(d_naiv_res[y*dim_x+x] - d_coalesced_res[y*dim_x+x]) > 0.0005) {
        printf("Mismatch found at %i, %i: %.2f != %.2f\n", y, x, d_naiv_res[y*dim_x+x], 
                                                                 d_coalesced_res[y*dim_x+x]);
        isEqual = false;
      }
    }
  }
  
  deallocMatr(h_out, dim_x);
  deallocMatr(h_in, dim_y);
  free(d_naiv_res);
  free(d_coalesced_res);
  free(d_out_tmp);
  free(d_in_tmp);
  cudaFree(d_naiv_out);
  cudaFree(d_naiv_in);
  cudaFree(d_coalesced_out1);
  cudaFree(d_coalesced_out2);
  cudaFree(d_coalesced_out3);
  cudaFree(d_coalesced_in);
  return 1;
}
