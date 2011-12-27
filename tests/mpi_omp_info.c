#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <stdlib.h>

#include <mpi.h>
#include <omp.h>

int main(int argc, char* argv[]) {
  int myrank, nprocs, ierr;
  int *rbuf = NULL;
  int num_threads = -1;
  
  MPI_Init(&argc, &argv);
  MPI_Comm_rank(MPI_COMM_WORLD, &myrank);
  MPI_Comm_size(MPI_COMM_WORLD, &nprocs);

  if(myrank == 0) {
    rbuf = (int*)calloc(nprocs, sizeof(int));
    assert(rbuf);
  }

  num_threads = omp_get_max_threads();

  ierr = MPI_Gather(&num_threads, 1, MPI_INT,
                    rbuf, 1, MPI_INT,
                    0, MPI_COMM_WORLD);
  assert(ierr == MPI_SUCCESS);
  
  if(myrank == 0) {
    int i;
    for(i = 0; i < nprocs; i++) {
      printf("Rank %d : %d threads\n", i, rbuf[i]);
    }
    free(rbuf);
  }

  MPI_Finalize();

  return 0;
}


