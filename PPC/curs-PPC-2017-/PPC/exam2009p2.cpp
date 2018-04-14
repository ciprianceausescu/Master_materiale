#include<mpi.h>
#include<stdlib.h>
#include<stdio.h>
#include<Windows.h>

enum {
	tag_send,
	terminate,
	result
};



int main(int argc, char *argv[]) {
	int rc;
	rc = MPI_Init(&argc, &argv);
	if (rc != MPI_SUCCESS) {
		printf("Eroare");
		MPI_Abort(MPI_COMM_WORLD, rc);
		return 0;
	}

	int rank, size;
	MPI_Comm_rank(MPI_COMM_WORLD, &rank);
	MPI_Comm_size(MPI_COMM_WORLD, &size);


	int a[6][6] = { { 1,2,3,4,5,6 },
					{ 2,3,4,5,6,7 },
					{ 3,0,5,6,7,8 },
					{ 0,0,0,0,1,0 },
					{ 0,0,0,0,2,0 },
					{ 0,0,0,0,3,4 } };

	if (rank == 0) {
			int n = 5;// lungime matrice
			int existaPattern = 0;
			// impartirea este n - 1 =  pe coloane
			// n-2 pe linii (n-1) x (n-2) iteratii chunkuri

			// trimitem chunk la toti
			int chunk[2];
			chunk[0] = 0;
			chunk[1] = 0;
			int index = 0;
			for (int i = 1; i < size; i++) {
				MPI_Send(chunk, 2, MPI_INT, i, tag_send, MPI_COMM_WORLD);
				chunk[1] += 1;
				if (chunk[1] > n - 1)
				{
					chunk[1] = 0;
					chunk[0]++;
				}
			}

			while (true) {
				MPI_Status status;
				int slaveResult = 0;
				MPI_Recv(&slaveResult, 1, MPI_INT, MPI_ANY_SOURCE, result, MPI_COMM_WORLD, &status);
				existaPattern += slaveResult;

				if (chunk[0] > n-2) {
					break;
				}

				MPI_Send(chunk, 2, MPI_INT, status.MPI_SOURCE, tag_send, MPI_COMM_WORLD);
				chunk[1] += 1;
				if (chunk[1] > n - 1)
				{
					chunk[1] = 0;
					chunk[0]++;
				}
			}

			// send termination
			for (int i = 1; i < size; i++) {
				char dummy;
				MPI_Send(&dummy, 1, MPI_CHAR, i, terminate, MPI_COMM_WORLD);
			}

			if (existaPattern == 0) printf("nu a gasit \n");
			else printf("a gasit patter");
	}
	else {

		while (true) {
			// get how much is the size of an element
			MPI_Status stats;
			int sizeCount = 0;
			MPI_Probe(MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD, &stats);
			MPI_Get_count(&stats, MPI_INT, &sizeCount);

			if (stats.MPI_TAG == terminate) break;

			int *chunk = (int*)malloc(sizeCount * sizeof(int));
			MPI_Recv(chunk, sizeCount, MPI_INT, MPI_ANY_SOURCE, tag_send, MPI_COMM_WORLD, &stats);

			int localSum = 0;
			printf("Mat[%d][%d]: %d,%d\n", chunk[0], chunk[1], a[chunk[0]][chunk[1]], a[chunk[0]][chunk[1]+1]); fflush(stdout);
			if (a[chunk[0]][chunk[1]] == 1 && a[chunk[0] + 1][chunk[1]] == 2
											&& a[chunk[0] + 2][chunk[1]] == 3
											&& a[chunk[0] + 2][chunk[1] + 1] == 4) 
			{	
				localSum = 1;
			}

			MPI_Send(&localSum, 1, MPI_INT, 0, result, MPI_COMM_WORLD);
		}

	}
	MPI_Finalize();
}