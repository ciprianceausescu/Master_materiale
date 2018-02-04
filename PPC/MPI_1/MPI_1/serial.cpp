#include <stdio.h>
#include <stdlib.h>
#include <fstream>

using namespace std;
//numarul de randuri din matricea A 
#define NRA 200  
//numarul de coloane din matricea A 
#define NCA 200   
//numarul de coloane din matricea B
#define NCB 200  
int main(int argc, char *argv[])
{
	int    i, j, k, x;			
	double	a[NRA][NCA];//matricea A       
	double	b[NCA][NCB];//matricea B
	double  c[NRA][NCB];//matricea rezultat C

	FILE *inFile;

	inFile = fopen("C:/Users/Ciprian Mihai/Documents/Visual Studio 2013/Projects/MPI_1/MPI_1/rand_data.txt", "r");

	if (inFile == NULL) {
		fprintf(stderr, "Fisierul nu s-a putut deschide.\n");
		exit(1);
	}

	printf("Start programul serial.\n");
	printf("Se folosesc matricele: a[%d][%d], b[%d][%d], c[%d][%d]\n",
		NRA, NCA, NCA, NCB, NRA, NCB);

	/* Initialize A, B, and C matrices */
	printf("Citirea matricelor.\n");
	for (i = 0; i<NRA; i++)
		for (j = 0; j < NCA; j++)
		{
			fscanf(inFile, "%d", &x);
			a[i][j] = x;
		}
	for (i = 0; i<NCA; i++)
		for (j = 0; j < NCB; j++)
		{
			fscanf(inFile, "%d", &x);
			b[i][j] = x;
		}
	for (i = 0; i<NRA; i++)
		for (j = 0; j<NCB; j++)
			c[i][j] = 0.0;

	printf("Inmultirea matricelor.\n");
	for (i = 0; i<NRA; i++)
		for (j = 0; j<NCB; j++)
			for (k = 0; k<NCA; k++)
				c[i][j] += a[i][k] * b[k][j];

	printf("Rezultatul inmultirii:");
	for (i = 0; i<NRA; i++) {
		printf("\n");
		for (j = 0; j<NCB; j++)
			printf("%6.2f   ", c[i][j]);
	}
	printf("\Sfarsit.\n");
}