/*#include "mpi.h"
#include <stdio.h>
#include <stdlib.h>
#include <fstream>
#include <time.h>

using namespace std;
//numarul de randuri din matricea A 
#define NRA 200  
//numarul de coloane din matricea A 
#define NCA 200   
//numarul de coloane din matricea B
#define NCB 200  
//id-ul primului proces
#define MASTER 0   
//tip de mesaj trimis de la procesul master
#define FROM_MASTER 1  
//tip de mesaj trimis de la procesul slave
#define FROM_WORKER 2         

int main(int argc, char *argv[])
{
	//Algoritm paralel
	//numtasks = numarul total de procese
	//taskid = numarul procesului curent
	//numworkers = numarul de procese de tip slave
	//source = numarul procesului sursa
	//dest = numarul procesului sursa
	//mtype = tipul mesajului - tag
	//rows = randurile din matricea A care vor fi trimise catre procesele slave
	//averow, extra, offset = variabile folosite pentru a determina randurile trimise catre procesele slave
	int	numtasks,              
		taskid,                
		numworkers,            
		source,                
		dest,                  
		mtype,                 
		rows,                 
		averow, extra, offset, 
		i, j, k, rc, x;   

	double	a[NRA][NCA];//matricea A       
	double	b[NCA][NCB];//matricea B
	double  c[NRA][NCB];//matricea rezultat C
	MPI_Status status;

	FILE *inFile;

	inFile = fopen("C:/Users/Ciprian Mihai/Documents/Visual Studio 2013/Projects/MPI_1/MPI_1/rand_data.txt", "r");

	if (inFile == NULL) {
		fprintf(stderr, "Fisierul nu s-a putut deschide.\n");
		exit(1);
	}

	MPI_Init(&argc, &argv);
	MPI_Comm_rank(MPI_COMM_WORLD, &taskid);
	MPI_Comm_size(MPI_COMM_WORLD, &numtasks);
	if (numtasks < 2) {
		printf("Este nevoie de minim 2 procese pentru realizarea inmultirii.\n");
		MPI_Abort(MPI_COMM_WORLD, rc);
		exit(1);
	}
	numworkers = numtasks - 1;


	//procesul master
	if (taskid == MASTER)
	{
		printf("Programul a inceput cu un numar de %d procese.\n", numtasks);
		printf("Initializarea matricelor cu informatii din fisier.\n");
		for (i = 0; i<NRA; i++)
			for (j = 0; j < NCA; j++)
			{
				fscanf(inFile, "%d", &x);
				a[i][j] = x;
			}
				
		for (i = 0; i<NCA; i++)
			for (j = 0; j<NCB; j++)
			{
				fscanf(inFile, "%d", &x);
				b[i][j] = x;
			}

		//Trimitem un nr egal de randuri din matricea A catre procesele slave
		averow = NRA / numworkers;
		//Verificam cate randuri raman de trimis, dupa ce am trimis cate un nr egal de randuri catre procesele slave
		extra = NRA % numworkers;
		//offset reprezinta adresa de inceput a setului de randuri din matricea A pe care le trimitem proceselor slave
		offset = 0;
		//mtype reprezinta tipul de mesaj care poate fi tag 1 sau 2 
		mtype = FROM_MASTER;
		for (dest = 1; dest <= numworkers; dest++)
		{
			   //Numarul de randuri trimis catre fiecare proces slave creste cu 1 in momentul in care numarul total de randuri
			   //nu se imparte exact la numarul total de procese slave
			rows = (dest <= extra) ? averow + 1 : averow;
			printf("Se trimit %d randuri din matricea A catre %d incepand cu=%d\n", rows, dest, offset);
			   //Se trimite offset-ul = adresa de inceput a setului de randuri din matricea A care se trimit catre procesul
			   //curent 
			MPI_Send(&offset, 1, MPI_INT, dest, mtype, MPI_COMM_WORLD);
			   //Se trimite rows = numarul de randuri trimise catre procesul curent
			MPI_Send(&rows, 1, MPI_INT, dest, mtype, MPI_COMM_WORLD);
			   //Din matricea A se trimit cele rows randuri, incepand cu pozitia offset 
			MPI_Send(&a[offset][0], rows*NCA, MPI_DOUBLE, dest, mtype, MPI_COMM_WORLD);
			   //Se trimite matrcea B catre fiecare dintre cele numworkers procese slave 
			MPI_Send(&b, NCA*NCB, MPI_DOUBLE, dest, mtype, MPI_COMM_WORLD);
			   //adresa de inceput a setului de randuri din matricea A creste la fiecare pas cu numarul total de randuri
			   //care a fost trimis catre procesele slave 
			offset = offset + rows;
		}

		//Se primesc rezultatele de la procesele slave 
		//mtype reprezinta tipul de mesaj care poate fi tag 1 sau 2 
		mtype = FROM_WORKER;
		for (i = 1; i <= numworkers; i++)
		{
			   //Procesul master va primi de la procesele slave rezultatul inmultirii randurilor din A cu matricea B, si anume
			   //partea corespunzatoare care a fost calculata din matricea C
			source = i;
			   //offset = adresa de inceput a setului de randuri din matricea C care a fost calculat de catre procesul curent
			MPI_Recv(&offset, 1, MPI_INT, source, mtype, MPI_COMM_WORLD, &status);
			   //rows = numarul de randuri care au fost calculate
			MPI_Recv(&rows, 1, MPI_INT, source, mtype, MPI_COMM_WORLD, &status);
			   //Din matricea C se trimit cele rows randuri, incepand cu pozitia offset
			MPI_Recv(&c[offset][0], rows*NCB, MPI_DOUBLE, source, mtype, MPI_COMM_WORLD, &status);
			printf("S-au primit rezultatele de la procesul: %d\n", source);
		}

		//Afisarea rezultatelor
		printf("******************************************************\n");
		printf("Matricea A:\n");
		for (i = 0; i<NRA; i++)
		{
			printf("\n");
			for (j = 0; j<NCA; j++)
				printf("%6.2f   ", a[i][j]);
		}
		printf("\n******************************************************\n");
		printf("******************************************************\n");
		printf("Matricea B:\n");
		for (i = 0; i<NCA; i++)
		{
			printf("\n");
			for (j = 0; j<NCB; j++)
				printf("%6.2f   ", b[i][j]);
		}
		printf("\n******************************************************\n");
		printf("******************************************************\n");
		printf("Rezultatul inmultirii dintre A si B, adica matricea C:\n");
		for (i = 0; i<NRA; i++)
		{
			printf("\n");
			for (j = 0; j<NCB; j++)
				printf("%6.2f   ", c[i][j]);
		}
		printf("\n******************************************************\n");
		printf("Sfarsit.\n");
	}


	//procesele slave
	if (taskid > MASTER)
	{
		//Se seteaza tag-ul 2 pentru mtype
		mtype = FROM_MASTER;
		//Se primeste adresa de inceput a setului de randuri din matricea A
		MPI_Recv(&offset, 1, MPI_INT, MASTER, mtype, MPI_COMM_WORLD, &status);
		//Se primeste numarul de randuri alea matricei A care urmeaza a fi inmultite cu B
		MPI_Recv(&rows, 1, MPI_INT, MASTER, mtype, MPI_COMM_WORLD, &status);
		//Se primeste matricea A, si anume, cele rows randuri corespunzatoare procesului curent 
		MPI_Recv(&a, rows*NCA, MPI_DOUBLE, MASTER, mtype, MPI_COMM_WORLD, &status);
		//Se primeste matricea B 
		MPI_Recv(&b, NCA*NCB, MPI_DOUBLE, MASTER, mtype, MPI_COMM_WORLD, &status);
		//Se realizeaza inmultirea dintre cele rows randuri din matricea A si matricea B, adica matricea C 
		for (k = 0; k<NCB; k++)
			for (i = 0; i<rows; i++)
			{
				c[i][k] = 0.0;
				for (j = 0; j<NCA; j++)
					c[i][k] = c[i][k] + a[i][j] * b[j][k];
			}
		//Se seteaza tag-ul 1 
		mtype = FROM_WORKER;
		//Se trimite adresa de inceput a setului de randuri din matricea C 
		MPI_Send(&offset, 1, MPI_INT, MASTER, mtype, MPI_COMM_WORLD);
		//Se trimite numarul de randuri care au fost inmultite la procesul curent 
		MPI_Send(&rows, 1, MPI_INT, MASTER, mtype, MPI_COMM_WORLD);
		//Se trimite rezultatul, care a fost pus in matricea C 
		MPI_Send(&c, rows*NCB, MPI_DOUBLE, MASTER, mtype, MPI_COMM_WORLD);
	}
	MPI_Finalize();
}*/