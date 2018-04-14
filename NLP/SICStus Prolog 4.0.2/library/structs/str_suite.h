/************************************************************************
 * C include file                                                       *
 *      Q:/ptg3/snapshot/snapshot23/structs/str_suite.h                 *
 * generated automatically from Prolog file                             *
 *      Q:/ptg3/snapshot/snapshot23/structs/str_suite.pl                *
 * By structs_to_c utility                                              *
 ************************************************************************/

#include <sicstus/sicstus.h>

typedef int intgr;
typedef enum _bool bool;
typedef struct _position position;
typedef struct _size size;
typedef struct _mongo mongo;
typedef union _uex uex;
typedef struct _region region;
typedef void belch;
typedef int aliastest;
typedef aliastest aliasarray;
/****
   I would like this, but it causes type errors, since
   the prototype of test5 translates to

		long SPCDECL test5(aliasarray *x);


typedef aliastest (aliasarray)[10];

aliastest SPCDECL test5(x)
    aliasarray x;
{
	return x[1];
}
****/

enum _bool {
	false,
	true,
};

struct _position {
	int x;
	int y;
};

struct _size {
	short width;
	short height;
};

struct _mongo {
	intgr a;
	short b;
	char c;
	unsigned short d;
	unsigned char e;
	float f;
	double g;
	SP_atom h;
	char *i;
	void *j;
	char (k)[81];
	size l;
	position *(m);
	belch *(n);
	bool o;
	long p;
	mongo *(q);
};

union _uex {
	int a;
	long b;
	double c;
};

struct _region {
	size size;
	position position;
};

