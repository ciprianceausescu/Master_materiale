/* C part of test file for struct.pl module
   SCCS:  @(#)str_suite.c	21.1 05/16/95
Relevant parts of the Prolog test file structtest.pl:

:- foreign_type
	intgr		= integer,
	boolean		= enum([false,true]),
	position	= struct([	x:integer,
					y:integer
				]),
	size		= struct([	width:short,
					height:short
				]),
	mongo		= struct([	a:intgr,
					b:short,
					c:char,
					d:unsigned_short,
					e:unsigned_char,
					f:float,
					g:double,
					h:atom,
					i:string,
					j:address,
					k:array(81,char),
					l:size,
					m:pointer(position),
					n:pointer(belch),
					o:boolean,
					p:long
					q:pointer(mongo),
				]),
	uex		= union([	a:integer,
					b:long,
					c:double
				]).
 
 :- foreign_type
	region		= struct([size:size, position:position]),
	belch		= opaque.
 
:- foreign_type
	aliastest	= integer,
	aliasarray	= array(10, aliastest).

foreign_file(system(structs_suite), [test1,test2,test3,test4,test5,test6]).
 
foreign(test1, c, test1(+integer,+integer,[-pointer(position)])).
foreign(test2, c, test2(+integer,[-integer])).
foreign(test3, c, test3(+pointer(size),-integer,-integer)).
foreign(test4, c, test4(+atom, [-pointer(mongo)])).
foreign(test5, c, test5(+pointer(aliasarray), [-aliastest])).
foreign(test6, c, test6(+bool, -bool)).
foreign(test7_1, c, test7_1(+integer,[-pointer(uex)])).
foreign(test7_2, c, test7_2(+double,[-pointer(uex)])).
 
*/

#include "str_suite.h"
#include "str_suite_glue.h"

position pos42 = {
/*	int x; */		42,
/*	int y; */		42
};

mongo mongo42 = {
/*	int a; */	42,
/*	short INT b; */	42,
/*	char c; */		42,
/*	unsigned short d; */	42,
/*	unsigned char e; */	42,
/*	float f; */		42.0,
/*	double g; */		42.0,
/*	unsigned int h; */	0,
/*	char *i; */		"forty-two",
/*	void *j; */		0,
/*	char k[81]; */		{'f','o','r','t','y','-','t','w','o'},
/*	size l; */		{ 42, 42 },
/*	position *m; */		&pos42,
/*	belch *n; */		(belch *)0,
/*	boolean o; */		true,
/*	long p; */		42,
/*	mongo *q; */		(mongo *)0
};

position *SPCDECL test1(x, y)
    long x, y;
{
	position *pos;

	pos = (position *)malloc(sizeof(position));

	pos->x = x;
	pos->y = y;

	return pos;
}

long SPCDECL test2(x)
    long x;
{
	return ++x;
}

void SPCDECL test3(sz, widthptr, heightptr)
    size *sz;
    long *widthptr, *heightptr;
{
	*widthptr = sz->width;
	*heightptr = sz->height;
}

mongo *SPCDECL test4(atm)
SP_atom atm;
{
	mongo42.h = atm;
	return &mongo42;
}

long/*aliastest*/ SPCDECL test5(x)
    aliasarray *x;
{
	return x[1];
}

void SPCDECL test6(inl, outl)
     long inl;
     long *outl;
{
  bool in = inl;
  bool out = (in == true) ? false : true;

  *outl = out;
}

uex *SPCDECL test7_1(x)
    long x;
{
	uex *uexample;

	uexample = (uex *)malloc(sizeof(uex));

	uexample->a = x;

	return uexample;
}

uex *SPCDECL test7_2(x)
    double x;
{
	uex *uexample;

	uexample = (uex *)malloc(sizeof(uex));

	uexample->c = x;

	return uexample;
}

long SPCDECL test8_1(u)
    uex * u;
    {
	return u->a;
    }

long SPCDECL test8_2(u)
    uex * u;
    {
	return u->b;
    }

double SPCDECL test8_3(u)
    uex * u;
    {
	return u->c;
    }
