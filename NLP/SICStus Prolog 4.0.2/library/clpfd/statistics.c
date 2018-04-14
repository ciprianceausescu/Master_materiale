/* Copyright(C) 1999, Swedish Institute of Computer Science */

#include "fd.h"

#if MULTI_SP_AWARE
#endif /* MULTI_SP_AWARE */

long SPCDECL prolog_fd_statistics MAGIC (HIDDEN_PROTO long key)
{
  long l=0;
  
  switch (key) {
  case 0: l = fd.resumptions; fd.resumptions = 0; break;
  case 1: l = fd.entailments; fd.entailments = 0; break;
  case 2: l = fd.prunings; fd.prunings = 0; break;
  case 3: l = fd.failures; fd.failures = 0; break;
  case 4: l = fd.constraints; fd.constraints = 0; break;
  }
  return l;
}


long SPCDECL prolog_fd_debug MAGIC (HIDDEN_PROTO
				    SP_term_ref OldR,
				    SP_term_ref NewR)

{
  SP_term_ref old = SP_new_term_ref();
  SP_atom new;

  SP_put_atom(old,(fd.debugging ? atom_on : atom_off));
  if (!SP_unify(OldR,old))
    return 0;
  SP_get_atom(NewR,&new);
  fd.debugging = (new==atom_on);
  return 1;
}

long SPCDECL prolog_fd_overflow MAGIC (HIDDEN_PROTO
				       SP_term_ref OldR,
				       SP_term_ref NewR)

{
  SP_term_ref old = SP_new_term_ref();
  SP_atom new;

  SP_put_atom(old,(fd.overflowing ? atom_error : atom_fail));
  if (!SP_unify(OldR,old))
    return 0;
  SP_get_atom(NewR,&new);
  fd.overflowing = (new==atom_error);
  return 1;
}

#if DBG
static int kernstat[1000];

void addstat(int i)
{
  kernstat[i]++;
}

void clearstat(void)
{
  int i;
  for (i=0; i<1000; i++)
    kernstat[i] = 0;
}

void printstat(void)
{
  int i;
  for (i=0; i<1000; i++) {
    int n = kernstat[i];
    if (n>0) {
      if (i<10)
	fprintf(stderr, "# 00%d %d\n",i,n);
      else if (i<100)
	fprintf(stderr, "# 0%d %d\n",i,n);
      else
	fprintf(stderr, "# %d %d\n",i,n);
    }
  }
}
#endif
