#include <sicstus/sicstus.h>

#include <stdlib.h>             /* [PM] 3.9.1 !! See bdb.c */
#include <db.h>                 /* needed for types used in bdb_glue.h */
#include "bdb.h"                /* needed for types used in bdb_glue.h */

#include "bdb_glue.h"           /* [PM] 3.9 splfr-generated */
#include <errno.h>

long sync_db(db_struct *db)
{
  if (db)
  {
    if (db->termsdb) db->termsdb->sync(db->termsdb, 0); /* error handling? */
    if (db->indexdb) db->indexdb->sync(db->indexdb, 0);
    if (db->admindb) db->admindb->sync(db->admindb, 0);
    return 0;
  }
  return EINVAL;
}

