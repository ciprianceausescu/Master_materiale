#include <time.h>

typedef struct tm tm;
typedef time_t timestamp;

#include "timestruct_glue.h"

tm * SPCDECL pl_gmtime(time_t *timep)
{
#error "[PM] 4.0 gmtime is not thread safe. use gmtime_r instead"
  return gmtime(timep);
}

tm * SPCDECL pl_localtime(time_t *timep)
{

#error "[PM] 4.0 localtime is not thread safe. use localtime_r instead"
  return localtime(timep);
}

time_t SPCDECL pl_time(time_t *timep)
{

#error "[PM] 4.0 You do not know the size of time_t, it may be larger than long in which case timestruct.pl will die horribly"
  return time(timep);
}

