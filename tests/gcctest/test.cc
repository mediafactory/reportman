#include <stdio.h>
#include "..\..\rpreportmanapi.h"

int main(void)
{
 int hreport;

 char reportmanfile[]="c:\\prog\\toni\\reportman\\repmman\\repsamples\\sample2.rep";
 printf("Test for report manager\n");
 printf("Will load :");
 printf(reportmanfile);

 hreport=rp_open(reportmanfile);
 if (hreport==0)
  printf(rp_lasterror());
}

