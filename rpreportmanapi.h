//{*******************************************************}
//{                                                       }
//{       Report Manager                                  }
//{                                                       }
//{       rpreportmanapi                                  }
//{       Exported functions for the Standarc C Library   }
//{       Copyright (c) 1994-2003 Toni Martir             }
//{       toni@pala.com                                   }
//{                                                       }
//{       This file is under the MPL license              }
//{       If you enhace this file you must provide        }
//{       source code                                     }
//{                                                       }
//{                                                       }
//{*******************************************************}


#ifndef _REPORTMAN_H
#define _REPORTMAN_H

#define REP_LIBNAME "Reportman.ocx"

#ifdef __cplusplus
extern "C" {
#endif
//extern int rp_open(char *filename);//external REP_LIBNAME;
//extern int rp_execute(int hreport,char *outputfilename,int metafile,int compressed);//external REP_LIBNAME;
//extern int rp_close(int hreport);//external REP_LIBNAME;
//extern char *rp_lasterror(void);//external REP_LIBNAME;
int __stdcall rp_open(char *filename);
int __stdcall rp_execute(int hreport,char *outputfilename,int metafile,int compressed);
int __stdcall rp_close(int hreport);
char __stdcall *rp_lasterror(void);
#ifdef __cplusplus
}
#endif
#endif


