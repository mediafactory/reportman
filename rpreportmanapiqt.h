/*{*******************************************************}
{                                                       }
{       Report Manager                                  }
{                                                       }
{       rpreportmanapi                                  }
{       Exported functions for the Standarc C Library   }
{       Copyright (c) 1994-2003 Toni Martir             }
{       toni@pala.com                                   }
{                                                       }
{       This file is under the MPL license              }
{       If you enhace this file you must provide        }
{       source code                                     }
{                                                       }
{                                                       }
{*******************************************************}
*/

#ifndef _REPORTMAN_H
#define _REPORTMAN_H


#ifdef __cplusplus
extern "C" {
#endif
/* Add __stdcall in Microsoft Windows */
int rp_open(char *filename);
int rp_execute(int hreport,char *outputfilename,int metafile,int compressed);
int rp_close(int hreport);
char * rp_lasterror(void);
int rp_print(int hreport,char *title,int showprogress,int showprintdialog);
int rp_preview(int hreport,char *title);

#ifdef __cplusplus
}
#endif
#endif



