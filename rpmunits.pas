{*******************************************************}
{                                                       }
{       Report Manager                                  }
{                                                       }
{       rpmunits                                        }
{       Unit conversion to allow international          }
{       designment of reports                           }
{                                                       }
{                                                       }
{       Copyright (c) 1994-2002 Toni Martir             }
{       toni@pala.com                                   }
{                                                       }
{       This file is under the GPL license              }
{       A comercial license is also available           }
{       See license.txt for licensing details           }
{                                                       }
{                                                       }
{*******************************************************}

// The report units are always in twips that is 1440 twips=1 inchess=2.51 cms

unit rpmunits;

interface

uses rptypes,sysutils;

const
 CMS_PER_INCHESS=2.51;
 TWIPS_PER_INCHESS=1440;
type
 Trpmunits=(rpUnitcms,rpUnitInchess);

var
 defaultunit:Trpmunits;
 rpunitconversions:array [Low(TrpmUnits)..High(TRpmUnits)] of extended;
 rpunitlabels:array [Low(TrpmUnits)..High(TRpmUnits)] of string;
 rpunitformats:array [Low(TrpmUnits)..High(TRpmUnits)] of string;


function gettextfromtwips(twips1:TRptwips):string;
function gettwipsfromtext(atext:string):TRptwips;
function getdefaultunitstring:string;

implementation

function gettextfromtwips(twips1:TRptwips):string;
begin
 Result:=FormatFloat(rpunitformats[defaultunit],twips1/rpunitconversions[defaultunit]);
end;

function gettwipsfromtext(atext:string):TRptwips;
begin
 Result:=Round(StrToFloat(atext)*rpunitconversions[defaultunit]);
end;

function getdefaultunitstring:string;
begin
 Result:=rpunitlabels[defaultunit];
end;


initialization
 defaultunit:=rpUnitcms;
 rpunitconversions[rpUnitcms]:=TWIPS_PER_INCHESS/CMS_PER_INCHESS;
 rpunitlabels[rpUnitcms]:='cms.';
 rpunitformats[rpUnitcms]:='#######0.##';
 rpunitconversions[rpUnitInchess]:=TWIPS_PER_INCHESS;
 rpunitlabels[rpUnitInchess]:='inch.';
 rpunitformats[rpUnitInchess]:='#######0.###';
end.
