{*******************************************************}
{                                                       }
{       Report Manager                                  }
{                                                       }
{       Rpxmlstream                                     }
{       Streams properties                              }
{       common components of Report manager             }
{                                                       }
{                                                       }
{       Copyright (c) 1994-2005 Toni Martir             }
{       toni@pala.com                                   }
{                                                       }
{       This file is under the MPL license              }
{       If you enhace this file you must provide        }
{       source code                                     }
{                                                       }
{                                                       }
{*******************************************************}

unit rpxmlstream;

{$I rpconf.inc}

interface

uses Classes,sysutils,rptypes,rpreport,rpdatainfo,rpsubreport,
 rpsection,rpparams,rpprintitem,rplabelitem,rpmdbarcode,rpmdchart,
{$IFDEF USEVARIANTS}
 Variants,
{$ENDIF}
 rpdrawitem,rpmdconsts;


const
 C_MAXDATAWIDTH=40;

type
 TRpPropertytypes=(rppropinteger,rppropdouble,rppropdatetime,rppropstring,
  rppropwidestring,rppropstream);



function StringToRpString(astring:String):String;
function RpStringToString(rpstring:string):String;
function WStringToRpString(astring:WideString):String;
function RpStringToWString(rpstring:string):WideString;

procedure WritePropertyI(propname:string;propvalue:integer;stream:TStream);
procedure WritePropertyD(propname:string;propvalue:double;stream:TStream);
procedure WritePropertyBool(propname:string;propvalue:Boolean;stream:TStream);
procedure WritePropertyS(propname:string;propvalue:String;stream:TStream);
procedure WritePropertyW(propname:string;propvalue:WideString;stream:TStream);
procedure WritePropertyB(propname:string;propvalue:TStream;stream:TStream);
procedure WriteReportXML(areport:TComponent;Stream:TStream);
procedure ReadReportXML(areport:TComponent;Stream:TStream);

procedure WriteDatabaseInfoXML(dbinfo:TRpDatabaseInfoItem;Stream:TStream);
procedure WriteDataInfoXML(dinfo:TRpDataInfoItem;Stream:TStream);
procedure WriteReportPropsXML(report:TRpReport;Stream:TStream);
procedure WriteParamXML(aparam:TRpParam;Stream:TStream);
procedure WriteSubreportXML(subrep:TRpSubReport;Stream:TStream);
procedure WriteSectionXML(section:TRpSection;Stream:TStream);
procedure WriteComponentXML(comp:TRpCommonPosComponent;Stream:TStream);

function RpIsAlpha(achar:char):Boolean;
function RpIsAlphaW(achar:Widechar):Boolean;

implementation

function RpIsAlpha(achar:char):Boolean;
begin
 Result:=achar in ['0'..'9','A'..'Z','a'..'z'];
end;

function RpIsAlphaW(achar:Widechar):Boolean;
begin
 Result:=achar in [WideChar('0')..WideChar('9'),
  WideChar('A')..WideChar('Z'),WideChar('a')..WideChar('z')];
end;

procedure WriteDatabaseInfoXML(dbinfo:TRpDatabaseInfoItem;Stream:TStream);
begin
 WritePropertyS('ALIAS',dbinfo.Alias,Stream);
 WritePropertyS('CONFIGFILE',dbinfo.Configfile,Stream);
 WritePropertyBool('LOADPARAMS',dbinfo.LoadParams,Stream);
 WritePropertyBool('LOADDRIVERPARAMS',dbinfo.LoadDriverParams,Stream);
 WritePropertyBool('LOGINPROMPT',dbinfo.LoginPrompt,Stream);
 WritePropertyI('DRIVER',Integer(dbinfo.Driver),Stream);
 WritePropertyS('REPORTTABLE',dbinfo.ReportTable,Stream);
 WritePropertyS('REPORTSEARCHFIELD',dbinfo.ReportSearchField,Stream);
 WritePropertyS('REPORTFIELD',dbinfo.ReportField,Stream);
 WritePropertyS('REPORTGROUPSTABLE',dbinfo.ReportGroupsTable,Stream);
end;

procedure WriteDataInfoXML(dinfo:TRpDataInfoItem;Stream:TStream);
begin
 WritePropertyS('ALIAS',dinfo.Alias,Stream);
 WritePropertyS('DATABASEALIAS',dinfo.DatabaseAlias,Stream);
 WritePropertyW('SQL',dinfo.SQL,Stream);
 WritePropertyS('DATASOURCE',dinfo.DataSource,Stream);
 WritePropertyS('MYBASEFILENAME',dinfo.MyBaseFileName,Stream);
 WritePropertyS('MYBASEFIELDS',dinfo.MyBaseFields,Stream);
 WritePropertyS('MYBASEINDEXFIELDS',dinfo.MyBaseIndexFields,Stream);
 WritePropertyS('MYBASEMASTERFIELDS',dinfo.MyBaseMasterFields,Stream);
 WritePropertyS('BDEINDEXFIELDS',dinfo.BDEIndexFields,Stream);
 WritePropertyS('BDEINDEXNAME',dinfo.BDEIndexName,Stream);
 WritePropertyS('BDETABLE',dinfo.BDETable,Stream);
 WritePropertyI('BDETYPE',Integer(dinfo.BDEType),Stream);
 WritePropertyS('BDEFILTER',dinfo.BDEFilter,Stream);
 WritePropertyS('BDEMASTERFIELDS',dinfo.BDEMasterFields,Stream);
 WritePropertyS('BDEFIRSTRANGE',dinfo.BDEFirstRange,Stream);
 WritePropertyS('BDELASTRANGE',dinfo.BDELastRange,Stream);
 WritePropertyS('DATAUNIONS',dinfo.DataUnions.Text,Stream);
 WritePropertyBool('GROUPUNION',dinfo.GroupUnion,Stream);
end;

procedure WriteReportPropsXML(report:TRpReport;Stream:TStream);
begin
 WritePropertyW('WFONTNAME',report.WFontName,Stream);
 WritePropertyW('LFONTNAME',report.LFontName,Stream);
 WritePropertyBool('GRIDVISIBLE',report.GridVisible,Stream);
 WritePropertyBool('GRIDLINES',report.GridLines,Stream);
 WritePropertyBool('GRIDENABLED',report.GridEnabled,Stream);
 WritePropertyI('GRIDCOLOR',report.GridColor,Stream);
 WritePropertyI('GRIDWIDTH',report.GridWidth,Stream);
 WritePropertyI('GRIDHEIGHT',report.GridHeight,Stream);
 WritePropertyI('PAGEORIENTATION',Integer(report.PageOrientation),Stream);
 WritePropertyI('PAGESIZE',Integer(report.PageSize),Stream);
 WritePropertyI('PAGESIZEQT',report.PageSizeQt,Stream);
 WritePropertyI('PAGEHEIGHT',report.PageHeight,Stream);
 WritePropertyI('PAGEWIDTH',report.PageWidth,Stream);
 WritePropertyI('CUSTOMPAGEHEIGHT',report.CustomPageHeight,Stream);
 WritePropertyI('CUSTOMPAGEWIDTH',report.CustomPageWidth,Stream);
 WritePropertyI('PAGEBACKCOLOR',Integer(report.PageBackColor),Stream);
 WritePropertyI('PREVIEWSTYLE',Integer(report.PreviewStyle),Stream);
 WritePropertyBool('PREVIEWMARGINS',report.PreviewMargins,Stream);
 WritePropertyI('PREVIEWWINDOW',Integer(report.PreviewWindow),Stream);
 WritePropertyI('LEFTMARGIN',report.LeftMargin,Stream);
 WritePropertyI('TOPMARGIN',report.TopMargin,Stream);
 WritePropertyI('RIGHTMARGIN',report.RightMargin,Stream);
 WritePropertyI('BOTTOMMARGIN',report.BottomMargin,Stream);
 WritePropertyI('PRINTERSELECT',Integer(report.PrinterSelect),Stream);
 WritePropertyI('LANGUAGE',report.Language,Stream);
 WritePropertyI('COPIES',report.Copies,Stream);
 WritePropertyBool('COLLATECOPIES',report.CollateCopies,Stream);
 WritePropertyBool('TWOPASS',report.TwoPass,Stream);
 WritePropertyI('PRINTERFONTS',Integer(report.PrinterFonts),Stream);
 WritePropertyBool('PRINTONLYIFDATAAVAILABLE',report.PrintOnlyIfDataAvailable,Stream);
 WritePropertyI('STREAMFORMAT',Integer(report.StreamFormat),Stream);
 WritePropertyBool('REPORTACTIONDRAWERBEFORE',rpDrawerBefore in report.ReportAction,Stream);
 WritePropertyBool('REPORTACTIONDRAWERAFTER',rpDrawerAfter in report.ReportAction,Stream);
 WritePropertyBool('PREVIEWABOUT',report.PreviewAbout,Stream);
 WritePropertyI('TYPE1FONT',Integer(report.Type1Font),Stream);
 WritePropertyI('FONTSIZE',report.FontSize,Stream);
 WritePropertyI('FONTROTATION',report.FontRotation,Stream);
 WritePropertyI('FONTSTYLE',report.FontStyle,Stream);
 WritePropertyI('FONTCOLOR',report.FontColor,Stream);
 WritePropertyI('BACKCOLOR',report.BackColor,Stream);
 WritePropertyBool('TRANSPARENT',report.Transparent,Stream);
 WritePropertyBool('CUTTEXT',report.CutText,Stream);
 WritePropertyI('ALIGNMENT',report.Alignment,Stream);
 WritePropertyI('VALIGNMENT',report.VAlignment,Stream);
 WritePropertyBool('WORDWRAP',report.WordWrap,Stream);
 WritePropertyBool('SINGLELINE',report.SingleLine,Stream);
 WritePropertyS('BIDIMODES',report.BidiModes.Text,Stream);
 WritePropertyBool('MULTIPAGE',report.MultiPage,Stream);
 WritePropertyI('PRINTSTEP',Integer(report.PrintStep),Stream);
 WritePropertyI('PAPERSOURCE',report.PaperSource,Stream);
 WritePropertyI('DUPLEX',report.duplex,Stream);
 WritePropertyS('FORCEPAPERNAME',report.ForcePaperName,Stream);
end;

procedure WriteParamXML(aparam:TRpParam;Stream:TStream);
begin
 WritePropertyW('DESCRIPTION',aparam.Description,Stream);
 WritePropertyW('HINT',aparam.Hint,Stream);
 WritePropertyW('SEARCH',aparam.Search,Stream);
 WritePropertyS('NAME',aparam.Name,Stream);
 WritePropertyBool('VISIBLE',aparam.Visible,Stream);
 WritePropertyBool('ISREADONLY',aparam.IsReadOnly,Stream);
 WritePropertyBool('NEVERVISIBLE',aparam.NeverVisible,Stream);
 WritePropertyBool('ALLOWNULLS',aparam.AllowNulls,Stream);
 WritePropertyI('PARAMTYPE',Integer(aparam.ParamType),Stream);
 WritePropertyS('DATASETS',aparam.Datasets.Text,Stream);
 WritePropertyS('ITEMS',aparam.Items.Text,Stream);
 WritePropertyS('VALUES',aparam.Values.Text,Stream);

 if (aparam.Value<>Null) then
 begin
  case aparam.ParamType of
   rpParamString,rpParamExpreA,rpParamExpreB,rpParamSubst,rpParamList,rpParamUnknown:
    WritePropertyS('VALUE',aparam.AsString,Stream);
   rpParamInteger:
    begin
     WritePropertyI('VALUE',aparam.Value,Stream);
    end;
   rpParamDouble:
    begin
     WritePropertyD('VALUE',aparam.Value,Stream);
    end;
   rpParamCurrency:
    begin
     WritePropertyD('VALUE',aparam.Value,Stream);
    end;
   rpParamDate,rpParamTime,rpParamDateTime:
    WritePropertyD('VALUE',double(aparam.Value),Stream);
   rpParamBool:
    WritePropertyS('VALUE',aparam.Value,Stream);
  end;
 end;
end;

procedure WriteSubreportXML(subrep:TRpSubReport;Stream:TStream);
begin
 WritePropertyS('ALIAS',subrep.Alias,Stream);
 if assigned(subrep.ParentSubReport) then
 begin
  WritePropertyS('PARENTSUBREPORT',subrep.ParentSubReport.Name,Stream);
 end;
 if assigned(subrep.ParentSection) then
 begin
  WritePropertyS('PARENTSECTION',subrep.ParentSection.Name,Stream);
 end;
 WritePropertyBool('PRINTONLYIFDATAAVAILABLE',subrep.PrintOnlyIfDataAvailable,Stream);
 WritePropertyBool('REOPENONPRINT',subrep.ReOpenOnPrint,Stream);
end;

procedure WriteSectionXML(section:TRpSection;Stream:TStream);
var
 i:integer;
 aitem:TRpCommonPosComponent;
begin
 WriteStringToStream('<SECTION>'+#10,Stream);

 WritePropertyS('NAME',section.Name,Stream);
 WritePropertyI('WIDTH',section.Width,Stream);
 WritePropertyI('HEIGHT',section.height,Stream);
 if assigned(section.SubReport) then
 begin
  WritePropertyS('SUBREPORT',section.Subreport.Name,Stream);
 end;
 WritePropertyS('GROUPNAME',section.GroupName,Stream);
 WritePropertyBool('CHANGEBOOL',section.ChangeBool,Stream);
 WritePropertyBool('PAGEREPEAT',section.PageRepeat,Stream);
 WritePropertyBool('SKIPPAGE',section.SkipPage,Stream);
 WritePropertyBool('ALIGNBOTTOM',section.AlignBottom,Stream);
 WritePropertyI('SECTIONTYPE',Integer(section.SectionType),Stream);
 WritePropertyBool('AUTOEXPAND',section.AutoExpand,Stream);
 WritePropertyBool('AUTOCONTRACT',section.AutoContract,Stream);
 WritePropertyBool('HORZDESP',section.HorzDesp,Stream);
 WritePropertyBool('VERTDESP',section.VertDesp,Stream);
 WritePropertyS('EXTERNALFILENAME',section.ExternalFilename,Stream);
 WritePropertyS('EXTERNALCONNECTION',section.ExternalConnection,Stream);
 WritePropertyS('EXTERNALTABLE',section.ExternalTable,Stream);
 WritePropertyS('EXTERNALFIELD',section.ExternalField,Stream);
 WritePropertyS('EXTERNALSEARCHFIELD',section.ExternalSearchField,Stream);
 WritePropertyS('EXTERNALSEARCHVALUE',section.ExternalSearchValue,Stream);
 WritePropertyI('StreamFormat',Integer(section.StreamFormat),Stream);
 if assigned(section.ChildSubReport) then
 begin
  WritePropertyS('CHILDSUBREPORT',section.ChildSubreport.Name,Stream);
 end;
 WritePropertyBool('SKIPRELATIVEH',section.SkipRelativeH,Stream);
 WritePropertyBool('SKIPRELATIVEV',section.SkipRelativeV,Stream);
 WritePropertyI('SKIPRELATIVEV',Integer(section.SkipType),Stream);
 WritePropertyBool('ININUMPAGE',section.IniNumPage,Stream);
 WritePropertyBool('GLOBAL',section.Global,Stream);
 WritePropertyI('DPIRES',section.dpires,Stream);
 WritePropertyI('BACKSTYLE',Integer(section.BackStyle),Stream);
 WritePropertyI('DRAWSTYLE',Integer(section.DrawStyle),Stream);


 for i:=0 to section.ReportComponents.Count-1 do
 begin
  aitem:=TRpCommonPosComponent(section.ReportComponents.Items[i].Component);
  WriteComponentXML(aitem,Stream);
 end;
 WriteStringToStream('</SECTION>'+#10,Stream);
end;

procedure WriteComponentXML(comp:TRpCommonPosComponent;Stream:TStream);
var
 compt:TRpGenTextComponent;
 compl:TRpLabel;
 comps:TRpShape;
 compi:TRpImage;
 compe:TRpExpression;
 compb:TRpBarCode;
 compc:TRpChart;
begin
 WriteStringToStream('<COMPONENT>'+#10,Stream);

 WritePropertyS('CLASSNAME',UpperCase(comp.ClassName),Stream);
 WritePropertyI('WIDTH',comp.Width,Stream);
 WritePropertyI('HEIGHT',comp.height,Stream);
 // CommonPos
 WritePropertyI('POSX',comp.PosX,Stream);
 WritePropertyI('POSY',comp.PosX,Stream);
 WritePropertyI('ALIGN',Integer(comp.Align),Stream);
 // Common text component
 if comp is TRpGenTextComponent then
 begin
  compt:=TRpGenTextComponent(comp);
  WritePropertyW('WFONTNAME',compt.WFontName,Stream);
  WritePropertyW('LFONTNAME',compt.LFontName,Stream);
  WritePropertyI('BIDIMODE',Integer(compt.BidiMode),Stream);
  WritePropertyBool('RIGHTTOLEFT',compt.RightToLeft,Stream);
  WritePropertyI('PRINTALIGNMENT',Integer(compt.PrintAlignment),Stream);
  WritePropertyI('TYPE1FONT',Integer(compt.Type1Font),Stream);
  WritePropertyI('FONTSIZE',compt.FontSize,Stream);
  WritePropertyI('FONTROTATION',compt.FontRotation,Stream);
  WritePropertyI('FONTSTYLE',compt.FontStyle,Stream);
  WritePropertyI('FONTCOLOR',compt.FontColor,Stream);
  WritePropertyI('BACKCOLOR',compt.BackColor,Stream);
  WritePropertyBool('TRANSPARENT',compt.Transparent,Stream);
  WritePropertyBool('CUTTEXT',compt.CutText,Stream);
  WritePropertyI('ALIGNMENT',compt.Alignment,Stream);
  WritePropertyI('VALIGNMENT',compt.VAlignment,Stream);
  WritePropertyBool('WORDWRAP',compt.WordWrap,Stream);
  WritePropertyBool('SINGLELINE',compt.SingleLine,Stream);
  WritePropertyS('BIDIMODES',compt.BidiModes.Text,Stream);
  WritePropertyBool('MULTIPAGE',compt.Multipage,Stream);
  WritePropertyI('PRINTSTEP',Integer(compt.PrintStep),Stream);
 end;
 // TRpLabel
 if comp is TRpLabel then
 begin
  compl:=TRpLabel(comp);
  WritePropertyW('WIDETEXT',compl.WideText,Stream);
 end
 else
 // TRpExpression
 if comp is TRpExpression then
 begin
  compe:=TRpExpression(comp);

  WritePropertyW('EXPRESSION',compe.Expression,Stream);
  WritePropertyW('AGINIVALUE',compe.AgIniValue,Stream);
  WritePropertyW('EXPORTEXPRESSION',compe.ExportExpression,Stream);
  WritePropertyI('DATATYPE',Integer(compe.DataType),Stream);
  WritePropertyW('DISPLAYFORMAT',compe.DisplayFormat,Stream);
  WritePropertyS('IDENTIFIER',compe.Identifier,Stream);
  WritePropertyI('AGGREGATE',Integer(compe.Aggregate),Stream);
  WritePropertyS('GROUPNAME',compe.GroupName,Stream);
  WritePropertyI('AGTYPE',Integer(compe.AgType),Stream);
  WritePropertyBool('AUTOEXPAND',compe.AutoExpand,Stream);
  WritePropertyBool('AUTOCONTRACT',compe.AutoContract,Stream);
  WritePropertyBool('PRINTONLYONE',compe.PrintOnlyOne,Stream);
  WritePropertyBool('PRINTNULLS',compe.PrintNulls,Stream);
  WritePropertyW('EXPORTDISPLAYFORMAT',compe.ExportDisplayFormat,Stream);
  WritePropertyI('EXPORTLINE',compe.ExportLine,Stream);
  WritePropertyI('EXPORTPOSITION',compe.ExportPosition,Stream);
  WritePropertyI('EXPORTSIZE',compe.ExportSize,Stream);
  WritePropertyBool('EXPORTDONEWLINE',compe.ExportDoNewLine,Stream);
 end
 else
 // TRpShape
 if comp is TRpShape then
 begin
  comps:=TRpShape(comp);
  WritePropertyI('SHAPE',Integer(comps.Shape),Stream);
  WritePropertyI('BRUSHSTYLE',comps.BrushStyle,Stream);
  WritePropertyI('BRUSHCOLOR',comps.BrushColor,Stream);
  WritePropertyI('PENSTYLE',comps.PenStyle,Stream);
  WritePropertyI('PENCOLOR',comps.PenColor,Stream);
  WritePropertyI('PENWIDTH',comps.PenWidth,Stream);
 end
 else
 // TRpImage
 if comp is TRpImage then
 begin
  compi:=TRpImage(comp);
  WritePropertyW('EXPRESSION',compi.Expression,Stream);
  if assigned(compi.Stream) then
  begin
   if compi.Stream.Size>0 then
   begin
    compi.Stream.Seek(0,soFromBeginning);
    WritePropertyB('STREAM',compi.Stream,Stream);
   end;
  end;
  WritePropertyI('ROTATION',compi.Rotation,Stream);
  WritePropertyI('DRAWSTYLE',Integer(compi.DrawStyle),Stream);
  WritePropertyI('DPIRES',compi.dpires,Stream);
  WritePropertyI('COPYMODE',compi.CopyMode,Stream);
 end
 else
 // TRpChart
 if comp is TRpChart then
 begin
  compc:=TRpChart(comp);
  WritePropertyW('VALUEEXPRESSION',compc.ValueExpression,Stream);
  WritePropertyW('GETVALUECONDITION',compc.GetValueCondition,Stream);
  WritePropertyW('CHANGESERIEEXPRESSION',compc.ChangeSerieExpression,Stream);
  WritePropertyW('CAPTIONEXPRESSION',compc.CaptionExpression,Stream);
  WritePropertyW('SERIECAPTION',compc.SerieCaption,Stream);
  WritePropertyW('CLEAREXPRESSION',compc.ClearExpression,Stream);
//  WritePropertyI('SERIES',Integer(compc.Series),Stream);
  WritePropertyBool('CHANGESERIEBOOL',compc.ChangeSerieBool,Stream);
  WritePropertyI('CHARTTYPE',Integer(compc.ChartType),Stream);
  WritePropertyW('IDENTIFIER',compc.Identifier,Stream);
  WritePropertyBool('CLEAREXPRESSIONBOOL',compc.ClearExpressionBool,Stream);
  WritePropertyI('DRIVER',Integer(compc.Driver),Stream);
  WritePropertyBool('VIEW3D',compc.View3d,Stream);
  WritePropertyBool('VIEW3DWALLS',compc.View3dWalls,Stream);
  WritePropertyI('PERSPECTIVE',compc.Perspective,Stream);
  WritePropertyI('ELEVATION',compc.Elevation,Stream);
  WritePropertyI('ROTATION',compc.Rotation,Stream);
  WritePropertyI('ZOOM',compc.Rotation,Stream);
  WritePropertyI('HORZOFFSET',compc.HorzOffset,Stream);
  WritePropertyI('VERTOFFSET',compc.VertOffset,Stream);
  WritePropertyI('TILT',compc.Tilt,Stream);
  WritePropertyBool('ORTHOGONAL',compc.Orthogonal,Stream);
  WritePropertyI('MULTIBAR',Integer(compc.Multibar),Stream);
  WritePropertyI('RESOLUTION',compc.Resolution,Stream);
  WritePropertyBool('SHOWLEGEND',compc.ShowLegend,Stream);
  WritePropertyBool('SHOWHINT',compc.ShowHint,Stream);
 end
 else
 // TRpBarcode
 if comp is TRpBarcode then
 begin
  compb:=TRpBarcode(comp);
  WritePropertyW('EXPRESSION',compb.Expression,Stream);
  WritePropertyI('MODUL',compb.Modul,Stream);
  WritePropertyD('RATIO',compb.Modul,Stream);
  WritePropertyI('TYP',Integer(compb.Typ),Stream);
  WritePropertyBool('CHECKSUM',compb.CheckSum,Stream);
  WritePropertyW('DISPLAYFORMAT',compb.DisplayFormat,Stream);
  WritePropertyI('ROTATION',compb.Rotation,Stream);
  WritePropertyI('BCOLOR',compb.BColor,Stream);
  WritePropertyI('NUMCOLUMNS',compb.NumCOlumns,Stream);
  WritePropertyI('NUMROWS',compb.NumRows,Stream);
  WritePropertyI('ECCLevel',compb.ECCLevel,Stream);
  WritePropertyBool('TRUNCATED',compb.Truncated,Stream);
 end;
 WriteStringToStream('</COMPONENT>'+#10,Stream);
end;


function StringToRpString(astring:String):String;
var
 i:integer;
 alen:integer;
 asubs:String;
begin
 Result:='';
 alen:=0;
 for i:=1 to Length(astring) do
 begin
  if RpIsAlpha(astring[i]) then
  begin
   Result:=Result+astring[i];
   inc(alen);
   if (alen > C_MAXDATAWIDTH) then
   begin
    alen:=0;
    Result:=Result+#10;
   end;
  end
  else
  begin
   asubs:='#'+IntToStr(Ord(astring[i]))+'#';
   Result:=Result+asubs;
   alen:=alen+Length(asubs);
   if (alen > C_MAXDATAWIDTH) then
   begin
    alen:=0;
    Result:=Result+#10;
   end;
  end;
 end;
end;

function WStringToRpString(astring:WideString):String;
var
 i,alen:integer;
 subs:String;
begin
 Result:='';
 alen:=0;
 for i:=1 to Length(astring) do
 begin
  if RpIsAlphaW(astring[i]) then
  begin
   Result:=Result+astring[i];
   inc(alen);
   if (alen >C_MAXDATAWIDTH) then
   begin
    alen:=0;
    Result:=Result+#10;
   end;
  end
  else
  begin
   subs:='#'+IntToStr(Ord(astring[i]))+'#';
   Result:=Result+subs;
   alen:=alen+Length(subs);
   if (alen > C_MAXDATAWIDTH) then
   begin
    alen:=0;
    Result:=Result+#10;
   end;
  end;
 end;
end;

function RpStringToString(rpstring:string):String;
var
 anumber:string;
 i:integer;
begin
 Result:='';
 i:=1;
 while (i<Length(rpstring)) do
 begin
  if ((RpIsAlpha(rpstring[i])) or (rpstring[i]='#')) then
  begin
    if rpstring[i]='#' then
    begin
     anumber:='0';
     while (i<Length(rpstring)) do
     begin
      if rpstring[i]='#' then
      begin
       inc(i);
       break;
      end
      else
      begin
       anumber:=anumber+rpstring[i];
       inc(i);
      end;
     end;
     Result:=Result+Chr(StrToInt(anumber) mod 256);
    end
    else
    begin
     Result:=Result+rpstring[i];
     inc(i);
    end;
  end
  else
   inc(i);
 end;
end;

function RpStringToWString(rpstring:string):WideString;
var
 anumber:string;
 i:integer;
begin
 Result:='';
 i:=1;
 while (i<Length(rpstring)) do
 begin
  if ((RpIsAlpha(rpstring[i])) or (rpstring[i]='#')) then
  begin
    if rpstring[i]='#' then
    begin
     anumber:='0';
     while (i<Length(rpstring)) do
     begin
      if rpstring[i]='#' then
      begin
       inc(i);
       break;
      end
      else
      begin
       anumber:=anumber+rpstring[i];
       inc(i);
      end;
     end;
     Result:=Result+WideChar(StrToInt(anumber) mod 65535);
    end
    else
    begin
     Result:=Result+WideChar(rpstring[i]);
     inc(i);
    end;
  end
  else
   inc(i);
 end;
end;

procedure WritePropertyI(propname:string;propvalue:integer;stream:TStream);
var
 astring:string;
begin
 astring:='<'+propname+' type="Integer">'+IntToStr(propvalue)+'</'+propname+'>'+#10;
 WriteStringToStream(astring,stream);
end;

function RpDoubleToStr(avalue:double):string;
var
 olddec:char;
begin
 olddec:=DecimalSeparator;
 try
  Result:=FloatToStr(avalue);
 finally
  DecimalSeparator:=olddec;
 end;
end;

procedure WritePropertyD(propname:string;propvalue:double;stream:TStream);
var
 astring:string;
begin
 astring:='<'+propname+' type="Double">'+RpDoubleToStr(propvalue)+'</'+propname+'>'+#10;
 WriteStringToStream(astring,stream);
end;


function RpBoolToStr(avalue:Boolean):String;
begin
 if avalue then
  Result:='True'
 else
  Result:='False';
end;

procedure WritePropertyBool(propname:string;propvalue:Boolean;stream:TStream);
var
 astring:string;
begin
 astring:='<'+propname+' type="Boolean">'+RpBoolToStr(propvalue)+'</'+propname+'>'+#10;
 WriteStringToStream(astring,stream);
end;

procedure WritePropertyS(propname:string;propvalue:String;stream:TStream);
var
 astring:string;
begin
 astring:='<'+propname+' type="String">'+StringToRpString(propvalue)+'</'+propname+'>'+#10;
 WriteStringToStream(astring,stream);
end;

procedure WritePropertyW(propname:string;propvalue:WideString;stream:TStream);
var
 astring:string;
begin
 astring:='<'+propname+' type="WideString">'+WStringToRpString(propvalue)+'</'+propname+'>'+#10;
 WriteStringToStream(astring,stream);
end;

function NibbleToHex(n:integer):String;
begin
 Result:='';
 case n of
  0..9:
   Result:=IntToStr(n);
  10..15:
   Result:=Char(Ord('A')+n-10);
 end;
end;

function StreamToBin(astream:TStream):String;
var
 achar:byte;
 alen:integer;
begin
 Result:='';
 alen:=0;
 while astream.Read(achar,1)>0 do
 begin
  Result:=Result+NibbleToHex(achar shr 4)+NibbleToHex(achar AND $0F);
  inc(alen);
  if (alen mod C_MAXDATAWIDTH)=0 then
  begin
   Result:=Result+#10;
   alen:=0;
  end;
 end;
end;

procedure WritePropertyB(propname:string;propvalue:TStream;stream:TStream);
var
 astring:string;
begin
 astring:='<'+propname+' type="Binary">'+StreamToBin(propvalue)+'</'+propname+'>'+#10;
 WriteStringToStream(astring,stream);
end;

procedure WriteReportXML(areport:TComponent;Stream:TStream);
var
 report:TRpReport;
 i,j:integer;
 astring:string;
 asubrep:TRpSubReport;
 asec:TRpSection;
begin
 report:=TRpReport(areport);
 // Write header
 astring:='<?xml version="1.0" standalone="no"?>'+#10;
 WriteStringToStream(astring,stream);
 astring:='<!DOCTYPE REPORT_MANAGER_2>'+#10;
 WriteStringToStream(astring,stream);
 // Write XML Report properties
 astring:='<REPORT>'+#10;
 WriteStringToStream(astring,stream);
 WriteReportPropsXML(report,Stream);

 // Write database info list
 for i:=0 to report.databaseinfo.count-1 do
 begin
  astring:='<DATABASEINFO>'+#10;
  WriteStringToStream(astring,stream);
  WriteDatabaseInfoXML(report.databaseinfo.Items[i],Stream);
  astring:='</DATABASEINFO>'+#10;
  WriteStringToStream(astring,stream);
 end;
 // Write data info list
 for i:=0 to report.datainfo.count-1 do
 begin
  astring:='<DATAINFO>'+#10;
  WriteStringToStream(astring,stream);
  WriteDataInfoXML(report.datainfo.Items[i],Stream);
  astring:='</DATAINFO>'+#10;
  WriteStringToStream(astring,stream);
 end;
 // Write parameter list
 for i:=0 to report.Params.count-1 do
 begin
  astring:='<PARAMETER>'+#10;
  WriteStringToStream(astring,stream);
  WriteParamXML(report.params.Items[i],Stream);
  astring:='</PARAMETER>'+#10;
  WriteStringToStream(astring,stream);
 end;
 // Write SubReports
 for i:=0 to report.SubReports.Count-1 do
 begin
  asubrep:=report.SubReports.Items[i].SubReport;
  astring:='<SUBREPORT>'+#10;
  WriteStringToStream(astring,stream);
  WriteSubReportXML(asubrep,Stream);
  for j:=0 to asubrep.Sections.Count-1 do
  begin
   asec:=asubrep.Sections.Items[j].Section;
   WriteSectionXML(asec,Stream);
  end;
  astring:='</SUBREPORT>'+#10;
  WriteStringToStream(astring,stream);
 end;

 astring:='</REPORT>'+#10;
 WriteStringToStream(astring,stream);
end;

procedure ReadReportXML(areport:TComponent;Stream:TStream);
var
 astring:string;
 position:integer;
 propname:String;
 proptype:String;
 propvalue:String;

 procedure FindNextName;
 var
  abegin,aend:integer;
  typepos:integer;
 begin
  propname:='';
  proptype:='';
  abegin:=0;
  aend:=0;
  while position<Length(astring) do
  begin
   if astring[position]='<' then
   begin
    if abegin>0 then
     Raise Exception.Create(SRpStreamFormat);
    abegin:=position+1;
   end
   else
   if astring[position]='>' then
   begin
    if abegin=0 then
     Raise Exception.Create(SRpStreamFormat);
    aend:=position;
    inc(position);
    break;
   end;
   inc(position);
  end;
  if aend=0 then
   Raise Exception.Create(SRpStreamFormat);
  propname:=Trim(Copy(astring,abegin,aend-abegin));
  typepos:=Pos('type',propname);
  if typepos>0 then
  begin
   proptype:=Copy(propname,typepos+7,Length(propname));
   proptype:=Copy(proptype,1,Length(proptype)-1);
   propname:=Copy(propname,1,typepos-1);
  end;
  propvalue:='';
  while position<Length(astring) do
  begin
   if astring[position]='<' then
   begin
    break;
   end
   else
   begin
    propvalue:=propvalue+astring[position];
    inc(position);
   end;
  end;
  if propname='' then
   Raise Exception.Create(SRpStreamFormat);
 end;

begin
 // Find the <report label>
 SetLength(astring,Stream.Size);
 Stream.Read(astring[1],Stream.size);
 position:=Pos('<REPORT',astring);
 if position<1 then
  Raise Exception.Create(SRpStreamFormat);
 // Next name must be
 FindNextName;
 while Length(propname)>0 do
 begin
  if propname='/REPORT' then
   break;
  if propname='DATABASEINFO' then
  begin
   FindNextName;
   while propname<>'/DATABASEINFO' do
   begin

    FindNextName;
   end;
  end
  else
  if propname='DATAINFO' then
  begin
   FindNextName;
   while propname<>'/DATAINFO' do
   begin

    FindNextName;
   end;
  end
  else
  if propname='PARAMETER' then
  begin
   FindNextName;
   while propname<>'/PARAMETER' do
   begin

    FindNextName;
   end;
  end
  else
  if propname='SUBREPORT' then
  begin
   FindNextName;
   while propname<>'/SUBREPORT' do
   begin
    // Read subreport props
    if propname='SECTION' then
    begin
     FindNextName;
     while propname<>'/SECTION' do
     begin
      // Read Section props
      if propname='COMPONENT' then
      begin
       FindNextName;
       while propname<>'/COMPONENT' do
       begin
        // Read component props

        FindNextName;
       end;
      end;
      FindNextName;
     end;
    end;
    FindNextName;
   end;
  end;
  FindNextName;
 end;
end;

end.
