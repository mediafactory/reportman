{*******************************************************}
{                                                       }
{       Report Manager                                  }
{                                                       }
{       Rpexceldriver                                   }
{       Exports a metafile to a excel sheet             }
{       can be used only for windows                    }
{                                                       }
{       Copyright (c) 1994-2003 Toni Martir             }
{       toni@pala.com                                   }
{                                                       }
{       This file is under the MPL license              }
{       If you enhace this file you must provide        }
{       source code                                     }
{                                                       }
{                                                       }
{*******************************************************}

unit rpexceldriver;

interface

{$I rpconf.inc}

uses
 mmsystem,windows,
{$IFDEF USEEXCEL97}
 excel97,
{$ENDIF}
{$IFDEF USEEXCEL2000}
 excel97,
{$ENDIF}
{$IFDEF USEEXCELXP}
 excelXP,
{$ENDIF}
 Classes,sysutils,rpmetafile,rpmdconsts,Graphics,Forms,
 rpmunits,Dialogs, Controls,
 StdCtrls,ExtCtrls,rppdffile,rpgraphutilsvcl,
{$IFDEF USEVARIANTS}
 types,Variants,
{$ENDIF}
 rptypes,rpvgraphutils,jpeg;


const
 XLS_PRECISION=100;
type
  TFRpExcelProgress = class(TForm)
    BCancel: TButton;
    LProcessing: TLabel;
    LRecordCount: TLabel;
    LTitle: TLabel;
    LTittle: TLabel;
    BOK: TButton;
    GPrintRange: TGroupBox;
    EFrom: TEdit;
    ETo: TEdit;
    LTo: TLabel;
    LFrom: TLabel;
    RadioAll: TRadioButton;
    RadioRange: TRadioButton;
    procedure FormCreate(Sender: TObject);
    procedure BCancelClick(Sender: TObject);
    procedure BOKClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    allpages:boolean;
    isvisible:boolean;
    frompage,topage:integer;
    dook:boolean;
    procedure AppIdle(Sender:TObject;var done:boolean);
  public
    { Public declarations }
    cancelled:boolean;
    oldonidle:TIdleEvent;
    tittle:string;
    filename:string;
    metafile:TRpMetafileReport;
  end;



function ExportMetafileToExcel (metafile:TRpMetafileReport; filename:string;
 showprogress,visible,allpages:boolean; frompage,topage:integer):boolean;

implementation



{$R *.dfm}

const
 AlignmentFlags_SingleLine=64;
 AlignmentFlags_AlignHCenter = 4 { $4 };
 AlignmentFlags_AlignTop = 8 { $8 };
 AlignmentFlags_AlignBottom = 16 { $10 };
 AlignmentFlags_AlignVCenter = 32 { $20 };
 AlignmentFlags_AlignLeft = 1 { $1 };
 AlignmentFlags_AlignRight = 2 { $2 };


function IsValidNumberChar(achar:char):Boolean;
begin
 Result:=false;
 if (achar in ['-','+','e','E','0'..'9',' ',DecimalSeparator]) then
  Result:=true;
end;

function VarTryStrToFloat(S: string; var Value: Extended): Boolean;
var
 index,i:integer;
begin
 Result:=true;
 S:=Trim(S);
 // Remove thousand separators
 repeat
  index:=Pos(ThousandSeparator,S);
  if index>0 then
   S:=Copy(S,1,index-1)+Copy(S,index+1,Length(S));
 until index=0;
 for i:=1 to Length(S) do
 begin
  if Not IsValidNumberChar(S[i]) then
  begin
   result:=false;
   break;
  end;
 end;
 if not result then
  exit;
 try
  Value:=StrToFloat(S);
 except
  Result:=false;
 end;
end;

procedure PrintObject(sh:ExcelWorkSheet;page:TRpMetafilePage;obj:TRpMetaObject;dpix,dpiy:integer;toprinter:boolean;
 rows,columns:TStringList;FontName:String;FontSize:integer);
var
 aansitext:string;
 arow,acolumn:integer;
 leftstring,topstring:String;
 number:extended;
 isanumber:boolean;
 afontStyle:TFontStyles;
 acolor:TColor;
begin
 topstring:=FormatCurr('0000000000',obj.Top/XLS_PRECISION);
 if (obj.AlignMent AND AlignmentFlags_AlignRight)>0 then
  leftstring:=FormatCurr('0000000000',(obj.Left+obj.Width)/XLS_PRECISION)
 else
  if (obj.AlignMent AND AlignmentFlags_AlignHCenter)>0 then
   leftstring:=FormatCurr('0000000000',(obj.Left+(obj.Width/2))/XLS_PRECISION)
  else
   leftstring:=FormatCurr('0000000000',obj.Left/XLS_PRECISION);
 arow:=rows.IndexOf(topstring)+1;
 acolumn:=columns.IndexOf(leftstring)+1;
 if acolumn<1 then
  acolumn:=1;
 if arow<1 then
  arow:=1;
 case obj.Metatype of
  rpMetaText:
   begin
    aansitext:=page.GetText(Obj);
    // If it's a number
    isanumber:=VarTryStrToFloat(aansitext,number);
    if isanumber then
     sh.Cells.item[arow,acolumn].Value:=number
    else
     sh.Cells.item[arow,acolumn].Value:=aansitext;
    if FontName<>page.GetWFontName(Obj) then
     sh.Cells.item[arow,acolumn].Font.Name:=page.GetWFontName(Obj);
    if obj.FontSize<>FontSize then
     sh.Cells.item[arow,acolumn].Font.Size:=Obj.FontSize;
    acolor:=CLXColorToVCLColor(Obj.FontColor);
    if acolor<>clBlack then
     sh.Cells.item[arow,acolumn].Font.Color:=acolor;
    afontstyle:=CLXIntegerToFontStyle(obj.FontStyle);
    if fsItalic in afontstyle then
     sh.Cells.item[arow,acolumn].Font.Italic:=true;
    if fsBold in afontstyle then
     sh.Cells.item[arow,acolumn].Font.Bold:=true;
    if fsUnderline in afontstyle then
    sh.Cells.item[arow,acolumn].Font.Underline:=true;
    if fsStrikeout in afontstyle then
     sh.Cells.item[arow,acolumn].Font.Strikethrough:=true;
    // Font rotation not possible
    if (obj.AlignMent AND AlignmentFlags_AlignHCenter)>0 then
     sh.Cells.item[arow,acolumn].HorizontalAlignment:=xlHAlignCenter;
    // Multiline not supported
 //    if (obj.AlignMent AND AlignmentFlags_SingleLine)=0 then
//     sh.Cells.item[arow,acolumn].Multiline:=true;
    if (obj.AlignMent AND AlignmentFlags_AlignLEFT)>0 then
     if isanumber then
      sh.Cells.item[arow,acolumn].HorizontalAlignment:=xlHAlignLeft;
    if (obj.AlignMent AND AlignmentFlags_AlignRight)>0 then
     if not isanumber then
      sh.Cells.item[arow,acolumn].HorizontalAlignment:=xlHAlignRight;
    // Word wrap not supported
//    if obj.WordWrap then
//     sh.Cells.item[arow,acolumn].WordWrap:=True;
//    if Not obj.CutText then
//     aalign:=aalign or DT_NOCLIP;
//    if obj.RightToLeft then
//     aalign:=aalign or DT_RTLREADING;
    if Not obj.Transparent then
     sh.Cells.Item[arow,acolumn].Color:=CLXColorToVCLColor(obj.BackColor);
   end;
  rpMetaDraw:
   begin
{    Width:=round(obj.Width*dpix/TWIPS_PER_INCHESS);
    Height:=round(obj.Height*dpiy/TWIPS_PER_INCHESS);
    abrushstyle:=obj.BrushStyle;
    if obj.BrushStyle>integer(bsDiagCross) then
     abrushstyle:=integer(bsDiagCross);
    Canvas.Pen.Color:=CLXColorToVCLColor(obj.Pencolor);
    Canvas.Pen.Style:=TPenStyle(obj.PenStyle);
    Canvas.Brush.Color:=CLXColorToVCLColor(obj.BrushColor);
    Canvas.Brush.Style:=TBrushStyle(abrushstyle);
    Canvas.Pen.Width:=Round(dpix*obj.PenWidth/TWIPS_PER_INCHESS);
    X := Canvas.Pen.Width div 2;
    Y := X;
    W := Width - Canvas.Pen.Width + 1;
    H := Height - Canvas.Pen.Width + 1;
    if Canvas.Pen.Width = 0 then
    begin
     Dec(W);
     Dec(H);
    end;
    if W < H then
     S := W
    else
     S := H;
    if TRpShapeType(obj.DrawStyle) in [rpsSquare, rpsRoundSquare, rpsCircle] then
    begin
     Inc(X, (W - S) div 2);
     Inc(Y, (H - S) div 2);
     W := S;
     H := S;
    end;
    case TRpShapeType(obj.DrawStyle) of
     rpsRectangle, rpsSquare:
      Canvas.Rectangle(X+PosX, Y+PosY, X+PosX + W, Y +PosY+ H);
     rpsRoundRect, rpsRoundSquare:
      Canvas.RoundRect(X+PosX, Y+PosY, X +PosX + W, Y + PosY+ H, S div 4, S div 4);
     rpsCircle, rpsEllipse:
      Canvas.Ellipse(X+PosX, Y+PosY, X+PosX + W, Y+PosY + H);
     rpsHorzLine:
      begin
       Canvas.MoveTo(X+PosX, Y+PosY);
       Canvas.LineTo(X+PosX+W, Y+PosY);
      end;
     rpsVertLine:
      begin
       Canvas.MoveTo(X+PosX, Y+PosY);
       Canvas.LineTo(X+PosX, Y+PosY+H);
      end;
     rpsOblique1:
      begin
       Canvas.MoveTo(X+PosX, Y+PosY);
       Canvas.LineTo(X+PosX+W, Y+PosY+H);
      end;
     rpsOblique2:
      begin
       Canvas.MoveTo(X+PosX, Y+PosY+H);
       Canvas.LineTo(X+PosX+W, Y+PosY);
      end;
    end;
}   end;
  rpMetaImage:
   begin
    // Inserting images to excel is not supported for now
{    Width:=round(obj.Width*dpix/TWIPS_PER_INCHESS);
    Height:=round(obj.Height*dpiy/TWIPS_PER_INCHESS);
    rec.Top:=PosY;
    rec.Left:=PosX;
    rec.Bottom:=rec.Top+Height-1;
    rec.Right:=rec.Left+Width-1;

    stream:=page.GetStream(obj);
    bitmap:=TBitmap.Create;
    try
     bitmap.PixelFormat:=pf32bit;
     bitmap.HandleType:=bmDIB;
     if GetJPegInfo(stream,bitmapwidth,bitmapheight) then
     begin
      jpegimage:=TJPegImage.Create;
      try
       jpegimage.LoadFromStream(stream);
       bitmap.Assign(jpegimage);
      finally
       jpegimage.free;
      end;
     end
     else
     // Looks if it's a jpeg image
      bitmap.LoadFromStream(stream);
//     Copy mode does not work for StretDIBBits
//     Canvas.CopyMode:=CLXCopyModeToCopyMode(obj.CopyMode);

     case TRpImageDrawStyle(obj.DrawImageStyle) of
      rpDrawFull:
       begin
        rec.Bottom:=rec.Top+round(bitmap.height/obj.dpires*dpiy)-1;
        rec.Right:=rec.Left+round(bitmap.width/obj.dpires*dpix)-1;
        recsrc.Left:=0;
        recsrc.Top:=0;
        recsrc.Right:=bitmap.Width-1;
        recsrc.Bottom:=bitmap.Height-1;
        DrawBitmap(Canvas,bitmap,rec,recsrc);
       end;
      rpDrawStretch:
       begin
        recsrc.Left:=0;
        recsrc.Top:=0;
        recsrc.Right:=bitmap.Width-1;
        recsrc.Bottom:=bitmap.Height-1;
        DrawBitmap(Canvas,bitmap,rec,recsrc);
       end;
      rpDrawCrop:
       begin
        recsrc.Left:=0;
        recsrc.Top:=0;
        recsrc.Right:=rec.Right-rec.Left;
        recsrc.Bottom:=rec.Bottom-rec.Top;
        DrawBitmap(Canvas,bitmap,rec,recsrc);
       end;
      rpDrawTile:
       begin
        // Set clip region
        oldrgn:=CreateRectRgn(0,0,2,2);
        aresult:=GetClipRgn(Canvas.Handle,oldrgn);
        newrgn:=CreateRectRgn(rec.Left,rec.Top,rec.Right,rec.Bottom);
        SelectClipRgn(Canvas.handle,newrgn);
        DrawBitmapMosaicSlow(Canvas,rec,bitmap);
        if aresult=0 then
         SelectClipRgn(Canvas.handle,0)
        else
         SelectClipRgn(Canvas.handle,oldrgn);
       end;
     end;
    finally
     bitmap.Free;
    end;}
   end;
 end;
end;



procedure DoExportMetafile(metafile:TRpMetafileReport;filename:string;
 aform:TFRpExcelProgress;visible,allpages:boolean;frompage,topage:integer);
var
 i:integer;
 j:integer;
 apage:TRpMetafilePage;
 dpix,dpiy:integer;
 mmfirst,mmlast:DWORD;
 difmilis:int64;
 wb:ExcelWorkBook;
 sh:ExcelWorkSheet;
 Excel:TExcelApplication;
 columns:TStringList;
 rows:TStringList;
 index:integer;
 topstring,leftstring:string;
 shcount:integer;
 FontName:String;
 FontSize:integer;
begin
 dpix:=Screen.PixelsPerInch;
 dpiy:=dpix;
 // Get the time
 mmfirst:=TimeGetTime;
 if allpages then
 begin
  frompage:=0;
  topage:=metafile.PageCount-1;
 end
 else
 begin
  frompage:=frompage-1;
  topage:=topage-1;
  if topage>metafile.PageCount-1 then
   topage:=metafile.PageCount-1;
 end;
 // Distribute in rows and columns
 columns:=TStringList.Create;
 rows:=TStringList.Create;
 try
   rows.sorted:=true;
   columns.sorted:=true;
   // Creates the excel file
   Excel:=TExcelApplication.Create(Application);
   Excel.Visible[1]:=Visible;
   wb:=Excel.Workbooks.add(null,1);
   shcount:=1;
   sh:=wb.Worksheets.item[shcount] As ExcelWorkSheet;
   FontName:=sh.Cells.Font.Name;
   FontSize:=sh.Cells.Font.Size;

   for i:=frompage to topage do
   begin
    if wb.Worksheets.Count<shcount then
     wb.Worksheets.Add(NULL,wb.Worksheets.Item[wb.Worksheets.Count],1,NULL,1);
    sh:=wb.Worksheets.item[shcount] As ExcelWorkSheet;
    inc(shcount);
    apage:=metafile.Pages[i];
    rows.clear;
    columns.clear;
    for j:=0 to apage.ObjectCount-1 do
    begin
     if apage.Objects[j].Metatype in [rpMetaText,rpMetaImage] then
     begin
      topstring:=FormatCurr('0000000000',apage.Objects[j].Top/XLS_PRECISION);
      if (apage.Objects[j].AlignMent AND AlignmentFlags_AlignRight)>0 then
        leftstring:=FormatCurr('0000000000',(apage.Objects[j].Left+apage.Objects[j].Width)/XLS_PRECISION)
      else
       if (apage.Objects[j].AlignMent AND AlignmentFlags_AlignHCenter)>0 then
        leftstring:=FormatCurr('0000000000',(apage.Objects[j].Left+apage.Objects[j].Width/2)/XLS_PRECISION)
       else
        leftstring:=FormatCurr('0000000000',apage.Objects[j].Left/XLS_PRECISION);
      index:=rows.IndexOf(topstring);
      if index<0 then
       rows.Add(topstring);
      index:=columns.IndexOf(leftstring);
      if index<0 then
       columns.Add(leftstring);
     end;
    end;
    for j:=0 to apage.ObjectCount-1 do
    begin
     PrintObject(sh,apage,apage.Objects[j],dpix,dpiy,true,
      rows,columns,FontName,FontSize);
     if assigned(aform) then
     begin
      mmlast:=TimeGetTime;
      difmilis:=(mmlast-mmfirst);
      if difmilis>MILIS_PROGRESS then
      begin
       // Get the time
       mmfirst:=TimeGetTime;
       aform.LRecordCount.Caption:=SRpPage+':'+ IntToStr(i+1)+
         ' - '+SRpItem+':'+ IntToStr(j+1);
       Application.ProcessMessages;
       if aform.cancelled then
        Raise Exception.Create(SRpOperationAborted);
      end;
     end;
    end;
   end;
 finally
  columns.free;
  rows.free;
 end;
 if Length(Filename)>0 then
 begin
{$IFDEF USEEXCELXP}
  wb.SaveAs(Filename, Null, Null, Null, False, False, xlNoChange, Null, True, Null, Null,0,Null);
{$ENDIF}
{$IFNDEF USEEXCELXP}
  wb.SaveAs(Filename, Null, Null, Null, False, False, xlNoChange, Null, True, Null, Null,0);
{$ENDIF}
  wb.Close(True,Filename,False,0);
 end;
 if not visible then
  Excel.Disconnect;
 if assigned(aform) then
  aform.close;
end;

function ExportMetafileToExcel (metafile:TRpMetafileReport; filename:string;
 showprogress,visible,allpages:boolean; frompage,topage:integer):boolean;
var
 dia:TFRpExcelProgress;
begin
 Result:=true;
 if Not ShowProgress then
 begin
  DoExportMetafile(metafile,filename,nil,visible,allpages,frompage,topage);
  exit;
 end;
 dia:=TFRpExcelProgress.Create(Application);
 try
  dia.oldonidle:=Application.OnIdle;
  try
   dia.metafile:=metafile;
   dia.filename:=filename;
   dia.allpages:=allpages;
   dia.frompage:=frompage;
   dia.isvisible:=visible;
   dia.topage:=topage;
   Application.OnIdle:=dia.AppIdle;
   dia.ShowModal;
   Result:=Not dia.cancelled;
  finally
   Application.OnIdle:=dia.oldonidle;
  end;
 finally
  dia.free;
 end;
end;


procedure TFRpExcelProgress.FormCreate(Sender: TObject);
begin
 LRecordCount.Font.Style:=[fsBold];
 LTittle.Font.Style:=[fsBold];

 BOK.Caption:=TranslateStr(93,BOK.Caption);
 BCancel.Caption:=TranslateStr(94,BCancel.Caption);
 LTitle.Caption:=TranslateStr(252,LTitle.Caption);
 LProcessing.Caption:=TranslateStr(253,LProcessing.Caption);
 GPrintRange.Caption:=TranslateStr(254,GPrintRange.Caption);
 LFrom.Caption:=TranslateStr(255,LFrom.Caption);
 LTo.Caption:=TranslateStr(256,LTo.Caption);
 RadioAll.Caption:=TranslateStr(257,RadioAll.Caption);
 RadioRange.Caption:=TranslateStr(258,RadioRange.Caption);
 Caption:=TranslateStr(259,Caption);

end;

procedure TFRpExcelProgress.AppIdle(Sender:TObject;var done:boolean);
begin
 cancelled:=false;
 Application.OnIdle:=nil;
 done:=false;
 LTittle.Caption:=tittle;
 LProcessing.Visible:=true;
 DoExportMetafile(metafile,filename,self,isvisible,allpages,frompage,topage);
end;


procedure TFRpExcelProgress.BCancelClick(Sender: TObject);
begin
 cancelled:=true;
end;




procedure TFRpExcelProgress.BOKClick(Sender: TObject);
begin
 FromPage:=StrToInt(EFrom.Text);
 ToPage:=StrToInt(ETo.Text);
 if FromPage<1 then
  FromPage:=1;
 if ToPage<FromPage then
  ToPage:=FromPage;
 Close;
 dook:=true;
end;

procedure TFRpExcelProgress.FormShow(Sender: TObject);
begin
 if BOK.Visible then
 begin
  EFrom.Text:=IntToStr(FromPage);
  ETo.Text:=IntToStr(ToPage);
 end;
end;

procedure GraphicExtent(Stream:TMemoryStream;var extent:TPoint;dpi:integer);
var
 graphic:TBitmap;
 jpegimage:TJpegImage;
 bitmapwidth,bitmapheight:integer;
begin
 if dpi<=0 then
  exit;
 graphic:=TBitmap.Create;
 try
  if GetJPegInfo(Stream,bitmapwidth,bitmapheight) then
  begin
   jpegimage:=TJpegImage.Create;
   try
    jpegimage.LoadFromStream(Stream);
    graphic.Assign(jpegimage);
   finally
    jpegimage.free;
   end;
  end
  else
   Graphic.LoadFromStream(Stream);
  extent.X:=Round(graphic.width/dpi*TWIPS_PER_INCHESS);
  extent.Y:=Round(graphic.height/dpi*TWIPS_PER_INCHESS);
 finally
  graphic.Free;
 end;
end;

end.

