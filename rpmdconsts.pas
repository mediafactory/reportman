{*******************************************************}
{                                                       }
{       Report Manager                                  }
{                                                       }
{       Rpmdconsts                                        }
{                                                       }
{       Resource strings for reportmanager engine and   }
{       designer                                        }
{                                                       }
{       Copyright (c) 1994-2002 Toni Martir             }
{       toni@pala.com                                   }
{                                                       }
{       This file is under the MPL license              }
{       If you enhace this file you must provide        }
{       source code                                     }
{                                                       }
{                                                       }
{*******************************************************}

unit rpmdconsts;

interface



resourcestring

  SRpNoFilename='No filename assigned';

  SRpErrorFork='Error forking';
  SRpEnglish='English';
  SRpSpanish='Spanish';
  SRpCatalan='Catalan';
  SRpDirCantBeCreated='Directory can''t be created: ';
  SRpConfigFileNotExists='Configuration file missing: ';
  SRpPage='Page';
  SRpItem='Item';
  SRpCancel='Cancel';
  SRpInvalidClipboardFormat='Invalid clipboard format';
  SRpErrorReadingReport='Error reading report: ';
  SRpIgnoreError='Ignore?';
  SRpMainDataset='Main Dataset';
  SRpNewGroup='New Group';
  SRpSGroupName='Group Name';
  SRpGroupAlreadyExists='Group already exists:';
  SRpNoSpaceToPrint='No space to print ';
  SRpSection='Section';
  SRpNothingToPrint='Nothing to print';
  SRpLastPageReached='Last page already reached';
  SRPAliasNotExists='Data alias does not exists: ';
  SRpCopyStreamError='Error copying stream in metafile image object';
  SRpDriverNotSupported='Database driver not supported ';
  SrpDriverIBX='Interbase Express';
  SrpDriverADO='ADO Express (dbGo)';
  SrpDriverBDE='Borland Database Engine (BDE)';
  SrpDriverDBX='SQL Express (DBX)';
  SRpDriverAliasIsNotInterbase='The dbx driver alias is not Interbase';
  SRpNoDatabase='No Interbase Database specified';
  SRpSubreportAliasNotFound='Subreport alias not found:';
  SrpSAggregate='Aggregate';
  SRpNone='None';
  SRpGeneral='General';
  SrpSAgeGroup='Ag.Group';
  SrpSAgeType='Ag.Type';
  SrpSum='Sum';
  SRpMin='Min';
  SRpMax='Max';
  SRpAvg='Average';
  SRpStdDev='Std.Desv.';
  SrpSIniValue='Ag.I.Value';
  SrpSIdentifier='Identifier';
  SRpErrorIdenExpression='Error expression item no assigned (idenexpression)';
  SRpSHorzLine='Horz.Line';
  SRpSVertLine='Vert.Line';
  SRPHorzDesp='Horz.Desp.';
  SRpInvalidBoolean='Invalid boolean: ';
  SRpPaperNotFount='Paper name not found';
  SRpErrorCreatingPaper='Error creating paper form (privileges?): ';

  SRpSureDeleteSection='Delete selected section?';
  SRpNoDatasets='No datasets defined';
  SRpSampleTextToLabels='Text';
  SRpSampleExpression='2+2';
  SRpOnlyAReportOwner='Only a report can be the owner of :';
  SrpErrorProcesingFileMenu='Error procesing the File Menu for last saved files';
  SRpRepToTxt1='reptotxt v1.0';
  SRpRepToTxt2='Converts a report file from report manager (.rep) to a plain file';
  SRpRepToTxt3='Usage: reptotxt sourcefilename destinationfilename';

  SRpMetaPrint1='metaprint v1.0';
  SRpMetaPrint2='Prints a metafile report (.rpmf)';
  SRpMetaPrint3='Usage: metaprint [Options] metafilename';
  SRpMetaPrint4='Options: -d        Delete the file after printing it';
  SRpMetaPrint5='         -q        Quiet mode, don''t show progress';
  SRpMetaPrint6='         -from   n Prints report from page pnum';
  SRpMetaPrint7='         -to     n Prints report to page pnum';
  SRpMetaPrint8='         -copies n Prints pnum copies';
  SRpMetaPrint9='         -collate  Collate the copies';
  SRpTooManyParams='Too many parameters';
  SRpPrintingFile='Printing';
  SRpPrinted='Printed';
  SRpPrintedFileDeleted='Printed file deleted';
  SRpNoDriverPassedToPrint='No driver passed to beginprint of TrpReport';

  SRpTxtToRep1='txttorep v1.0';
  SRpTxtToRep2='Converts a plain file containing object descriptions to a report manager (.rep) file';
  SRpTxtToRep3='Usage: txttorep sourcefilename destinationfilename';

  SRpPrintRep1='printrep v1.0';
  SRpPrintRep2='Prints a report manager (.rep) file';
  SRpPrintRep3='Usage: printrep [Options] reportfilename';
  SRpPrintRep4='         -q        Quiet mode, don''t show progress';
  SRpPrintRep5='         -from   n Prints report from page pnum';
  SRpPrintRep6='         -to     n Prints report to page pnum';
  SRpPrintRep7='         -copies n Prints pnum copies';
  SRpPrintRep8='         -collate  Collate the copies';


  SRpPrintPDFRep1='printreptopdf v1.0';
  SRpPrintPDFRep2='Prints a report manager (.rep) file to a Adobe PDF file';
  SRpPrintPDFRep3='Usage: printrep [Options] reportfilename pdffilename';
  SRpPrintPDFRep4='         -q        Quiet mode, don''t show progress';
  SRpPrintPDFRep5='         -from   n Prints report from page pnum';
  SRpPrintPDFRep6='         -to     n Prints report to page pnum';
  SRpPrintPDFRep7='         -copies n Prints pnum copies';
  SRpPrintPDFRep8='         -u        Generate not compressed pdf';

  SrpSubReport='SubReport';
  SRpRepman='Report manager designer';
  SRpError='Error';
  SRpOpenSelec='Opens the selected file';
  SRptReportnotfound='Report not found';
  SRpMustInstall='No printers installed, you must install one';
  SRpConverstoolarge='Conversion overflow';
  SRpErrorMapmode='Error in mapmode of the device';
  SRpErrorUnitconversion='Error in unit conversion';
  SRpMinHeight='Minimal height exceeded';
  SRpMaxWidth='Maximum height exceeded';
  SRpAssignFunc='Can not set a value to a function';
  SRpAssignConst='Can not set a value to a constant';
  SRpAssignfield='Can not set a value to a field';
  SRpNohelp='No help available';
  SRpNoaparams='No parameters';
  SRpNomodel='No model';
  SRpTrueHelp='A constant with True value';
  SRpFalseHelp='A constant with False value';
  SRpFieldHelp='Database field';

  // Constants for TRpeval functions
  SRpUpperCase='Return the string in uppercase';
  SRpPUpperCase='s is the string to do uppercase';

  SRpLowerCase='Returns the string in lowercase';
  SRpPLowerCase='s is the string to do lowercase';

  SRpHourMinSec='Converts a number to a time string hh:mm:ss';
  SRpPHourMinSec='H represents hours, idenH is the conversion string '
   +chr(13)+'for hours, idenM for minutes and '+chr(13)
   +'idenS for seconds';

  SRpFloatToDateTime='Converts a float to a datetime type';
  SRpPFloatToDateTime='n is the number to convert';

  SRpSin='Returns the sin of an angle';
  SRpPSin='ang is expressed in radians';

  SRpRound='Rounds to a multiple of a number.';
  SRpPRound='num is the number to round and r i the multiplier';

  SRpInt='Returns the integer part of a number';
  SRpPInt='num is the number to obtain the int part.';

  SRpStr='Converts a number to a string';
  SRpPStr='num is the number to convert';

  SRpVal='Converts a string to a number';
  SRpPVal='s is the string to convert';

  SRpTrim='Returns a string without spaces in right-left sides';
  SRpPtrim='s is the string to be trimmed';

  SRpLeft='Returns the left side of a string';
  SRpPLeft='s is the source string,count is the number of chars to return';

  SRpPos='Returns the position index of a string inside another string (0 if not found)';
  SRpPPos='SubStr is the substring to search for';

  SRpAllDriver='[All]';
  SRpSelectDriver='You must select a driver first';
  SRpNewConnection='Create a new connection';
  SRpConnectionName='Connection name';
  SRpDropConnection='Drop connection';
  SRpSureDropConnection='Are you sure you want to drop connection: ';
  SRpVendorLib='Vendor Library';
  SRpLibraryName='Library name';
  SRpSelectConnectionFirst='You must select a connection first';
  SRpConnectionOk='Test connection passed';
  SRpFieldNotFound='Field not found: ';
  SRpNotAField='Not a field: ';
  SRpNotBinary='Not a binary field: ';
  SRpErrorReadingFromFieldStream='Error reading from field stream';

  SRpSQrt='The square of a number';
  SRpPSQRt='num is the number to square';

  SRpMod='Returns the module that is the rest of the integer division';
  SRpPMod='d1 is the dividend d2 is the divisor';

  SRpToday='Returns today date in datetime data type';
  SRpNow='Returns today and time in datetime data type';
  SRpTime='Returns the time in datetime datatype';

  SRpNull='Null';

  SRpMonthName='The name of the month in string';
  SRpPMonthName='d es de date to be decoded';

  SRpEvalText='Evals an expresion, returns the evaluated result';
  SRpPEvalText='expr is the expresion to evaluate';

  SRpMonth='Returns the month number (1-12)';
  SRpPMonth='d is the date to decode';

  SRpYear='Returns the year';
  SRpPyear='d is the date to decode';

  SRpDay='Returns the day';
  SRpPDay='d is the date to decode';

  SRpRight='Returns the right side of a string';
  SRpPRight='s is the source string and count is the number of characters to copy';

  SRpSubStr='Returns a substring of a string';
  SRpPSubStr='cadena is the sorce string, index is the index to copy from '+
   ' and count is the number of characters to copy';

  SRpFormatStr='Formats a string in diferent ways taking a picture of characters';
  SRpPFormatStr='Format is the format string: ex.''dd/mm/yyyy'''+
   ' and v is the value to convert to a formated string';
  SRpNumToText='Text representation of a number';
  SRpPNumToText='n is the number, f says if it'' female';

  SRpDivisioZero = 'Division by zero';
  SRpEvalType = 'Type conversion error';
  SRpInvalidOperation = 'Invalid operation';
  SRpEvalDescIden = 'Unknown identifier: ';
  SRpEvalParent = 'Parentesis error';

  SRpConvertError = 'Type conversion error';

  // TRpParser
  SRpIdentifierexpected = 'Indentifier expected';
  SRpstringexpected = 'String expected';
  SRpNumberexpected = 'Number expected';
  SRpOperatorExpected = 'Operator expected';
  SRpInvalidBinary = 'Invalid Binary';
  SRpExpected = 'Expected %s';
  SRpEvalsyntax = 'Syntax error';

  // TRpEvaluator
  SRpsetexpression='Can not set a value to a expression';
  SRpFieldDuplicated = 'Duplicated field you must use the alias';
  SRpVariabledefined = 'Variable redefined';

  // TRpExpredialog
  SRpOperatorSum='Sum operator';
  SRpOperatorDif='Substract operator';
  SRpOperatorMul='Multiply operator';
  SRpOperatorDiv='Division operator';
  SRpOperatorComp='Comparison operator';
  SRpOperatorLog='Logical operator';
  SRpOperatorDec='Decision operator';
  SRpOperatorDecM='IIF(condition,action1,action2)';
  SRpOperatorDecP='Condition is a boolean expresion, if it''s true the '+
        ' first parameter is executed, else the second is executed';
  SRpOperatorSep='Separator operator';
  SRpOperatorSepP='Is used to execute more than one expresión, the last is the result';

  SRpErrorOpenImp='Error opening the printer ';
  SRpPaperexists='The paper size already exists';

  // Printer constans
  SRpPrinting='The printer is already printing';
  SRpDefaultPrinter='Default printer';
  SRpReportPrinter='Reporting printer';
  SRpTicketPrinter='Ticket printer';
  SRpGraphicprinter='Graphics printer';
  SRpCharacterprinter='Character based printer';
  SRpReportPrinter2='Reporting printer 2';
  SRpTicketPrinter2='Ticket printer 2';
  SRpUserPrinter1='User printer 1';
  SRpUserPrinter2='User printer 2';
  SRpUserPrinter3='User printer 3';
  SRpUserPrinter4='User printer 4';
  SRpUserPrinter5='User printer 5';
  SRpUserPrinter6='User printer 6';
  SRpUserPrinter7='User printer 7';
  SRpUserPrinter8='User printer 8';
  SRpUserPrinter9='User printer 9';

  // Designer constants
  SRpREmoveElements='This sections has childs that will be removed.';
  SRpPageHeader='Page header';
  SRpReportHeader='Report header';
  SRpGeneralPageHeader='General page header';
  SRpGeneralReportHeader='General report header';
  SRpDetail='Detail';
  SRpHeader='Header';
  SRpFooter='Footer';
  SRpPageFooter='Page footer';
  SRpReportFooter='Report footer';
  SRpGroup='Group';
  SRpMaxGroupsExceded='Max number of groups execeded';
  SRpSelectGroupToRemove='Select the group to remove';
  SRpSelectGroup='Select a group';
  SRpGroupNameError='The group name is already in use, change it';
  SRpReportChanged='Report changed. Save the changes before close?';

  // Write read errors
  SRpErrorWriteSeccion='Error writing a section';
  SRpErrorReadSeccion='Error reading a section';
  SRpUntitled='Untitled';
  //  SRpHour = 'Hora';
  SRpSaveAborted='Save aborted';
  SRpOperationAborted='Operation aborted';
  SRpFileNameRequired='File name required to adquire config file';
  SRpAliasExists='The database alias already exists: ';
  SRPDabaseAliasNotFound='Database alias not found';
  SRpCircularDatalink='There is a circular datalink with: ';
  SRPMasterNotFound='Master dataset not found: ';


  SRpInvalidComponentAssigment='Error assigning the type must be a valid database';
  SRpNewDatabaseconf='New database alias configuration';
  SRpEnterTheName='Enter the alias';
  SRpChangeAliasConf='Rename the database configuration alias';
  SRpEnterTheNewName='Enter the new name';
  SRpDatabaseAliasNull='A database alias cannot be null';
  SRpDatabasenotassined='There is not database to connect/disconnect in alias';
  SRpConnectionsuccesfull='Connection Test OK';

  SRpNewaliasDef='New table/query';
  SRpAliasName='Alias Name';
  SRpTableAliasExists='The Alias Name already exists';


  SRpBadSignature='Bad report metafile signature';
  SRpBadFileHeader='Bad report metafile file header';
  SrpMtPageSeparatorExpected='Page separator expected in report metafile';
  SrpMtObjectSeparatorExpected='Object separator expected in report metafile';
  SrpObjectTypeError='Object type error in report metafile';
  SrpObjectDataError='Object data error in report metafile';
  SRpMetaIndexPageOutofBounds='Index page out of bounds in metafile';
  SRpMetaIndexObjectOutofBounds='Index object out of bounds in metafile';
  SRpPrintDriverIncorrect='Incorrect print driver';
  SRpWinGDINotInit='Report metafile wingdi driver not initialized';
  SRpQtDriverNotInit='Report metafile qt driver not initialized';
  SRpGDIDriverNotInit='Report metafile GDI driver not initialized';


  SRPNoSelectedSubreport='No selected subreport';
  SRPNoSelectedSection='No selected section';
  SRpGroupNameRequired='Group name required';
  SRpGroupNameExists='Group name already exists';
  SRpSubReportNotFound='Subreport not found';
  SRpSectionNotFound='Section not found';
  SRpAtLeastOneDetail='At least one detail must exists in a subreport';
  SRpAtLeastOneSubreport='At least one subreport must exists in a report';
  SrpNewDataset='New dataset';
  SrpRenameDataset='Rename dataset';
  SRpSaveChanges='Save configuration?';
  SRpParamNotFound='Parameter not found: ';
  SRpNewParam='New param';
  SRpParamName='Name';
  SRpParamNameExists='Param name already exists';
  SRpRenameParam='Rename param';
  // Font propd
  SRpBold='Bold';
  SRpUnderline='Underline';
  SRpItalic='Italic';
  SRpStrikeOut='StrikeOut';
  SRpSFontRotation='F.Rotation';

  // Component properties
  SRpSTop='Top';
  SRpSLeft='Left';
  SRpSWidth='Width';
  SRpSHeight='Height';
  SRpSCurrency='Currency';
  SRpSString='String';
  SRpSColor='Color';
  SRpSInteger='Integer';
  SRpSWFontName='WFont Name';
  SRpSLFontName='LFont Name';
  SRpSType1Font='PDF Font';
  SrpSFontSize='Font Size';
  SrpSFontColor='Font Color';
  SrpSBackColor='Back Color';
  SrpSFontStyle='Font Style';
  SrpSTransparent='Transparent';
  SRpSBool='Boolean';
  SRpSList='List';
  SrpSText='Text';
  SrpSExpression='Expression';
  SrpSBarcode='Expression';
  SrpSCalculatingBarcode='Error calculating barcode';
  SrpSDisplayFormat='Display format';
  SRpUnknownType='(Unknown type)';
  SrpSOnlyOne='P.Only One';
  SRpSBarcodeType='Bar type';
  SRpSShowText='Show Text';
  SRpSChecksum='Calc.Checksum';
  SRpSModul='Bar.Modul';
  SRpSRatio='Bar.Ratio';
  SRpWrongBarcodeType='Wrong Barcode Type';

  SRpSBSolid='Solid';
  SRpSBClear='Clear';
  SRpSBHorizontal='Horizontal';
  SRpSBVertical='Vertical';
  SRpSBFDiagonal='Diagonal A';
  SRpSBBDiagonal='Diagonal B';
  SRpSBCross='Cross';
  SRpSBDiagCross='Diag. Cross';
  SRpSBDense1='Dense 1';
  SRpSBDense2='Dense 2';
  SRpSBDense3='Dense 3';
  SRpSBDense4='Dense 4';
  SRpSBDense5='Dense 5';
  SRpSBDense6='Dense 6';
  SRpSBDense7='Dense 7';
  SRpSPSolid='Solid';
  SRpSPDash='Dash';
  SRpSPDot='Dot';
  SRpSPDashDot='Dash-Dot';
  SRpSPDashDotDot='Dash-Dot-Dot';
  SRpSPClear='Clear';
  SRpsSCircle='Circle';
  SRpsSEllipse='Ellipse';
  SRpsSRectangle='Rectangle';
  SRpsSRoundRect='Round Rect';
  SRpsSRoundSquare='Round Square';
  SRpsSSquare='Square';
  SRpSAutoExpand='Auto Expand.';
  SRpSAutoContract='Auto Contract';
  SRpSAfterPrint='After print';
  SRpSBeforePrint='Before print';
  SRpSPrintCondition='Print condition';
  SRpSGroupExpression='Group Expression';
  SRPSChangeBool='Bool Expression';
  SRPSPageRepeat='Page repeat';
  SRPSBeginPage='Begin Page';
  SRPSkipPage='Skip Page';
  SRPAlignBottom='Align Bottom';
  SRPBottom='Bottom';
  SRpSRight='Right';
  SRPAlign='Align';
  SRpIdentifierAlreadyExists='Identifier already exists';
  SrpSCutText='Cut Text';
  SrpSWordwrap='Word wrap';
  SrpSSingleLine='Single Line';
  SrpSAlignment='H.Alignment';
  SrpSVAlignment='V.Alignment';
  SRpSAlignNone='None';
  SRpSAlignLeft='Left';
  SRpSAlignRight='Right';
  SRpSAlignCenter='Center';
  SRpSAlignTop='Top';
  SRpSAlignBottom='Bottom';

  SRPSDrawCrop='Crop';
  SRPSDrawStretch='Strecth';
  SRPSDrawFull='Full';

  SrpSImage='Image';
  SRpKbytes='Kbytes';
  SRpInvalidImageFormat='Invalid image format';

  SRpPropertyisnotstream='Property is not a stream: ';

  SrpSShape='Shape';
  SrpSBrushStyle='Brush Style';
  SrpSBrushColor='Brush Color';
  SrpSPenStyle='Pen Style';
  SrpSPenColor='Pen Color';
  SrpSPenWIdth='Pen Width';


  SRpBlackness='Blackness';
  SRpDstInvert='DstInvert';
  SRpMergeCopy='MergeCopy';
  SRpMergePaint='MergePaint';
  SRpNotSrcCopy='NotSrcCopy';
  SRpNotSrcErase='NotSrcErase';
  SRpPatCopy='PatCopy';
  SRpPatInvert='PatInvert';
  SRpPatPaint='PatPaint';
  SRpSrcAnd='SrcAnd';
  SRpSrcCopy='SrcCopy';
  SRpSrcErase='SrcErase';
  SRpSrcInvert='SrcInvert';
  SRpSrcPaint='SrcPaint';
  SRpWhiteness='SrcWhiteness';
  SRpCreateMask='CreateMask';

  SrpCopyMode='CopyMode';
  SRpDPIRes='Resolution(dpi)';


  SRpDrawStyle='Draw Style';
  SRpDrawTile='Tile';
  SRpErrorWritingPage='Error writting metafile page';
  SrpStreamErrorPage='Error in metafile page format';

  SRpBringToFront='To Front';
  SRpSendToBack='To Back';
  SRpInvalidStreaminRpImage='Invalid stream in TRpImage';

  SRpPropertyNotFound='Property not found: ';
  SRpPropertyHaveNoListValues='This property have not a list of values';
  SRpIncorrectComponentForInterface='Incorrect component type for interface creation';

  SRpPropName='Property Name';
  SRpPropValue='Property Value';

  SRpUndefinedPaintInterface='Undefined paint interface';
  SRpNoDriverName='No drivername assigned to connection: ';

  SRpIncorrectCalltoDeawGrid='Incorrect call to DrawBitmapGrid';
  SRpSNotYetImplemented='Feature not yet implemented';
  SRpNoFileNameProvided='No filename provided';
  SRpRecordCount='Record count';

  SRpNoStreamToSaveReport='No Stream to save Report (TRpDesigner)';
  SRpDocNotInstalled='Documentation not installed.';
  SRpDocNotInstalled2='Download it and install in the application directory.';
  SRpDocNotInstalled3='http://reportman.sourceforge.net';

  SRpSelectAddConnection='You must select first add/select connection';

  SRpStreamNotValid='PDF Stream not valid';
  SRpNotPrintingPDF='Not in pdf printing state';

  SRpInvalidBitmapHeaderSize='Invalid Bitmap Header Size';
  SRpBadBitmapFileHeader='Bad bitmap file header';
  SRpBadBitmapStream='Bad bitmap stream';
  SRpBitMapInfoHeaderBitCount='Invalid bit depth in bitmap';
  SRpInvalidBitmapPalette='Invalid bitmap palette';
  SRpBadColorIndex='Bad color index in bitmap';
  SRpRLECompBitmapPDF='Compressed RLE bitmaps not supported in PDF Export';
  SRpMonochromeBitmapPDF='Monochrome bitmaps not supported in PDF Export';
  SRpParamBDENotSupported='Parameters not supported in BDE Tables';

  SRpPDFFile='PDF File - compressed';
  SRpPDFFileUn='PDF File - uncompressed';
  SRpRepMetafile='Report Metafile';
  SRpRepFile='Report File';
  SRpAnyFile='Any File';
  SRpBitmapImages='Bitmap images';
  SRpUnkownClassForMultiSelect='Unknown class for multi-select';
  SRpClassNotRegistered='Class not registered -';
  SRpSampleBarCode='5449000000996';
  SRpDatasetNotExists='Dataset not exists';
  SRpDatabaseNotExists='Database not exists';
implementation


end.
