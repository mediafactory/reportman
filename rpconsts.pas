{*******************************************************}
{                                                       }
{       Report Manager                                  }
{                                                       }
{       Rpconsts                                        }
{                                                       }
{       Resource strings for reportmanager engine and   }
{       designer                                        }
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

unit rpconsts;

interface



resourcestring
  SRpEnglish='English';
  SRpSpanish='Spanish';
  SRpCatalan='Catalan';

  SRpOnlyAReportOwner='Only a report can be the owner of :';
  SrpErrorProcesingFileMenu='Error procesing the File Menu for last saved files';
  SRpRepToTxt1='reptotxt v1.0';
  SRpRepToTxt2='Converts a report file from report manager (.rep) to a plain file';
  SRpRepToTxt3='Usage: reptotxt sourcefilename destinationfilename';

  SRpTxtToRep1='txttorep v1.0';
  SRpTxtToRep2='Converts a plain file containing object descriptions to a report manager (.rep) file';
  SRpTxtToRep3='Usage: txttorep sourcefilename destinationfilename';

  SRpPrintRep1='printrep v1.0';
  SRpPrintRep2='Prints a report manager (.rep) file';
  SRpPrintRep3='Usage: printrep reportfilename';

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
//  SRpGraphicClear='Elimina toda la informaci�n acumulada en una variable de tipo gr�fico';
//  SRpPGraphicClear='Gr es el nombre de la variable de tipo gr�fico, devuelve cierto';

//  SRpGraphicNew='A�ade un punto de informaci�n en una variable de tipo gr�fico, pudiendo cambiar de serie';
//  SRpPGraphicNew='Gr es el nombre de la variable de tipo gr�fico, '+
//    ' V es el punto, C es si debemos cambiar de serie, etiq es la etiqueta del punto y caption es la leyenda de la serie';

  SRpSQrt='The square of a number';
  SRpPSQRt='num is the number to square';

  SRpMod='Returns the module that is the rest of the integer division';
  SRpPMod='d1 is the dividend d2 is the divisor';

  SRpToday='Returns today date in datetime data type';
  SRpNow='Returns today and time in datetime data type';
  SRpTime='Returns the time in datetime datatype';

  SRpNull='The NULL value';

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
  SRpOperatorSepP='Is used to execute more than one expresi�n, the last is the result';

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

  // Component properties
  SRpSTop='Top';
  SRpSLeft='Left';
  SRpSWidth='Width';
  SRpSHeight='Height';
  SRpSCurrency='Currency';
  SRpSString='String';
  SRpSInteger='Integer';
  SRpSFontName='Font Name';
  SrpSFontSize='Font Size';
  SrpSFontColor='Font Color';
  SrpSBackColor='Back Color';
  SrpSFontStyle='Font Style';
  SrpSTransparent='Transparent';
  SRpSBool='Boolean';
  SrpSText='Text';

  SRpPropertyNotFound='Property not found: ';
  SRpPropertyHaveNoListValues='This property have not a list of values';
  SRpIncorrectComponentForInterface='Incorrect component type for interface creation';

  SRpPropName='Property Name';
  SRpPropValue='Property Value';

  SRpUndefinedPaintInterface='Undefined paint interface';
  SRpNoDriverName='No drivername assigned to connection: ';

  SRpIncorrectCalltoDeawGrid='Incorrect call to DrawBitmapGrid';
   {  SIInformenotrobat = 'El informe no fue encontrado: ';
  SITitolInforme = 'Nuevo informe';
  SIObrirNovaLlibreria = 'Nueva biblioteca de informes';
  SIObrirLlibreriaFiltre = '*.dbf';
  SIExtensionDBF = 'La extensi�n debe ser .DBF';
  SILibNoCorrecta ='Biblioteca de informes incorrecta';
  SISeleccioneLibreria ='Seleccione la biblioteca';
  SIPreview = 'Presentaci�n preliminar';
  SIEntreALias = 'Entre el alias';
  SIDatosIgnorados = 'Los datos de base de datos ser�n ignorados';
  SITablaicampo = 'Debe especificarse una tabla y un campo';
  SINohaynadaqueimprimir = 'No hay nada que imprimir';
  SINoGuardado ='El informe %s ha sido modificado, �desea guardar los cambios?';
  SIGrupDuplicat = 'Grupo duplicado';
  SIGrupNoNul = 'El grupo no puede ser nulo';
  SIError = 'Error';
  SIMarges = 'Una secci�n sobrepasa los m�rgenes de p�gina';
  SIErrorCapiPeuPlana = 'La cabecera y pie de p�gina son demasiado grandes';
  SISobrepasaBordes = 'La secci�n sobrepasa los bordes de la p�gina';
  SIOcultaControles = 'El tama�o especificado oculta elementos de la secci�n';
  SISeguroEliminar = '�Desea eliminar los controles de la secci�n?';
  SIEsperatIdentificador = 'Se esperaba un identificador';
  SIEsperadaCadena = 'Se esperaba una cadena';
  SIEsperatNumero = 'Se esperaba un n�mero';
  SIEsperatOperador = 'Se esperaba un operador';
  SIInvalidBinary = 'Binario no v�lido';
  SISeEsperaba = 'Se esperaba un %s';
  SIAvalSintaxis = 'Error de sintaxis';
  SIAssignacioaFuncio = 'Asignaci�n a funci�n';
  SIAssignacioaConstant = 'Asignaci�n a constante';
  SIAssignacioaCamp = 'Asignaci�n a campo';
  SIDivisioZero = 'Divisi�n por cero';
  SIAvalTipus = 'Error de conversi�n de tipos';
  SIInvalidOperation = 'Operaci�n no v�lida';
  SIAvalDesconegutIden = 'Identificador desconocido: ';
  SIAvalParentesis = 'Error de par�ntesis';
  SICampDoble = 'Campo duplicado debe especificar el archivo';
  SITexteMosTra = 'Texto de muestra';
  SICodigoNoValido = 'C�digo de formato no v�lido';
  SIRangByte = 'Rango de byte';
  SIDefinido = 'Definido por el usuario';
  SIGeneral = 'General';
  SIFormatoFecha = 'Fecha';
  SICientific = 'Cient�fico';
  SiMoneda = 'Moneda';
  SIFormatoNumerico = 'Num�rico';
  SiDecimal = 'Decimal';
  SIcaracter = 'Caracter';
  SIPredeterminado = 'Predeterminado';
  SIDefinidoUsuario = 'Definido por el usuario';
  SIMedidaPapel = 'Carta (216 x 279 mm)';
  SIMedidaPapel1 = 'Carta peque�a (216 x 279 mm)';
  SIMedidaPapel2 = 'Tabloide (279 x 432 mm)';
  SIMedidaPapel3 = 'Doble carta (432 x 280 mm)';
  SIMedidaPapel4 = 'Oficio (216 x 356 mm)';
  SIMedidaPapel5 = 'Estamento (140 x 216 mm)';
  SIMedidaPapel6 = 'Ejecutivo (195 x 254 mm)';
  SIMedidaPapel7 = 'A3 (297 x 420 mm)';
  SIMedidaPapel8 = 'A4 (210 x 297 mm)';
  SIMedidaPapel9 = 'A4 peque�o (210 x 297 mm)';
  SIMedidaPapel10 = 'A5 (148 x 210 mm)';
  SIMedidaPapel11 = 'B4 (JIS) (250 x 354 mm)';
  SIMedidaPapel12 = 'B5 (JIS) (182 x 257 mm)';
  SIMedidaPapel13 = 'Folio (216 x 330 mm)';
  SIMedidaPapel14 = 'Cuarto (215 x 275 mm)';
  SIMedidaPapel15 = '(254 x 356 mm)';
  SIMedidaPapel16 = '(254 x 432 mm)';
  SIMedidaPapel17 = 'Nota (216 x 279 mm)';
  SIMedidaPapel18 = 'Sobre #9(980 x 225 mm)';
  SIMedidaPapel19 = 'Sobre #10(105 x 241 mm)';
  SIMedidaPapel20 = 'Sobre #11 (114 x 263 mm)';
  SIMedidaPapel21 = 'Sobre #12(124 x 279 mm)';
  SIMedidaPapel22 = 'Sobre #14 (127 x 292 mm)';
  SIMedidaPapel23 = 'Hoja tama�o C';
  SIMedidaPapel24 = 'Hoja tama�o D';
  SIMedidaPapel25 = 'Hoja tama�o E';
  SIMedidaPapel26 = 'Sobre DL (110 x 220 mm)';
  SIMedidaPapel27 = 'Sobre C5 (162 x 229 mm)';
  SIMedidaPapel28 = 'Sobre C3 (324 x 458 mm)';
  SIMedidaPapel29 = 'Sobre C4 (229 x 324 mm)';
  SIMedidaPapel30 = 'Sobre C6 (114 x 162 mm)';
  SIMedidaPapel31 = 'Sobre C65 (114 x 229 mm)';
  SIMedidaPapel32 = 'Sobre B4 (250 x 353 mm)';
  SIMedidaPapel33 = 'Sobre B5 (176 x 250 mm)';
  SIMedidaPapel34 = 'Sobre B6 (176 x 125 mm)';
  SIMedidaPapel35 = 'Sobre (110 x 230 mm)';
  SIMedidaPapel36 = 'Sobre monarca (670 x 483 mm)';
  SIMedidaPapel37 = 'Sobre 6 3/4 (480 x 165 mm)';
  SIMedidaPapel38 = 'Continuo USA est�ndar (310 x 280 mm)';
  SIMedidaPapel39 = 'Continuo alem�n (216 x 305 mm)';
  SIMedidaPapel40 = 'Continuo alem�n oficio (216 x 330 mm)';
  SIMedidaPapel41 = 'B4 (ISO) (250 x 353 mm)';
  SIMedidaPapel42 = 'Postal Japonesa (100 x 148 mm)';
  SIMedidaPapel43 = '228 x 279 mm';
  SIMedidaPapel44 = '254 x 279 mm';
  SIMedidaPapel45 = '381 x 279 mm';
  SIMedidaPapel46 = 'Sobre Invite (220 x 220 mm)';
  SIMedidaPapel47 = '---------------------------';
  SIMedidaPapel48 = '---------------------------';
  SIMedidaPapel49 = 'Carta extra (241 x 305 mm)';
  SIMedidaPapel50 = 'Oficio extra (241 x 305 mm)';
  SIMedidaPapel51 = 'Tabloide extra (297 x 457 mm)';
  SIMedidaPapel52 = 'A4 Extra (236 x 322 mm)';
  SIMedidaPapel53 = 'Carta transversal (279 x 216 mm)';
  SIMedidaPapel54 = 'A4 transversal (210 x 297 mm)';
  SIMedidaPapel55 = 'Carta extra transversal (241 x 305 mm)';
  SIMedidaPapel56 = 'SuperA/SuperA/A4 (227 x 356 mm)';
  SIMedidaPapel57 = 'SuperB/SuperB/A3 (305 x 487 mm)';
  SIMedidaPapel58 = 'Carta plus (216 x 322 mm)';
  SIMedidaPapel59 = 'A4 Plus (210 x 330 mm)';
  SIMedidaPapel60 = 'A5 Transversal (148 x 210 mm)';
  SIMedidaPapel61 = 'B5 (JIS) Transversal (182 x 257 mm)';
  SIMedidaPapel62 = 'A3 Extra (322 x 445 mm)';
  SIMedidaPapel63 = 'A5 Extra (174 x 235 mm)';
  SIMedidaPapel64 = 'B5 (ISO) Extra (201 x 276 mm)';
  SIMedidaPapel65 = 'A2 (297 x 420 mm)';
  SIMedidaPapel66 = 'A3 Transversal (297 x 420 mm)';
  SIMedidaPapel67 = 'A3 Extra Transversal (322 x 445 mm)';

  SIRangcms = 'Debe especificar el rango en cent�metros';
  SINumeroNoValid = 'El n�mero no es v�lido';
  SIValorReixaNoValid = 'Valor de rejilla no v�lido';
  SICadenaCampos = 'Campos';
  SIinformenotroba = 'Informe no encontrado';
  SIIntroduzcaAltura = 'Introduzca la altura';
  SIIntroduzcaAnchura = 'Introduzca la anchura';
  SICapceleraInforme = 'Cabecera de informe';
  SIPeuInforme = 'Pie de informe';
  SICapceleraPlana = 'Cabecera de p�gina';
  SIPeuPlana = 'Pie de p�gina';
  SIDetall = 'Detalle';
  SICapPlanaGeneral = 'Cabecera de p�gina general';
  SICapInformeGeneral = 'Cabecera de informe general';
  SIElementsSeccioBorrats = 'Los elementos de la secci�n ser�n eliminados';
  SIAtencio = 'Atenci�n';
  SIEscollirGrupEliminar = 'Escoja el grupo a eliminar';
  SIGrupo = 'Grupo';
  SIMaxGrupsExcedit = 'M�ximo n�mero de grupos excedido';
  SIEscollirGrup = 'Escoja un grupo';
  SIInformeIncorrecto = 'Informe incorrecto';
  SISeccioGran = 'La secci�n es demasiado grande';
  SIImpresionCancelada = 'Impresi�n cancelada';
  SIErrorDecimalsDefecte = 'Error en decimales por defecto';
  SiErrorVariables = 'Error en inicializaci�n de variables';
  SIExportados = 'Campos exportados';
  SIExportando = 'Exportando';
  SINoLocalizaReg = 'Registro no encontrado';
  SICampNoTrobat = 'Campo no encontrado';
  SIErrorGravSeccion = 'Error de gravaci�n de secci�n';
  SIErrorLecSeccion = 'Error de lectura de secci�n';
  SITitolPropietats = 'Propiedades';
  SIArchivosBMP='Archivos bitmap';
  SiBMPIncorrecto ='Archivo bitmap incorrecto';
  SICortarImagen = '&Cortar imagen';
  SICopiarImagen = 'C&opiar imagen';
  SIPegarImagen = '&Pegar imagen';
  SILeerImagen = '&Leer imagen';
  SIDibuixNoCamp = 'No se encuentra el campo para la imagen';
  SICampoNoPicture = 'El campo no es de tipo imagen';
  SIIdenDuplicat = 'Identificador duplicado';
  SISumaNula = 'Suma nula';
  SIFormatMalament = 'Formato incorrecto';
  SIEliminarComponent = '&Eliminar componente';
  SIDavant = '&Traer delante';
  SIDarrera ='Enviar al &fondo';
  SIPropietats = '&Propiedades';
  SIConvertError = 'Error de conversi�n de tipos';
  SIVerdader = 'Cierto';
  SIFals = 'Falso';
  SIHoy = 'Hoy';
  SIAhora = 'Ahora';
  SIHora = 'Hora';
  SIInvalidMemoSize = 'Tama�o de texto no v�lido';
  SISelecion = 'Selecci�n';
  SIEtiquetas = 'Etiqueta de texto';
  SIExpresiones = 'Campo o expresi�n';
  SIImagenes = 'Imagen';
  SIElementoGrafico = 'Elemento gr�fico simple';
  SILlegirSeccio = '&Leer secci�n';
  SIEscriuSeccio = '&Guardar secci�n';
  SIAlturaMinima    =  'Error de tama�o';

  // Constantes de ayuda de funciones
  SAUpperCase='Devuelve una cadena (string) en may�sculas';
  SAPUpperCase='Donde s es el string a transformar.';

  SALowerCase='Devuelve una cadena (string) en min�sculas';
  SAPLowerCase='Donde s es el string a transformar.';

  SAHorMinSeg='Devuelve el racional en una cadena con formato horas, minutos y segundos';
  SAPHorMinSeg='Donde h es el tiempo pasado a horas, idenH es la cadena que '
   +chr(13)+'identifica las horas, idenM es la cadena que identifica los minutos y '+chr(13)
   +'idenS es la cadena que identifica los segundos';

  SAFloatToDateTime='Convierte un numero en fecha y hora';
  SAPFloatToDateTime='Donde n es el n�mero a convertir';

  SASin='Devuelve el seno del �ngulo';
  SAPSin='Donde ang es el �ngulo expresado en radianes';

  SARound='Devuelve el valor real de un n�mero redondeado en multiplos.';
  SAPRound='num �s el n�mero a redondear y r es el m�ltiplo al cual redondear';

  SAInt='Devuelve la parte entera de un n�mero redondeado.';
  SAPInt='num �s el n�mero a redondear';

  SAStr='Convierte un n�mero a string';
  SAPStr='num es el n�mero a convertir';

  SAVal='Convierte un string a n�mero';
  SAPVal='s es la cadena a convertir';

  SATrim='Devuelve una cadena quitando espacios por ambos lados';
  SAPtrim='s es la cadena de origen';

  SALeft='Devuelve una subcadena desde la izquierda de una cadena';
  SAPLeft='s es la cadena de origen y count es el n�mero de caracteres';

  SAPos='Devuelve la posicion de una cadena dentro de otra o 0 si no est� contenida';
  SAPPos='SubStr es la cadena a buscar en str';

  SAGraphicClear='Elimina toda la informaci�n acumulada en una variable de tipo gr�fico';
  SAPGraphicClear='Gr es el nombre de la variable de tipo gr�fico, devuelve cierto';

  SAGraphicNew='A�ade un punto de informaci�n en una variable de tipo gr�fico, pudiendo cambiar de serie';
  SAPGraphicNew='Gr es el nombre de la variable de tipo gr�fico, '+
    ' V es el punto, C es si debemos cambiar de serie, etiq es la etiqueta del punto y caption es la leyenda de la serie';

  SASQrt='Devuelve la ra�z cuadrada de un n�mero';
  SAPSQRt='num es el n�mero a operar';

  SAMod='Devuelve el resto de la divisi�n entera';
  SAPMod='d1 es el dividendo d2 es el divisor';

  SAToday='Devuelve el d�a actual en formato fecha';
  SANow='Devuelve el d�a y hora actual en formato fecha y hora';
  SATime='Devuelve la hora actual en formato hora';

  SANull='Devuelve el valor NULL';

  SAMonthName='Devuelve el nombre del mes en formato cadena';
  SAPMonthName='d es la fecha a decodificar';

  SAEvalText='Evalua una expresi�n i devuelve el resultado';
  SAPEvalText='expr es la expresi�n a evaluar';

  SAMonth='Devuelve el n�mero de mes';
  SAPMonth='d es la fecha a decodificar';

  SAYear='Devuelve el a�o';
  SAPyear='d es la fecha a decodificar';

  SADay='Devuelve el d�a';
  SAPDay='d es la fecha a decodificar';

  SARight='Devuelve una subcadena desde la derecha de una cadena';
  SAPRight='s es la cadena de origen y count es el n�mero de caracteres';

  SASubStr='Devuelve una subcadena a partir de una cadena';
  SAPSubStr='cadena es la cadena de origen, index a partir de donde y'+
   ' count es el n�mero de caracteres';

  SAFormatStr='Devuelve una cadena formateando el valor dependiendo del tipo de datos';
  SANumToText='Devuelve la representaci�n en modo texto de un n�mero';
  SAPNumToText='n es el n�mero, f indica si es femenino';
  SAPFormatStr='Format es la cadena de formateo: ej.''dd/mm/yyyy'''+
   ' y v es el valor a convertir a string';


  // Dies i mesos de l'any
  // Castella
  SIMesCurtIdioma00='Ene';
  SIMesCurtIdioma01='Feb';
  SIMesCurtIdioma02='Mar';
  SIMesCurtIdioma03='Abr';
  SIMesCurtIdioma04='May';
  SIMesCurtIdioma05='Jun';
  SIMesCurtIdioma06='Jul';
  SIMesCurtIdioma07='Ago';
  SIMesCurtIdioma08='Sep';
  SIMesCurtIdioma09='Oct';
  SIMesCurtIdioma010='Nov';
  SIMesCurtIdioma011='Dic';
  SIMesLlargIdioma00='Enero';
  SIMesLlargIdioma01='Febrero';
  SIMesLlargIdioma02='Marzo';
  SIMesLlargIdioma03='Abril';
  SIMesLlargIdioma04='Mayo';
  SIMesLlargIdioma05='Junio';
  SIMesLlargIdioma06='Julio';
  SIMesLlargIdioma07='Agosto';
  SIMesLlargIdioma08='Septiembre';
  SIMesLlargIdioma09='Octubre';
  SIMesLlargIdioma010='Noviembre';
  SIMesLlargIdioma011='Diciembre';
  SIDiaCurtIdioma00='Dom';
  SIDiaCurtIdioma01='Lun';
  SIDiaCurtIdioma02='Mar';
  SIDiaCurtIdioma03='Mie';
  SIDiaCurtIdioma04='Jue';
  SIDiaCurtIdioma05='Vie';
  SIDiaCurtIdioma06='Sab';
  SIDiaLlargIdioma00='Domingo';
  SIDiaLlargIdioma01='Lunes';
  SIDiaLlargIdioma02='Martes';
  SIDiaLlargIdioma03='Miercoles';
  SIDiaLlargIdioma04='Jueves';
  SIDiaLlargIdioma05='Viernes';
  SIDiaLlargIdioma06='S�bado';
  // Catal�
  SIMesCurtIdioma10='Gen';
  SIMesCurtIdioma11='Feb';
  SIMesCurtIdioma12='Mar';
  SIMesCurtIdioma13='Abr';
  SIMesCurtIdioma14='Mai';
  SIMesCurtIdioma15='Jun';
  SIMesCurtIdioma16='Jul';
  SIMesCurtIdioma17='Ago';
  SIMesCurtIdioma18='Sep';
  SIMesCurtIdioma19='Oct';
  SIMesCurtIdioma110='Nov';
  SIMesCurtIdioma111='Des';
  SIMesLlargIdioma10='Gener';
  SIMesLlargIdioma11='Febrer';
  SIMesLlargIdioma12='Mar�';
  SIMesLlargIdioma13='Abril';
  SIMesLlargIdioma14='Maig';
  SIMesLlargIdioma15='Juny';
  SIMesLlargIdioma16='Juliol';
  SIMesLlargIdioma17='Agost';
  SIMesLlargIdioma18='Septembre';
  SIMesLlargIdioma19='Octubre';
  SIMesLlargIdioma110='Novembre';
  SIMesLlargIdioma111='Desembre';
  SIDiaCurtIdioma10='Diu';
  SIDiaCurtIdioma11='Dil';
  SIDiaCurtIdioma12='Dma';
  SIDiaCurtIdioma13='Dme';
  SIDiaCurtIdioma14='Dij';
  SIDiaCurtIdioma15='Div';
  SIDiaCurtIdioma16='Dis';
  SIDiaLlargIdioma10='Diumenge';
  SIDiaLlargIdioma11='Dilluns';
  SIDiaLlargIdioma12='Dimarts';
  SIDiaLlargIdioma13='Dimecres';
  SIDiaLlargIdioma14='Dijous';
  SIDiaLlargIdioma15='Divendres';
  SIDiaLlargIdioma16='Dissabte';
  // Angl�s
  SIMesCurtIdioma20='Jan';
  SIMesCurtIdioma21='Feb';
  SIMesCurtIdioma22='Mar';
  SIMesCurtIdioma23='Apr';
  SIMesCurtIdioma24='May';
  SIMesCurtIdioma25='Jun';
  SIMesCurtIdioma26='Jul';
  SIMesCurtIdioma27='Aug';
  SIMesCurtIdioma28='Sep';
  SIMesCurtIdioma29='Oct';
  SIMesCurtIdioma210='Nov';
  SIMesCurtIdioma211='Des';
  SIMesLlargIdioma20='January';
  SIMesLlargIdioma21='February';
  SIMesLlargIdioma22='March';
  SIMesLlargIdioma23='April';
  SIMesLlargIdioma24='May';
  SIMesLlargIdioma25='June';
  SIMesLlargIdioma26='July';
  SIMesLlargIdioma27='August';
  SIMesLlargIdioma28='September';
  SIMesLlargIdioma29='October';
  SIMesLlargIdioma210='November';
  SIMesLlargIdioma211='Desember';
  SIDiaCurtIdioma20='Sun';
  SIDiaCurtIdioma21='Mon';
  SIDiaCurtIdioma22='Tue';
  SIDiaCurtIdioma23='Wed';
  SIDiaCurtIdioma24='Thu';
  SIDiaCurtIdioma25='Fri';
  SIDiaCurtIdioma26='Sat';
  SIDiaLlargIdioma20='Sunday';
  SIDiaLlargIdioma21='Monday';
  SIDiaLlargIdioma22='Tuesday';
  SIDiaLlargIdioma23='Wednesday';
  SIDiaLlargIdioma24='Thursday';
  SIDiaLlargIdioma25='Friday';
  SIDiaLlargIdioma26='Saturday';
  SIArxiuWord='Documento de Word';
  SIArxiuDBF='Base de dados DBF';
  SIArxiuAscii='Archivo de texto';
  SIArxiuAsciiDel='Archivo de texto delimitado';
  SICalcUltPag='Calculando �ltima p�gina: ';
  SICalcPag='Calculando p�gina: ';
  SIEnSeccion='-En secci�n: ';
  SINoEtiqCon='No se soportan etiquetas con secciones condicionales.';
  SIEjecutandoSQL='Ejecutando la consulta...';
  SINoPrin='No hay tabla principal';
  SIErrorLecCab='No se pudo leer la cabecera general de informe';
  SIErrorLecCabg='No se pudo leer la cabecera de pagina general';
  SIDebeInst='Debe instalar una impresora';
  SIMarcanil='Error Marca nil en cinforme toLastAccesed';
  SINoAliasCanvi='No se puede cambiar alias cuando se esta ejecutando';
  SIErrorFiltre='Error en la el filtro de la tabla:';
  SINorelacionscon='No se pueden establecer relaciones uno a varios con consultas';
  SIRelacioInco='Relacion con principal incorrecta';
  SIRelaciog='--Relaci�n ';
  SIErrorAlias='Error Alias ';
  SIErrorAlias2=' no asignado';
  SIAliasRelacio='Alias relacion directa no encontrado: tabla ';
  SIIndexnamet='No se aplica indexname para consultas';
  SIAnaReg='Analizando registro:';
  SIExportcom='Exportar datos como';
  SINocreafile='No pudo crearse el archivo: ';
  SIImprimiendopag='Imprimiendo p�gina: ';
  SIErrorParam='Error en el par�metro:';
  SIConnotro='-Consulta no encontrada';
  SIDatosnocon='- Los datos no son de una consulta';
  SIAsignan='-Asignando par�metro ';
  SICadenaEn=' en ';
  SIvarjaenus='Variable ya en uso, parametro:';
  SIObrint=' abriendo ';
  SICompnoreg='Component no registrado.';
  SIErrorPP='Error, en los puntos por pulgada';
  SIDatosError='Los datos del programa se han estropeado, avise al servicio t�cnico.';
  SIInformenotrobatl='El informe no se encuentra en la libreria.';
  SIInformemodificat='El informe ha sido modificado.';
  SIErrorAbort='Error en set abortproc.';
  SIRefCruz='Error referencia cruzada.';
  SIIntentFor='Se intento modificar una formula';
  SIVarjadef='Variable ya definida: ';
  SIConversgran='Conversi�n demasiado grande';
  SInoimpobre='No puede estar imprimiendo mientras se abre el caj�n';
  SIPredeterminada='Predeterminada';
  SIImpListados='Impresora listados';
  SIImpTickets='Impresora tickets';
  SIImpGr='Impresora gr�ficos';
  SIImpcr='Impresora listados caracter';
  SIImplistados2='Impresora listados 2';
  SIImpTickets2='Impresora tickets 2';
  SIImpusuari1='Impresora usuario 1';
  SIImpusuari2='Impresora usuario 2';
  SIImpusuari3='Impresora usuario 3';
  SIImpusuari4='Impresora usuario 4';
  SIImpusuari5='Impresora usuario 5';
  SIImpusuari6='Impresora usuario 6';
  SIIntenformu='Se intent� asignar a una f�rmula';
  SIErrorOpenImp='Error al abrir impresora:';
  SITipoPapExis='El tipo de papel ya existe';
  SIBaseExis='La base de datos ya existe';
  SINoPudoCon='No se pudo conectar a: ';
  SIDeseaGuardar='�Desea guardar los cambios?';
  SINombreuso='El nombre est� en uso';
  SINuevaTabla='Nueva tabla o consulta';
  SIIntroNom='Introduzca el nombre';
  SIRenomCom='Renombrar tabla o consulta';
  SINuevopara='Nuevo nombre para ';
  SINombreExis='El nombre ya existe ';
  SIParamnoexis='Par�metro no existe: ';
  SILlistanoasi='La lista de alias no fue asignada';
  SINoRecogegr='No se puede recoger el valor de un gr�fico';
  SIDebeEspeAr='Debe especificarse un archivo';
  SINombreInfno='Nombre de informe no encontrado';
  SIInformenocar='Informe no cargado';
  SILibNoCompat='Biblioteca no compatible, �ndice NOMBRE TAG1 no encontrado';
  SIBaseInexis='Base de datos inexistente';
  SILibreSQL='Un biblioteca de informes debe almacenarse en un servidor SQL';
  SINomLlibreriaIncorrecte='Nombre de biblioteca incorrecto';
  SILLibreriaSenseNom='Biblioteca incorrecta: Falta el campo NOMBRE';
  SILlibreriaTipusnom='Biblioteca incorrecta: Tipo de campo NOMBRE incorrecto';
  SILlibrerianoInforme='Biblioteca incorrecta: Falta el campo INFORME';
  SILlibreriaTipusinforme='Biblioteca incorrecta: Tipo de campo INFORME incorrecto';
  SILlibrerianoindex='Biblioteca incorrecta: Error abriendo el �ndice NOMBRE: ';
  SINOMbre='Nombre';
  SIGrupoLibreria='Grupo';
  SIAbreSelec='Abre el archivo seleccionado';
  SINoPaleta='No se asigno una paleta de componentes';
  SICondicioSaltPlana='Condici�n de salto de p�gina: ';
  SICapInformeGNhicap='La cabecera de informe general no cabe en la p�gina';
  SICapinformenohicap='La cabecera de informe no cabe en la p�gina';
  SICapceleresdegrupnohicap='La repetici�n de cabeceras de grupo no cabe en la p�gina';
  SISinTitulo='Sin t�tulo';
  SIErrorTrobachecksum='Cadena nula en trobachecksum';
  SICharnovalid='Caracter no v�lido para codificar/decodificar.';
  SINumSerienocorrect='N�mero de serie incorrecto.';
  SIContrasenyaincorrecta='Contrase�a incorrecta.';
  SINumIntents='N�mero de intentos agotado.';
  SIAvisllicencia='Licencia no v�lida, la aplicaci�n dejar� de funcionar dentro de unos d�as.';
  SIFillicencia='Licencia no v�lida.';
  SISeccionohicap='Una de las secciones no cabe en la p�gina';
  SIGuardartexto='�Desea guardar los cambios en un archivo?';
  SISeccion='Secci�n';
  SICabecera='Cabecera';
  SIPie='Pie';
  SICondicionImpre='Error en condici�n de impresi�n';
  SIAfterImpre='Error en expresi�n evaluar despu�s de impresi�n';
  SIADquirirImagen='Adquirir...';
  SINingunDispTWAIN='No hay ning�n dispositivo TWAIN';
  SIBNoCargaTWAIN='No se pudo cargar la biblioteca TWAIN';
  SIBNoAdquire='No se puede adquirir';
}

implementation


end.
