object modserver: Tmodserver
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Left = 360
  Top = 264
  Height = 172
  Width = 309
  object RepServer: TIdTCPServer
    Bindings = <>
    CommandHandlers = <>
    DefaultPort = 3060
    Greeting.NumericCode = 0
    MaxConnectionReply.NumericCode = 0
    OnConnect = RepServerConnect
    OnExecute = RepServerExecute
    OnDisconnect = RepServerDisconnect
    ReplyExceptionCode = 0
    ReplyTexts = <>
    ReplyUnknownCommand.NumericCode = 0
    Left = 32
    Top = 12
  end
  object adata: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 108
    Top = 32
    object adataID: TIntegerField
      FieldName = 'ID'
    end
    object adataLASTOPERATION: TDateTimeField
      FieldName = 'LASTOPERATION'
    end
    object adataCONNECTIONDATE: TDateTimeField
      FieldName = 'CONNECTIONDATE'
    end
    object adataUSERNAME: TStringField
      FieldName = 'USERNAME'
      Size = 40
    end
    object adataRUNNING: TBooleanField
      FieldName = 'RUNNING'
    end
  end
end
