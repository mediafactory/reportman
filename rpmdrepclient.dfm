object modclient: Tmodclient
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Left = 273
  Top = 153
  Height = 176
  Width = 278
  object RepClient: TIdTCPClient
    OnDisconnected = RepClientDisconnected
    Port = 3060
    Left = 36
    Top = 16
  end
end
