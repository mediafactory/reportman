object repwebmod: Trepwebmod
  OldCreateOrder = False
  OnCreate = WebModuleCreate
  OnDestroy = WebModuleDestroy
  Actions = <
    item
      Name = 'aversion'
      PathInfo = '/version'
      OnAction = repwebmodaversionAction
    end
    item
      Name = 'aindex'
      PathInfo = '/index'
      OnAction = repwebmodaindexAction
    end
    item
      Name = 'alogin'
      PathInfo = '/login'
      OnAction = repwebmodaloginAction
    end
    item
      Name = 'ashowalias'
      PathInfo = '/showalias'
      OnAction = repwebmodashowaliasAction
    end
    item
      Name = 'ashowparams'
      PathInfo = '/showparams'
      OnAction = repwebmodashowparamsAction
    end
    item
      Name = 'aexecute'
      PathInfo = '/execute'
      OnAction = repwebmodaexecuteAction
    end>
  Left = 192
  Top = 114
  Height = 150
  Width = 215
end