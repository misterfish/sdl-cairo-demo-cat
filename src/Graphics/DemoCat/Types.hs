module Graphics.DemoCat.Types ( Logger
                              , Log (Log, info, warn, err)
                              ) where

type Logger = String -> IO ()
data Log = Log { info :: Logger
               , warn :: Logger
               , err :: Logger }


