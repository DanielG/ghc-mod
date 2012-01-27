{-# LANGUAGE CPP #-}

module Flag where

import DynFlags
import Types

listFlags :: Options -> IO String
listFlags opt = return $ convert opt $
   [ "-f" ++ prefix ++ option
#if __GLASGOW_HASKELL__ == 702
   | (option,_,_,_) <- fFlags
#else
   | (option,_,_) <- fFlags
#endif
   , prefix <- ["","no-"]
   ]
