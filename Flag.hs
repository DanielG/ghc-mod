module Flag where

import DynFlags
import Types

listFlags :: Options -> IO String
listFlags opt = return $ convert opt $
   [ "-f" ++ prefix ++ option
   | (option,_,_) <- fFlags, prefix <- ["","no-"]
   ]
