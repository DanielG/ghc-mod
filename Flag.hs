{-# LANGUAGE CPP #-}

module Flag where

import Types
import qualified Gap

listFlags :: Options -> IO String
listFlags opt = return $ convert opt [ "-f" ++ prefix ++ option
                                     | option <- Gap.fOptions
                                     , prefix <- ["","no-"]
                                     ]
