{-# LANGUAGE CPP #-}
module CabalHelper.Types where

newtype GmModuleName = GmModuleName String
    deriving (Read, Show)

data GmComponentName = GmSetupHsName
                     | GmLibName
                     | GmExeName String
                     | GmTestName String
                     | GmBenchName String
  deriving (Eq, Ord, Read, Show)

data GmCabalHelperResponse
    = GmCabalHelperStrings [(GmComponentName, [String])]
    | GmCabalHelperEntrypoints [(GmComponentName, Either FilePath [GmModuleName])]
    | GmCabalHelperLbi String
  deriving (Read, Show)
