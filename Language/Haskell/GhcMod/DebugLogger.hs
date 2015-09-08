module Language.Haskell.GhcMod.DebugLogger where


import GHC
import FastString
import Pretty
import Outputable (SDoc, PprStyle, runSDoc, initSDocContext, blankLine)
import qualified Outputable
import ErrUtils
import DynFlags (LogAction)

import Language.Haskell.GhcMod.Error
import Language.Haskell.GhcMod.Output
import Language.Haskell.GhcMod.Monad.Types
import Prelude

debugLogAction :: (String -> IO ()) -> LogAction
debugLogAction putErr dflags severity srcSpan style msg
    = case severity of
      SevOutput      -> printSDoc msg style
      SevDump        -> printSDoc (msg Outputable.$$ blankLine) style
      SevInteractive -> putStrSDoc msg style
      SevInfo        -> printErrs msg style
      SevFatal       -> printErrs msg style
      _              -> do putErr "\n"
                           printErrs (mkLocMessage severity srcSpan msg) style
                           -- careful (#2302): printErrs prints in UTF-8,
                           -- whereas converting to string first and using
                           -- hPutStr would just emit the low 8 bits of
                           -- each unicode char.
    where printSDoc  = debugLogActionHPrintDoc  dflags putErr
          printErrs  = debugLogActionHPrintDoc  dflags putErr
          putStrSDoc = debugLogActionHPutStrDoc dflags putErr

debugLogActionHPrintDoc :: DynFlags -> (String -> IO ()) -> SDoc -> PprStyle -> IO ()
debugLogActionHPrintDoc dflags put d sty
 = debugLogActionHPutStrDoc dflags put (d Outputable.$$ Outputable.text "") sty
      -- Adds a newline

debugLogActionHPutStrDoc :: DynFlags -> (String -> IO ()) -> SDoc -> PprStyle -> IO ()
debugLogActionHPutStrDoc dflags put d sty
  = gmPrintDoc_ Pretty.PageMode (pprCols dflags) put doc
  where   -- Don't add a newline at the end, so that successive
          -- calls to this log-action can output all on the same line
    doc = runSDoc d (initSDocContext dflags sty)


gmPrintDoc :: Mode -> Int -> (String -> IO ()) -> Doc -> IO ()
-- printDoc adds a newline to the end
gmPrintDoc mode cols put doc = gmPrintDoc_ mode cols put (doc $$ text "")

gmPrintDoc_ :: Mode -> Int -> (String -> IO ()) -> Doc -> IO ()
gmPrintDoc_ mode pprCols putS doc
  = fullRender mode pprCols 1.5 put done doc
  where
    put (Chr c)  next = putS [c] >> next
    put (Str s)  next = putS s >> next
    put (PStr s) next = putS (unpackFS s) >> next
    put (ZStr s) next = putS (zString s) >> next
    put (LStr s _l) next = putS (unpackLitString s) >> next

    done = return () -- hPutChar hdl '\n'
