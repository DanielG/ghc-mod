module GHCMod.Options.DocUtils (
  module PP,
  desc,
  code
) where

import Options.Applicative
import Text.PrettyPrint.ANSI.Leijen as PP hiding ((<>), (<$>), int)

desc :: [Doc] -> InfoMod a
desc = footerDoc . Just . indent 2 . vsep

code :: [String] -> Doc
code x = vsep [line, indent 4 $ vsep $ map text x, line]
