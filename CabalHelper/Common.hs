-- ghc-mod: Making Haskell development *more* fun
-- Copyright (C) 2015  Daniel Gröber <dxld ÄT darkboxed DOT org>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Affero General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Affero General Public License for more details.
--
-- You should have received a copy of the GNU Affero General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}
module CabalHelper.Common where

import Control.Applicative
import Control.Exception as E
import Control.Monad
import Data.List
import Data.Maybe
import Data.Version
import Data.Typeable
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import System.Environment
import System.IO
import System.Exit
import Text.ParserCombinators.ReadP

data Panic = Panic String deriving (Typeable, Show)
instance Exception Panic

panic :: String -> a
panic msg = throw $ Panic msg

handlePanic :: IO a -> IO a
handlePanic action =
    action `E.catch` \(Panic msg) -> errMsg msg >> exitFailure

errMsg :: String -> IO ()
errMsg str = do
  prog <- getProgName
  hPutStrLn stderr $ prog ++ ": " ++ str

align :: String -> String -> String -> String
align n an str = let
    h:rest = lines str
    [hm]   = match n h
    rest'  = [ move (hm - rm) r | r <- rest, rm <- match an r]
    in
      unlines (h:rest')
 where
   match p str' = maybeToList $
     fst <$> find ((p `isPrefixOf`) . snd) ([0..] `zip` tails str')
   move i str' | i > 0  = replicate i ' ' ++ str'
   move i str' = drop i str'


-- | @getCabalConfigHeader "dist/setup-config"@ returns the cabal version and
-- compiler version
getCabalConfigHeader :: FilePath -> IO (Maybe (Version, Version))
getCabalConfigHeader file = bracket (openFile file ReadMode) hClose $ \h -> do
  parseHeader <$> BS.hGetLine h

parseHeader :: ByteString -> Maybe (Version, Version)
parseHeader header = case BS8.words header of
  ["Saved", "package", "config", "for", _pkgId ,
   "written", "by", cabalId,
   "using", compId]
    -> liftM2 (,) (ver cabalId) (ver compId)
  _ -> Nothing
 where
   ver i = snd <$> parsePkgId i

parsePkgId :: ByteString -> Maybe (ByteString, Version)
parsePkgId bs =
    case BS8.split '-' bs of
      [pkg, vers] -> Just (pkg, parseVer $ BS8.unpack vers)
      _ -> Nothing

parseVer :: String -> Version
parseVer vers = runReadP parseVersion vers

-- majorVer :: Version -> Version
-- majorVer (Version b _) = Version (take 2 b) []

-- sameMajorVersion :: Version -> Version -> Bool
-- sameMajorVersion a b = majorVer a == majorVer b

runReadP :: ReadP t -> String -> t
runReadP p i = case filter ((=="") . snd) $ readP_to_S p i of
                 (a,""):[] -> a
                 _ -> error $ "Error parsing: " ++ show i
