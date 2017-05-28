-- ghc-mod: Happy Haskell Hacking
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

{-# LANGUAGE OverloadedStrings #-}

module HomeModuleGraphSpec where

import GhcMod.HomeModuleGraph
import GhcMod.LightGhc
import TestUtils

import GHC
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe

import Test.Hspec

runAGhc :: [GHCOption] -> (HscEnv -> LightGhc a) -> IO a
runAGhc opts action = withLightHscEnv opts $ \env -> do
  runLightGhc env $ getSession >>= action

hmGraph :: FilePath -> [String] -> String -> IO GmModuleGraph
hmGraph dir opts mn = runAGhc opts $ \env -> liftIO $ do
    runD' dir $ do
      smp <- liftIO $ findModulePathSet env [mkModuleName mn]
      homeModuleGraph env smp

uhmGraph :: FilePath -> [String] -> String -> String -> GmModuleGraph -> IO GmModuleGraph
uhmGraph dir opts mn umn g = runAGhc opts $ \env -> liftIO $ do
    runD' dir $ do
      smp <- liftIO $ findModulePathSet env [mkModuleName mn]
      usmp <- liftIO $ findModulePathSet env [mkModuleName umn]
      updateHomeModuleGraph env g smp usmp

mapMap :: (Ord k, Ord k')
  => (k -> k') -> (a -> a') -> Map.Map k a -> Map.Map k' a'
mapMap fk fa = Map.mapKeys fk . Map.map fa

mapMpFn :: (FilePath -> FilePath) -> ModulePath -> ModulePath
mapMpFn f (ModulePath mn fn) = ModulePath mn (f fn)

mp :: ModuleName -> ModulePath
mp mn = ModulePath mn $ moduleNameString mn ++ ".hs"

spec :: Spec
spec = do
    describe "reachable" $ do
        let
            smp =
              Set.fromList
                [ mp "A"
                , mp "B"
                , mp "C"
                , mp "D"
                , mp "E"
                , mp "F"
                , mp "G"
                , mp "H"
                , mp "I"
                ]

            moduleMap = mkModuleMap smp

            completeGraph =
                Map.map (Set.map lookupMM) . Map.mapKeys lookupMM

            lookupMM = fromJust . flip Map.lookup moduleMap

            graph = completeGraph $
              Map.fromList
                [ ("A", Set.fromList ["B"])
                , ("B", Set.fromList ["C", "D"])
                , ("C", Set.fromList ["F"])
                , ("D", Set.fromList ["E"])
                , ("E", Set.fromList [])
                , ("F", Set.fromList [])
                , ("G", Set.fromList [])
                , ("H", Set.fromList [])
                , ("I", Set.fromList [])
                ]

            really_reachable =
              Set.fromList
                [ mp "A"
                , mp "B"
                , mp "C"
                , mp "D"
                , mp "E"
                , mp "F"
                ]

            g = GmModuleGraph {
               gmgGraph     = graph
             }

        it "reachable Set.empty g == Set.empty" $ do
            reachable Set.empty g `shouldBe` Set.empty

        it "lists only reachable nodes" $ do
            reachable (Set.fromList [mp "A"]) g `shouldBe` really_reachable


    describe "homeModuleGraph" $ do
        it "cycles don't break it" $ do
            let tdir = "test/data/home-module-graph/cycle"
            g <- hmGraph tdir [] "A"
            gmgGraph g `shouldBe`
              Map.fromList
                [ (mp "A",  Set.fromList [mp "B"])
                , (mp "B",  Set.fromList [mp "A"])
                ]

        it "follows imports" $ do
            let tdir = "test/data/home-module-graph/indirect"
            g <- hmGraph tdir [] "A"
            gmgGraph g `shouldBe`
              Map.fromList
                [ (mp "A",  Set.fromList [mp "A1", mp "A2", mp "A3"])
                , (mp "A1", Set.fromList [mp "B"])
                , (mp "A2", Set.fromList [mp "C"])
                , (mp "A3", Set.fromList [mp "B"])
                , (mp "B",  Set.fromList [])
                , (mp "C",  Set.fromList [])
                ]

        it "returns partial results on parse errors" $ do
            let tdir = "test/data/home-module-graph/errors"
            g <- hmGraph tdir [] "A"
            gmgGraph g `shouldBe`
              Map.fromList
                [ (mp "A",  Set.fromList [mp "A1", mp "A2", mp "A3"])
                , (mp "A1", Set.fromList [])  -- parse error here
                , (mp "A2", Set.fromList [])
                , (mp "A3", Set.fromList [mp "B"])
                , (mp "B",  Set.fromList [])
                ]

        it "returns partial results on CPP errors" $ do
            let tdir = "test/data/home-module-graph/cpp"
            g <- hmGraph tdir [] "A"
            gmgGraph g `shouldBe`
              Map.fromList
                [ (mp "A",  Set.fromList [mp "A1", mp "A2", mp "A3"])
                , (mp "A1", Set.fromList [])  -- CPP error here
                , (mp "A2", Set.fromList [])
                , (mp "A3", Set.fromList [mp "B"])
                , (mp "B",  Set.fromList [])
                ]

    describe "updateHomeModuleGraph" $ do
        it "removes unreachable nodes" $ do
            let tdir = "test/data/home-module-graph/indirect"
            let tdir' = "test/data/home-module-graph/indirect-update"
            ig <- hmGraph tdir [] "A"
            g <- uhmGraph tdir' [] "A" "A2" ig
            gmgGraph g `shouldBe`
              Map.fromList
                [ (mp "A",  Set.fromList [mp "A1", mp "A2", mp "A3"])
                , (mp "A1", Set.fromList [mp "B"])
                , (mp "A2", Set.fromList [])
                , (mp "A3", Set.fromList [mp "B"])
                , (mp "B",  Set.fromList [])
                -- C was removed
                ]
