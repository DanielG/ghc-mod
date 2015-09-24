module Language.Haskell.GhcMod.CustomPackageDb where


import Control.Applicative
import Control.Monad
import Control.Category ((.))
import Data.Maybe
import Data.Traversable
import Language.Haskell.GhcMod.Types
import Language.Haskell.GhcMod.Monad.Types
import Language.Haskell.GhcMod.PathsAndFiles
import Prelude hiding ((.))


parseCustomPackageDb :: String -> [GhcPkgDb]
parseCustomPackageDb src = map parsePkgDb $ filter (not . null) $ lines src
 where
   parsePkgDb "global" = GlobalDb
   parsePkgDb "user" = UserDb
   parsePkgDb s = PackageDb s

getCustomPkgDbStack :: (MonadIO m, GmEnv m) => m (Maybe [GhcPkgDb])
getCustomPkgDbStack = do
    mCusPkgDbFile <- liftIO . (traverse readFile <=< findCustomPackageDbFile) . cradleRootDir =<< cradle
    return $ parseCustomPackageDb <$> mCusPkgDbFile
