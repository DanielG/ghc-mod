import Distribution.Verbosity
import Distribution.PackageDescription.Parse (readPackageDescription)
import Distribution.PackageDescription.Configuration (flattenPackageDescription)
import Distribution.PackageDescription
import Distribution.Package
import Distribution.Text
import Text.PrettyPrint
import System.Environment

main = do
  [f] <- getArgs
  pd <- flattenPackageDescription <$> readPackageDescription silent f
  mapM_ putStrLn $ map (\(Dependency n v) -> unPackageName n ++ "\t" ++ render (disp v)) $ buildDepends pd
