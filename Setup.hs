import Data.Maybe
import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup
import Distribution.Simple.UserHooks
import Distribution.Simple.Utils
import System.Directory
import System.FilePath
import System.Process


main = defaultMainWithHooks simpleUserHooks
    { confHook = customConfHook
    , postConf = buildLibGo
    }

buildLibGo :: Args -> ConfigFlags -> PackageDescription -> LocalBuildInfo -> IO ()
buildLibGo _ _ _ _ = do
    wd <- getCurrentDirectory
    let name         = "libfungo"
        dynamicPath  = wd </> (name ++ ".dylib")
        buildDynamic = shell ("go build -buildmode=c-shared -o " ++ dynamicPath ++ " go/src/fmt.go")
        rmHeader     = shell ("rm " ++ name ++ ".h")
    putStrLn ("Compiling Go dynamic library to " ++ dynamicPath)
    readCreateProcess buildDynamic "" >>= putStr
    readCreateProcess rmHeader "" >>= putStr

addLibDirsToBuildInfo :: BuildInfo -> IO BuildInfo
addLibDirsToBuildInfo buildInfo = do
    wd <- getCurrentDirectory
    return $ buildInfo { extraLibDirs = wd : extraLibDirs buildInfo }

addLibDirToTests :: TestSuite -> IO TestSuite
addLibDirToTests suite = do
    withLibDirs <- addLibDirsToBuildInfo (testBuildInfo suite)
    return $ suite { testBuildInfo = withLibDirs }

addLibDirToExecs :: Executable -> IO Executable
addLibDirToExecs exe = do
    withLibDirs <- addLibDirsToBuildInfo (buildInfo exe)
    return $ exe { buildInfo = withLibDirs }

customConfHook :: (GenericPackageDescription, HookedBuildInfo) -> ConfigFlags -> IO LocalBuildInfo
customConfHook (description, buildInfo) flags = do
    localBuildInfo <- confHook simpleUserHooks (description, buildInfo) flags
    let packageDescription = localPkgDescr localBuildInfo
        lib   = fromJust $ library packageDescription
        execs = executables packageDescription
        tests = testSuites packageDescription

    libWithLibDirs         <- addLibDirsToBuildInfo (libBuildInfo lib)
    executablesWithLibDirs <- mapM addLibDirToExecs execs
    testSuitesWithLibDirs  <- mapM addLibDirToTests tests

    return $ localBuildInfo {
        localPkgDescr = packageDescription
        { library     = Just $ lib { libBuildInfo = libWithLibDirs }
        , executables = executablesWithLibDirs
        , testSuites  = testSuitesWithLibDirs
        }
    }
