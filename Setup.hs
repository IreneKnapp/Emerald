{- source partly taken from http://wewantarock.wordpress.com/tag/cabal/ -}
module Main (main) where

import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.Simple.Utils (rawSystemExit)
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Program
import Distribution.Verbosity
import Distribution.PackageDescription
import Distribution.System
import System.Directory
import Control.Monad.State
import Control.Monad (when, mzero)
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class (lift)
import Data.Maybe (fromJust)

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
    { buildHook = myBuildHook
    , instHook  = myInstHook
    }
    
    
type Flags = [String]
type Libs  = [String]

data ConfState = ConfState Flags Libs


addFlag :: String -> StateT ConfState IO ()
addFlag flag = StateT $ \(ConfState fs ls) -> return ((), ConfState (flag:fs) ls)


addLib :: String -> StateT ConfState IO ()
addLib lib = StateT $ \(ConfState fs ls) -> return ((), ConfState fs (lib:ls))


myInstHook pkgDesc localBuildInfo userHooks installFlags = do
    let file = tmpFile $ fromJust . pkgDescrFile $ localBuildInfo
    libs <- readFile file
    
    let lib       = fromJust . library $ pkgDesc
        info      = libBuildInfo lib
        libs'     = libs : extraLibs info
        info'     = info { extraLibs = libs' }
        lib'      = Just $ lib { libBuildInfo = info' }
        pkgDesc'  = pkgDesc { library = lib' }
        
    --putStrLn $ "Install hook> extra libs: " ++ (show libs')
    removeFile file
    
    instHook simpleUserHooks pkgDesc' localBuildInfo userHooks installFlags


myBuildHook pkgDesc localBuildInfo userHooks buildFlags = do
    putStrLn "Configuring C build"
    --let verbosity = fromFlag . buildVerbosity $ buildFlags
    
    (flags, libs) <- case buildOS of
                     Windows -> configureWindows
                     OSX     -> configureOSX
                     _       -> configureUnix
                     
    let lib       = fromJust . library $ pkgDesc
        info      = libBuildInfo lib
        info'     = info
                  { ccOptions = ccOptions info ++ flags
                  , extraLibs = extraLibs info ++ libs
                  }
        lib'      = Just $ lib { libBuildInfo = info' }
        pkgDesc'  = pkgDesc { library = lib' }
        cabalFile = fromJust . pkgDescrFile $ localBuildInfo
        
    writeBuildInfo libs cabalFile
    
    buildHook simpleUserHooks pkgDesc' localBuildInfo userHooks buildFlags
    
    
configureWindows :: IO (Flags, Libs)
configureWindows = return ([], [])


configureOSX :: IO (Flags, Libs)
configureOSX = return ([], [])


configureUnix :: IO (Flags, Libs)
configureUnix = do
    ConfState flags libs <- execStateT configureUnix' $ ConfState [] []
    putStr "Flags:"
    mapM_ (putStr . (' ':)) flags
    putStrLn ""
    putStr "Libs:"
    mapM_ (putStr . (' ' :)) libs
    putStrLn ""
    
    return (flags, libs)
    
    
configureUnix' :: StateT ConfState IO ()
configureUnix' = do
    runMaybeT $ do
        maybeCheck checkXrandr  "Xrandr"  ["-D_GLFW_HAS_XRANDR"]      ["Xrandr"]
        maybeCheck checkVidMode "VidMode" ["-D_GLFW_HAS_XF86VIDMODE"] ["Xxf86vm", "Xext"]
    
    runMaybeT $ do
        maybeCheck checkGlXGetProcAddress    "glXGetProcAddress"    ["-D_GLFW_HAS_GLXGETPROCADDRESS"]    []
        maybeCheck checkGlXGetProcAddressARB "glXGetProcAddressARB" ["-D_GLFW_HAS_GLXGETPROCADDRESSARB"] []
        maybeCheck checkGlXGetProcAddressEXT "glXGetProcAddressEXT" ["-D_GLFW_HAS_GLXGETPROCADDRESSEXT"] []
        maybeCheck checkDlOpen               "dlopen"               ["-D_GLFW_HAS_DLOPEN"]               []
        
    check checkSysConf "sysconf" ["-D_GLFW_HAS_SYSCONF"] []
    check checkSysCtl  "sysctl"  ["-D_GLFW_HAS_SYSCTL"]  []
    
    return ()


check :: IO (Bool) -> String -> Flags -> Libs -> StateT ConfState IO Bool
check performCheck name flags libs = do
    lift . putStr $ "Checking for " ++ name ++ " support..."
    success <- lift performCheck
    if success then do
        mapM addFlag flags
        mapM addLib libs
        lift $ putStrLn "yes"
        return True
    else do
        lift $ putStrLn "no"
        return False
    
    
maybeCheck :: IO (Bool) -> String -> Flags -> Libs -> MaybeT (StateT ConfState IO) ()
maybeCheck performCheck name flags libs  = do
    success <- lift $ check performCheck name flags libs
    if success then
        mzero
    else
        return ()
    

checkXrandr :: IO (Bool)
checkXrandr = performTest contents
    where
        contents = unlines
            ["#include <X11/Xlib.h>"
            ,"#include <X11/extensions/Xrandr.h>"
            ,"int main() {return 0;}"
            ]
    
    
checkVidMode :: IO (Bool)
checkVidMode = performTest contents
    where
        contents = unlines
            ["#include <X11/Xlib.h>"
            ,"#include <X11/extensions/xf86vmode.h>"
            ,"#if defined(__APPLE_CC__)"
            ,"#error Not supported under Mac OS X"
            ,"#endif"
            ,"int main() {return 0;}"
            ]
          
checkGlXGetProcAddress :: IO (Bool)
checkGlXGetProcAddress = performTest contents
    where
        contents = unlines
            ["#include <X11/Xlib.h>"
            ,"#include <GL/glx.h>"
            ,"#include <GL/gl.h>"
            ,"int main() {void *ptr=(void*)glXGetProcAddress(\"glFun\"); return 0;}"
            ]
          
          
checkGlXGetProcAddressARB :: IO (Bool)
checkGlXGetProcAddressARB = performTest contents
    where
        contents = unlines
            ["#include <X11/Xlib.h>"
            ,"#include <GL/glx.h>"
            ,"#include <GL/gl.h>"
            ,"int main() {void *ptr=(void*)glXGetProcAddressARB(\"glFun\"); return 0;}"
            ]
          
          
checkGlXGetProcAddressEXT :: IO (Bool)
checkGlXGetProcAddressEXT = performTest contents
    where
        contents = unlines
            ["#include <X11/Xlib.h>"
            ,"#include <GL/glx.h>"
            ,"#include <GL/gl.h>"
            ,"int main() {void *ptr=(void*)glXGetProcAddressEXT(\"glFun\"); return 0;}"
            ]
    
    
checkDlOpen :: IO (Bool)
checkDlOpen = performTest contents
    where
        contents = unlines
            ["#include <dlfcn.h>"
            ,"int main() {void *l=dlopen(\"libGL.so\",RTLD_LAZY|RTLD_GLOBAL); return 0;}"
            ]
    
    
checkSysConf :: IO (Bool)
checkSysConf = performTest contents
    where
        contents = unlines
            ["#include <unistd.h>"
            ,"#ifndef _SC_NPROCESSORS_ONLN"
            ,"#ifndef _SC_NPROC_ONLN"
            ,"#error Neither _SC_NPROCESSORS_ONLN nor _SC_NPROC_ONLN available"
            ,"#endif"
            ,"#endif"
            ,"int main() {long x=sysconf(_SC_ARG_MAX); return 0; }"
            ]
    
    
checkSysCtl :: IO (Bool)
checkSysCtl = performTest contents
    where
        contents = unlines
            ["#include <sys/types.h>"
            ,"#include <sys/sysctl.h>"
            ,"#ifdef CTL_HW"
            ,"#ifndef HW_NCPU"
            ,"  error;"
            ,"#endif"
            ,"#endif"
            ,"int main() { return 0; }"
            ]
    
    
performTest :: String -> IO (Bool)
performTest contents = do
    let path = "/tmp/glfw-test.c"
        out  = toObject path
    
    writeFile path contents
    rawSystemExit silent "cc" ["-c", path, "-o", out]
    removeFile path
    
    success <- doesFileExist out
    when (success) $ removeFile out
    
    return success
    
    
toObject :: FilePath -> FilePath
toObject = reverse . ('o':) . ('.':) . drop 2 . reverse


writeBuildInfo :: Libs -> FilePath -> IO ()
writeBuildInfo libs cabalFile = do
    let file = tmpFile cabalFile
        contents = unwords libs
        
    writeFile file contents


tmpFile :: FilePath -> FilePath
tmpFile = (++ "tmp") . reverse . drop 5 . reverse
