{- source partly taken from http://wewantarock.wordpress.com/tag/cabal/ -}
module Main (main) where

import Distribution.Simple 
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Program
import Distribution.Verbosity
import Distribution.PackageDescription
import System.FilePath 
import System.Directory
import Data.Maybe (fromJust)
import Foreign (bitSize)
import Control.Monad (when)

main :: IO ()
main = defaultMainWithHooks simpleUserHooks { buildHook = myBuildHook }

myBuildHook pkg_descr local_bld_info user_hooks bld_flags =
    do
    let lib       = fromJust (library pkg_descr)
        lib_bi    = libBuildInfo lib
        custom_bi = customFieldsBI lib_bi
    case lookup "x-cc-name" custom_bi of
      Nothing -> buildHook simpleUserHooks pkg_descr local_bld_info user_hooks bld_flags
      Just cpp_name -> do
        let cpp_name  = fromJust (lookup "x-cc-name" custom_bi)
            c_srcs    = cSources lib_bi
            mbits     = bitSize (undefined :: Int)
            cc_opts   = ("-m" ++ show mbits) : "-S" : ccOptions lib_bi
            inc_dirs  = includeDirs lib_bi
            lib_dirs  = extraLibDirs lib_bi
            bld_dir   = buildDir local_bld_info
            prog      = ConfiguredProgram { programId = cpp_name, programVersion = Nothing,
                                            programDefaultArgs = [], programOverrideArgs = [],
                                            programLocation = FoundOnSystem { locationPath = cpp_name } }
        -- Compile C/C++ sources
        putStrLn $ "invoking my compile phase " ++ cpp_name
        objs <- mapM (compileCxx prog cc_opts inc_dirs bld_dir) c_srcs
        -- Remove C/C++ source code from the hooked build (don't change libs)
        let 
            lib_bi'    = lib_bi { cSources = map replaceWithAsm c_srcs }
            lib'       = lib    { libBuildInfo = lib_bi' }
            pkg_descr' = pkg_descr { library = Just lib' }
        -- The following line invokes the standard build behaviour
        putStrLn "Invoke default build hook"
        buildHook simpleUserHooks pkg_descr' local_bld_info user_hooks bld_flags

compileCxx :: ConfiguredProgram  -- ^ C/C++ compiler (gcc)
           -> [String]           -- ^ Compile options from Cabal and wxConfig
           -> [String]           -- ^ Include paths from Cabal and wxConfig
           -> FilePath           -- ^ Base output directory
           -> FilePath           -- ^ Path to source file
           -> IO FilePath        -- ^ Path to generated object code
compileCxx gcc opts incls out_path cxx_src = do
    let includes  = map ("-I" ++) incls
        -- out_path' = normalisePath out_path
        -- cxx_src'  = normalisePath cxx_src
        -- out_file  = out_path </> dropFileName cxx_src </>
        --                replaceExtension (takeFileName cxx_src) ".o"
        out_file = replaceWithAsm cxx_src
        out       = ["-c", cxx_src, "-o", out_file]
        -- opts     = opts ++ osCompileOpts
        do_it     = True -- needsCompiling cxx_src out_file
    when do_it $ createDirectoryIfMissing True (dropFileName out_file) >> 
                 runProgram verbose gcc (includes ++ opts ++ out)
    return out_file

replaceWithAsm p = replaceExtension p ".s"
