-- |
-- Copyright   : (c) 2024 Pierre Le Marre
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
--
-- Unicode version module
module UCD2Haskell.Modules.Version
    ( writeModule
    ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Short as BS
import Data.Version (Version, showVersion, versionBranch)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>), (<.>))

import UCD2Haskell.Generator (moduleToFileName, unlinesBB, apacheLicense, dirFromFileName)

writeModule ::
    Version ->
    FilePath ->
    String ->
    BS.ShortByteString ->
    IO ()
writeModule version outDir moduleName since = do
    let outFile = outDir </> moduleToFileName moduleName <.> "hs"
    let outFileDir = dirFromFileName outFile
    createDirectoryIfMissing True outFileDir
    B.writeFile outFile . B.toStrict . BB.toLazyByteString . unlinesBB $
        [ "-- DO NOT EDIT MANUALLY: autogenerated by ucd2haskell"
        , "{-# OPTIONS_HADDOCK hide #-}"
        , ""
        , apacheLicense 2024 (BB.string7 moduleName)
        , "module " <> BB.string7 moduleName <> " (unicodeVersion) where"
        , ""
        , "import Data.Version (Version, makeVersion)"
        , ""
        , "-- | Version of the Unicode standard used by this package:"
        , mconcat
            [ "-- ["
            , BB.string7 (showVersion version)
            , "](https://www.unicode.org/versions/Unicode"
            , BB.string7 (showVersion version)
            , "/)." ]
        , "--"
        , "-- @since " <> BB.shortByteString since
        , "unicodeVersion :: Version"
        , "unicodeVersion = makeVersion "
            <> BB.string7 (show (versionBranch version))
        ]
