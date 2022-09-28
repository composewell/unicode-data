-- |
-- Module      : Unicode.Char
-- Copyright   : (c) 2020 Composewell Technologies and Contributors
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
--
-- This module provides APIs to access the Unicode character database (UCD)
-- corresponding to [Unicode Standard version
-- 14.0.0](https://www.unicode.org/versions/Unicode14.0.0/).
--
-- This module re-exports several sub-modules under it.  The sub-module
-- structure under `Unicode.Char` is largely based on the
-- ["Property Index by Scope of Use" in Unicode® Standard Annex #44](https://www.unicode.org/reports/tr44/#Property_Index_Table).
--
-- The @Unicode.Char.*@ modules in turn depend on @Unicode.Internal.Char.*@
-- modules which are programmatically generated from the Unicode standard's
-- Unicode character database files. The module structure under
-- @Unicode.Internal.Char@ is largely based on the UCD text file names from
-- which the properties are generated.
--
-- For the original UCD files used in this code please refer to the @UCD@
-- section on the Unicode standard page.  See
-- https://www.unicode.org/reports/tr44/ to understand the contents and the
-- format of the unicode database files.
--

module Unicode.Char
    ( module Unicode.Char.General
    , module Unicode.Char.General.Compat
    , module Unicode.Char.Case
    , module Unicode.Char.Case.Compat
    , module Unicode.Char.Numeric
    , module Unicode.Char.Normalization
    , module Unicode.Char.Identifiers
    , unicodeVersion

    -- * Re-export from @base@
    , ord
    , chr
    )
where

import Data.Char (chr, ord)
import Data.Version (Version, makeVersion)
import Unicode.Char.Case hiding (Unfold(..), Step(..))
import Unicode.Char.Case.Compat hiding (isLower, isUpper)
import Unicode.Char.General
import Unicode.Char.General.Compat hiding (isLetter, isSpace)
import Unicode.Char.Identifiers
import Unicode.Char.Numeric
import Unicode.Char.Normalization

-- | Version of Unicode standard used by @unicode-data@.
--
-- @since 0.3.0
unicodeVersion :: Version
unicodeVersion = makeVersion [14, 0, 0]
