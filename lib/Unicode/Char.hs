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
    , module Unicode.Char.Case
    , module Unicode.Char.Normalization
    , module Unicode.Char.Identifiers

    -- * Re-export
    -- ** Numbers
    , isDigit
    , isOctDigit
    , isHexDigit
    -- ** Subranges
    , isAscii
    , isLatin1
    , isAsciiUpper
    , isAsciiLower
    -- ** Numeric representations
    , ord
    , chr
    )
where

import Data.Char 
    ( isDigit, isOctDigit, isHexDigit
    , isAscii, isLatin1, isAsciiUpper, isAsciiLower
    , chr, ord )
import Unicode.Char.Case
import Unicode.Char.General
import Unicode.Char.Normalization
import Unicode.Char.Identifiers
