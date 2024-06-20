-- autogenerated from https://www.unicode.org/Public/15.0.0/ucd/intentional.txt
-- |
-- Module      : Unicode.Internal.Char.Security.IntentionalConfusables
-- Copyright   : (c) 2022 Composewell Technologies and Contributors
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental

{-# OPTIONS_HADDOCK hide #-}

module Unicode.Internal.Char.Security.IntentionalConfusables
(intentionalConfusables)
where

import GHC.Exts (Addr#)

-- | Returns the /intentional/ confusables of a character, if any.
--
-- The resulting 'CString' is null-terminated and encoded in UTF-8.
--
-- @since 0.1.0
intentionalConfusables :: Char -> Addr#
intentionalConfusables = \case
    '!' -> "\199\131\0"#
    'A' -> "\206\145\0"#
    'B' -> "\206\146\0"#
    'C' -> "\208\161\0"#
    'E' -> "\206\149\0"#
    'H' -> "\206\151\0"#
    'I' -> "\206\153\0"#
    'J' -> "\208\136\0"#
    'K' -> "\206\154\0"#
    'M' -> "\206\156\0"#
    'N' -> "\206\157\0"#
    'O' -> "\206\159\0"#
    'P' -> "\206\161\0"#
    'S' -> "\208\133\0"#
    'T' -> "\206\164\0"#
    'X' -> "\206\167\0"#
    'Y' -> "\206\165\0"#
    'Z' -> "\206\150\0"#
    'a' -> "\208\176\0"#
    'c' -> "\209\129\0"#
    'd' -> "\212\129\0"#
    'e' -> "\208\181\0"#
    'h' -> "\210\187\0"#
    'i' -> "\209\150\0"#
    'j' -> "\207\179\0"#
    'o' -> "\206\191\0"#
    'p' -> "\209\128\0"#
    's' -> "\209\149\0"#
    'x' -> "\209\133\0"#
    'y' -> "\209\131\0"#
    '\198' -> "\211\148\0"#
    '\208' -> "\196\144\0"#
    '\230' -> "\211\149\0"#
    '\272' -> "\195\144\0"#
    '\312' -> "\208\186\0"#
    '\386' -> "\208\145\0"#
    '\399' -> "\211\152\0"#
    '\415' -> "\211\168\0"#
    '\425' -> "\206\163\0"#
    '\451' -> "\33\0"#
    '\477' -> "\201\153\0"#
    '\581' -> "\206\155\0"#
    '\601' -> "\199\157\211\153\0"#
    '\603' -> "\206\181\0"#
    '\617' -> "\206\185\0"#
    '\618' -> "\211\143\0"#
    '\629' -> "\211\169\0"#
    '\658' -> "\211\161\0"#
    '\665' -> "\208\178\0"#
    '\668' -> "\208\189\0"#
    '\913' -> "\65\0"#
    '\914' -> "\66\0"#
    '\915' -> "\208\147\0"#
    '\917' -> "\69\0"#
    '\918' -> "\90\0"#
    '\919' -> "\72\0"#
    '\921' -> "\73\0"#
    '\922' -> "\75\0"#
    '\923' -> "\201\133\0"#
    '\924' -> "\77\0"#
    '\925' -> "\78\0"#
    '\927' -> "\79\0"#
    '\928' -> "\208\159\0"#
    '\929' -> "\80\0"#
    '\931' -> "\198\169\0"#
    '\932' -> "\84\0"#
    '\933' -> "\89\0"#
    '\935' -> "\88\0"#
    '\945' -> "\226\141\186\0"#
    '\949' -> "\201\155\0"#
    '\953' -> "\201\169\226\141\179\0"#
    '\959' -> "\111\0"#
    '\961' -> "\226\141\180\0"#
    '\969' -> "\226\141\181\0"#
    '\1011' -> "\106\0"#
    '\1029' -> "\83\0"#
    '\1032' -> "\74\0"#
    '\1041' -> "\198\130\0"#
    '\1043' -> "\206\147\0"#
    '\1055' -> "\206\160\0"#
    '\1057' -> "\67\0"#
    '\1072' -> "\97\0"#
    '\1074' -> "\202\153\0"#
    '\1075' -> "\225\180\166\0"#
    '\1077' -> "\101\0"#
    '\1082' -> "\196\184\0"#
    '\1083' -> "\225\180\171\0"#
    '\1084' -> "\225\180\141\0"#
    '\1085' -> "\202\156\0"#
    '\1087' -> "\225\180\168\0"#
    '\1088' -> "\112\0"#
    '\1089' -> "\99\0"#
    '\1090' -> "\225\180\155\0"#
    '\1091' -> "\121\0"#
    '\1093' -> "\120\0"#
    '\1109' -> "\115\0"#
    '\1110' -> "\105\0"#
    '\1178' -> "\226\177\169\0"#
    '\1186' -> "\226\177\167\0"#
    '\1211' -> "\104\0"#
    '\1231' -> "\201\170\0"#
    '\1236' -> "\195\134\0"#
    '\1237' -> "\195\166\0"#
    '\1240' -> "\198\143\0"#
    '\1241' -> "\201\153\0"#
    '\1249' -> "\202\146\0"#
    '\1256' -> "\198\159\0"#
    '\1257' -> "\201\181\0"#
    '\1281' -> "\100\0"#
    '\4125' -> "\225\129\128\0"#
    '\4160' -> "\225\128\157\0"#
    '\6050' -> "\225\158\163\0"#
    '\6051' -> "\225\158\162\0"#
    '\6197' -> "\225\161\149\0"#
    '\6229' -> "\225\160\181\0"#
    '\6558' -> "\225\167\144\0"#
    '\6577' -> "\225\167\145\0"#
    '\6608' -> "\225\166\158\0"#
    '\6609' -> "\225\166\177\0"#
    '\6725' -> "\225\170\128\225\170\144\0"#
    '\6784' -> "\225\169\133\0"#
    '\6800' -> "\225\169\133\0"#
    '\6925' -> "\225\173\146\0"#
    '\6929' -> "\225\173\147\0"#
    '\6952' -> "\225\173\152\0"#
    '\6992' -> "\225\173\156\0"#
    '\6994' -> "\225\172\141\0"#
    '\6995' -> "\225\172\145\0"#
    '\7000' -> "\225\172\168\0"#
    '\7004' -> "\225\173\144\0"#
    '\7437' -> "\208\188\0"#
    '\7448' -> "\225\180\169\0"#
    '\7451' -> "\209\130\0"#
    '\7462' -> "\208\179\0"#
    '\7464' -> "\208\191\0"#
    '\7465' -> "\225\180\152\0"#
    '\7467' -> "\208\187\0"#
    '\9075' -> "\206\185\0"#
    '\9076' -> "\207\129\0"#
    '\9077' -> "\207\137\0"#
    '\9082' -> "\206\177\0"#
    '\11367' -> "\210\162\0"#
    '\11369' -> "\210\154\0"#
    '\43462' -> "\234\167\144\0"#
    '\43472' -> "\234\167\134\0"#
    '\66434' -> "\240\144\143\145\0"#
    '\66451' -> "\240\144\143\147\0"#
    '\66458' -> "\240\146\128\184\0"#
    '\66513' -> "\240\144\142\130\0"#
    '\66515' -> "\240\144\142\147\0"#
    '\66694' -> "\240\144\146\160\0"#
    '\66720' -> "\240\144\146\134\0"#
    '\73784' -> "\240\144\142\154\0"#
    _ -> "\0"#
