{-# LANGUAGE LambdaCase #-}

module Unicode.Char.Bench
    ( Bench(..)
    , CharRange(..)
    , bgroup'
    , benchChars
    , benchCharsNF
    ) where

import Control.DeepSeq (NFData, deepseq, force)
import Control.Exception (evaluate)
import Test.Tasty.Bench (Benchmark, bgroup, bench, bcompare, env, nf)
import Test.Tasty.Options
    ( IsOption(defaultValue, optionHelp, optionName, parseValue) )

import qualified Data.Char as Char
import qualified Unicode.Char.General as G

-- | A unit benchmark
data Bench a = Bench
  { _title :: !String  -- ^ Name
  , _func :: Char -> a -- ^ Function to benchmark
  }

data CharRange = CharRange !Char !Char

instance IsOption CharRange where
    defaultValue = CharRange minBound maxBound
    parseValue = \case
        "ascii"      -> Just (CharRange minBound '\x7f')
        "bmp"        -> Just (CharRange minBound '\xffff')
        "planes0To3" -> Just (CharRange minBound '\x3FFFF')
        -- [TODO] handle errors
        s ->
          let (l, u) = break (== '-') s
          in Just (CharRange (Char.chr (read l)) (Char.chr (read (drop 1 u))))
    optionName = pure "chars"
    optionHelp = pure "Range of chars to test"

{-# INLINE bgroup' #-}
bgroup' :: NFData a => String -> CharRange -> [Bench a] -> Benchmark
bgroup' groupTitle charRange bs = bgroup groupTitle
    [ benchChars' title f
    | Bench title f <- bs
    ]
    where
    {-# INLINE benchChars' #-}
    benchChars' title = case title of
        "base" -> benchChars title charRange
        _      -> bcompare' "base" . benchChars title charRange

    {-# INLINE bcompare' #-}
    -- [NOTE] Works if groupTitle uniquely identifies the benchmark group.
    bcompare' ref = bcompare
        (mconcat ["$NF == \"", ref, "\" && $(NF-1) == \"", groupTitle, "\""])

{-# INLINE benchChars #-}
benchChars
    :: (NFData a)
    => String
    -> CharRange
    -> (Char -> a)
    -> Benchmark
benchChars t charRange = benchCharsNF t charRange isValid
    where
    -- Filter out: Surrogates, Private Use Areas and unsassigned code points
    isValid c = G.generalCategory c < G.Surrogate

{-# INLINE benchCharsNF #-}
benchCharsNF
    :: forall a. (NFData a)
    => String
    -> CharRange
    -> (Char -> Bool)
    -> (Char -> a)
    -> Benchmark
benchCharsNF t charRange isValid f =
    -- Avoid side-effects with garbage collection (see tasty-bench doc)
    env
        (evaluate (force chars'))     -- initialize
        (bench t . nf (foldString f)) -- benchmark
    where
    CharRange l u = charRange
    chars = filter isValid [l..u]
    -- Ensure to have sufficiently chars
    n = 0x10FFFF `div` length chars
    chars' = mconcat (replicate n chars)

{-# INLINE foldString #-}
foldString :: forall a. (NFData a) => (Char -> a) -> String -> ()
foldString f = foldr (deepseq . f) ()
