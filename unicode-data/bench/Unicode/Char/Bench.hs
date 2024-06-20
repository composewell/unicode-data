{-# LANGUAGE CPP #-}

module Unicode.Char.Bench
    ( -- Range benchmark
      benchRange
      -- Char benchmark
    , Bench(..)
    , CharRange(..)
    , bgroupWithValidCharRange
    , bgroupWithValidCharRange'
    , bgroupWithCharRange
    , bgroupWithCharRange'
    , bgroupWithChars
    ) where

import Control.DeepSeq (NFData (..), deepseq)
import Control.Exception (evaluate, assert)
import Data.Char (ord)
import qualified Data.Char as Char
import Foreign (Storable (..))
import qualified GHC.Exts as Exts
import GHC.IO (IO (..))
import Test.Tasty.Bench (Benchmark, bcompare, bench, bgroup, env, nf)
import Test.Tasty.Options (
    IsOption (defaultValue, optionHelp, optionName, parseValue),
 )
#if MIN_VERSION_base(4,10,0) && !MIN_VERSION_base(4,15,0)
import qualified GHC.Magic as Exts (noinline)
#endif

import qualified Unicode.Char.General as G
import Data.Ix (Ix (..))

--------------------------------------------------------------------------------
-- Range benchmark
--------------------------------------------------------------------------------

{-# INLINE benchRange #-}
benchRange
    :: forall a b. (Bounded a, Ix a, NFData b)
    => String
    -> (a -> b)
    -> Benchmark
benchRange t f = bench t (nf (fold_ f) (minBound, maxBound))

{-# INLINE fold_ #-}
fold_
    :: forall a b. (Ix a, NFData b)
    => (a -> b)
    -> (a, a)
    -> ()
fold_ f = foldr (deepseq . f) () . range

--------------------------------------------------------------------------------
-- Char range
--------------------------------------------------------------------------------

-- | Characters range
data CharRange = CharRange !Char !Char

-- | Characters range configurable from CLI
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

--------------------------------------------------------------------------------
-- Characters benchmark
--------------------------------------------------------------------------------

-- | A unit benchmark
data Bench a = Bench
  { _title :: !String     -- ^ Name
  , _func :: !(Char -> a) -- ^ Function to benchmark
  }

-- | Helper to compare benchmarks of function from this package to ones in base.
{-# INLINE bgroupWithValidCharRange #-}
bgroupWithValidCharRange ::
    String ->
    CharRange ->
    (Char -> Bool) ->
    (Chars -> [Benchmark]) ->
    Benchmark
bgroupWithValidCharRange groupTitle charRange isValid mkBenches =
    -- Avoid side-effects with garbage collection (see tasty-bench doc for env).
    -- We use pinned ByteArray# instead of lists to avoid that GC kicks in.
    env
        (initialize isValid charRange >>= evaluate)
        (bgroup groupTitle . mkBenches)

-- | Helper to compare benchmarks of function from this package to ones in base.
-- Filter out Surrogates, Private Use Areas and unsassigned code points.
{-# INLINE bgroupWithCharRange #-}
bgroupWithCharRange ::
    String ->
    CharRange ->
    (Chars -> [Benchmark]) ->
    Benchmark
bgroupWithCharRange title charRange =
    bgroupWithValidCharRange title charRange isValid
    where
    isValid c = G.generalCategory c < G.Surrogate

-- | Variant of 'bgroupWithValidCharRange'
{-# INLINE bgroupWithValidCharRange' #-}
bgroupWithValidCharRange' ::
    (NFData a) =>
    String ->
    CharRange ->
    (Char -> Bool) ->
    [Bench a] ->
    Benchmark
bgroupWithValidCharRange' groupTitle charRange isValid bs =
    bgroupWithValidCharRange groupTitle charRange isValid $ \chars ->
        [ benchCharsRange groupTitle title chars f
        | Bench title f <- bs
        ]

{-# INLINE benchCharsRange #-}
benchCharsRange :: NFData a => String -> String -> Chars -> (Char -> a) -> Benchmark
benchCharsRange groupTitle title chars = case title of
    "base" -> benchCharsNF title chars
    _      -> bcompare' "base" . benchCharsNF title chars
    where
    {-# INLINE bcompare' #-}
    -- [NOTE] Works if groupTitle uniquely identifies the benchmark group.
    bcompare' ref = bcompare
        (mconcat ["$NF == \"", ref, "\" && $(NF-1) == \"", groupTitle, "\""])

-- | Variant of 'bgroupWithCharRange'
{-# INLINE bgroupWithCharRange' #-}
bgroupWithCharRange' ::
    (NFData a) =>
    String ->
    CharRange ->
    [Bench a] ->
    Benchmark
bgroupWithCharRange' groupTitle charRange =
    bgroupWithValidCharRange' groupTitle charRange isValid
    where
    isValid c = G.generalCategory c < G.Surrogate

-- | Helper to compare benchmarks of function from this package to ones in base.
{-# INLINE bgroupWithChars #-}
bgroupWithChars :: (NFData a) => String -> Chars -> [Bench a] -> Benchmark
bgroupWithChars groupTitle chars bs = bgroup groupTitle
    [ benchCharsRange groupTitle title chars f
    | Bench title f <- bs
    ]

-- | Helper to bench a char function on a filtered char range
{-# INLINE benchCharsNF #-}
benchCharsNF
    :: (NFData a)
    => String
    -> Chars
    -> (Char -> a)
    -> Benchmark
benchCharsNF title chars f = bench title (nf (foldrChars f) chars)

--------------------------------------------------------------------------------
-- Chars byte array
--------------------------------------------------------------------------------

-- | Pinned array of characters
data Chars = Chars !Exts.ByteArray# !Int

instance NFData Chars where
    rnf (Chars !_ !_) = ()

-- | Fold over a chars byte array
foldrChars :: NFData a => (Char -> a) -> Chars -> ()
foldrChars f = go
    where
    -- Loop over the pinned char array. The loop itself does not allocate.
    go (Chars cs len) = foldr
        (\(Exts.I# k) ->
            let c = Exts.indexWideCharArray# cs (k Exts.-# 1#)
#if MIN_VERSION_base(4,10,0)
            -- `inline` is necessary to avoid excessive inlining, resulting
            -- in benchmarking empty loop iterations, i.e. not the function.
            -- We could use `inline` with more care at call site, but then we
            -- would have to test the functions one by one and everytime we
            -- modify them. Using it here is a hammer but more secure and
            -- maintainable.
            -- Note that we may improve this by controling the inlining for each
            -- phase.
            in deepseq (Exts.noinline f (Exts.C# c)))
#else
            -- HACK: No `inline` for GHC < 8.2. Should we drop support?
            in deepseq (f (Exts.C# c)))
#endif
        ()
        [1..len]

-- | Create a byte array of the chars to bench
initialize :: (Char -> Bool) -> CharRange -> IO Chars
initialize isValid charRange = IO $ \s1 ->
    case Exts.newPinnedByteArray# initialLength s1 of { (# s2, ma #) ->
    -- Write the filtered char range
    case writeChars isValid ma 0# s2 start end of { (# s3, filteredCount #) ->
    -- Duplicate to get enough chars to bench
    case tile ma 0# finalLength filteredLength s3 of { s4 ->
    case Exts.unsafeFreezeByteArray# ma s4 of { (# s5, a #) ->
        (# s5, Chars a (Exts.I# (replications Exts.*# filteredCount)) #)
    }}
       where
       -- Ensure to have enough chars
       replications = case Exts.quotInt# targetCharsCount filteredCount of
           0# -> 1#
           r# -> r#
       filteredLength = filteredCount Exts.*# wcharSize
       finalLength = filteredLength Exts.*# replications
   }}
    where
    targetCharsCount = 0x10FFFF#
    !(CharRange start end) = assert
        (ord end - ord start + 1 < Exts.I# targetCharsCount)
        charRange
    !initialLength = targetCharsCount Exts.*# wcharSize
    !(Exts.I# wcharSize) = sizeOf 'x'

-- | Write a range of chars that match the given predicate
writeChars ::
    (Char -> Bool) ->
    Exts.MutableByteArray# d ->
    Exts.Int# ->
    Exts.State# d ->
    Char ->
    Char ->
    (# Exts.State# d, Exts.Int# #)
writeChars isValid ma = go
    where
    go i s c1@(Exts.C# c1#) !c2 = if c1 < c2
        then go i' s' (succ c1) c2
        else (# s', i' #)
        where
        !(# s', i' #) = if isValid c1
            then (# Exts.writeWideCharArray# ma i c1# s, i Exts.+# 1# #)
            else (# s, i #)

-- | Duplicate a portion of an array
--
-- Adapted from Data.Text.Array.tile
tile ::
    -- | Mutable array
    Exts.MutableByteArray# s ->
    -- | Start of the portion to duplicate
    Exts.Int# ->
    -- | Total length of the duplicate
    Exts.Int# ->
    -- | Length of the portion to duplicate
    Exts.Int# ->
    Exts.State# s ->
    Exts.State# s
tile dest destOff totalLen = go
    where
    go l s
        | Exts.isTrue# ((2# Exts.*# l) Exts.># totalLen) =
            Exts.copyMutableByteArray#
                dest
                destOff
                dest
                (destOff Exts.+# l)
                (totalLen Exts.-# l)
                s
        | otherwise =
            case Exts.copyMutableByteArray#
                    dest
                    destOff
                    dest
                    (destOff Exts.+# l)
                    l
                    s of
                        s' -> go (2# Exts.*# l) s'
