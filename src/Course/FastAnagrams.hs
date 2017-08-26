{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.FastAnagrams where

import Course.Core
import Course.List
import Course.Functor
import qualified Data.Set as S

-- Return all anagrams of the given string
-- that appear in the given dictionary file.
fastAnagrams ::
  Chars
  -> Filename
  -> IO (List Chars)
fastAnagrams string filename =
  (\fileContents ->
     let dictonary = S.fromList (hlist (NoCaseString <$> lines fileContents)) in
       filter (\x -> S.member (NoCaseString x) dictonary) (permutations string)) <$> readFile filename 

newtype NoCaseString =
  NoCaseString {
    ncString :: Chars
  }

instance Eq NoCaseString where
  (==) = (==) `on` map toLower . ncString

instance Show NoCaseString where
  show = show . ncString

instance Ord NoCaseString where
  compare x y = compare (toLower <$> ncString x) (toLower <$> ncString y) 
