-----------------------------------------------------------------------------

-- |
-- Module      :  Distribution.Lex
-- Copyright   :  Ben Gamari 2015-2019
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- This module contains a simple lexer supporting quoted strings
module Distribution.Lex
  ( tokenizeQuotedWords
  ) where

import Distribution.Compat.DList
import Distribution.Compat.Prelude
import Prelude ()

-- | A simple tokenizer supporting quoted strings, used in
-- Distribution.Simple.GHC.Internal to parse output from
-- ghc -info.
--
-- Please be aware that this will only split strings when seeing whitespace
-- outside of quotation marks, i.e, @"foo\"bar baz\"qux quux"@ will be
-- converted to @["foobar bazqux", "quux"]@.
--
-- This behavior can be useful when parsing text like
-- @("GCC extra via C opts","")@, for instance.
tokenizeQuotedWords :: String -> [String]
tokenizeQuotedWords = filter (not . null) . go False mempty
  where
    go
      :: Bool
      -- \^ in quoted region
      -> DList Char
      -- \^ accumulator
      -> String
      -- \^ string to be parsed
      -> [String]
    -- \^ parse result
    go _ accum []
      | [] <- accum' = []
      | otherwise = [accum']
      where
        accum' = runDList accum
    go False accum (c : cs)
      | isSpace c = runDList accum : go False mempty cs
      | c == '"' = go True accum cs
    go True accum (c : cs)
      | c == '"' = go False accum cs
    go quoted accum (c : cs) =
      go quoted (accum `mappend` singleton c) cs

{- Note: As this is a hidden module and is only used by one place in
   the Cabal codebase, this may get moved to its use site
   in the future. liamzee @ 27 Oct 23. -}