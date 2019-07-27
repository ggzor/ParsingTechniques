{-# LANGUAGE RecordWildCards #-}

module Grammars.Generation where

import Data.Algorithms.KMP.Utils
import Data.Either
import Data.Functor ((<&>))
import Data.List
import Data.List.Utils
import Data.Sequence hiding (length)

import Grammars

applyRule :: (Eq n, Eq t) => Rule n t -> SententialForm n t -> Maybe (SententialForm n t)
applyRule (lhs, rhs) s = eraseInsertAt s rhs (length lhs) <$> subIndex lhs s

generateSentences :: (Eq n, Eq t) => Grammar n t -> [[t]]
generateSentences g = [rights s | s <- generateSententialForms g, all isRight s]

generateSententialForms :: (Eq n, Eq t) => Grammar n t -> [[Either n t]]
generateSententialForms Grammar {..} = go (singleton [Left startSymbol])
  where
    go Empty = []
    go (sentence :<| queue) =
      let
        newSententialForms = rules <&> (`applyRule` sentence)
        newQueue = foldl' (\q s -> maybe q (q |>) s) queue newSententialForms
      in sentence : go newQueue