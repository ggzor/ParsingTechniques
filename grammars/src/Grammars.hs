module Grammars where

type SententialForm n t = [Either n t] 
type Rule n t = (SententialForm n t, SententialForm n t)

data Grammar n t = Grammar { rules :: [Rule n t], startSymbol :: n }