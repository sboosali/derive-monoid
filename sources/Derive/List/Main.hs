{-# LANGUAGE TemplateHaskell, TypeFamilies, OverloadedLists #-} -- TODO PatternSynonyms, 
{-# OPTIONS_GHC -ddump-splices -fno-warn-missing-signatures #-} -- for debugging 
{-# OPTIONS_HADDOCK show-extensions #-}

{-| (see source) 

-}
module Derive.List.Main where 
import Derive.List.Internal 

import Data.Semigroup 

import           GHC.Exts                          (IsList (..))


data Elisp
 = ElispAtom (Either String Integer)
 | ElispSexp [Elisp]
 deriving (Show)

deriveList ''Elisp 'ElispSexp


{-| tests all instances and declarations 

-}
main = do
 putStrLn "" 
 print $ makeDeriveListNames defaultDeriveListConfig ''Elisp 'ElispSexp
 putStrLn "" 
 print$ emptyElisp
 print$ toElispList (ElispAtom (Right 1))
 print$ ElispSexp [ElispAtom (Left "+"), ElispAtom (Right 1), ElispAtom (Right 2)] <> mempty <> ElispAtom (Right 3)

