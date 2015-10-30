{-# LANGUAGE TemplateHaskell, TypeFamilies, OverloadedLists #-} -- TODO PatternSynonyms, 
{-# OPTIONS_GHC -ddump-splices #-} -- for debugging 
{-# OPTIONS_HADDOCK show-extensions #-}

{-| (see source) 

-}
module Derive.Monoid.Main where 
import Derive.Monoid.Internal 

import Data.Semigroup 

import           GHC.Exts                          (IsList (..))


data Elisp
 = ElispAtom (Either String Integer)
 | ElispSexp [Elisp]
 deriving (Show)

-- deriveList ''Elisp 'ElispSexp
deriveMonoid ''Elisp 'ElispSexp

{-| tests all instances and declarations 

-}
main :: IO () 
main = do
 putStrLn "" 
 print $ makeDeriveListNames defaultDeriveListConfig ''Elisp 'ElispSexp
 putStrLn "" 
 print$ emptyElisp
 print$ toElispList (ElispAtom (Right 1))
 print$ ElispSexp [ElispAtom (Left "+"), ElispAtom (Right 1), ElispAtom (Right 2)] <> mempty <> ElispAtom (Right 3)
 return() 
