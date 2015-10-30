{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -ddump-splices #-} -- for debugging 
module Build.Derive.List.Monoid where 
import Derive.List
import Data.Monoid

data Elisp
 = ElispAtom (Either String Integer)
 | ElispSexp [Elisp]
 deriving (Show)

deriveMonoid ''Elisp 'ElispSexp

main = do
 putStrLn "" 
 print$ emptyElisp
 print$ toElispList (ElispAtom (Right 1))
 print$ ElispSexp [ElispAtom (Left "+"), ElispAtom (Right 1), ElispAtom (Right 2)] `mappend` mempty `mappend` ElispAtom (Right 3)
