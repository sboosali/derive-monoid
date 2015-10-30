{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -ddump-splices #-} -- for debugging 
module Build.Derive.List.Semigroup where 
import Derive.List
import Data.Semigroup 

data Elisp
 = ElispAtom (Either String Integer)
 | ElispSexp [Elisp]
 deriving (Show)

deriveSemigroup ''Elisp 'ElispSexp

main = do
 putStrLn ""
 print$ toElispList (ElispAtom (Right 1))
 print$ ElispSexp [ElispAtom (Left "+"), ElispAtom (Right 1), ElispAtom (Right 2)] <> ElispAtom (Right 3)

