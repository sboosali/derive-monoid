{-# LANGUAGE TemplateHaskell, TypeFamilies, OverloadedLists #-}
{-# OPTIONS_GHC -ddump-splices #-} -- for debugging 
module Build.Derive.List.IsList where 
import Derive.List
import GHC.Exts (IsList (..))

data Elisp
 = ElispAtom (Either String Integer)
 | ElispSexp [Elisp]
 deriving (Show)

deriveIsList ''Elisp 'ElispSexp

main = do
 putStrLn "" 
 print$ toElispList (ElispAtom (Right 1))
 print$ ([ElispAtom (Left "+"), ElispAtom (Right 1), ElispAtom (Right 2)] :: [Elisp]) 

