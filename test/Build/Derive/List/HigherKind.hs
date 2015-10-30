{-# LANGUAGE TemplateHaskell, TypeFamilies, OverloadedLists #-}
{-# OPTIONS_GHC -ddump-splices #-} -- for debugging 
module Build.Derive.List.HigherKind where 
import Derive.List
import Data.Semigroup 
import GHC.Exts (IsList (..))

data Elisp a 
 = ElispAtom a 
 | ElispSexp [Elisp a]
 deriving (Show)

deriveList ''Elisp 'ElispSexp

main = do
 putStrLn "" 
 print$ (emptyElisp :: Elisp Bool)
 print$ toElispList (ElispAtom True)
 print$ ElispSexp [ElispAtom (Left "&&"), ElispAtom (Right True), ElispAtom (Right True)] <> mempty <> ElispAtom (Right False)
