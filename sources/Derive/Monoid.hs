{-# OPTIONS_HADDOCK not-home #-}

{-| 

when your type can hold a list of itself, 'deriveList' can generate instances for: 

* 'Semigroup'
* 'Monoid'
* 'IsList'

which are lawful (trivially, being based on the list instances).

usage: 

@
data T = ... | C [T] | ...
deriveList ''T 'C
@

For example: 

@
{-# LANGUAGE TemplateHaskell, QuasiQuotes, TypeFamilies, PatternSynonyms #-}
{-# OPTIONS_GHC -ddump-splices #-}
import Data.Semigroup 
import GHC.Exts (IsList (..))

-- a sum type 
data Elisp
 = ElispAtom (Either String Integer)
 | ElispSexp [Elisp]

'deriveList' \'\'Elisp \'ElispSexp
@

generates: 

@
instance 'Semigroup' Elisp where
 ('<>') x y = ElispSexp (toElispList x <> toElispList y)

instance 'Monoid' Elisp where
 'mempty' = EmptyElisp
 'mappend' = (<>)

instance 'IsList' Elisp where
 type 'Item' Elisp = Elisp
 'fromList' = ElispSexp
 'toList' = toElispList 

emptyElisp :: ElispSexp
emptyElisp = ElispSexp []

toElispList :: Elisp -> [Elisp]
toElispList (ElispSexp ts) = ts
toElispList t = [t]

@ 

alternatives: 

* manual instances.  
* @GeneralizeNewtypeDeriving@: works with @newtype@, but not with @data@. 
* the <http://hackage.haskell.org/package/derive derive> package: derives a different monoid (i.e. pairwise appending, when your type is a product type), which doesn't work for sum types. it also doesn't work with Semigroup. 

-} 

{-

TODO pattern EmptyElisp = ElispSexp []

TODO Or with fewer extensions and dependencies: 

@
'deriveMonoidWith' defaultDeriveListConfig{ _usePatternSynonyms=False } \'\'Elisp \'ElispSexp
@

generates: 

@
instance 'Monoid' Elisp where
 mempty = EmptyElisp [] 
 mappend x y = ElispSexp (mappend (toElispList x toElispList y))
 where 
 toElispList :: Elisp -> [Elisp]
 toElispList (ElispSexp ts) = ts
 toElispList t = [t]
@

-}

module Derive.Monoid ( deriveList ) where 
import Derive.Monoid.Internal 

