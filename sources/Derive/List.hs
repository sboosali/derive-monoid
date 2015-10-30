{-# OPTIONS_HADDOCK not-home #-}

module Derive.List 
 ( -- $introduction

   -- $example

   -- $conclusion

   deriveList
 , deriveMonoid
 , deriveSemigroup
 , deriveIsList
 , DeriveListConfig(..)
 , deriveListWith 
 , deriveMonoidWith
 , deriveSemigroupWith
 , deriveIsListWith
   -- $alternatives

 ) where 
import Derive.List.Internal 





{- $introduction

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

-} 





-- $example
-- = Examples 
-- 
-- this declaration: 
-- 
-- @
-- 
-- {-\# LANGUAGE TemplateHaskell, TypeFamilies \#-}    -- minimal extensions necessary 
-- {-\# OPTIONS_GHC -ddump-splices \#-}                -- prints out the generated code 
-- 
-- import GHC.Exts (IsList (..))                     -- minimal imports necessary 
-- import Data.Semigroup                             -- from the <https://hackage.haskell.org/package/semigroups semigroups> package 
-- 
-- -- a sum type 
-- data Elisp
--  = ElispAtom (Either String Integer)
--  | ElispSexp [Elisp]
-- 
-- 'deriveList' \'\'Elisp \'ElispSexp
-- @
-- 
-- generates these instances: 
-- 
-- @
-- instance 'Semigroup' Elisp where
--  ('<>') x y = ElispSexp (toElispList x '<>' toElispList y)
-- 
-- instance 'Monoid' Elisp where
--  'mempty' = emptyElisp
--  'mappend' = ('<>')
-- 
-- instance 'IsList' Elisp where
--  type 'Item' Elisp = Elisp
--  'fromList' = ElispSexp
--  'toList' = toElispList 
-- 
-- emptyElisp :: ElispSexp
-- emptyElisp = ElispSexp []
-- 
-- toElispList :: Elisp -> [Elisp]
-- toElispList (ElispSexp ts) = ts
-- toElispList t = [t]
-- 
-- @ 
-- 





{- $conclusion


= Documentation 

you can document functions/variables (though not instances), by placing their signatures __after__ the macro:

@
data Elisp
 = ElispAtom (Either String Integer)
 | ElispSexp [Elisp]

'deriveList' \'\'Elisp \'ElispSexp

-- | ...
emptyElisp  :: Elisp 
-- | ...
appendElisp :: Elisp -> Elisp -> Elisp
-- | ...
toElispList :: Elisp -> [Elisp]
@


= Kind

works on type constructors of any kind. that is, a polymorphic @Elisp@ would work too:  

@
data Elisp a 
 = ElispAtom a 
 | ElispSexp [Elisp]

'deriveList' \'\'Elisp \'ElispSexp

@


= Selecting Instances 
if you don't want all three instances, you can use one of: 

* 'deriveMonoid'
* 'deriveSemigroup'
* 'deriveIsList'

but only one, as they would generate duplicate declarations. 


-} 





{- $alternatives

= Alternatives to @derive-monoid@ 

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
