{-# LANGUAGE TemplateHaskell, QuasiQuotes, RecordWildCards #-}
{-| (@.Internal@ modules may violate the PVP) -}
module Derive.Monoid.Internal where 
import Data.Semigroup 

import Language.Haskell.TH
import GHC.Exts (IsList (..))


data DeriveListNames = DeriveListNames
 { theType :: Name 
 , theConstructor :: Name 
 , theEmpty :: Name 
 , theToList :: Name 
 } deriving (Show,Eq,Ord)


data DeriveListConfig = DeriveListConfig
 { _makeEmptyName  :: String -> String 
 , _makeToListName :: String -> String 
 -- , _usePatternSynonyms :: Bool
 -- , _useSemigroup :: Bool
 }


defaultDeriveListConfig :: DeriveListConfig 
defaultDeriveListConfig = DeriveListConfig{..}
 where
 _makeEmptyName = (\typename -> "empty"<>typename)
 _makeToListName  = (\typename -> "to"<>typename<>"List")
 -- _usePatternSynonyms = True
 -- _useSemigroup = True


{-| can debug 'deriveList' with:  

@
print $ makeDeriveListNames defaultDeriveListConfig ''T 'C
@

-}
makeDeriveListNames :: DeriveListConfig -> Name -> Name -> DeriveListNames
makeDeriveListNames DeriveListConfig{..} theType theConstructor = DeriveListNames{..}
 where 
 theEmpty  = mkName $ _makeEmptyName  (nameBase theType) 
 theToList = mkName $ _makeToListName (nameBase theType) 


{-| derives 'Semigroup', 'Monoid', 'IsList'.  

-}
deriveList :: Name -> Name -> DecsQ 
deriveList = deriveListWith defaultDeriveListConfig


deriveListWith :: DeriveListConfig -> Name -> Name -> DecsQ 
deriveListWith config@DeriveListConfig{..} theType theConstructor = fmap concat . traverse id $ 
 [ deriveSemigroup names 
 , deriveMonoid names 
 , deriveIsList names 
 , makeEmpty names 
 , makeToList names 
 ] 
 where 
 names = makeDeriveListNames config theType theConstructor 


{-| drives 'Semigroup'.  

needs no constraints.
 
assumes 'makeToList'

-}
deriveSemigroup :: DeriveListNames -> DecsQ 
deriveSemigroup DeriveListNames{..} = do 
 [d| instance Semigroup $theTypeT where
       (<>) x y = $theConstructorE ($theToListE x <> $theToListE y) |]

 where 
 theTypeT = conT theType 
 theConstructorE = conE theConstructor 
 theToListE = varE theToList 


{-| derives 'Monoid'.  

needs no constraints.

assumes 'deriveSemigroup', 'makeEmpty'

-}
deriveMonoid :: DeriveListNames -> DecsQ 
deriveMonoid DeriveListNames{..} = do 
 [d| instance Monoid $theTypeT where
      mempty = $theEmptyE
      mappend = (<>) |]

 where 
 theTypeT = conT theType 
 theEmptyE = varE theEmpty 


{-| derives 'IsList'.  

needs no constraints.

assumes 'makeToList'

-}
deriveIsList :: DeriveListNames -> DecsQ 
deriveIsList DeriveListNames{..} = do 
 [d| instance IsList $theTypeT where
       type Item $theTypeT = $theTypeT
       fromList = $theConstructorE
       toList = $theToListE |]

 where 
 theTypeT = conT theType 
 theConstructorE = conE theConstructor 
 theToListE = varE theToList 


{-| `PatternSynonyms` won't work until <https://ghc.haskell.org/trac/ghc/ticket/8761> 

-}
makeEmpty :: DeriveListNames -> DecsQ
makeEmpty DeriveListNames{..} = return [signatureD, definitionD]
 where 

 signatureD = SigD theEmpty (ConT theType) 

 definitionD = FunD theEmpty [Clause [] (NormalB bodyE) []]

 bodyE = (ConE theConstructor `AppE` (ListE []))

-- makeEmpty :: DeriveListNames -> DecsQ
-- makeEmpty DeriveListNames{..} = patternQD 
--  where 
--  patternQD = [d|pattern $theEmptyQP = $theConstructorQE []|]  
--  theEmptyQP = return$ ConP theEmpty []
--  theConstructorQE = return$ ConE theConstructor

-- [d|pattern $theEmptyQP = $theConstructorQE []|] 


makeToList :: DeriveListNames -> DecsQ
makeToList DeriveListNames{..} = traverse id [signatureD, definitionD]
 where 

 signatureD = SigD theToList <$> [t|$theTypeT -> [$theTypeT]|]  

 definitionD = do
  tsN <- newName "ts"
  tN <- newName "t"
  return$ FunD theToList
   [ Clause [ConP theConstructor [VarP tsN]] (NormalB (VarE tsN))        [] 
   , Clause [VarP tN]                        (NormalB (ListE [VarE tN])) [] 
   ]

 theTypeT = conT theType 

-- [d|
--    $theListName :: $theType -> [$theType]
--    $theListName ($theConstructor ts) = ts
--    $theListName t = [t]
-- ]

