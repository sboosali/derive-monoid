{-# LANGUAGE TemplateHaskell, QuasiQuotes, RecordWildCards #-}
{-| (@.Internal@ modules may violate the PVP) -}
module Derive.List.Internal where 
import Data.Semigroup 

import Language.Haskell.TH
import GHC.Exts (IsList (..))


data DeriveListConfig = DeriveListConfig
 { _getEmptyName  :: String -> String 
 , _getAppendName :: String -> String 
 , _getToListName :: String -> String 
 -- , _usePatternSynonyms :: Bool
 -- , _useSemigroup :: Bool
 }

data DeriveListNames = DeriveListNames
 { theType :: Name 
 , theConstructor :: Name 
 , theEmpty :: Name 
 , theAppend :: Name 
 , theToList :: Name 
 } deriving (Show,Eq,Ord)



{-| derives 'Semigroup', 'Monoid', 'IsList'.  
-}
deriveList :: Name -> Name -> DecsQ 
deriveList = deriveListWith defaultDeriveListConfig


{-| derives 'Semigroup', 'Monoid' only. 
-}
deriveMonoid :: Name -> Name -> DecsQ 
deriveMonoid = deriveMonoidWith defaultDeriveListConfig


{-| derives 'Semigroup' only. 
-}
deriveSemigroup :: Name -> Name -> DecsQ 
deriveSemigroup = deriveSemigroupWith defaultDeriveListConfig


{-| derives 'IsList' only.  
-}
deriveIsList :: Name -> Name -> DecsQ 
deriveIsList = deriveIsListWith defaultDeriveListConfig


{-| derives 'Semigroup', 'Monoid', 'IsList'.  
-}
deriveListWith :: DeriveListConfig -> Name -> Name -> DecsQ 
deriveListWith config@DeriveListConfig{..} theType theConstructor = fmap concat . traverse id $ 
 [ deriveSemigroup_ names 
 , deriveMonoid_ names 
 , deriveIsList_ names 
 , makeEmpty names 
 , makeAppend names 
 , makeToList names 
 ] 
 where 
 names = makeDeriveListNames config theType theConstructor 


{-| derives 'Semigroup', 'Monoid' only. 
-}
deriveMonoidWith :: DeriveListConfig -> Name -> Name -> DecsQ 
deriveMonoidWith config@DeriveListConfig{..} theType theConstructor = fmap concat . traverse id $ 
 [ deriveMonoid_ names 
 , makeEmpty names 
 , makeAppend names 
 , makeToList names 
 ] 
 where 
 names = makeDeriveListNames config theType theConstructor 


{-| derives 'Semigroup' only. 
-}
deriveSemigroupWith :: DeriveListConfig -> Name -> Name -> DecsQ 
deriveSemigroupWith config@DeriveListConfig{..} theType theConstructor = fmap concat . traverse id $ 
 [ deriveSemigroup_ names 
 , makeAppend names 
 , makeToList names 
 ] 
 where 
 names = makeDeriveListNames config theType theConstructor 


{-| derives 'IsList' only.  
-}
deriveIsListWith :: DeriveListConfig -> Name -> Name -> DecsQ 
deriveIsListWith config@DeriveListConfig{..} theType theConstructor = fmap concat . traverse id $ 
 [ deriveIsList_ names 
 , makeToList names 
 ] 
 where 
 names = makeDeriveListNames config theType theConstructor 


{-| 

needs no constraints.
 
assumes 'makeAppend'

-}
deriveSemigroup_ :: DeriveListNames -> DecsQ 
deriveSemigroup_ DeriveListNames{..} = do 
 [d| instance Semigroup $theTypeT where
       (<>) = $theAppendE
       {-# INLINEABLE (<>) #-} |]

 where 
 theTypeT = conT theType 
 theAppendE = varE theAppend 


{-| 

needs no constraints.

assumes 'makeAppend', 'makeEmpty'

-}
deriveMonoid_ :: DeriveListNames -> DecsQ 
deriveMonoid_ DeriveListNames{..} = do 
 [d| instance Monoid $theTypeT where
      mempty = $theEmptyE
      {-# INLINEABLE mempty #-}
      mappend = $theAppendE
      {-# INLINEABLE mappend #-} |]

 where 
 theTypeT = conT theType 
 theEmptyE = varE theEmpty 
 theAppendE = varE theAppend 


{-| 

needs no constraints.

assumes 'makeToList'

-}
deriveIsList_ :: DeriveListNames -> DecsQ 
deriveIsList_ DeriveListNames{..} = do 
 [d| instance IsList $theTypeT where
      type Item $theTypeT = $theTypeT
      fromList = $theConstructorE
      {-# INLINEABLE fromList #-}
      toList = $theToListE 
      {-# INLINEABLE toList #-} |]

 where 
 theTypeT = conT theType 
 theConstructorE = conE theConstructor 
 theToListE = varE theToList 


{-| `PatternSynonyms` won't work until <https://ghc.haskell.org/trac/ghc/ticket/8761> 

-}
makeEmpty :: DeriveListNames -> DecsQ
makeEmpty DeriveListNames{..} = return [definitionD, inlinableD]
 where 
 definitionD = FunD theEmpty [Clause [] (NormalB bodyE) []]
 bodyE = ConE theConstructor `AppE` (ListE [])
 inlinableD = PragmaD (InlineP theEmpty Inlinable ConLike AllPhases)

-- makeEmpty :: DeriveListNames -> DecsQ
-- makeEmpty DeriveListNames{..} = patternQD 
--  where 
--  patternQD = [d|pattern $theEmptyQP = $theConstructorQE []|]  
--  theEmptyQP = return$ ConP theEmpty []
--  theConstructorQE = return$ ConE theConstructor

-- [d|pattern $theEmptyQP = $theConstructorQE []|] 


{-| 

assumes 'makeToList'

-}
makeAppend :: DeriveListNames -> DecsQ
makeAppend DeriveListNames{..} = do
 definitionD <- [d| $theAppendP = \x y -> $theConstructorE ($theToListE x <> $theToListE y) |]
 return$ concat [definitionD, inlinableD]

 where 
 theConstructorE = conE theConstructor 
 theToListE = varE theToList 
 theAppendP = varP theAppend 
 inlinableD = [PragmaD (InlineP theAppend Inlinable FunLike AllPhases)]

 -- [d| $theAppendP x y = $theConstructorE ($theToListE x <> $theToListE y) |]


makeToList :: DeriveListNames -> DecsQ
makeToList DeriveListNames{..} = traverse id [definitionD, inlinableD]
 where 

 definitionD = do
  tsN <- newName "ts"
  tN <- newName "t"
  return$ FunD theToList
   [ Clause [ConP theConstructor [VarP tsN]] (NormalB (VarE tsN))        [] 
   , Clause [VarP tN]                        (NormalB (ListE [VarE tN])) [] 
   ]

 inlinableD = return$ PragmaD (InlineP theToList Inlinable FunLike AllPhases)

-- [d|
--    $theListName :: $theType -> [$theType]
--    $theListName ($theConstructor ts) = ts
--    $theListName t = [t]
-- ]



{-| can debug 'deriveList' with:  

@
print $ makeDeriveListNames 'defaultDeriveListConfig' \'\'T \'C
@

-}
makeDeriveListNames :: DeriveListConfig -> Name -> Name -> DeriveListNames
makeDeriveListNames DeriveListConfig{..} theType theConstructor = DeriveListNames{..}
 where 
 theEmpty  = mkName $ _getEmptyName  (nameBase theType) 
 theAppend = mkName $ _getAppendName (nameBase theType) 
 theToList = mkName $ _getToListName (nameBase theType) 


{-| by default, the functions generated for a type @"T"@ are @"emptyT"@ and @"toTList"@. 

-}
defaultDeriveListConfig :: DeriveListConfig 
defaultDeriveListConfig = DeriveListConfig{..}
 where
 _getEmptyName  = (\typename -> "empty"<>typename)
 _getAppendName = (\typename -> "append"<>typename)
 _getToListName = (\typename -> "to"<>typename<>"List")
 -- _usePatternSynonyms = True
 -- _useSemigroup = True
