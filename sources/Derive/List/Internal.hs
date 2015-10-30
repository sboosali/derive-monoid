{-# LANGUAGE TemplateHaskell, QuasiQuotes, RecordWildCards, LambdaCase #-}
{-| (@.Internal@ modules may violate the PVP) -}
module Derive.List.Internal where 
import Data.Semigroup 

import Language.Haskell.TH
import GHC.Exts (IsList (..))
import Control.Monad


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
 theTypeT = saturateT theType 
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
 theTypeT = saturateT theType 
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
 theTypeT = saturateT theType 
 theConstructorE = conE theConstructor 
 theToListE = varE theToList 


{-| `PatternSynonyms` won't work until <https://ghc.haskell.org/trac/ghc/ticket/8761> 

-}
makeEmpty :: DeriveListNames -> DecsQ
makeEmpty DeriveListNames{..} = return [definitionD, inlinableD]
 where 
 definitionD = FunD theEmpty [Clause [] (NormalB bodyE) []]
 bodyE = ConE theConstructor `AppE` (ListE [])
 inlinableD = PragmaD (InlineP theEmpty Inlinable ConLike AllPhases) -- TODO ConLike?

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


{-| saturates a type constructor with type variables. outputs a 'Type' of kind @*@. 

>>> :set -XTemplateHaskell  
>>> import Language.Haskell.TH
>>> $(printQ (saturateT ''Bool))
ConT GHC.Types.Bool 
>>> $(printQ (saturateT ''[]))
AppT (ConT GHC.Types.[]) (VarT a)
>>> $(printQ (saturateT ''Either))
AppT (AppT (ConT Data.Either.Either) (VarT a)) (VarT b)

-- >>> $(printQ (saturateT_ 'False))
-- Exception when trying to run compile-time code:
--   the name {{GHC.Types.False}} has no arity (maybe it's been given to some macro that expects a Type?)

-}
saturateT :: Name -> Q Type 
-- saturateT = conT
saturateT n = do
 arity_ <- getArityT n
 let arity = maybe (error$ messageNoArity n) id arity_
 unless (arity <= 26) $ (error$ messageHugeArity n)
 let arguments = take arity (map (mkName . (:[])) ['a'..'z']) 
 return$ foldl AppT (ConT n) (VarT <$> arguments) 
 where 
 messageNoArity n = "the name {{" <> show n <> "}} has no arity (maybe it's been given to some macro that expects a Type?)"
 messageHugeArity n = "the name {{" <> show n <> "}} has too big an arity (over 26)"



{-| get the arity of a (type) name. 

>>> :set -XTemplateHaskell  
>>> import Language.Haskell.TH
>>> $(printQ (getArityT ''Bool))
Just 0
>>> $(printQ (getArityT 'False))
Nothing
>>> $(printQ (getArityT ''Either))
Just 2

-}
getArityT :: Name -> Q (Maybe Int)
getArityT n = getArityI <$> reify n 


{-| get the arity of a type constructor or a primitive type constructor, by name. 

-}
getArityI :: Info -> Maybe Int
getArityI = \case 
 TyConI d -> getArityD d
 PrimTyConI _ arity _ -> Just arity
 _ -> Nothing 


{-| get the arity of a @data@ or a @newtype@. 

TODO saturate @type@ synonym0

-}
getArityD :: Dec -> Maybe Int
getArityD = \case 
 DataD    _ _ variables _ _ -> Just (length variables)
 NewtypeD _ _ variables _ _ -> Just (length variables)
 -- TySynD Name [TyVarBndr] Type
 _ -> Nothing 


{-| show the result of a Template Haskell action, in IO.

>>> :set -XTemplateHaskell  
>>> import Language.Haskell.TH
>>> $(printQ $ reify ''Bool)
TyConI (DataD [] GHC.Types.Bool [] [NormalC GHC.Types.False [],NormalC GHC.Types.True []] [])

works around @"Template Haskell error: Can't do `reify' in the IO monad"@, 
which prevents them from being printed directly.  

see <http://stackoverflow.com/questions/16690925/template-haskell-reify-in-ghci stackoverflow>

-}
printQ :: (Show a) => Q a -> Q Exp
printQ q = go <$> q
 where 
 go x = VarE 'putStrLn `AppE` (LitE . StringL . show) x

-- getVariableName :: TyVarBndr -> Name 
-- getVariableName = \case 
--  PlainTV name    -> name 	
--  KindedTV name _ -> name 

