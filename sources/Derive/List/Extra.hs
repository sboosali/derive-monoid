{-# LANGUAGE TemplateHaskell, LambdaCase #-}
module Derive.List.Extra where 

import Language.Haskell.TH
import Control.Monad


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

output is deterministic.

-}
saturateT :: Name -> Q Type 
-- saturateT = conT
saturateT n = do
 arity_ <- getArityT n
 let arity = maybe (error$ messageNoArity n) id arity_
 let arguments = take arity (map mkName alphabeticalVariables) 
 return$ foldl AppT (ConT n) (VarT <$> arguments) 
 where 
 messageNoArity n = "the name {{" ++ show n ++ "}} has no arity (maybe it's been given to some macro that expects a Type?)"

{-| >>> take 3 alphabeticalVariables 
["a","b","c"]
-}
alphabeticalVariables :: [String]
alphabeticalVariables = map (:[]) ['a'..'z'] ++ do
  x <- ['a'..'z']
  y <- ['a'..'z']
  return [x, y]

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
 DataD    _ _ variables _ _ _ -> Just (length variables)
 NewtypeD _ _ _ variables _ _ -> Just (length variables)
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

