{-# LANGUAGE ExistentialQuantification #-}

-- |
-- Module      : Czar.Language.TypeCheck
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Czar.Language.TypeCheck where

-- Untyped terms (what I get from my parser):
data Exp
    = EDouble Double
    | EString String
    | EPrim String
    | EApp Exp Exp deriving (Show)

-- Typed terms
data Term a where
    Num :: Double -> Term Double
    Str :: String -> Term String
    App :: Term (a->b) -> Term a -> Term b
    Fun :: (a->b) -> Term (a->b)

-- Typed evaluator
eval :: Term a -> a
eval (Num x)     = x
eval (Str x)     = x
eval (Fun x )    = x
eval (App e1 e2) = (eval e1) (eval e2)

-- Types and type comparison
data Typ a where
    TDouble :: Typ Double
    TString :: Typ String
    TFun    :: Typ a -> Typ b -> Typ (a->b)

data EQ a b where
    Refl :: EQ a a

-- checking that two types are the same. If so, give the witness
eqt :: Typ a -> Typ b -> Maybe (EQ a b)
eqt TDouble TDouble = Just $ Refl
eqt TString TString = Just $ Refl
eqt (TFun ta1 tb1) (TFun ta2 tb2) = eqt' (eqt ta1 ta2) (eqt tb1 tb2)
  where
    eqt' :: (Maybe (EQ ta1 ta2)) -> Maybe (EQ tb1 tb2) ->
            Maybe (EQ (ta1 -> tb1) (ta2 -> tb2))
    eqt' (Just Refl) (Just Refl) = Just Refl
    eqt' _ _ = Nothing
eqt _ _ = Nothing

instance Show (Typ a) where
    show TDouble = "Double"
    show TString = "String"
    show (TFun ta tb) = "(" ++ show ta ++ "->" ++ show tb ++ ")"

-- Type checking
data MostlyStatic = forall t. MostlyStatic (Typ t, Term t)

-- Typing environment
type Gamma = [(String, MostlyStatic)]

-- Initial environment (the types of primitives)
env0 =
    [ ("rev",  MostlyStatic (TFun TString TString, Fun (reverse :: String -> String)))
     -- sorry, no polymorphism!
    , ("show", MostlyStatic (TFun TDouble TString, Fun (show :: Double -> String)))
    , ("inc",  MostlyStatic (TFun TDouble TDouble, Fun ((+ (1.0 :: Double)))))
    , ("+",    MostlyStatic (TFun TDouble (TFun TDouble TDouble), Fun ((+) :: Double -> Double -> Double)))
    ]

typecheck :: Gamma -> Exp -> Either String MostlyStatic
  -- literals
typecheck _ (EDouble x) = Right $ MostlyStatic (TDouble, Num x)
typecheck _ (EString x) = Right $ MostlyStatic (TString, Str x)
typecheck env (EPrim x) = maybe err Right $ lookup x env
  where
    err = Left $ "unknown primitive " ++ x
typecheck env (EApp e1 e2) = case (typecheck env e1, typecheck env e2) of
    (Right e1, Right e2) -> typechecka e1 e2
    (Left err, Right _)  -> Left err
    (Right _,  Left err) -> Left err
    (Left e1,  Left e2)  -> Left (e1 ++ " and " ++ e2)

-- typecheck the application
typechecka (MostlyStatic ((TFun ta tb),e1)) (MostlyStatic (t2,e2)) =
    typechecka' (eqt ta t2) tb e1 e2
  where
    typechecka' :: Maybe (EQ ta t2) -> Typ tb -> Term (ta->tb) -> Term t2 -> Either String MostlyStatic
    typechecka' (Just Refl) tb e1 e2 = Right $ MostlyStatic (tb,App e1 e2)
    typechecka' _ tb e1 e2 =
        Left $ unwords ["incompatible type of the application:", show (TFun ta tb), "and", show t2]

typechecka (MostlyStatic (t1,e1)) _ =
    Left $ "Trying to apply not-a-function: " ++ show t1

-- tests
te1 = EApp (EPrim "inc") (EDouble 10.0)
te2 = EApp (EDouble 10.0) (EPrim "inc")
te3 = EApp (EApp (EPrim "+")
             (EApp (EPrim "inc") (EDouble 10.0)))
           (EApp (EPrim "inc") (EDouble 20.0))

te4 = EApp (EPrim "rev") te3
te5 = EApp (EPrim "rev") (EApp (EPrim "show") te3)

-- typecheck-and-eval
teval :: Exp  -> String
teval exp = either (terror) (ev) (typecheck env0 exp)
  where
    terror err = "Type error: " ++ err
    ev (MostlyStatic (t,e)) = "type " ++ show t ++ ", value " ++
        (tryshow (eqt t TString) (eqt t TDouble) (eval e))
    tryshow :: Maybe (EQ t String) -> Maybe (EQ t Double) -> t -> String
    tryshow (Just Refl) _ t = t
    tryshow _ (Just Refl) t = show t
    tryshow _ _ _ = "<fun>"