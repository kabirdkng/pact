{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}


-- |
-- Module      :  Pact.Core.IR.Typecheck
-- Copyright   :  (C) 2022 Kadena
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Jose Cardona <jose@kadena.io>
--
-- HM type inference for core IR.
--
module Pact.Core.IR.Typecheck where

import Control.Lens hiding (Level)
import Control.Monad.Reader
import Control.Monad.ST
import Data.IntMap.Strict(IntMap)
import Data.Foldable(traverse_)
import Data.List.NonEmpty(NonEmpty(..))
import Data.STRef
import Data.Text(Text)

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Data.IntMap.Strict as IntMap
import qualified Data.RAList as RAList

import Pact.Core.Builtin
import Pact.Core.Type
import Pact.Core.Names
import qualified Pact.Core.IR.Term as IR
import qualified Pact.Core.Typed.Term as Typed

-- inference based on https://okmij.org/ftp/ML/generalization.html
-- Note: Type inference levels in the types
-- a-la sound-lazy might be worth implementing later on.
-- The eager implementation is simpler to maintain and extend to typeclasses.

-- note, we will debruijnize, so this is purely for
-- Display purposes
type UniqueSupply s = STRef s Unique
type Level = Int

data Tv s
  = Unbound !Text !Unique !Level
  | Bound !Text !Unique
  | Link !(Type (TvRef s))
  deriving Eq

-- Note: TyVar equality
-- is reference equality
newtype TvRef s =
  TvRef (STRef s (Tv s))
  deriving Eq

data TCState s b
  = TCState
  { _tcSupply :: UniqueSupply s
  -- ^ Supply for fresh variables.
  , _tcVarEnv :: IntMap (TypeScheme (TvRef s))
  -- Variable environment for locally bound and top level names
  , _tcBuiltins :: b -> TypeScheme NamedDeBruijn
  -- ^ Builtins map, that uses the enum instance
  , _tcLevel :: STRef s Level
  -- Type Variable "Region"
  }

makeLenses ''TCState

type TCType s = Type (TvRef s)
type TCPred s = Pred (TvRef s)
type TCOName s = OverloadedName (TCPred s)
type IRTerm b i = IR.Term IRName TypeVar b i
type TypedTCTerm s b i = Typed.Term (TCOName s) (TvRef s) (b, [TCType s], [TCPred s]) i
type OBundle b = (b, [Type NamedDeBruijn], [Pred NamedDeBruijn])
type TypedTerm b i =
  Typed.Term (OverloadedName (Pred NamedDeBruijn)) NamedDeBruijn (OBundle b) i

type InferT s b a = ReaderT (TCState s b) (ST s) a

_dbgTypeScheme :: TypeScheme (TvRef s) -> InferT s b (TypeScheme String)
_dbgTypeScheme (TypeScheme tvs preds ty) = do
  tvs' <- traverse rv tvs
  preds' <- traverse _dbgPred preds
  ty' <- _dbgType ty
  pure (TypeScheme tvs' preds' ty')
  where
  rv n = readTvRef n >>= \case
    Unbound u l _ -> pure ("unbound" <> show (u, l))
    Bound u l -> pure ("bound" <> show (u, l))
    Link _ -> pure "linktv"

_dbgPred :: TCPred s -> InferT s b (Pred String)
_dbgPred (Pred i t) = Pred i <$> _dbgType t

_dbgType :: TCType s -> InferT s b (Type String)
_dbgType = \case
  TyVar tv -> readTvRef tv >>= \case
    Unbound u l _ -> pure (TyVar ("unbound" <> show (u, l)))
    Bound u l -> pure (TyVar ("bound" <> show (u, l)))
    Link ty -> _dbgType ty
  TyFun l r -> TyFun <$> _dbgType l <*> _dbgType r
  TyRow r -> TyRow <$> _dbgRow r
  TyTable r -> TyTable <$> _dbgRow r
  TyList t -> TyList <$> _dbgType t
  TyPrim p -> pure (TyPrim p)
  TyCap -> pure TyCap
  TyForall {} -> fail "impredicative"
  where
  _dbgRow = \case
    RowVar tv -> readTvRef tv >>= \case
      Unbound u l l' -> pure (RowVar ("unboundrow" <> show (u, l, l')))
      Bound u l -> pure (RowVar ("boundrow" <> show (u, l)))
      Link ty -> _dbgType ty >>= \case
        TyRow r -> pure r
        _ -> fail "impossible"
    EmptyRow -> pure EmptyRow
    RowTy obj mrv -> do
      obj' <- traverse _dbgType obj
      case mrv of
        Just tv -> readTvRef tv >>= \case
          Unbound u l l' -> pure (RowTy obj' (Just ("unboundrow" <> show (u, l, l'))))
          Bound u l -> pure (RowTy obj' (Just ("boundrow" <> show (u, l))))
          Link ty -> _dbgType ty >>= \case
            TyRow (RowVar v) -> pure (RowTy obj' (Just v))
            TyRow EmptyRow -> pure (RowTy obj' Nothing)
            TyRow (RowTy obj'' mrv') -> pure (RowTy (Map.union obj' obj'') mrv')
            _ -> fail "impossible"
        Nothing -> pure (RowTy obj' Nothing)


enterLevel :: InferT s b ()
enterLevel = do
  lref <- asks _tcLevel
  lift (modifySTRef' lref succ)

leaveLevel :: InferT s b ()
leaveLevel = do
  lref <- asks _tcLevel
  lift (modifySTRef' lref pred)

currentLevel :: InferT s b Level
currentLevel =
  asks _tcLevel >>= lift . readSTRef

readTvRef :: TvRef s -> InferT s b (Tv s)
readTvRef (TvRef tv) = lift (readSTRef tv)

writeTvRef :: TvRef s -> Tv s -> InferT s b ()
writeTvRef (TvRef tv) t = lift (writeSTRef tv t)

newTvRef :: InferT s b (TvRef s)
newTvRef = do
  uref <- asks _tcSupply
  u <- lift (readSTRef uref)
  let tvName = "'a_" <> T.pack (show u)
  l <- currentLevel
  lift (modifySTRef' uref (+ 1))
  TvRef <$> lift (newSTRef (Unbound tvName u l))

newSupplyIx :: InferT s b Unique
newSupplyIx = do
  uref <- asks _tcSupply
  u <- lift (readSTRef uref)
  lift (modifySTRef' uref (+ 1))
  pure u
---------------------------------------------------------------
-- Type class instances,
-- entailment, context reduction.
-- ---------------------------------------------------------------

-- | For some typeclass C,
-- is there an instance of C t?
-- If so, return the qualifiers of the instance.
-- that is, for (C a_1, .., C a_n) => C t
-- byInst (C t) returns Just [C a_1, .., C a_n].
-- Note: if these were user defined, if we decide to extend to this
-- byInst would have to match the type of C (K t) to an instantiated version
-- of the qualified type (C a_1, .., C a_n) => C (K t_1) for type constructors
byInst :: Pred (TvRef s) -> InferT s b (Maybe [Pred (TvRef s)])
byInst (Pred p ty) = case p of
  Eq -> eqInst ty
  Add -> addInst ty
  Num -> numInst ty
  Ord -> ordInst ty
  Show -> showInst ty
  ListLike -> listLikeInst ty
  Fractional -> fractionalInst ty
  WithoutField f -> withoutFieldInst f ty

-- | Instances of Eq:
--
--  instance Eq integer
--  instance Eq decimal
--  instance Eq string
--  instance Eq unit
--  instance Eq bool
--  instance Eq time <- todo
--
--  instance (Eq 'a) => Eq (list 'a)
--  For rows:
--  instance (Eq {l1:t1, .., ln:tn}) where t1..tn are monotypes without type variables.
--
eqInst :: TCType s -> InferT s b (Maybe [Pred (TvRef s)])
eqInst = \case
  TyVar tv -> readTvRef tv >>= \case
    Link ty -> eqInst ty
    _ -> pure Nothing
  -- All prims have an EQ instance
  TyPrim _ -> pure (Just [])
  TyList t -> pure (Just [Pred Eq t])
  TyRow t -> case t of
    EmptyRow -> pure (Just [])
    -- in short, row equality is only defined on closed rows,
    -- without parametric args, for now
    RowTy m Nothing -> do
      fmap concat . sequence <$> traverse eqInst (Map.elems m) >>= \case
        Just [] -> pure (Just [])
        _ -> pure Nothing
    _ -> pure Nothing
  _ -> pure Nothing

-- | Instances of Ord:
--
--  instance Ord integer
--  instance Ord decimal
--  instance Ord string
--  instance Ord unit
--  instance Ord time <- todo
--
--  instance (Ord 'a) => Ord (list 'a)
--  For rows:
--
ordInst :: TCType s -> InferT s b (Maybe [Pred (TvRef s)])
ordInst = \case
  TyVar tv -> readTvRef tv >>= \case
    Link ty -> ordInst ty
    _ -> pure Nothing
  -- All prims have an Ord instance
  TyPrim p -> case p of
    PrimInt -> pure (Just [])
    PrimDecimal -> pure (Just [])
    PrimString -> pure (Just [])
    PrimTime -> pure (Just [])
    PrimUnit -> pure (Just [])
    _ -> pure Nothing
  TyList t -> pure (Just [Pred Ord t])
  _ -> pure Nothing


-- | Instances of Add:
--
--  instance Add integer
--  instance Add decimal
--  instance Add string
--  instance Add (list 'a)
--
--
addInst :: TCType s -> InferT s b (Maybe [Pred (TvRef s)])
addInst = \case
  TyVar tv -> readTvRef tv >>= \case
    Link ty -> addInst ty
    _ -> pure Nothing
  -- All prims have an EQ instance
  TyPrim p -> pure $ case p of
    PrimString -> Just []
    PrimInt -> Just []
    PrimDecimal -> Just []
    _ -> Nothing
  TyList _ -> pure (Just [])
  _ -> pure Nothing

-- | Instances of num:
-- instance Num integer
-- instance Num decimal
numInst :: TCType s -> InferT s b (Maybe [Pred (TvRef s)])
numInst = \case
  TyVar tv -> readTvRef tv >>= \case
    Link ty -> numInst ty
    _ -> pure Nothing
  -- All prims have an EQ instance
  TyPrim p -> pure $ case p of
    PrimInt -> Just []
    PrimDecimal -> Just []
    _ -> Nothing
  _ -> pure Nothing

-- | Instances of fractional:
-- instance Fractional integer
-- instance Fractional decimal
fractionalInst :: TCType s -> InferT s b (Maybe [Pred (TvRef s)])
fractionalInst = \case
  TyVar tv -> readTvRef tv >>= \case
    Link ty -> fractionalInst ty
    _ -> pure Nothing
  -- All prims have an EQ instance
  TyPrim p -> pure $ case p of
    PrimInt -> Just []
    PrimDecimal -> Just []
    _ -> Nothing
  _ -> pure Nothing

listLikeInst :: TCType s -> InferT s b (Maybe [Pred (TvRef s)])
listLikeInst = \case
  TyVar tv -> readTvRef tv >>= \case
    Link ty -> fractionalInst ty
    _ -> pure Nothing
  -- All prims have an EQ instance
  TyPrim p -> pure $ case p of
    PrimInt -> Just []
    PrimDecimal -> Just []
    _ -> Nothing
  _ -> pure Nothing

-- | Compile-time evidence typeclass:
-- (r\f) implies some row varible `r` does not contain the field `f`.
-- A type `{a_1:t1_..a_k:t_k|r} satisfies (r\f)
-- if f is not in contained in {a_1,..,a_n}
withoutFieldInst :: Field -> TCType s -> InferT s b (Maybe [Pred (TvRef s)])
withoutFieldInst f = \case
  TyRow t -> case t of
    RowVar tv -> readTvRef tv >>= \case
      Link ty -> withoutFieldInst f ty
      _ -> pure Nothing
    EmptyRow -> pure (Just [])
    RowTy obj mty -> do
      when (Map.member f obj) $ fail "BOOM has row"
      case mty of
        Nothing -> pure (Just [])
        Just r -> pure $ Just [Pred (WithoutField f) (TyRow (RowVar r))]
  _ -> pure Nothing

-- | Instances of Show:
--
--  instance Show integer
--  instance Show decimal
--  instance Show string
--  instance Show unit
--  instance Show bool
--  instance Show time <- todo
--
--  instance (Show 'a) => Show (list 'a)
--  instance (Show )
--  For rows:
--  instance (Eq {l1:t1, .., ln:tn}) where t1..tn are monotypes without type variables.
--
showInst :: TCType s -> InferT s b (Maybe [Pred (TvRef s)])
showInst = \case
  TyVar tv -> readTvRef tv >>= \case
    Link ty -> showInst ty
    _ -> pure Nothing
  -- All prims have an EQ instance
  TyPrim _p -> pure (Just [])
  TyList t -> pure (Just [Pred Show t])
  _ -> pure Nothing

entail :: [Pred (TvRef s)] -> Pred (TvRef s) -> InferT s b Bool
entail ps p = byInst p >>= \case
  Nothing -> pure False
  Just qs -> and <$> traverse (entail ps) qs

isHnf :: Pred (TvRef s) -> InferT s b Bool
isHnf (Pred c t) = case t of
  TyVar tv -> readTvRef tv >>= \case
    Link ty -> isHnf (Pred c ty)
    _ -> pure True
  TyRow (RowVar tv) -> readTvRef tv >>= \case
    Link ty -> isHnf (Pred c ty)
    _ -> pure True
  TyRow (RowTy obj (Just rv)) | Map.null obj -> readTvRef rv >>= \case
    Link ty -> isHnf (Pred c ty)
    _ -> pure True
  _ -> pure False

toHnf :: Pred (TvRef s) -> InferT s b [Pred (TvRef s)]
toHnf p = isHnf p >>= \case
  True -> pure [p]
  False -> byInst p >>= \case
    Nothing -> fail "context reduction failure"
    Just ps -> toHnfs ps

toHnfs :: [Pred (TvRef s)] -> InferT s b [Pred (TvRef s)]
toHnfs ps = do
  pss <- traverse toHnf ps
  pure (concat pss)

simplify :: [Pred (TvRef s)] -> InferT s b [Pred (TvRef s)]

simplify = loop []
  where
  loop rs [] = pure rs
  loop rs (p:ps) = entail (rs ++ rs) p >>= \cond ->
    if cond then loop rs ps else loop (p:rs) ps

reduce :: [Pred (TvRef s)]-> InferT s b [Pred (TvRef s)]
reduce ps = toHnfs ps >>= simplify

split :: [TCPred s] -> InferT s b ([TCPred s], [TCPred s])
split ps = do
  ps' <- reduce ps
  partition' ([], []) ps'
  where
  liftRes p ma =
    if p then pure p else ma
  partition' (ds, rs) (p@(Pred _ ty) : xs) = do
    cond <- hasUnbound ty
    if cond then partition' (p:ds, rs) xs
    else partition' (ds, p:rs) xs
  partition' (ds, rs) [] =
    pure (reverse ds, reverse rs)
  varUnbound ref = readTvRef ref >>= \case
    Unbound{} -> pure True
    Link ty -> hasUnbound ty
    Bound {} -> pure False
  hasUnbound = \case
    TyVar n -> varUnbound n
    TyPrim _ -> pure False
    TyList t -> hasUnbound t
    TyFun l r -> do
      l' <- hasUnbound l
      liftRes l' (hasUnbound r)
    TyRow r -> case r of
      RowVar rv -> varUnbound rv
      RowTy m o -> do
        p <- hasUnbound' (Map.elems m)
        liftRes p (maybe (pure False) varUnbound o)
      _ -> pure False
    TyCap -> pure False
    -- TCTyCon _ ty -> hasUnbound ty
    _ -> pure False
  hasUnbound' [] = pure False
  hasUnbound' (x:xs) = do
    p' <- hasUnbound x
    liftRes p' (hasUnbound' xs)

---------------------------------------------------------------
-- -- Instantiations
-- ---------------------------------------------------------------

-- | Instantiate a typescheme with bound variables with fresh bound variables
-- Corresponds to the following inference rule
--
-- That is (∀E):
--     P | Γ ⊢ E : ∀a_1..a_n. ρ
--     b_1, ..,  b_n fresh
--     ---------------------------------------
--     P | Γ ⊢ E ~> E_f[b_1,..,b c_n] : ρ
instantiateWithTerm
  :: TypeScheme (TvRef s)
  -> TypedTCTerm s b i
  -> InferT s b (TCType s, TypedTCTerm s b i, [TCPred s])
instantiateWithTerm (TypeScheme ts preds ty) term = do
  nts <- fmap TyVar <$> traverse (const newTvRef) ts
  let m = zip ts nts
  preds' <- traverse (instPred m) preds
  ty' <- instBound m ty
  case nts of
    x:xs -> do
      let tyapps = Typed.TyApp term (x:|xs) info
      dvars <- traverse toDVar (removeFieldConstraints preds')
      case dvars of
        p:ps -> pure (ty', Typed.App tyapps (p:|ps) info, preds')
        [] -> pure (ty', tyapps, [])
    [] -> pure (ty', term, [])
  where
  info = term ^. Typed.termInfo
  toDVar p = do
    i <- newSupplyIx
    let n = OverloadedName ("_dict" <> T.pack (show i)) (OBuiltinDict p)
    pure $ Typed.Var n info
  instPred m (Pred n tt) =
    Pred n <$> instBound m tt
  instBound m = \case
    t@(TyVar tv) -> readTvRef tv >>= \case
      Bound{} -> case lookup tv m of
        Just t' -> pure t'
        Nothing -> pure t
      Link lt -> instBound m lt
      _ -> pure t
    TyPrim p -> pure (TyPrim p)
    TyFun l r ->
      TyFun <$> instBound m l <*> instBound m r
    TyRow r -> TyRow <$> instBoundRow m r
    TyList t -> TyList <$> instBound m t
    TyTable r -> TyTable <$> instBoundRow m r
    t -> pure t
  instBoundRow _m EmptyRow = pure EmptyRow
  instBoundRow m t@(RowVar rv) = readTvRef rv >>= \case
    Bound{} -> case lookup rv m of
      Just (TyVar rv') -> pure (RowVar rv')
      _ -> pure t
    Link lt -> instBound m lt >>= \case
      TyRow r' -> pure r'
      TyVar rv' -> pure (RowVar rv')
      _ -> fail "impossible?"
    _ -> pure t
  instBoundRow m t@(RowTy o mrv) = do
    obj <- traverse (instBound m) o
    case mrv of
      Just rv -> readTvRef rv >>= \case
        Bound{} -> case lookup rv m of
          Just (TyVar rv') -> pure (RowTy obj (Just rv'))
          _ -> pure t
        Link lt -> instBound m lt >>= \case
          TyRow (RowTy obj' r) -> pure (RowTy (Map.union obj obj') r)
          TyRow EmptyRow -> pure (RowTy obj Nothing)
          TyRow (RowVar rv') -> pure (RowTy obj (Just rv'))
          TyVar rv' -> pure (RowTy obj (Just rv'))
          _ -> fail "impossible?"
        _ -> pure t
      Nothing -> pure (RowTy obj Nothing)

instantiateImported :: TypeScheme NamedDeBruijn -> InferT s b (TCType s, [TvRef s], [TCPred s])
instantiateImported  = \case
  TypeScheme tvs preds ty -> do
    ntvs <- traverse (const newTvRef) tvs
    let rl = RAList.fromList (reverse ntvs)
    ty' <- inst rl ty
    preds' <- traverse (instPred rl) preds
    pure (ty', ntvs, preds')
  where
  instPred rl (Pred tc ty) =
    Pred tc <$> inst rl ty
  inst rl = \case
    TyVar (NamedDeBruijn i _) -> pure (TyVar (rl RAList.!! i))
    TyPrim p -> pure (TyPrim p)
    TyFun l r -> TyFun <$> inst rl l <*> inst rl r
    TyRow r -> TyRow <$> instRow rl r
    TyList t -> TyList <$> inst rl t
    TyTable t -> TyTable <$> instRow rl t
    TyCap -> pure TyCap
    -- TCTyCon _ _ -> fail "typeclass in arg position? "
    -- Impredicative type might work
    -- If we change unification.
    TyForall _ _ -> fail "unsupported impredicative polymorphism"
  instRow rl (RowVar (NamedDeBruijn i _)) = pure (RowVar (rl RAList.!! i))
  instRow _rl EmptyRow = pure EmptyRow
  instRow rl (RowTy obj mrv) = do
    obj' <- traverse (inst rl) obj
    case mrv of
      Just (NamedDeBruijn i _) -> pure (RowTy obj' (Just (rl RAList.!! i)))
      Nothing -> pure (RowTy obj' Nothing)

occurs :: TvRef s -> TCType s -> InferT s b ()
occurs tv = \case
  TyVar tv' | tv == tv' -> fail "occurs check failed"
  TyVar tv' -> bindRef tv'
  TyFun l r -> occurs tv l *> occurs tv r
  TyRow r -> occursRow r
  TyList l -> occurs tv l
  TyTable r -> occursRow r
  _ -> pure ()
  where
  bindRef tv' = readTvRef tv' >>= \case
    Unbound n u l' -> do
      ml <- minLevel
      writeTvRef tv' (Unbound n u ml)
      where
      minLevel = readTvRef tv >>= \case
        Unbound _ _ l -> pure (min l l')
        _ -> pure l'
    Link ty -> occurs tv ty
    _ -> pure ()
  occursRow (RowVar tv') = do
    when (tv == tv') $ fail "occurs check failed for row variable"
    bindRef tv'
  occursRow EmptyRow = pure ()
  occursRow (RowTy obj mtv) = do
    when (Just tv == mtv) $ fail "occurs check failed for row variable"
    traverse_ (occurs tv) obj
    case mtv of
      Just tv' -> bindRef tv'
      Nothing -> pure ()

unifyTyVar :: TvRef s -> TCType s -> InferT s b ()
unifyTyVar tv t1 = readTvRef tv >>= \case
  Unbound{} -> do
    occurs tv t1
    writeTvRef tv (Link t1)
  Link t2 -> unify t2 t1
  _ -> pure ()

unify :: TCType s -> TCType s -> InferT s b ()
unify t1 t2 | t1 == t2 = pure ()
unify (TyVar tv) t = unifyTyVar tv t
unify t (TyVar tv) = unifyTyVar tv t
unify (TyFun l r) (TyFun l' r') = unify l l' *> unify r r'
unify (TyRow r) (TyRow r') = unifyRow r r'
unify (TyList t) (TyList t') = unify t t'
unify (TyTable r) (TyTable r') = unifyRow r r'
unify (TyPrim p) (TyPrim p') | p == p' = pure ()
unify TyCap TyCap = pure ()
unify _ _ = fail "types do not unify"

unifyRow :: Row (TvRef s) -> Row (TvRef s) -> InferT s b ()
unifyRow (RowVar n) t = unifyTyVar n (TyRow t)
unifyRow t (RowVar n) = unifyTyVar n (TyRow t)
unifyRow EmptyRow EmptyRow = pure ()
unifyRow (RowTy _ (Just rv)) EmptyRow = unifyTyVar rv (TyRow EmptyRow)
unifyRow EmptyRow (RowTy _ (Just rv)) = unifyTyVar rv (TyRow EmptyRow)
unifyRow (RowTy objL lrv) (RowTy objR rrv') =
  case (lrv, rrv') of
    -- Two open rows, we unify them back into open rows
    -- with each other's fields that are missing
    -- with a fresh row variable, which indicates that they are indeed the same row
    (Just tvl, Just tvr) -> do
      traverse_ (uncurry unify) (Map.intersectionWith (,) objL objR)
      if Map.keys objL == Map.keys objR
        then do
          unifyTyVar tvl (TyRow (RowVar tvr))
        else do
          newRv <- newTvRef
          let notInL = Map.difference objR objL
              notInR = Map.difference objL objR
          unifyTyVar tvl (TyRow (RowTy notInL (Just newRv)))
          unifyTyVar tvr (TyRow (RowTy notInR (Just newRv)))
    -- note: the NOTHING branch here means a closed row,
    -- therefore the open row's fields must be a subset.
    (Just tv, Nothing) -> unifyWithClosed tv objL objR
    (Nothing, Just tv) -> unifyWithClosed tv objR objL
    (Nothing, Nothing) -> do
      when (Map.keys objL /= Map.keys objR) $ fail "closed rows do not unify"
      traverse_ (uncurry unify) (Map.intersectionWith (,) objL objR)
    where
    unifyWithClosed tv m m' = do
      when (not $ Map.isSubmapOfBy (\_ _ -> True) m m') $ fail "Closed rows submap check"
      traverse_ (uncurry unify) (Map.intersectionWith (,) m m')
      -- members not in submap
      let diff = Map.difference m' m
      unifyTyVar tv (TyRow (RowTy diff Nothing))
unifyRow _ _ = fail "row unification failed"

removeFieldConstraints :: [Pred tv] -> [Pred tv]
removeFieldConstraints = filter $ \(Pred tc _) -> case tc of
  WithoutField _ -> False
  _ -> True

generalizeWithTerm
  :: TCType s
  -> [TCPred s]
  -> TypedTCTerm s b i
  -> InferT s b (TypeScheme (TvRef s), TypedTCTerm s b i, [TCPred s])
generalizeWithTerm ty pp term = do
  preds <- nubPreds pp
  sts <- lift (newSTRef Set.empty)
  (ftvs, ty') <- gen' sts ty
  (deferred, retained) <- split preds
  retained' <- traverse (genPred sts) retained
  case ftvs of
    [] -> do
      when (retained /= []) $ fail "boom2"
      pure (TypeScheme [] [] ty' , term, deferred)
    (x:xs) -> case removeFieldConstraints retained' of
      y:ys -> do
        let typedArgs = toTyArg <$> (y:|ys)
            tlam = Typed.Lam (fst $ NE.head typedArgs) typedArgs term info
            tfinal = Typed.TyAbs (x :| xs) tlam info
        pure (TypeScheme ftvs retained' ty', tfinal, deferred)
      _ ->  pure (TypeScheme ftvs retained ty', Typed.TyAbs (x:|xs) term info, deferred)
  where
  nubPreds li = nubPreds' li []
  -- we expect
  nubPreds' (p@(Pred tc x) : xs) elems = case x of
    TyVar rv -> readTvRef rv >>= \case
      Link tl -> nubPreds' (Pred tc tl :xs) elems
      _ ->
        if elem p elems
        then nubPreds' xs elems
        else nubPreds' xs (Pred tc x:elems)
    TyRow (RowVar rv) -> readTvRef rv >>= \case
      Link (TyRow r) -> nubPreds' (Pred tc (TyRow r) :xs) elems
      _ ->
        if elem p elems
        then nubPreds' xs elems
        else nubPreds' xs (Pred tc x:elems)
    _ -> nubPreds' xs elems
  nubPreds' [] elems = pure (reverse elems)

  info = term ^. Typed.termInfo
  toTyArg p@(Pred t tytv) =
    let n  = OverloadedName "#dictVar" (OBuiltinDict p)
        nty = tcToRowType t tytv
    in (n, nty)
  genPred sts (Pred t pty) = do
    (o, pty')  <- gen' sts pty
    when (o /= []) $ fail "BOOM1"
    pure (Pred t pty')
  gen' sts (TyVar tv) = readTvRef tv >>= \case
    Unbound n u l -> do
      cl <- currentLevel
      if l > cl then do
        s <- lift (readSTRef sts)
        writeTvRef tv (Bound n u)
        if Set.member u s then pure ([], TyVar tv)
        else lift (writeSTRef sts (Set.insert u s)) *> pure ([tv], TyVar tv)
      else pure ([], TyVar tv)
    Link t' -> gen' sts t'
    Bound _ _ -> pure ([], TyVar tv)
  gen' sts (TyFun l r) = do
    (ftvl, l') <- gen' sts l
    (ftvr, r') <- gen' sts r
    pure (ftvl ++ ftvr,TyFun l' r')
  gen' _ t@TyPrim{} = pure ([], t)
  gen' sts (TyRow r) = over _2 TyRow <$> genRow sts r
  gen' sts (TyList t) = over _2 TyList <$> gen' sts t
  gen' sts (TyTable t) = over _2 TyTable <$> genRow sts t
  gen' _sts TyCap = pure ([], TyCap)
  -- gen' _sts t@TCTyCon{} = pure ([], t)
  gen' _sts t@TyForall{} = pure ([], t)
  genRow _sts EmptyRow = pure ([], EmptyRow)
  genRow sts (RowVar rv) = readTvRef rv >>= \case
    Unbound n u l -> do
      cl <- currentLevel
      if l > cl then do
        s <- lift (readSTRef sts)
        writeTvRef rv (Bound n u)
        if Set.member u s then pure ([], RowVar rv)
        else lift (writeSTRef sts (Set.insert u s)) *> pure ([rv], RowVar rv)
      else pure ([], RowVar rv)
    Link t' -> gen' sts t' >>= \case
      (l, TyRow r) -> pure (l, r)
      _ -> fail "found row variable instantiate to top level type"
    Bound _ _ -> pure ([], RowVar rv)
  genRow sts (RowTy obj mrv) = do
    objTup <- traverse (gen' sts) obj
    let obj' = snd <$> objTup
        ftvs = concat $ fmap fst $ Map.elems objTup
    case mrv of
      Just tv -> readTvRef tv >>= \case
        Unbound n u l -> do
          cl <- currentLevel
          if l > cl then do
            s <- lift (readSTRef sts)
            writeTvRef tv (Bound n u)
            if Set.member u s then pure (ftvs, RowTy obj' mrv)
            else lift (writeSTRef sts (Set.insert u s)) *> pure (ftvs ++ [tv], RowTy obj' mrv)
          else pure ([], RowTy obj' mrv)
        Link t' -> gen' sts t' >>= \case
          (l, TyRow (RowVar v')) -> pure (ftvs ++ l, RowTy obj' (Just v'))
          (l, TyRow EmptyRow) -> pure (ftvs ++ l, RowTy obj' Nothing)
          (l, TyRow (RowTy objr rvr)) -> pure (ftvs ++ l, RowTy (Map.union obj' objr) rvr)
          _ -> fail "Row variable linked to non-row"
        Bound _ _ -> pure (ftvs, RowTy obj' mrv)
      Nothing -> pure (ftvs, RowTy obj' Nothing)

liftTypeVar :: Type TypeVar -> InferT s b (TCType s)
liftTypeVar = \case
  TyVar tyv -> TyVar <$> liftRef tyv
  TyPrim p -> pure (TyPrim p)
  TyFun l r ->
    TyFun <$> liftTypeVar l <*> liftTypeVar r
  TyRow r -> TyRow <$> liftTVRow r
  TyTable r -> TyTable <$> liftTVRow r
  TyList l -> TyList <$> liftTypeVar l
  -- TCTyCon tc t ->
  --   TCTyCon tc <$> liftTypeVar t
  TyCap -> pure TyCap
  TyForall _ _ -> fail "impossible"
  where
  -- BIG TODO: placeholder impl, DEFINITELY
  -- unsound in the presence of type anns (vars that should not be generalized)
  -- would get a level higher than where they were declared.
  -- Type anns need to be indexed @ their declared binder
  liftRef tyv = do
    level <- currentLevel
    let name = _tyVarName tyv
        u = _tyVarUnique tyv
    TvRef <$> lift (newSTRef (Unbound name u level))
  liftTVRow = \case
    EmptyRow -> pure EmptyRow
    RowVar v -> RowVar <$> liftRef v
    RowTy obj mrv -> do
      obj' <- traverse liftTypeVar obj
      mrv' <- traverse liftRef mrv
      pure (RowTy obj' mrv')

toOName :: IRName -> OverloadedName b
toOName (IRName n nk u) =
  OverloadedName n $ case nk of
    IRBound -> OBound u
    IRTopLevel m -> OTopLevel m (error "unimplemented")

-- Todo: bidirectionality
inferTerm :: IRTerm b i -> InferT s b (TCType s, TypedTCTerm s b i, [TCPred s])
inferTerm = \case
  IR.Var n@(IRName _ IRBound u) i -> do
    views tcVarEnv (IntMap.lookup u) >>= \case
      Just ts -> do
        let v' = Typed.Var (toOName n) i
        instantiateWithTerm ts v'
      Nothing -> fail ("unbound variable in term infer" <> show n)
  IR.Var _ _ -> fail "unsupported top level"
  IR.Lam lamname nts e i -> do
    let names = fst <$> nts
    ntys <- fmap TyVar <$> traverse (const newTvRef) names
    let ntysc = TypeScheme [] [] <$> ntys
    -- Todo: bidirectionality
    let m = IntMap.fromList $ NE.toList $ NE.zipWith (\n t ->  (_irUnique n, t)) names ntysc
    (ty, e', preds) <- locally tcVarEnv (IntMap.union m) $ inferTerm e
    let nts' = NE.zipWith mkNameTup names ntys
        rty = foldr TyFun ty ntys
    pure (rty, Typed.Lam (toOName lamname) nts' e' i, preds)
    where
    mkNameTup name ty = (toOName name, ty)
  IR.App e args i -> do
    tv1 <- TyVar <$> newTvRef
    (te, e', pte) <- inferTerm e
    as <- traverse inferTerm args
    let tys = view _1 <$> as
        args' = view _2 <$> as
        preds' = concat (pte : NE.toList (view _3 <$> as))
    unify te (foldr TyFun tv1 tys)
    pure (tv1, Typed.App e' args' i, preds')
  IR.Let n _ e1 e2 i -> do
    enterLevel
    (te1, e1Unqual, pe1) <- inferTerm e1
    leaveLevel
    (ts, e1Qual, deferred) <- generalizeWithTerm te1 pe1 e1Unqual
    let u = _irUnique n
    (te2, e2', rest) <- locally tcVarEnv (IntMap.insert u ts) $ inferTerm e2
    pure (te2, Typed.Let (toOName n) e1Qual e2' i, deferred ++ rest)
  IR.Block nel i -> do
    nelTup <- traverse inferTerm nel
    let nel' = view _2 <$> nelTup
        outTy = view _1 (NE.last nelTup)
        preds' = concat (view _3 <$> nelTup)
    pure (outTy, Typed.Block nel' i, preds')
  IR.Error e i -> do
    tv <- TyVar <$> newTvRef
    pure (tv, Typed.Error e tv i, [])
  -- Todo: Here, convert to dictionary
  IR.Builtin b i -> do
    tyImported <- views tcBuiltins ($ b)
    (ty, tvs, preds) <- instantiateImported tyImported
    let tvs' = TyVar <$> tvs
    let term' = Typed.Builtin (b, tvs', preds) i
    pure (ty, term', preds)
    -- tyAppTerm <- case tvs' of
    --   x:xs -> pure (Typed.TyApp term' (x:|xs) i)
    --   [] -> pure term'
    -- predTerm <- case preds of
    --   x:xs -> do
    --     let args = flip Typed.Var i . OverloadedName "#dictVar" . OBuiltinDict <$> (x :| xs)
    --     pure (Typed.App tyAppTerm args i)
    --   [] -> pure tyAppTerm
    -- pure (ty, predTerm, preds)
  -- TODO: note,
  -- for this to work, we have to have proper bidirectionality working, including scoped type variables working fine
  IR.DynAccess {} ->
    error "todo: Dyn access"
  IR.Constant l i ->
    pure (typeOfLit l, Typed.Constant l i,[])
  -- note: object literals are closed rows.
  IR.ObjectLit obj i -> do
    objTup <- traverse inferTerm obj
    let obj' = view _2 <$> objTup
        objTy = TyRow (RowTy (view _1 <$> objTup) Nothing)
        objPreds = concat (view _3 <$> objTup)
    pure (objTy, Typed.ObjectLit obj' i, objPreds)
  IR.ObjectOp oop i -> case oop of
    ObjectAccess f o -> do
      tv <- TyVar <$> newTvRef
      (objTyp, o', p1) <- inferTerm o
      let ty = objectAccessType f
      (tyInst, _tvs, preds) <- instantiateImported ty
      unify tyInst (TyFun objTyp tv)
      let term' = Typed.ObjectOp (ObjectAccess f o') i
      pure (tv, term', p1 ++ preds)
    ObjectRemove f o -> do
      tv <- TyVar <$> newTvRef
      (objTyp, o', p1) <- inferTerm o
      let ty = objectRemoveType f
      (tyInst, _tvs, preds) <- instantiateImported ty
      unify tyInst (TyFun objTyp tv)
      let term' = Typed.ObjectOp (ObjectRemove f o') i
      pure (tv, term', p1 ++ preds)
    ObjectUpdate f v obj -> do
      tv <- TyVar <$> newTvRef
      (vTyp, v', pv) <- inferTerm v
      (objTyp, o', pobj) <- inferTerm obj
      let ty = objectUpdateType f
      (tyInst, _tvs, preds) <- instantiateImported ty
      unify tyInst (TyFun vTyp (TyFun objTyp tv))
      let term' = Typed.ObjectOp (ObjectUpdate f o' v') i
      pure (tv, term', pv ++ pobj ++ preds)
  IR.ListLit li i -> do
    tv <- TyVar <$> newTvRef
    liTup <- traverse inferTerm li
    let preds = concat (view _3 <$> liTup)
    _ <- traverse (unify tv . view _1) liTup
    pure (TyList tv, Typed.ListLit tv (view _2 <$> liTup) i, preds)


-- -- | Transform types into their debruijn-indexed version
-- -- Essentially: Start at depth 0:
-- --  rename : (Term, Γ, Int) -> IxTerm
-- --  rename (ΛX.e, tyEnv, DEPTH) = Λ. (rename (e, tyEnv[depth/X], DEPTH+1))
-- --  .. other cases are simply renaming recursively and calling `renameType`
-- --  on occurences of Type
-- --
-- --  NOTE: the passed in DEPTH is 1 higher than the highest binder.
-- --
-- --  renameType : (Type, Γ, Int) -> IxType
-- --  renameType (a, env, DEPTH) = DEPTH - env(a) - 1
-- --  .. other recursive cases are straightforward
-- --
-- --  Quip: when we debruijnize types, we expect no impredicative polymorphism atm,
-- --  thus we will fail on universially quantified types found in application and
-- --  var binding sites.
-- --  The typechecker does not spit out impredicative polymorphism, but while
-- --  it would be trivial to support their renaming here, I'd rather fail
-- --  for now as the typechecker does not support it and it functions as a sanity check
debruijnizeTermTypes :: TypedTCTerm s b i -> InferT s b (TypedTerm b i)
debruijnizeTermTypes = dbj [] 0
  where
  dbj :: [(TvRef s, NamedDeBruijn)] -> DeBruijn -> TypedTCTerm s b i -> InferT s b (TypedTerm b i)
  dbj env depth = \case
    Typed.Var n i ->
      Typed.Var <$> dbjName env depth n <*> pure i
    Typed.Lam n nts e i -> do
      n' <- dbjName env depth n
      nts' <- traverse dbjBoth nts
      e' <- dbj env depth e
      pure (Typed.Lam n' nts' e' i)
      where
      dbjBoth (nn, tty) = do
        nn' <- dbjName env depth nn
        tty' <- dbjTyp env depth tty
        pure (nn', tty')
    Typed.App l r i ->
      Typed.App <$> dbj env depth l <*> traverse (dbj env depth) r <*> pure i
    Typed.Let n e1 e2 i -> do
      n' <- dbjName env depth n
      e1' <- dbj env depth e1
      e2' <- dbj env depth e2
      pure (Typed.Let n' e1' e2' i)
    Typed.TyApp e args i -> do
      e' <- dbj env depth e
      args' <- traverse (dbjTyp env depth) args
      pure (Typed.TyApp e' args' i)
    Typed.TyAbs ntys e i -> do
      let len = fromIntegral (NE.length ntys)
          ixs = NE.fromList [depth .. depth + len - 1]
      names <- traverse (nameTvs (depth + len)) (NE.zip ntys ixs)
      let env' = NE.toList $ NE.zip ntys names
      Typed.TyAbs names <$> dbj (env' ++ env) (depth + len) e <*> pure i
    Typed.Block nel i ->
      Typed.Block <$> traverse (dbj env depth) nel <*> pure i
    Typed.ObjectLit obj i ->
      Typed.ObjectLit <$> traverse (dbj env depth) obj <*> pure i
    Typed.ObjectOp oop i -> fmap (`Typed.ObjectOp` i) $ case oop of
      ObjectAccess f o -> do
        o' <- dbj env depth o
        pure (ObjectAccess f o')
      ObjectRemove f o -> do
        o' <- dbj env depth o
        pure (ObjectRemove f o')
      ObjectUpdate f v o -> do
        v' <- dbj env depth v
        o' <- dbj env depth o
        pure (ObjectUpdate f v' o')
    Typed.ListLit ty v i ->
      Typed.ListLit <$> dbjTyp env depth ty <*> traverse (dbj env depth) v <*> pure i
    Typed.Error e t i ->
      Typed.Error e <$> dbjTyp env depth t <*> pure i
    Typed.Builtin (b, tys, preds) i -> do
      tys' <- traverse (dbjTyp env depth) tys
      preds' <- traverse (dbjPred env depth) preds
      pure (Typed.Builtin (b, tys', preds') i)
    Typed.Constant l i -> pure (Typed.Constant l i)

nameTvs :: DeBruijn -> (TvRef s, DeBruijn) -> InferT s b NamedDeBruijn
nameTvs depth (nt, i) = readTvRef nt >>= \case
  Bound n _ -> pure (NamedDeBruijn (depth - i - 1) n)
  _ -> fail "found unbound variable"

dbjName
  :: [(TvRef s, NamedDeBruijn)]
  -> DeBruijn
  -> OverloadedName (TCPred s)
  -> InferT s b (OverloadedName (Pred NamedDeBruijn))
dbjName env depth (OverloadedName n kind) = fmap (OverloadedName n) $ case kind of
  OBound b -> pure (OBound b)
  OTopLevel m h -> pure (OTopLevel m h)
  OBuiltinDict b -> OBuiltinDict <$> dbjPred env depth b

debruijnizeTypeScheme :: TypeScheme (TvRef s) -> InferT s b (TypeScheme NamedDeBruijn)
debruijnizeTypeScheme (TypeScheme tvs preds t) = do
    let len = fromIntegral (length tvs)
    let ixs = [0.. len - 1]
    names <- traverse (nameTvs len) (zip tvs ixs)
    let env = zip tvs names
    t' <- dbjTyp env len t
    preds' <- traverse (dbjPred env len) preds
    pure (TypeScheme names preds' t')

dbjPred
  :: [(TvRef s, NamedDeBruijn)]
  -> DeBruijn
  -> TCPred s
  -> InferT s b (Pred NamedDeBruijn)
dbjPred env depth (Pred tc ty) =
  Pred tc <$> dbjTyp env depth ty

dbjTyp
  :: [(TvRef s, NamedDeBruijn)]
  -> DeBruijn
  -> TCType s
  -> InferT s b (Type NamedDeBruijn)
dbjTyp env depth = \case
  TyVar n -> case lookup n env of
    Just v -> pure (TyVar v)
    Nothing -> readTvRef n >>= \case
      Unbound {} -> fail "unbound type"
      Bound{} -> pure (TyVar (NamedDeBruijn 9999 "azzzzzzzzzzz"))
      Link ty -> dbjTyp env depth ty
  TyPrim p -> pure (TyPrim p)
  TyFun l r -> TyFun <$> dbjTyp env depth l <*> dbjTyp env depth r
  TyRow r -> TyRow <$> dbjRow env depth r
  TyList l -> TyList <$> dbjTyp env depth l
  TyTable r -> TyTable <$> dbjRow env depth r
  TyCap -> pure TyCap
  _ -> fail "impredicative"

dbjRow
  :: [(TvRef s, NamedDeBruijn)]
  -> DeBruijn
  -> Row (TvRef s)
  -> InferT s b (Row NamedDeBruijn)
dbjRow env depth = \case
  RowVar rv -> case lookup rv env of
    Just v -> pure (RowVar v)
    Nothing ->  readTvRef rv >>= \case
      Unbound {} -> fail "unbound row var"
      Bound{} -> fail "impossible"
      Link ty -> dbjTyp env depth ty >>= \case
        TyRow r -> pure r
        _ -> fail "invalid substitution in sanity check"
  EmptyRow -> pure EmptyRow
  RowTy obj mrv -> do
    obj' <- traverse (dbjTyp env depth) obj
    case mrv of
      Just rv -> case lookup rv env of
        Just v -> pure (RowTy obj' (Just v))
        Nothing -> readTvRef rv >>= \case
          Unbound _ u l -> fail $ "unbound row type" <> show (u, l)
          Bound{} -> fail "impossible"
          Link ty -> dbjTyp env depth ty >>= \case
            TyRow (RowVar rv') -> pure (RowTy obj' (Just rv'))
            TyRow (RowTy obj2 mrv2) -> pure (RowTy (Map.union obj' obj2) mrv2)
            TyRow EmptyRow -> pure (RowTy obj' Nothing)
            _ -> fail "invalid substitution in sanity check"
      Nothing -> pure (RowTy obj' Nothing)

-- -----------------------------------------
-- --- Built-in type wiring
-- ------------------------------------------
-- -- todo: overloading

-- -- todo: debruijnize automatically
rawBuiltinType :: RawBuiltin -> TypeScheme NamedDeBruijn
rawBuiltinType = \case
  -- Add
  RawAdd -> addBinopType
  -- Num
  RawSub ->
    numBinopType
  RawMultiply ->
    numBinopType
  RawDivide ->
    numBinopType
  RawNegate ->
    unaryNumType
  RawAbs ->
    unaryNumType
  -- Bool ops
  RawAnd ->
    binaryBool
  RawOr ->
    binaryBool
  RawNot ->
    TypeScheme [] [] (TyBool :~> TyBool)
  -- Eq ops
  RawEq ->
    eqTyp
  RawNeq ->
    eqTyp
  -- Ord ops
  RawGT ->
    ordTyp
  RawGEQ ->
    ordTyp
  RawLT ->
    ordTyp
  RawLEQ ->
    ordTyp
  -- Integer ops
  RawBitwiseAnd ->
    binaryInt
  RawBitwiseOr ->
    binaryInt
  RawBitwiseXor ->
    binaryInt
  RawBitwiseFlip ->
    unaryInt
  RawMod ->
    binaryInt
  RawBitShift ->
    unaryInt
  -- Rounding ops
  RawRound ->
    roundingFn
  RawCeiling ->
    roundingFn
  RawFloor ->
    roundingFn
  -- Fractional
  RawExp ->
    unaryFractional
  RawLn ->
    unaryFractional
  RawSqrt ->
    unaryFractional
  RawLogBase ->
    let aVar = nd "a" 0
        a = TyVar aVar
    in TypeScheme [aVar] [Pred Fractional a] (a :~> a :~> a)
  -- ListLike
  RawConcat ->
    let aVar = nd "a" 0
        a = TyVar aVar
    in TypeScheme [aVar] [Pred ListLike a] (TyList a :~> a)
  RawTake ->
    takeDropTy
  RawDrop ->
    takeDropTy
  RawLength ->
    let aVar = nd "a" 0
        a = TyVar aVar
    in TypeScheme [aVar] [Pred ListLike a] (a :~> TyInt)
  RawReverse ->
    let aVar = nd "a" 0
        a = TyVar aVar
    in TypeScheme [aVar] [Pred ListLike a] (a :~> a)
  -- General
  RawMap ->
    let aVar = nd "a" 1
        bVar = nd "b" 0
        a = TyVar aVar
        b = TyVar bVar
    in TypeScheme [aVar, bVar] [] ((a :~> b) :~> TyList a :~> TyList b)
  RawFold ->
    let aVar = nd "a" 1
        bVar = nd "b" 0
        a = TyVar aVar
        b = TyVar bVar
    in TypeScheme [aVar, bVar] [] ((a :~> b :~> a) :~> a :~> TyList b :~> a)
  RawFilter ->
    let aVar = nd "a" 0
        a = TyVar aVar
    in TypeScheme [aVar] [] ((a :~> TyBool) :~> TyList a :~> TyList a)
  RawZip ->
    let aVar = nd "a" 2
        a = TyVar aVar
        bVar = nd "b" 1
        b = TyVar bVar
        cVar = nd "c" 0
        c = TyVar cVar
    in TypeScheme [aVar, bVar, cVar] [] ((a :~> b :~> c) :~> TyList a :~> TyList b :~> TyList c)
  RawIf ->
    let aVar = nd "a" 2
        bVar = nd "b" 1
        cVar = nd "c" 0
        a = TyVar aVar
        b = TyVar bVar
        c = TyVar cVar
    in TypeScheme [aVar] [] ((a :~> b :~> c) :~> TyList a :~> TyList b :~> TyList c)
  RawIntToStr ->
    TypeScheme [] [] (TyInt :~> TyString)
  RawStrToInt ->
    TypeScheme  [] [] (TyString :~> TyInt)
  RawDistinct ->
    let aVar  = nd "a" 0
        a = TyVar aVar
    in TypeScheme [aVar] [Pred Eq a] (TyList a :~> TyList a)
  RawEnforce ->
    TypeScheme [] [] (TyBool :~> TyString :~> TyUnit)
  RawEnforceOne -> error "todo"
  RawEnumerate ->
    TypeScheme [] [] (TyInt :~> TyInt :~> TyList TyInt)
  RawEnumerateStepN ->
    TypeScheme [] [] (TyInt :~> TyInt :~> TyInt :~> TyList TyInt)
  RawShow ->
    let aVar = nd "a" 0
        a = TyVar aVar
    in TypeScheme [aVar] [Pred Show a] (a :~> TyString)
  RawDummy ->
    let aVar = nd "a" 0
    in TypeScheme [aVar] [] (TyRow (RowVar aVar) :~> TyRow (RowVar aVar) :~> TyUnit)
  where
  nd b a = NamedDeBruijn a b
  unaryNumType =
    let aVar = nd "a" 0
        a = TyVar aVar
    in TypeScheme [aVar] [Pred Num a] (a :~> a)
  unaryFractional =
    let aVar = nd "a" 0
        a = TyVar aVar
    in TypeScheme [aVar] [Pred Fractional a] (a :~> TyDecimal)
  addBinopType =
    let aVar = nd "a" 0
        a = TyVar aVar
    in TypeScheme [aVar] [Pred Add a] (a :~> a :~> a)
  numBinopType =
    let aVar = nd "a" 0
        a = TyVar aVar
    in TypeScheme [aVar] [Pred Num a] (a :~> a :~> a)
  eqTyp =
    let aVar = nd "a" 0
        a = TyVar aVar
    in TypeScheme [aVar] [Pred Eq a] (a :~> a :~> TyBool)
  ordTyp =
    let aVar = nd "a" 0
        a = TyVar aVar
    in TypeScheme [aVar] [Pred Ord a] (a :~> a :~> TyBool)
  unaryInt = TypeScheme [] [] (TyInt :~> TyInt)
  -- integer -> integer -> integer
  binaryInt = TypeScheme [] [] (TyInt :~> TyInt :~> TyInt)
  -- decimal -> integer
  roundingFn = TypeScheme [] [] (TyDecimal :~> TyInt)
  -- bool -> bool -> bool
  binaryBool = TypeScheme [] [] (TyBool :~> TyBool :~> TyBool)
  -- forall a. ListLike a => int -> a -> a
  takeDropTy =
    let aVar = nd "a" 0
        a = TyVar aVar
    in TypeScheme [aVar] [Pred ListLike a] (TyInt :~> a :~> a)


objectAccessType :: Field -> TypeScheme NamedDeBruijn
objectAccessType f =
  let aVar = NamedDeBruijn 1 "a"
      rVar = NamedDeBruijn 0 "r"
      a = TyVar aVar
      p = Pred (WithoutField f) (TyRow (RowVar rVar))
      rowTy = TyRow (RowTy (Map.singleton f a) (Just rVar))
  in TypeScheme [aVar, rVar] [p] (rowTy :~> a)

objectUpdateType :: Field -> TypeScheme NamedDeBruijn
objectUpdateType f =
  let aVar = NamedDeBruijn 1 "a"
      rVar = NamedDeBruijn 0 "r"
      p = Pred (WithoutField f) (TyRow (RowVar rVar))
      a = TyVar aVar
      rowTy0 = TyRow (RowVar rVar)
      rowTy1 = TyRow (RowTy (Map.singleton f a) (Just rVar))
  in TypeScheme [aVar, rVar] [p] (a :~> rowTy0 :~> rowTy1)

objectRemoveType :: Field -> TypeScheme  NamedDeBruijn
objectRemoveType f =
  let aVar = NamedDeBruijn 1 "a"
      rVar = NamedDeBruijn 0 "r"
      a = TyVar aVar
      p = Pred (WithoutField f) (TyRow (RowVar rVar))
      rowTy0 = TyRow (RowTy (Map.singleton f a) (Just rVar))
      rowTy1 = TyRow (RowVar rVar)
  in TypeScheme [aVar, rVar] [p] (rowTy0 :~> rowTy1)

runInferTerm :: Supply -> (b -> TypeScheme NamedDeBruijn) -> IRTerm b i -> (TypeScheme NamedDeBruijn, TypedTerm b i)
runInferTerm u bfn term0 = runST $ do
  uref <- newSTRef u
  lref <- newSTRef 1
  let tcs = TCState uref mempty bfn lref
  flip runReaderT tcs $ do
    enterLevel
    (ty, term1, preds) <- inferTerm term0
    leaveLevel
    (tys, term2, _deferred) <- generalizeWithTerm ty preds term1
    ts <- debruijnizeTypeScheme tys
    term3 <- debruijnizeTermTypes term2
    pure (ts, term3)