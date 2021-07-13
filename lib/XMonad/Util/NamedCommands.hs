{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DefaultSignatures         #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}


{-# OPTIONS_GHC -Wno-orphans -Wno-redundant-constraints #-}
------------------------------------------------------------------------------
-- |
-- Module      : XMonad.Util.NamedCommands
-- Description : Named commands
-- Copyright   : (c) Samuli Thomasson, 2019
-- License     : BSD-3
--
-- Maintainer  : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability   : unstable
-- Portability : unportable
--
-- Creating (single commands): (?), cmdT, SendMessage
--
-- Combining: (:??), (:>>)
--
-- Inspecting: describe, describeType, getAction
--
-- Prompting: cmdPrompt
------------------------------------------------------------------------------

module XMonad.Util.NamedCommands (
  IsCmd(..), SomeCmd,
  cmdT, cmdT1, msgT,
  (?), (.?), (:?)(..), (:??), (:>>)((:>>)),
  SendMessage(SendMessage), -- TODO depcrecat
  -- * Generics
  enumConstrs,
  -- * Prompt Utilities
  CmdPrompt(..), cmdPrompt, mkCmdPrompt,
  -- re-exports
  Data, typeOf
  ) where

import           XMonad
import qualified XMonad.Prompt                       as XP
import           XMonad.Util.NamedActions           (NamedAction)
import qualified XMonad.Util.NamedActions           as NA

import           Prelude
import           Control.Applicative
import           Data.Data
import           Data.Kind
import           Data.Proxy
import           Text.Printf (PrintfArg, printf)
import           GHC.TypeLits

-- * Command Decoration

-- ** Class

class IsCmd a where
  -- | Execute command
  command :: a -> NamedAction
  default command :: (Show a, Message a) => a -> NamedAction
  command = NA.sendMessage'

  -- | Description string derived from type. Default: "typeRep" ("Typeable" instance required).
  describeType :: a -> String
  default describeType :: Typeable a => a -> String
  describeType = show . typeOf

  -- | Description string derived from value.
  describe :: a -> String
  describe = unwords . NA.showName . command

  getAction :: a -> X ()
  getAction = NA.getAction . command

  -- | Value enumeration. Useful for generating values in bindings, options in prompts, etc..
  -- Default: "enumConstrs" (instance "Data" required).
  cmdEnum :: a -> [a]
  default cmdEnum :: Data a => a -> [a]
  cmdEnum _ = enumConstrs

  -- | Existential wrapper
  toCmd :: a -> SomeCmd
  toCmd = SomeCmd

-- ** Existential (SomeCmd)

-- | Extistential for "IsCmd" class.
data SomeCmd = forall a. IsCmd a => SomeCmd a

instance Show SomeCmd where
    show (SomeCmd cmd) = describe cmd

instance IsCmd SomeCmd where
    command      (SomeCmd x) = command x
    describeType (SomeCmd x) = describeType x
    cmdEnum      (SomeCmd x) = SomeCmd <$> cmdEnum x
    toCmd                    = id

-- ** NamedActions integration

instance IsCmd NamedAction where
  command        = id
  describeType _ = show (typeRep (Proxy :: Proxy (X ())))
  cmdEnum _      = []

-- | Assign name to arbitrary X action.
--
-- @termLauncher ? "Launch my terminal"@
--
-- @flip NA.addName@
(?) :: X () -> String -> NamedAction
(?) = flip NA.addName

-- | For building a @NamedAction@ from some @a@ by two functions:
-- A @a -> X ()@ to give the action, and @a -> String@ to give a name.
--
-- @liftA2 (?)@
(.?) :: (a -> X ()) -> (a -> String) -> a -> NamedAction
(.?) = liftA2 (?)

infixr 0 ?
infixr 2 .?

-- * Type-level Command Annotations

data (name :: Symbol) :? (action :: Type) where
  -- | Command with type-level string description. Similar to (?).
  CmdX :: action -> name :? action
  -- | Parameterized command. similar to (.?).
  CmdParamX :: (p -> action) -> (p -> String) -> p -> name :? (p -> action)

instance forall name. (KnownSymbol name) => IsCmd (name :? X ()) where
  command (CmdX a) = a ? symbolVal (Proxy :: Proxy name)
  describeType _ = symbolVal (Proxy :: Proxy name)
  cmdEnum _ = []

instance forall name p. (KnownSymbol name, Typeable p) => IsCmd (name :? (p -> X ())) where
  -- TODO should be illegal
  -- command (CmdX a) =
  command (CmdParamX f g p) = f p ? printf (symbolVal (Proxy :: Proxy name)) (g p)
  describeType _ = (symbolVal (Proxy :: Proxy name))
  cmdEnum _ = []

-- ** Smart constructors

-- | cmdT action
-- @
-- cmdT @"Command description" (withWindowSet ..)
-- @
cmdT :: forall (name :: Symbol) action. (KnownSymbol name, IsCmd (name :? action)) => action -> name :? action
cmdT = CmdX

-- | @
-- myCmd  = cmdT1 @"Command with param <%s>" (\param -> io (print param)) :: String -> "..." :? (String -> X ())
-- myCmd' = myCmd' "foo"
-- @
cmdT1 :: forall name action p. (KnownSymbol name, IsCmd (name :? action)) => (p -> action) -> (p -> String) -> p -> name :? (p -> action)
cmdT1 = CmdParamX

-- | The ""sendmessage"" function.
msgT :: (Message msg, Show msg) => msg -> NamedAction
msgT msg = (NA.sendMessage' msg)

-- * TODO

data SendMessage msg = SendMessage !msg deriving Show

instance (Show a, Message a, Data a) => IsCmd (SendMessage a) where
  command (SendMessage a) = NA.sendMessage' a
  cmdEnum _ = SendMessage <$> enumConstrs


-- | Combined commands (sequential)
data c :>> d = c :>> d
instance (IsCmd c, IsCmd d) => IsCmd (c :>> d) where
  command (c :>> d) = let a = command c
                          b = command d
                       in command $ NA.NamedAction (NA.getAction a >> NA.getAction b, [unwords $ NA.showName a ++ [">>"] ++ NA.showName b])
  describeType (c :>> d) = describeType c <> ", " <> describeType d
  cmdEnum _ = [] -- TODO


-- | Named command group (list). @ type LayoutBSPCmd = "BSP" :?? '[BSP.Rotate, BSP.Swap, BSP.ResizeDirectional, BSP.TreeRotate, BSP.TreeBalance, BSP.FocusParent, BSP.SelectMoveNode] @
data (name :: Symbol) :?? (commands :: [Type]) = CmdL SomeCmd
instance Show (s :?? cl) where
  show (CmdL s) = show s
instance forall name. KnownSymbol name => IsCmd (name :?? '[]) where
  command      _ = NA.noName (return ())
  describeType _ = symbolVal (Proxy :: Proxy name)
  cmdEnum      _ = []
instance forall name x xs. (KnownSymbol name, IsCmd x, IsCmd (name :?? xs)) => IsCmd (name :?? (x ': xs)) where
  command (CmdL (SomeCmd x)) = command x
  describeType _ = symbolVal (Proxy :: Proxy name)
  cmdEnum _ = map (CmdL . SomeCmd) (cmdEnum undefined :: [x]) <> map (\(CmdL c) -> CmdL c) (cmdEnum undefined :: [name :?? xs])

-- * Generics

enumConstrs :: forall a. Data a => [a]
enumConstrs = case dataTypeRep (dataTypeOf @a undefined) of
                AlgRep cs -> cs >>= fromConstrM enumConstrs
                _         -> []

-- * Prompt

data CmdPrompt = CmdPrompt String [SomeCmd]

instance XP.XPrompt CmdPrompt where
  showXPrompt (CmdPrompt s _) = printf "%s: " s
  nextCompletion     _ = XP.getNextCompletion
  commandToComplete  _ = id

-- | @ cmdPrompt xpconfig (Proxy @MagnifyMsg) @
cmdPrompt :: forall a. (IsCmd a) => XP.XPConfig -> Proxy a -> NamedAction
cmdPrompt xpc p = mkCmdPrompt prompt xpc ? printf "Prompt (cmd: %s)" (describeType cmd)
  where
    prompt = CmdPrompt nm xs
    cmd    = undefined `asProxyTypeOf` p
    nm     = printf "CMD (%s)" (describeType cmd)
    xs     = SomeCmd <$> cmdEnum cmd

mkCmdPrompt :: CmdPrompt -> XP.XPConfig -> X ()
mkCmdPrompt prompt@(CmdPrompt _title cmds) xpc = XP.mkXPrompt prompt xpc cf gf
  where
    xs   = [(describe x, NA.getAction x) | x <- map (\(SomeCmd x) -> command x) cmds]
    cf s = pure $ filter (XP.searchPredicate xpc s) (map fst xs)
    gf s = sequence_ (lookup s xs)
