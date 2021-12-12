module Debugger.Statement
  ( Line
  , Location(..)
  , Var
  , Expr
  , ShellCommand
  , Id(..)
  , Selection(..)
  , Port
  , TargetConfig(..)
  , InfoOptions(..)
  , Statement(..)
  , Script
  ) where

import Data.Text (Text)

type Var = Text  -- TODO different type?

type Expr = Text  -- TODO different type?

type ShellCommand = Text

-- | Datatype representing a ID of a breakpoint.
newtype Id = Id Text
  deriving (Eq, Show)

type Line = Int

-- | Helper datatype for a selection of 1 or more (breakpoints)
data Selection
  = Single Id
  | Many [Id]
  | All
  deriving (Eq, Show)

type Port = Int

-- TODO: add other variants
-- | Datatype for configuring the GDB target.
data TargetConfig
  = Remote Port  -- assumes tcp as protocol, and localhost as host for now
  deriving (Eq, Show)

-- | Enumeration of all things info can be requested about.
data InfoOptions
  = Breakpoints
  deriving (Eq, Show)

-- | A place to set a breakpoint at.
data Location
  = Function Text
  | File FilePath Line
  deriving (Eq, Show)

-- | Main AST data type used to build a script with.
data Statement
  = Break Location
  | Command Id [Statement]
  | Continue
  | Step (Maybe Int)
  | Next (Maybe Int)
  | Run
  | Reset
  | Delete Selection
  | Enable Selection
  | Disable Selection
  | Shell ShellCommand
  | Source FilePath
  | Print Expr
  | Set Var Expr
  | Call Expr
  | Target TargetConfig
  | Info InfoOptions
  deriving (Eq, Show)

-- | A script is a collection of statements.
type Script = [Statement]

  {-
TODO
data Statement
  = Break Location -- hbreak? conditional breakpoints?
  | Printf Text [Expr]
  | If Expr [Statement]
  -- set logging on (opts), ...
-}
