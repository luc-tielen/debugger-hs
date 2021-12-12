module Debugger.Internal.Statement
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

-- A unique ID for a breakpoint.
newtype Id = Id Text
  deriving (Eq, Show)

type Line = Int

-- Selection of 1 or more (breakpoints)
data Selection
  = Single Id
  | Many [Id]
  | All
  deriving (Eq, Show)

type Port = Int

-- TODO: add other variants
data TargetConfig
  = Remote Port  -- assumes tcp as protocol, and localhost as host for now
  deriving (Eq, Show)

data InfoOptions
  = Breakpoints
  deriving (Eq, Show)

-- A place to set a breakpoint at.
data Location
  = Function Text
  | File FilePath Line
  deriving (Eq, Show)

-- Main AST data type.
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

type Script = [Statement]
  {-
data Statement
  = Break Location -- hbreak? conditional breakpoints?
  | Printf Text [Expr]
  | If Expr [Statement]
  -- TODO info b, set logging on (opts), ...
-}
