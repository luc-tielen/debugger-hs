module Debugger.Internal.Statement
  ( Line
  , Location(..)
  , Var
  , Expr
  , Id(..)
  , Selection(..)
  , Statement(..)
  , Script
  ) where

import Data.Text (Text)


type Var = Text  -- TODO different type?

type Expr = Text  -- TODO different type?

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
  | Print Expr
  | Set Var Expr
  deriving (Eq, Show)

type Script = [Statement]
  {-
type ShellCommand = Text

data Statement
  = Break Location -- hbreak? conditional breakpoints?
  | Call Expr
  | Printf Text [Expr]
  | Shell ShellCommand
  | If Expr [Statement]
  -- TODO target, source, info b, set logging on (opts), ...
-}
