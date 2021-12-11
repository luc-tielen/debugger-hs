module Debugger.Internal.Statement
  ( Line
  , Location(..)
  , Var
  , Expr
  , Id(..)
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
  | Run
  | Reset
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
  | Enable Id
  | Disable Id
  | Delete
  | Step
  | Next
  | If Expr [Statement]
  -- TODO target, source, info b, set logging on (opts), ...
-}
