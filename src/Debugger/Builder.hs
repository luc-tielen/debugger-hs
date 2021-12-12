-- NOTE: meant to be imported qualified
module Debugger.Builder
  ( Builder
  , ToSelection(..)
  , runBuilder
  , break
  , command
  , continue
  , step
  , stepN
  , next
  , nextN
  , run
  , reset
  , print
  , set
  , call
  , delete
  , disable
  , enable
  , source
  , shell
  , target
  , info
  ) where

import Prelude hiding (break, print)
import Control.Monad.State.Strict
import qualified Data.Text as T
import qualified Data.DList as DList
import Debugger.Internal.Statement


type Counter = Int

data BuilderState
  = BuilderState
  { stmts :: DList.DList Statement
  , varCounter :: Counter
  }

-- | Builder pattern that allows using monadic do-syntax to build a GDB script.
newtype Builder a
  = Builder (State BuilderState a)
  deriving (Functor, Applicative, Monad, MonadState BuilderState)
  via State BuilderState


-- | Creates a GDB script based on a builder.
runBuilder :: Builder a -> Script
runBuilder = runBuilder' 0

runBuilder' :: Counter -> Builder a -> [Statement]
runBuilder' counter (Builder m) =
  let result = execState m (BuilderState DList.empty counter)
   in DList.apply (stmts result) []

-- | Emits a breakpoint statement.
--   Returns the 'Id' that corresponds with this newly set breakpoint.
break :: Location -> Builder Id
break loc = do
  emit $ Break loc
  var <- newBreakpointVar
  set var "$bpnum"
  pure $ Id var

-- | Emits a command statement, that should be triggered when a breakpoint is triggered.
command :: Id -> Builder () -> Builder ()
command bp cmds = do
  counter <- gets varCounter
  let statements = runBuilder' counter cmds
  emit $ Command bp statements

-- | Emits a continue statement.
continue :: Builder ()
continue = emit Continue

-- | Emits a run statement.
run :: Builder ()
run = emit Run

-- | Emits a reset statement.
reset :: Builder ()
reset = emit Reset

-- | Emits a print statement.
print :: Expr -> Builder ()
print = emit . Print

-- | Emits a set statement.
set :: Var -> Expr -> Builder ()
set var value = emit $ Set var value

-- | Emits a call statement.
call :: Expr -> Builder ()
call = emit . Call

-- | Helper typeclass, used for manipulating 1, many, or all 'Id's.
class ToSelection a where
  toSelection :: a -> Selection

instance ToSelection Id where
  toSelection = Single

instance ToSelection [Id] where
  toSelection = Many

instance ToSelection Selection where
  toSelection = id

-- | Emits a delete statement.
delete :: ToSelection a => a -> Builder ()
delete = emit . Delete . toSelection

-- | Emits a disable statement.
disable :: ToSelection a => a -> Builder ()
disable = emit . Disable . toSelection

-- | Emits an enable statement.
enable = emit . Enable . toSelection
enable :: ToSelection a => a -> Builder ()

-- | Emits a "next" statement. If you want to repeat next N times, use 'nextN'.
next :: Builder ()
next = emit $ Next Nothing

-- | Emits a "next" statement that is repeated N times.
nextN :: Int -> Builder ()
nextN = emit . Next . Just

-- | Emits a step statement. If you want to step N times, use 'stepN'.
step :: Builder ()
step = emit $ Step Nothing

-- | Emits a step statement that is repeated N times.
stepN :: Int -> Builder ()
stepN = emit . Step . Just

-- | Emits a shell statement.
shell :: ShellCommand -> Builder ()
shell = emit . Shell

-- | Emits a source statement.
source :: FilePath -> Builder ()
source = emit . Source

-- | Emits a target statement.
target :: TargetConfig -> Builder ()
target = emit . Target

-- | Emits an info statement.
info :: InfoOptions -> Builder ()
info = emit . Info

emit :: Statement -> Builder ()
emit stmt =
  modify $ \s -> s { stmts = DList.snoc (stmts s) stmt }

newBreakpointVar :: Builder Var
newBreakpointVar = do
  currentId <- gets varCounter
  modify $ \s -> s { varCounter = varCounter s + 1 }
  pure $ "$var" <> T.pack (show currentId)

