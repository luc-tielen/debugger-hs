-- NOTE: meant to be imported qualified
module Debugger.Builder
  ( Builder
  , runBuilder
  , break
  , command
  , continue
  , run
  , reset
  , print
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

newtype Builder a
  = Builder (State BuilderState a)
  deriving (Functor, Applicative, Monad, MonadState BuilderState)
  via State BuilderState


runBuilder :: Builder a -> [Statement]
runBuilder = runBuilder' 0

runBuilder' :: Counter -> Builder a -> [Statement]
runBuilder' counter (Builder m) =
  let result = execState m (BuilderState DList.empty counter)
   in DList.apply (stmts result) []

break :: Location -> Builder Id
break loc = do
  emit $ Break loc
  var <- newBreakpointVar
  emit $ Set var "$bpnum"
  pure $ Id var

command :: Id -> Builder () -> Builder ()
command bp cmds = do
  counter <- gets varCounter
  let statements = runBuilder' counter cmds
  emit $ Command bp statements

continue :: Builder ()
continue = emit Continue

run :: Builder ()
run = emit Run

reset :: Builder ()
reset = emit Reset

print :: Expr -> Builder ()
print = emit . Print

-- TODO: add set statement

emit :: Statement -> Builder ()
emit stmt =
  modify $ \s -> s { stmts = DList.snoc (stmts s) stmt }

newBreakpointVar :: Builder Var
newBreakpointVar = do
  currentId <- gets varCounter
  modify $ \s -> s { varCounter = varCounter s + 1 }
  pure $ "$var" <> T.pack (show currentId)

