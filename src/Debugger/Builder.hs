-- NOTE: meant to be imported qualified
module Debugger.Builder
  ( Builder
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
  , target
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
  set var "$bpnum"
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

set :: Var -> Expr -> Builder ()
set var value = emit $ Set var value

call :: Expr -> Builder ()
call = emit . Call

class ToSelection a where
  toSelection :: a -> Selection

instance ToSelection Id where
  toSelection = Single

instance ToSelection [Id] where
  toSelection = Many

instance ToSelection Selection where
  toSelection = id

delete, disable, enable :: ToSelection a => a -> Builder ()
delete = emit . Delete . toSelection
disable = emit . Disable . toSelection
enable = emit . Enable . toSelection

next :: Builder ()
next = emit $ Next Nothing

nextN :: Int -> Builder ()
nextN = emit . Next . Just

step :: Builder ()
step = emit $ Step Nothing

stepN :: Int -> Builder ()
stepN = emit . Step . Just

target :: TargetConfig -> Builder ()
target = emit . Target

emit :: Statement -> Builder ()
emit stmt =
  modify $ \s -> s { stmts = DList.snoc (stmts s) stmt }

newBreakpointVar :: Builder Var
newBreakpointVar = do
  currentId <- gets varCounter
  modify $ \s -> s { varCounter = varCounter s + 1 }
  pure $ "$var" <> T.pack (show currentId)

