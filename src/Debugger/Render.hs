module Debugger.Render
  ( renderIO
  , render
  ) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Debugger.Internal.Statement


renderIO :: [Statement] -> FilePath -> IO ()
renderIO stmt path =
  let txt = render stmt
   in TIO.writeFile path txt

render :: [Statement] -> T.Text
render stmts = T.unlines $ map render1 stmts

render1 :: Statement -> T.Text
render1 = \case
  Break loc -> "break " <> renderLoc loc
  Command bp stmts ->
    T.unlines [ "command " <> renderId bp
              -- TODO: handle nested indents?
              , T.unlines $ map (indent . render1) stmts
              , "end"
              ]
  Continue -> "continue"
  Set var expr ->
    T.unwords
      [ "set"
      , var
      , "="
      , expr
      ]
  Print val -> "print " <> val

indent :: T.Text -> T.Text
indent = ("  " <>)

renderId :: Id -> T.Text
renderId (Id txt) = txt

renderLoc :: Location -> T.Text
renderLoc = \case
  Function func -> func
  File path line -> T.pack path <> ":" <> T.pack (show line)

-- $> render [Break (Function "main"),Set "$var0" "$bpnum",Command (Id "$var0") [Print "123"],Continue]
