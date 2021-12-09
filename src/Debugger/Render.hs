module Debugger.Render
  ( renderIO
  , render
  ) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Debugger.Internal.Statement


renderIO :: Statement -> FilePath -> IO ()
renderIO stmt path =
  let txt = render stmt
   in TIO.writeFile path txt

render :: Statement -> T.Text
render = \case
  Break loc -> "break " <> renderLoc loc
  Command bp stmts ->
    T.unlines [ "command " <> renderId bp
              -- TODO: handle nested indents?
              , T.unlines $ map (indent . render) stmts
              , "end"
              ]
  Continue -> "continue"
  Print val -> "print " <> val

indent :: T.Text -> T.Text
indent = ("  " <>)

renderId :: Id -> T.Text
renderId (Id txt) = txt

renderLoc :: Location -> T.Text
renderLoc = \case
  Function func -> func
  File path line -> T.pack path <> ":" <> T.pack (show line)
