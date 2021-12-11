module Debugger.Render
  ( renderIO
  , renderScript
  ) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Debugger.Internal.Statement


renderIO :: Script -> FilePath -> IO ()
renderIO script path =
  let txt = renderScript script
   in TIO.writeFile path txt

renderScript :: Script -> T.Text
renderScript script = T.unlines $ map render script

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

