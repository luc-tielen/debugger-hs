module Debugger.Render
  ( renderIO
  , renderScript
  ) where

import Control.Monad.Reader
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Debugger.Internal.Statement


renderIO :: Script -> FilePath -> IO ()
renderIO script path =
  let txt = renderScript script
   in TIO.writeFile path txt

renderScript :: Script -> T.Text
renderScript script =
  interleaveNewlines $ map render script

render :: Statement -> T.Text
render stmt = runReader (go stmt) 0
  where
    go = \case
      Break loc ->
        pure $ "break " <> renderLoc loc
      Command bp stmts -> do
        block <- local (+2) $ traverse (indent <=< go) stmts
        -- need to indent the end explicitly, because lines will be joined
        -- together before returning and only start is indented
        end <- indent "end"
        pure $ interleaveNewlines
          [ "command " <> renderId bp
          , interleaveNewlines block
          , end
          ]
      Continue ->
        pure "continue"
      Next count ->
        pure $ "next" <> renderCount count
      Step count ->
        pure $ "step" <> renderCount count
      Run ->
        pure "run"
      Reset ->
        pure "monitor reset"
      Set var expr ->
        pure $ T.unwords ["set", var, "=", expr]
      Call expr ->
        pure $ T.unwords ["call", expr]
      Print val ->
        pure $ "print " <> "\"" <> val <> "\""
      Delete sel ->
        pure $ T.strip $ "delete " <> renderSelection sel
      Disable sel ->
        pure $ T.strip $ "disable " <> renderSelection sel
      Enable sel ->
        pure $ T.strip $ "enable " <> renderSelection sel
      Shell cmd ->
        pure $ "shell " <> cmd
      Source file ->
        pure $ "source " <> T.pack file
      Target target ->
        pure $ "target " <> renderTargetConfig target

    indent txt = do
      spaces <- ask
      let indentation = T.replicate spaces " "
      pure $ indentation <> txt


renderId :: Id -> T.Text
renderId (Id txt) = txt

renderLoc :: Location -> T.Text
renderLoc = \case
  Function func -> func
  File path line -> T.pack path <> ":" <> T.pack (show line)

renderSelection :: Selection -> T.Text
renderSelection = \case
  Single bp -> renderId bp
  Many bps -> T.intercalate " " $ map renderId bps
  All -> ""

renderTargetConfig :: TargetConfig -> T.Text
renderTargetConfig = \case
  Remote port -> "remote tcp:localhost:" <> T.pack (show port)

renderCount :: Maybe Int -> T.Text
renderCount = \case
  Nothing -> ""
  Just x -> " " <> T.pack (show x)

interleaveNewlines :: [T.Text] -> T.Text
interleaveNewlines txts = T.intercalate "\n" txts
