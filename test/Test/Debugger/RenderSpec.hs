{-# LANGUAGE QuasiQuotes #-}

module Test.Debugger.RenderSpec
  ( module Test.Debugger.RenderSpec
  ) where

import Prelude hiding (break, print)
import qualified Data.Text as T
import Data.Foldable
import Test.Hspec
import NeatInterpolation
import Debugger.Builder
import Debugger.Render
import Debugger.Internal.Statement


-- NOTE: 2 $s is to escape the quasiquoter, renders as 1 $


shouldRenderAs :: Builder a -> T.Text -> IO ()
shouldRenderAs script expected =
  renderScript (runBuilder script) `shouldBe` expected

spec :: Spec
spec = describe "rendering scripts" $ parallel $ do
  it "handles empty scripts" $ do
    pure () `shouldRenderAs` ""

  it "renders break statements" $ do
    break (Function "main") `shouldRenderAs` [text|
      break main
      set $$var0 = $$bpnum
      |]
    break (File "./a/b.c" 42) `shouldRenderAs` [text|
      break ./a/b.c:42
      set $$var0 = $$bpnum
      |]

  it "renders command statements" $ do
    let script = do
          bp <- break (Function "main")
          command bp $ do
            print "42"
            continue
    script `shouldRenderAs` [text|
      break main
      set $$var0 = $$bpnum
      command $$var0
        print "42"
        continue
      end
      |]

  it "renders nested command statements" $ do
    let script = do
          bp1 <- break (Function "main")
          command bp1 $ do
            bp2 <- break (File "file.cpp" 1234)
            command bp2 $ do
              print "42"
              continue
    script `shouldRenderAs` [text|
      break main
      set $$var0 = $$bpnum
      command $$var0
        break file.cpp:1234
        set $$var1 = $$bpnum
        command $$var1
          print "42"
          continue
        end
      end
      |]

  it "renders continue statements" $ do
    continue `shouldRenderAs` [text|
      continue
      |]

  it "renders print statements" $ do
    print "42" `shouldRenderAs` [text|
      print "42"
      |]
