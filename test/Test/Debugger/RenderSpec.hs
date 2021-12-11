{-# LANGUAGE QuasiQuotes #-}

module Test.Debugger.RenderSpec
  ( module Test.Debugger.RenderSpec
  ) where

import Prelude hiding (break, print)
import qualified Data.Text as T
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

  it "renders run statements" $ do
    run `shouldRenderAs` [text|
      run
      |]

  it "renders reset statements" $ do
    reset `shouldRenderAs` [text|
      monitor reset
      |]

  it "renders delete statements" $ do
    let script1 = do
          bp <- break (Function "main")
          delete bp
        script2 = do
          bp1 <- break (Function "f")
          bp2 <- break (Function "g")
          delete [bp1, bp2]
        script3 = delete All
    script1 `shouldRenderAs` [text|
      break main
      set $$var0 = $$bpnum
      delete $$var0
      |]
    script2 `shouldRenderAs` [text|
      break f
      set $$var0 = $$bpnum
      break g
      set $$var1 = $$bpnum
      delete $$var0 $$var1
      |]
    script3 `shouldRenderAs` [text|
      delete
      |]

  it "renders disable statements" $ do
    let script1 = do
          bp <- break (Function "main")
          disable bp
        script2 = do
          bp1 <- break (Function "f")
          bp2 <- break (Function "g")
          disable [bp1, bp2]
        script3 = disable All
    script1 `shouldRenderAs` [text|
      break main
      set $$var0 = $$bpnum
      disable $$var0
      |]
    script2 `shouldRenderAs` [text|
      break f
      set $$var0 = $$bpnum
      break g
      set $$var1 = $$bpnum
      disable $$var0 $$var1
      |]
    script3 `shouldRenderAs` [text|
      disable
      |]

  it "renders enable statements" $ do
    let script1 = do
          bp <- break (Function "main")
          enable bp
        script2 = do
          bp1 <- break (Function "f")
          bp2 <- break (Function "g")
          enable [bp1, bp2]
        script3 = enable All
    script1 `shouldRenderAs` [text|
      break main
      set $$var0 = $$bpnum
      enable $$var0
      |]
    script2 `shouldRenderAs` [text|
      break f
      set $$var0 = $$bpnum
      break g
      set $$var1 = $$bpnum
      enable $$var0 $$var1
      |]
    script3 `shouldRenderAs` [text|
      enable
      |]

  it "renders print statements" $ do
    print "42" `shouldRenderAs` [text|
      print "42"
      |]
