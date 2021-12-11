module Test.Debugger.BuilderSpec
  ( module Test.Debugger.BuilderSpec
  ) where

import Prelude hiding (break, print)
import Data.Foldable
import Test.Hspec
import Debugger.Builder
import Debugger.Internal.Statement


spec :: Spec
spec = describe "script builder" $ parallel $ do
  it "handles empty scripts" $ do
    runBuilder (pure ()) `shouldBe` []

  it "maintains order of statements" $ do
    let script = do
          print "1"
          print "2"
    runBuilder script `shouldBe` [Print "1", Print "2"]

  it "can emit break statements" $ do
    let script loc = do
          break loc
    for_ [Function "main", File "file.c" 42] $ \loc ->
      runBuilder (script loc) `shouldBe` [Break loc, Set "$var0" "$bpnum"]

  it "can emit command statements" $ do
    let script = do
          bp <- break (Function "func1")
          command bp $ do
            print "123"
            print "456"
    runBuilder script `shouldBe`
      [ Break (Function "func1")
      , Set "$var0" "$bpnum"
      , Command (Id "$var0") [Print "123", Print "456"]
      ]

  it "returns correct breakpoint variable for use in commands" $ do
    let script = do
          break (Function "func1")
          bp2 <- break (Function "func2")
          command bp2 $ do
            print "42"
    runBuilder script `shouldBe`
      [ Break (Function "func1")
      , Set "$var0" "$bpnum"
      , Break (Function "func2")
      , Set "$var1" "$bpnum"
      , Command (Id "$var1") [Print "42"]
      ]

  it "can handle nested commands" $ do
    let script = do
          bp1 <- break (Function "func1")
          command bp1 $ do
            bp2 <- break (Function "func2")
            command bp2 $ do
              print "42"
              continue
    runBuilder script `shouldBe`
      [ Break (Function "func1")
      , Set "$var0" "$bpnum"
      , Command (Id "$var0")
        [ Break (Function "func2")
        , Set "$var1" "$bpnum"
        , Command (Id "$var1")
          [ Print "42"
          , Continue
          ]
        ]
      ]

  it "can emit continue statements" $ do
    let script = continue
    runBuilder script `shouldBe` [Continue]

  it "can emit print statements" $ do
    let script = print "1"
    runBuilder script `shouldBe` [Print "1"]
