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
          _ <- break (Function "func1")
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

  it "can emit run statements" $ do
    let script = run
    runBuilder script `shouldBe` [Run]

  it "can emit reset statements" $ do
    let script = reset
    runBuilder script `shouldBe` [Reset]

  it "can emit delete statements" $ do
    let script1 = do
          bp <- break (Function "main")
          delete bp
        script2 = do
          bp1 <- break (Function "f")
          bp2 <- break (Function "g")
          delete [bp1, bp2]
        script3 = delete All
    runBuilder script1 `shouldBe`
      [ Break (Function "main")
      , Set "$var0" "$bpnum"
      , Delete $ Single (Id "$var0")
      ]
    runBuilder script2 `shouldBe`
      [ Break (Function "f")
      , Set "$var0" "$bpnum"
      , Break (Function "g")
      , Set "$var1" "$bpnum"
      , Delete $ Many [Id "$var0", Id "$var1"]
      ]
    runBuilder script3 `shouldBe` [Delete All]

  it "can emit disable statements" $ do
    let script1 = do
          bp <- break (Function "main")
          disable bp
        script2 = do
          bp1 <- break (Function "f")
          bp2 <- break (Function "g")
          disable [bp1, bp2]
        script3 = disable All
    runBuilder script1 `shouldBe`
      [ Break (Function "main")
      , Set "$var0" "$bpnum"
      , Disable $ Single (Id "$var0")
      ]
    runBuilder script2 `shouldBe`
      [ Break (Function "f")
      , Set "$var0" "$bpnum"
      , Break (Function "g")
      , Set "$var1" "$bpnum"
      , Disable $ Many [Id "$var0", Id "$var1"]
      ]
    runBuilder script3 `shouldBe` [Disable All]

  it "can emit disable statements" $ do
    let script1 = do
          bp <- break (Function "main")
          enable bp
        script2 = do
          bp1 <- break (Function "f")
          bp2 <- break (Function "g")
          enable [bp1, bp2]
        script3 = enable All
    runBuilder script1 `shouldBe`
      [ Break (Function "main")
      , Set "$var0" "$bpnum"
      , Enable $ Single (Id "$var0")
      ]
    runBuilder script2 `shouldBe`
      [ Break (Function "f")
      , Set "$var0" "$bpnum"
      , Break (Function "g")
      , Set "$var1" "$bpnum"
      , Enable $ Many [Id "$var0", Id "$var1"]
      ]
    runBuilder script3 `shouldBe` [Enable All]

  it "can emit print statements" $ do
    let script = print "1"
    runBuilder script `shouldBe` [Print "1"]
