{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeApplications  #-}

module Gardener.PackageSpec (spec) where

import Data.ByteString (ByteString)
import Data.Yaml (decodeEither)
import Gardener.Package
import Test.Hspec
import Text.RawString.QQ

spec :: Spec
spec =
  describe "Tests parsing of WeedTypes" $ do
    it "parses redundant dependencies no list" $
      let r = decodeEither redundantDependencyNoList
      in r `shouldBe` (Right (RedundantDependency [PackageName "async"]))

    it "parses redundant dependencies list" $
      let r = decodeEither redundantDependencyList
          deps = [PackageName "async", PackageName "bytestring"]
      in r `shouldBe` (Right (RedundantDependency deps))

    it "parses modules not compiled no list" $
      let r = decodeEither notCompiledNoList
      in r `shouldBe` (Right (NotCompiled [ModuleName "Package"]))

    it "parses modules not compiled list" $
      let r = decodeEither notCompiledList
          ps = [ModuleName "Package", ModuleName "Package.Foo"]
      in r `shouldBe` (Right (NotCompiled ps))

    it "parses unused imports no list" $
      let r = decodeEither unusedImportNoList
          name = ModuleName "Package"
      in r `shouldBe` (Right (UnusedImport name [Identifier "Data.Text"]))

    it "parses unused imports list" $
      let r = decodeEither unusedImportList
          name = ModuleName "Package"
          idents = [Identifier "Data.Text", Identifier "Data.ByteString"]
      in r `shouldBe` (Right (UnusedImport name idents))

    it "parses exported weeds no list" $
      let r = decodeEither exportedWeedsNoList
          name = ModuleName "Package"
      in r `shouldBe` (Right (ExportedWeed name [Identifier "foo"]))

    it "parses exported weeds list" $
      let r = decodeEither exportedWeedsList
          name = ModuleName "Package"
          idents = [Identifier "foo", Identifier "bar"]
      in r `shouldBe` (Right (ExportedWeed name idents))

    it "parses a weed with name" $
      let r = decodeEither @Weed redundantDependencyWeed
          name = "Redundant build-depends entry"
          deps = [PackageName "async", PackageName "bytestring"]
          weed = RedundantDependency deps
      in r `shouldBe` (Right (Weed name weed))

   -- it "parses a section" $

section :: ByteString
section = [r|
section:
  - name: test:hspec
  - message:
    - name: Redundant build-depends entry
    - depends:
      - async
      - bytestring
  - message:
    - name: Weeds exported
    - module:
      - name: Package
      - identifier: foo
|]

redundantDependencyWeed :: ByteString
redundantDependencyWeed = [r|
name: Redundant build-depends entry
depends:
  - async
  - bytestring
|]

redundantDependencyNoList :: ByteString
redundantDependencyNoList = [r| 
depends: async 
|]

redundantDependencyList :: ByteString
redundantDependencyList = [r|
depends:
  - async
  - bytestring
|]

notCompiledNoList :: ByteString
notCompiledNoList = [r|
module: Package
|]

notCompiledList :: ByteString
notCompiledList = [r|
module:
  - Package
  - Package.Foo
|]

unusedImportNoList :: ByteString
unusedImportNoList = [r|
module:
  - name: Package
  - identifier: Data.Text
|]

unusedImportList :: ByteString
unusedImportList = [r|
module:
  - name: Package
  - identifier:
    - Data.Text
    - Data.ByteString
|]

exportedWeedsNoList :: ByteString
exportedWeedsNoList = [r|
module:
  - name: Package
  - identifier: foo
|]

exportedWeedsList :: ByteString
exportedWeedsList = [r|
module:
  - name: Package
  - identifier:
    - foo
    - bar
|]
