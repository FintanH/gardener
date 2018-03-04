{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Gardener.Package where

import           Control.Applicative ((<|>))
import           Control.Lens ((^?))
import           Data.Aeson.Lens (nth)
import           Data.Foldable (asum)
import qualified Data.Yaml as Y
import           Data.Yaml (FromJSON(..), (.:), Value, Parser)
import           Data.Either (rights)
import qualified Data.HashMap.Strict as HM
import           Data.Text (Text)
import           GHC.Generics (Generic)
import           Data.String (IsString)

import Debug.Trace

newtype PackageName = PackageName Text
  deriving (Show, Eq)

newtype ModuleName = ModuleName Text
  deriving (Show, Eq)

newtype Identifier = Identifier Text
  deriving (Show, Eq)

data Package = Package
  { packageName :: Text
  , packageSections :: [Section]
  }

data Section = Section
  { sectionName :: Text
  , sectionWeeds :: [Weed]
  }

data Weed = Weed
  { weedName :: Text
  , weedType :: WeedType
  }
  deriving (Show, Eq)

data WeedType
  = RedundantDependency [PackageName]
  | NotCompiled [ModuleName]
  | UnusedImport ModuleName [Identifier]
  | ExportedWeed ModuleName [Identifier]
  deriving (Show, Eq)

instance FromJSON Section where
  parseJSON =
    Y.withObject "parse section" $ \o ->
      Section <$> o .: "name" <*> (parseJSON =<< o .: "message")

instance FromJSON Weed where
  parseJSON =
    Y.withObject "parse weed" $ \o ->
      Weed <$> o .: "name" <*> parseJSON (Y.Object o)

instance FromJSON WeedType where
  parseJSON v =
    asum [ parseRedundantDependency v
         , parseNotCompiled v
         , parseUnusedImport v
         , parseExportedWeed v
         ] 
    where
      parseSome :: FromJSON a => String -> Text -> Value -> Parser [a]
      parseSome message key =
        Y.withObject message $ \o ->
          o .: key <|> (pure <$> o .: key)

      parseModule :: String -> Value -> Parser (ModuleName, [Identifier])
      parseModule message =
        Y.withObject message $ \o -> do
          m <- o .: "module"
          name <- fromMaybe (fail "name field missing") $ m ^? nth 1 >>= (.: "name")
          identifiers <- parseSome "parse module identifiers" "identifier" (Y.Object m)
          pure (ModuleName name, map Identifier identifiers)

      parseRedundantDependency o = do
        depends <- parseSome "parse redundant dependency" "depends" o
        pure . RedundantDependency $ map PackageName depends

      parseNotCompiled o = do
        modules <- parseSome "parse module not compiled" "module" o
        pure . NotCompiled $ map ModuleName modules

      parseUnusedImport o = do
        (name, identifiers) <- parseModule "parse unused imports" o
        pure $ UnusedImport name identifiers

      parseExportedWeed o = do
        (name, identifiers) <- parseModule "parse weeds exported" o
        pure $ ExportedWeed name identifiers
