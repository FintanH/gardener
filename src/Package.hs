{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Package where

import  Control.Applicative ((<|>))
import  Data.Aeson
import  Data.Aeson.Types (Parser)
import  Data.Either (rights)
import  Data.Text (Text)
import  GHC.Generics (Generic)
import  Data.String (IsString)

newtype PackageName = PackageName Text
  deriving (Show)

newtype ModuleName = ModuleName Text
  deriving (Show)

newtype Identifier = Identifier Text
  deriving (Show)

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
  , weedType :: [WeedType]
  }

data WeedType
  = RedundantDependency [PackageName]
  | NotCompiled [ModuleName]
  | UnusedImport ModuleName [Identifier]
  | ExportedWeed ModuleName [Identifier]
  deriving (Show)


instance FromJSON WeedType where
  parseJSON = parseRedundantDependency

parseRedundantDependency :: Value -> Parser WeedType
parseRedundantDependency =
  withObject "parse redundant dependency" $ \o -> do
    dependencies <-
          (fmap pure $ o .: "depends" :: Parser [Text])
      <|> (o .: "depends" :: Parser [Text])
    pure . RedundantDependency $ map PackageName dependencies

parseNotCompiled :: Value -> Parser WeedType
parseNotCompiled =
  withObject "parse module not compiled" $ \o -> do
    modules <-
          (fmap pure $ o .: "module" :: Parser [Text])
      <|> (o .: "module" :: Parser [Text])
    pure . NotCompiled $ map ModuleName modules

parseUnunsedImport :: Value -> Parser WeedType
parseUnunsedImport =
  withObject "parse unused import" $ \o -> do
    m <- o .: "module"
    n <- headP . rights <$> traverse parseModuleName m
    i <- headP . rights <$> traverse parseModuleIdentifier m
    pure $ UnusedImport n i
  where
    headP :: [a] -> Parser a
    headP [] = fail "There was no head value"
    headP (x:_) = pure x

    parseModuleName =
      withObject "parse unused import module name" (.: "name")

    parseModuleIdentifier =
      withObject "parse module not compiled" $ \o -> do
        identifiers <-
              (fmap pure $ o .: "identifier" :: Parser [Text])
          <|> (o .: "identifier" :: Parser [Text])
        pure $ map Identifier identifiers
