{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module HsCabal.HIEBios.Pretty (writeCradleConfig) where

import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.TH
import qualified Data.ByteString as BS
import Data.Char (toLower)
import Data.Maybe (catMaybes)
import Data.String
import qualified Data.Yaml.Pretty as Yaml
import HIE.Bios.Config.YAML
import System.FilePath

instance ToJSON a => ToJSON (OneOrManyComponents a) where
  toJSON NoComponent = Object KM.empty
  toJSON (SingleComponent s) =
    Object
      ( KM.singleton
          "component"
          (String (fromString s))
      )
  toJSON (ManyComponents cs) =
    Object
      ( KM.singleton
          "components"
          (toJSON cs)
      )

deriveToJSON
  ( defaultOptions
      { fieldLabelModifier =
          camelTo2 '-'
            . drop 5
      }
  )
  ''CabalComponent

instance ToJSON CabalConfig where
  toJSON = toJSON . cabalComponents

deriveToJSON
  ( defaultOptions
      { fieldLabelModifier = \f ->
          if f == "stackComponentYAML"
            then "stackYaml"
            else camelTo2 '-' (drop 5 f),
        omitNothingFields = True
      }
  )
  ''StackComponent

instance ToJSON StackConfig where
  toJSON sc =
    case stackYaml sc of
      Just sy ->
        object
          ( ("stackYaml" .= sy)
              : case stackComponents sc of
                SingleComponent s -> ["component" .= s]
                ManyComponents ss -> ["components" .= ss]
                NoComponent -> []
          )
      Nothing -> toJSON (stackComponents sc)

deriveToJSON defaultOptions ''DirectConfig

instance ToJSON BiosConfig where
  toJSON bc =
    object
      ( catMaybes
          [ Just
              ( case callable bc of
                  Program p -> "program" .= p
                  Shell s -> "shell" .= s
              ),
            fmap
              ( \case
                  Program p -> "dependency-program" .= p
                  Shell s -> "dependency-shell" .= s
              )
              (depsCallable bc),
            fmap
              ("with-ghc" .=)
              (ghcPath bc)
          ]
      )

instance ToJSON NoneConfig where
  toJSON _ = Object KM.empty

instance ToJSON a => ToJSON (OtherConfig a) where
  toJSON oc = toJSON (otherConfig oc)

concat
  <$> sequence
    [ deriveToJSON
        (defaultOptions {omitNothingFields = True})
        ''CradleConfigYAML,
      deriveToJSON defaultOptions ''MultiSubComponent,
      deriveToJSON
        ( defaultOptions
            { constructorTagModifier = fmap toLower,
              sumEncoding = ObjectWithSingleField
            }
        )
        ''CradleComponent
    ]

writeCradleConfig :: ToJSON a => FilePath -> CradleConfigYAML a -> IO ()
writeCradleConfig fp cc =
  let dest = fp </> "hie.yaml"
   in putStrLn ("Generated hie-bios config: " ++ dest)
        >> BS.writeFile
          dest
          (Yaml.encodePretty Yaml.defConfig cc)