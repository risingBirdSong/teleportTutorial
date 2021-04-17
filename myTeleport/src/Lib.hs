module Lib
  ( someFunc,
  )
where

import Control.Monad
import Data.Aeson ((.:), (.=))
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy as B
import Data.List
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Encoding as T.Encoding
import Data.Traversable
import Filesystem.Path.CurrentOS as Path
import Options.Applicative
import qualified System.Console.ANSI as ANSI
import qualified Turtle
import Prelude hiding (FilePath)

data Command
  = CommandList
  | CommandAdd
      { addName :: String,
        folderPath :: FilePath
      }
  | CommandRemove
      { removeName :: String
      }
  | CommandGoto
      { gotoName :: String
      }
  deriving (Show)

someFunc :: IO ()
someFunc = putStrLn "someFunc"
