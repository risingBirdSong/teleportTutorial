{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
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

tpProgDesc :: String
tpProgDesc = "use teleport to setup teleport points and move to these " ++
               "when needed"

tpHeader :: String
tpHeader = "Teleport: move around your filesystem"

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

showHelpOnErrorExecParser :: ParserInfo a -> IO a
showHelpOnErrorExecParser = customExecParser (prefs showHelpOnError)



main :: IO ()
main = do
    -- command :: Command
    command <- showHelpOnErrorExecParser (info (helper <*> parseCommand)
                       (fullDesc  <>
                        progDesc tpProgDesc <>
                        header tpHeader))
    -- run :: IO ()
    run command


parseCommand :: Parser Command
parseCommand = subparser $
    -- add command
    (command
        "add" -- command name
        (info -- attach help information to the parser
            (helper <*> parseAddCommand) -- core parser with the --help option
            (fullDesc <> progDesc "add a teleport point") -- description of command (for info)
        )
    )
    <> -- combine with the next command

    -- list command
    (command "list"
        (info (helper <*> parseListCommand)
        (fullDesc <> progDesc "list all teleport points"))
    ) <>
    -- remove command
    (command "remove"
        (info (helper <*> parseRemoveCommand)
        (fullDesc <>progDesc "remove a teleport point"))
    ) <>
    -- goto command
    (command "goto"
        (info (helper <*> parseGotoCommand)
        (fullDesc <> progDesc "go to a created teleport point"))
    )


parseListCommand :: Parser Command
parseListCommand = pure (CommandList)

parseAddCommand :: Parser Command
parseAddCommand = 
                   (liftA2
                        CommandAdd  -- :: String -> FilePath -> Command
                        tpnameParser -- :: Parser String
                        folderParser -- :: Parser FilePath
                   )

tpnameParser :: Parser String
tpnameParser = argument  -- :: ReadM String -> Mod ArgumentFields String -> Parser String
                  str -- :: ReadM String
                  (metavar -- :: String -> Mod ArgumentFields String
                    "NAME" <>
                  help -- :: String -> Mod ArgumentFields String
                    "name of the teleport point for usage") -- Mod ArgumentFields String

readFolderPath :: String -> ReadM FilePath
readFolderPath s = do
  let path = Path.fromText (T.pack s)
  if Path.valid path
      then return path
      else readerError ("invalid path: " ++ (show path))


folderParser :: Parser FilePath
folderParser = argument
              (str -- :: ReadM String
                >>=
               readFolderPath) -- :: String -> ReadM FilePath
              (value "./"  <>
              metavar "FOLDERPATH" <>
              help ("path of the teleport folder to teleport to." ++ 
                   "By default, taken as current working directory"))


parseRemoveCommand :: Parser Command
parseRemoveCommand = fmap CommandRemove tpnameParser

parseGotoCommand :: Parser Command
parseGotoCommand = fmap CommandGoto tpnameParser

data TpPoint = TpPoint {
    name :: String,
    absFolderPath :: String
} deriving (Show)


instance JSON.FromJSON TpPoint where
     parseJSON (JSON.Object json) =
        liftA2 TpPoint (json .: "name")
                  (json .: "absFolderPath")


instance JSON.ToJSON TpPoint where
    toJSON (TpPoint {..}) =
        JSON.object [ "name" .= name
                     ,"absFolderPath" .= absFolderPath]


-- the main data that is loaded from JSON
data TpData = TpData {
    tpPoints :: [TpPoint]
} deriving (Show)

instance JSON.FromJSON TpData where
    parseJSON (JSON.Object v) =
        fmap TpData (v .: "tpPoints")

instance JSON.ToJSON TpData where
    toJSON(TpData{..}) = 
        JSON.object ["tpPoints" .= tpPoints]


defaultTpData :: TpData
defaultTpData = TpData {
    tpPoints = []
}


filePathToString :: FilePath -> String
filePathToString = Path.encodeString

-- Data Loading
-- """"""""""""

dieJSONParseError :: FilePath -> String -> IO a
dieJSONParseError jsonFilePath err = do
    let errorstr = ("parse error in: " ++ (show jsonFilePath) ++
                    "\nerror:------\n" ++ err)
    Turtle.die (T.pack errorstr)


decodeTpData :: FilePath -> IO TpData
decodeTpData jsonFilePath = do
    rawInput <- B.readFile (filePathToString jsonFilePath)
    let jsonResult = JSON.eitherDecode' rawInput

    case jsonResult of
      Left err -> dieJSONParseError jsonFilePath err
      Right json -> return json


createTpDataFile :: FilePath -> IO ()
createTpDataFile jsonFilePath = saveTpData jsonFilePath defaultTpData

loadTpData :: FilePath -> IO TpData
loadTpData jsonFilePath = do
    exists <- (Turtle.testfile jsonFilePath)
    if exists then
        decodeTpData jsonFilePath
    else
       do
           createTpDataFile jsonFilePath
           return defaultTpData


saveTpData :: FilePath -> TpData -> IO ()
saveTpData jsonFilePath tpData = do
    let dataBytestring = JSON.encode tpData
    Turtle.touch jsonFilePath
    B.writeFile (filePathToString jsonFilePath) dataBytestring


getTpDataPath :: IO FilePath
getTpDataPath = do
    homeFolder <- Turtle.home
    return $ homeFolder </> ".tpdata"

-- set terminal to output error color
setErrorColor :: IO ()
setErrorColor = ANSI.setSGR [-- color to set
                             ANSI.SetColor
                             -- wherther foreground / background should be affected
                             ANSI.Foreground
                             -- use the "vivid" color versus the muted colord
                             ANSI.Vivid 
                             -- use red
                             ANSI.Red
                            ]    



tpPointPrint :: TpPoint -> IO ()
tpPointPrint tpPoint = do
    ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.White]
    putStr (name tpPoint)
    ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Blue]    
    putStr "\t"
    putStr (absFolderPath tpPoint)
    putStr "\n"

-- error out that the given folder is not found
folderNotFoundError :: FilePath -> IO ()
folderNotFoundError path = do
    setErrorColor  
    let errorstr = T.pack ("unable to find folder: " ++ (show path)) 
    Turtle.die errorstr

-- error out that folder is required, but path points
-- to a file
needFolderNotFileError :: FilePath -> IO ()
needFolderNotFileError path = do
    setErrorColor
    let errorstr = T.pack ("expected folder, not file: " ++ (show path)) 
    Turtle.die errorstr

dieIfFolderNotFound :: FilePath -> IO ()
dieIfFolderNotFound path = 
    do
        folderExists <- Turtle.testdir path
        fileExists <- Turtle.testfile path
        -- error checking
        when fileExists (needFolderNotFileError path)
        unless folderExists (folderNotFoundError path)
       -- we know the folder exists

-- error out that the teleport point already exists
dieTpPointExists :: TpPoint -> IO ()
dieTpPointExists tpPoint  =  do
    setErrorColor
    putStrLn ("teleport point " ++ (name tpPoint) ++ " already exists:\n")
    tpPointPrint tpPoint
    Turtle.die ""


runAdd :: FilePath -> String -> IO ()
runAdd folderPath addname = do
    dieIfFolderNotFound folderPath
    tpDataPath <- getTpDataPath
    tpData <- loadTpData tpDataPath
    absFolderPath <- Turtle.realpath folderPath

    let existingTpPoint = find (\tp -> name tp == addname) (tpPoints tpData)
    case existingTpPoint of
        Just tpPoint -> dieTpPointExists tpPoint
        Nothing -> do
                        let newTpPoint = TpPoint {
                            name=addname,
                            absFolderPath=filePathToString absFolderPath
                        }

                        putStrLn "creating teleport point: \n"
                        tpPointPrint newTpPoint

                        let newTpData = TpData {
                             tpPoints= newTpPoint:(tpPoints tpData)   
                        }

                        saveTpData tpDataPath newTpData



runList :: IO ()
runList = do
    tpDataPath <- getTpDataPath
    tpData <- loadTpData tpDataPath
    let num_points = length $ tpPoints tpData
    putStr "teleport points: "

    ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Blue] 
    putStr $ "(total " <> (show num_points) <>  ")\n"
    forM_ (tpPoints tpData) tpPointPrint


dieTpPointNotFound :: String ->IO ()
dieTpPointNotFound name = do
    setErrorColor
    let errorname = T.pack (name ++ " tp point not found")
    Turtle.die errorname

runRemove :: String -> IO ()
runRemove removeName = do
    tpDataPath <- getTpDataPath
    tpData <- loadTpData tpDataPath

    let wantedTpPoint = find (\tp -> name tp == removeName) (tpPoints tpData)
    case wantedTpPoint of
        Nothing -> dieTpPointNotFound removeName
        Just _ ->  do
                    let newTpPoints = filter (\tp -> name tp /= removeName)
                                               (tpPoints tpData)
                    let newTpData = tpData {
                        tpPoints = newTpPoints
                    }

                    saveTpData tpDataPath newTpData
                    ANSI.setSGR [ANSI.SetColor ANSI.Foreground
                                 ANSI.Dull ANSI.White]    
                    putStr "removed teleport point ["
                    ANSI.setSGR [ANSI.SetColor ANSI.Foreground
                                 ANSI.Vivid ANSI.Blue]    
                    putStr removeName
                    ANSI.setSGR [ANSI.SetColor ANSI.Foreground
                                 ANSI.Dull ANSI.White]    
                    putStr "]"


runGoto :: String -> IO ()
runGoto gotoName = do
    tpDataPath <- getTpDataPath
    tpData <- loadTpData tpDataPath

    let wantedTpPoint = find (\tp -> name tp == gotoName) (tpPoints tpData)
    case wantedTpPoint of
        Nothing -> dieTpPointNotFound gotoName
        Just tpPoint -> do
                             Turtle.echo ( case (Turtle.textToLine $ T.pack (absFolderPath tpPoint)) of 
                                                    Just txt -> txt 
                                                    Nothing -> error "oh no!"
                                    )
                             Turtle.exit (Turtle.ExitFailure 2) 


run :: Command -> IO ()
run command = 
    case command of
        CommandAdd{..} -> runAdd folderPath addName
        CommandList -> runList
        CommandRemove{..} -> runRemove removeName
        CommandGoto{..} -> runGoto gotoName

someFunc :: IO ()
someFunc = putStrLn "someFunc"
