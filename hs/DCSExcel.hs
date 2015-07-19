{-# LANGUAGE OverloadedStrings #-}
module DCSExcel where

import qualified Codec.Xlsx as Xlsx
import qualified Data.ByteString.Lazy as L
import qualified Data.Map as Map
import Data.Maybe (fromJust, fromMaybe)
import Data.List
import Control.Lens ((^?))
import qualified Data.Text as T
import WebParsing.TimeTableParser (processCourseTable)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Trans.Reader (ReaderT)
import Config (databasePath)
import Database.Persist.Sqlite (runMigration, runSqlite, deleteWhere, 
        PersistValue(..))
import Database.Persist.Types (PersistFilter(..), Filter(..))
import Database.Tables

m :: IO ()
m = do
    bs <- L.readFile "master2.xlsx"
    let xlsx = Xlsx.toXlsx bs
        sheet = xlsx ^? Xlsx.ixSheet "MASTER in progress"
        cells = Map.filterWithKey f $
                Map.map (getVal . Xlsx._cellValue) $
                Xlsx._wsCells $
                fromJust sheet
        table = map (dcsToArtsci . (map T.pack))
                    [[fromMaybe "" (Map.lookup (x,y) cells) | y <- [1..12]]
                     | x <- [3..460]]
        nonEmpties = filter (not . all T.null) table
        courseParts = splitCourses nonEmpties []
    
    runSqlite databasePath $ do
        deleteWhere [Filter LecturesCode (Left $ "CSC%")
                            (BackendSpecificFilter "LIKE")]
        deleteWhere [Filter TutorialsCode (Left $ "CSC%")
                            (BackendSpecificFilter "LIKE")]
        mapM_ processCourseTable courseParts

    where
        f (r,c) _ = c <= 12 && r <= 465 && r >= 3
        getVal (Just (Xlsx.CellText s)) = T.unpack s
        getVal (Just (Xlsx.CellDouble d)) = show d
        getVal (Just (Xlsx.CellBool d)) = show d
        getVal Nothing = ""

        dcsToArtsci (_:
                     code:
                     session:
                     _:
                     section:
                     _:
                     time:
                     _:
                     instructor:
                     _) =
            [code, session, "", T.filter (/= '\"') section, "", time, "", instructor, "", "", ""]
        dcsToArtsci _ = ["","","","","","","","","","","",""]

        splitCourses [] cs = cs
        splitCourses s  cs =
            let (secs, rest) = span (T.null . head) (tail s)
            in
                splitCourses rest (cs ++ [head s:secs])