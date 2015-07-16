import Config (databasePath)
import Database.Persist
import Database.Persist.Sqlite
import Database.Tables
import Control.Monad.Trans.Reader (ReaderT)
import Data.Char (toUpper)
import qualified Data.Text as T
import Control.Monad.IO.Class (liftIO, MonadIO)

main :: IO ()
main = do
    s <- readFile "2015-07-15.txt"
    let f = lines s
        g = map (info . words) f
    --print g
    runSqlite databasePath $ do
        mapM_ updateNums g

info :: [String] -> (String, String, String, Int, Int, Int)
info strs =
    let code = strs !! 1
        nums = drop (length (last strs) - 20) (last strs)
        sec = (reverse strs) !! 2
        parsec = parsec
        cap = read (take 4 nums) :: Int
        enrol = read (drop 4 $ take 8 nums) :: Int
        wait = read (drop 10 nums) :: Int
    in
        (take (length code - 1) code, [last code], head sec : drop 3 sec, cap, enrol, wait)

updateNums :: MonadIO m => (String, String, String, Int, Int, Int) -> ReaderT SqlBackend m ()
updateNums (code, session, sec, cap, enrol, wait) =
    updateWhere [LecturesCode ==. T.pack code,
                 LecturesSection ==. T.pack sec,
                 LecturesSession ==. T.pack (map toUpper session)]
                [LecturesCapacity =. cap,
                 LecturesEnrolled =. enrol,
                 LecturesWaitlist =. wait]