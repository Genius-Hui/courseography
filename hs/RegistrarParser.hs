import Config (databasePath)
import Database.Persist
import Database.Persist.Sqlite
import Database.Tables
import Control.Monad.Trans.Reader (ReaderT)
import Data.Char (toUpper)
import qualified Data.Text as T
import Control.Monad.IO.Class (liftIO, MonadIO)
import qualified Network.HTTP as HTTP
import Network.HTTP.Auth as Auth
import Network.URI (parseURI)
import Data.Maybe (fromJust)
import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.Format
import System.Locale (defaultTimeLocale)
import Data.List (isInfixOf)

getToday :: IO String
getToday = do
    time <- getCurrentTime
    let (year, month, day) = toGregorian $ utctDay time
    return $ formatTime defaultTimeLocale "%F" time

baseUrl :: String
baseUrl = ""

main :: IO ()
main = do
    today <- getToday
    rsp <- HTTP.simpleHTTP (HTTP.getRequest $ baseUrl ++ today ++ ".txt")
    body <- HTTP.getResponseBody rsp
    if "<h1>Not Found</h1>" `isInfixOf` body
    then
        print body
    else
        let f = lines body
            g = map (info . words) f
        in
            runSqlite databasePath $
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