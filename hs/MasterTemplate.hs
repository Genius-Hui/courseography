{-# LANGUAGE OverloadedStrings #-}

module MasterTemplate where

import           Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Internal (stringValue)
import MakeElements

masterTemplate :: String -> [H.Html] -> H.Html -> H.Html -> H.Html
masterTemplate title headers body scripts =
    H.html $ do
        H.head $ do
            H.title (H.toHtml title)
            H.meta ! A.httpEquiv "Content-Type"
                   ! A.content "text/html;charset=utf-8"
            sequence_ headers
        H.body $ do
            body
            scripts

-- Insert the header of the Grid and Graph. This contains the year of the timetable, and
-- a link back to the Graph.
header :: String -> H.Html
header page =
    createTag H.nav "" "row header" $ do
        H.h2 ! A.id "courseography-header"
             ! H.customAttribute "context" (stringValue page) $ "Courseography"
        H.ul ! A.id "nav-links" $ do
            H.li $ makeA "" "" "graph" "" "Graph"
            H.li $ makeA "" "" "grid" "" "Grid"
            H.li $ makeA "" "" "timesearch" "" "Search"
            H.li $ makeA "" "" "draw" "" "Draw"
            H.li $ makeA "" "" "post" "" "Check My POSt!"
            H.li $ makeA "" "" "about" "" "About"
            H.li $ makeA "post-fb" "" "#" "" $ "Post to Facebook!"
            H.li ! A.id "facebook-name" $ ""
            H.li ! A.class_ "fb-login-button"
                 ! H.customAttribute "data-max-rows" "1"
                 ! H.customAttribute "data-size" "medium"
                 ! H.customAttribute "autologoutlink" "true"
                 ! H.customAttribute "data-show-faces" "false"
                 ! H.customAttribute "data-auto-logout-link" "false" $ ""

-- Disclaimer. This will be the same for both pages, I guess?
disclaimer :: H.Html
disclaimer =
    H.div ! A.id "disclaimerDiv" $ do
        "DISCLAIMER: Both the "
        H.a ! A.href "http://www.artsandscience.utoronto.ca/ofr/timetable/winter/csc.html"
            $ "Official Timetable"
        " and "
        H.a ! A.href "http://www.artsandscience.utoronto.ca/ofr/calendar/index.html"
            $ "Calendar"
        " take precedence over the information presented here. "
        "It's important that you double-check your course selection, "
        "prerequisites, and your program plans."
