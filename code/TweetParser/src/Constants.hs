module Constants where

import System.Directory

generateInputPath = canonicalizePath "../../raw/FullTweetArchive.txt"
generateOutputDirectory = canonicalizePath "../../archive/"


-- Personal constants
displayName = "Sridhar Ramesh"
atName = "RadishHarmers"

-- Frequently reconfigured constants
printForDate year month day = year == 2020
-- printForDate year month day = True