module Constants where

import System.Directory

generateInputPath = canonicalizePath "../../raw/TweetRecent.js"
generateOutputDirectory = canonicalizePath "../../archive/"
atName = "RadishHarmers"