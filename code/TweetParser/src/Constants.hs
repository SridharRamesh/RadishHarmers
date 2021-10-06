module Constants where

import System.Directory

generateInputPath = canonicalizePath "../../raw/FullTweetArchive.txt"
generateOutputDirectory = canonicalizePath "../../archive/"
atName = "RadishHarmers"