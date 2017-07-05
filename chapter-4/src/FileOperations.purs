module FileOperations where

import Prelude
import Data.Array (concatMap, (:))
import Data.Path (Path, ls)

allFiles :: Path -> Array Path
allFiles root = root : concatMap allFiles (ls root)

allFiles' :: Path -> Array Path
allFiles' file = file : do
  child <- ls file
  allFiles' child
