type Pos = (Int, Int)
type Path = [Pos]

getPaths :: [Pos] -> Path -> [Path]
getPaths ps path = [ (pos : path) | pos <- ps]

getPaths' :: Path -> [Pos] -> [Path]
getPaths' path = foldl f []
  where
    f :: [Path] -> Pos -> [Path]
    f paths p = (p : path) : paths