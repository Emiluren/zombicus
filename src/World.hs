module World (hitsHole, hitsObstacle, renderHoles) where

import qualified SDL
import qualified SDL.Primitive as SDL
import SDL (($=))
import SDL.Vect (V2(..), V4(..))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad (forM_, when)

import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as Vector
import Data.Int (Int16)

debugmode :: Bool
debugmode = False

holes :: Num a => [([a], [a])]
holes =
    [ ( [116, 134, 190, 248, 337, 245, 185]
      , [208, 129, 121, 79, 128, 172, 231]
      )
    , ( [203, 250, 342, 455, 515, 467, 286]
      , [376, 337, 369, 350, 401, 438, 425]
      )
    , ( [387, 371, 414, 503, 438, 412]
      , [200, 256, 308, 287, 215, 181]
      )
    , ( [558, 536, 612, 603]
      , [124, 191, 228, 155]
      )
    ]

type Polygon = [V2 Double]
type Edge = (V2 Double, V2 Double)
data Line = Vertical Double | Sloped Double Double

raycastingPolys :: [Polygon]
raycastingPolys = uncurry (zipWith V2) <$> holes

polyEdges :: Polygon -> [Edge]
polyEdges (v:vs) = zip (v:vs) (vs ++ [v])

findLine :: Edge -> Line
findLine (V2 x1 y1, V2 x2 y2) =
    if x1 == x2 then
        Vertical x1
    else
        let slope = (y2 - y1) / (x2 - x1)
            yint = y1 - slope * x1
        in Sloped slope yint

inorder :: Double -> Double -> Double -> Bool
inorder x1 x2 x3 = x1 <= x2 && x2 <= x3

rayFrom :: V2 Double -> Edge -> Bool
rayFrom (V2 x0 y) edge@(V2 ax _, V2 bx _) =
    case findLine edge of
        Vertical x1 ->
            x1 == x0
        Sloped slope yint ->
            let y' = yint + x0 * slope
            in y' > y && (inorder ax x0 bx || inorder bx x0 ax)

inPolygon :: V2 Double -> Polygon -> Bool
inPolygon point poly =
    let edges = polyEdges poly
        intersectCount = length $ filter (== True) $ rayFrom point <$> edges
    in odd intersectCount

hitsHole :: V2 Double -> Bool
hitsHole p = any (inPolygon p) raycastingPolys

hitsObstacle :: V2 Double -> Bool
hitsObstacle p@(V2 x y) = x < 0 || x > 747 || y < 0 || y > 504 || hitsHole p

sdlPolys :: [(Vector Int16, Vector Int16)]
sdlPolys = mkPoly <$> holes
    where mkPoly :: ([Int16], [Int16]) -> (Vector Int16, Vector Int16)
          mkPoly (xs, ys) = (Vector.fromList xs, Vector.fromList ys)

debugDrawLine :: MonadIO m => SDL.Renderer -> Line -> m ()
debugDrawLine renderer line = do
    SDL.rendererDrawColor renderer $= V4 255 0 255 255
    case line of
        Vertical x ->
            let x' = floor x
            in SDL.drawLine renderer (SDL.P $ V2 x' 0) (SDL.P $ V2 x' 500)
        Sloped slope yint ->
            let y0 = floor yint
                y1 = floor $ slope * 700 + yint
            in SDL.drawLine renderer (SDL.P $ V2 0 $ y0) (SDL.P $ V2 700 $ y1)

debugDrawHoles :: MonadIO m => SDL.Renderer -> m ()
debugDrawHoles renderer = do
    forM_ (V2 <$> [0,4..700] <*> [0,4..500]) $ \p -> do
        if hitsHole (fromIntegral <$> p) then
            SDL.rendererDrawColor renderer $= V4 255 0 0 255
        else
            SDL.rendererDrawColor renderer $= V4 0 255 0 255
        SDL.drawPoint renderer $ SDL.P p

    forM_ (concatMap polyEdges raycastingPolys) $ \edge@(p1, p2) -> do
        debugDrawLine renderer (findLine edge)

        SDL.rendererDrawColor renderer $= V4 255 0 0 255
        SDL.drawLine renderer (SDL.P $ floor <$> p1) (SDL.P $ floor <$> p2)

renderHoles :: MonadIO m => SDL.Renderer -> SDL.Color -> m ()
renderHoles renderer color = do
    mapM_ renderPolygon sdlPolys
    when debugmode $ debugDrawHoles renderer
    where renderPolygon (xs, ys) = SDL.fillPolygon renderer xs ys color
