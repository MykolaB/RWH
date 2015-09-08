data Direction = LeftTurn | RightTurn | Straight
    deriving (Show)

data Point = Point {
    x :: Double
   ,y :: Double
} deriving (Eq, Show)

data Vector = Vector {
    coordinate :: Point
} deriving (Eq, Show)

epsilon = 0.001

hiPoint = Point 0 9999999

vectorLength :: Vector -> Double
vectorLength v = sqrt(sqr(x (coordinate v)) + sqr(y (coordinate v)))
                 where sqr x = x * x

dotProduct :: Vector -> Vector -> Double
dotProduct v1 v2 = (x (coordinate v1) * x (coordinate v2) + y (coordinate v1) * y (coordinate v2))

determinant :: Vector -> Vector -> Double
determinant v1 v2 = (x (coordinate v1) * y (coordinate v2) - y (coordinate v1) * x (coordinate v2))

angle :: Vector -> Vector -> Double
angle v1 v2 = (-atan2 ((determinant v1 v2) / (vectorLength v1 * vectorLength v2)) ((dotProduct v1 v2))) / (vectorLength v1 * vectorLength v2) * 180 / pi

vectorFromPoint :: Point -> Point -> Vector
vectorFromPoint basePoint point = Vector (Point(x point - x basePoint) (y point - y basePoint))

getDirection :: Point -> Point -> Point -> Direction
getDirection a b c =
    if (abs (angle v1 v2) < epsilon ) then Straight
    else if (angle v1 v2) > 0 then RightTurn
    else LeftTurn
    where
    v1 = vectorFromPoint a b
    v2 = vectorFromPoint a c

turnSequence :: [Point] -> [Direction]
turnSequence [] = []
turnSequence [x] = []
turnSequence [x, y] = []
turnSequence (x : y : z : xs) = (getDirection x y z) : turnSequence(y : (z : xs))

findPointWithMinY_ :: Point -> [Point] -> Point
findPointWithMinY_ p points
        | points == [] = p
        | y p < y (head points) = findPointWithMinY_ p (tail points)
        | y p >= y (head points) = findPointWithMinY_ (head points) (tail points)

findPointWithMinY :: [Point] -> Point
findPointWithMinY points =
    findPointWithMinY_ hiPoint points

--orderPointsByYDelta :: Point -> [Point] -> [Point]

