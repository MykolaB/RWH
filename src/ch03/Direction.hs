import Data.List

data Direction = LeftTurn | RightTurn | Straight
    deriving (Show, Eq)

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
angle v1 v2 = (-atan2 (determinant v1 v2) (dotProduct v1 v2)) * 180 / pi

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

xVector = Vector (Point 1 0)

angleWithX :: Point -> Point -> Double
angleWithX p1 p2 = angle (vectorFromPoint p1 p2) (xVector)

orderPointsByAngleWithBase :: Point -> [Point] -> [Point]
orderPointsByAngleWithBase basePoint points = sortBy sortByAngle points
    where
        sortByAngle p1 p2 = compare (angleWithX basePoint p1) (angleWithX basePoint p2)

testPoints = [Point 1 1, Point 2 5.1, Point (-2) 2, Point 10 11]

grahamScan_ :: [Point] -> [Point]
grahamScan_ points =
    case points of
    [x1, x2] -> [x2]
    (x1 : x2 : x3 : xs) ->
        if (getDirection x1 x2 x3 == LeftTurn)
        then x2 : grahamScan_ (x2 : x3 : xs)
        else grahamScan_ (x1 : x3 : xs)


grahamScan :: [Point] -> [Point]
grahamScan points =
    let basePoint = findPointWithMinY points
        sortedPoints = tail (orderPointsByAngleWithBase basePoint points)
    in basePoint : (head sortedPoints : (grahamScan_ sortedPoints))