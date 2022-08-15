-- | Data types and functions for dealing with world geometry

module FunHack.Geometry
    (
        -- * Coordinates and distances
        Coord,
        Distance,

        -- * Points
        Point2D (..),
        Point3D (..),

        -- * Rectangles
        Rectangle (origin, width, height),

        -- ** Creating rectangles
        makeRectangle,
        makeRectangleFromCornerPoints,

        -- ** Rectangle properties
        nullRectangle,
        rectanglesIntersect,
        containsRectangle,

        -- ** Rectangle operations
        rectangleIntersection,
        splitRectangleAround
    ) where

import Data.Int (Int64)
import Data.Maybe (catMaybes)

-- | A numeric coordinate (may be positive or negative)
type Coord = Int64

-- | A distance between two coordinate (may be positive or negative)
type Distance = Int64

-- | A two-dimensional point
data Point2D = Point2D {
    -- | The X coordinate
    x :: Coord,

    -- | The Y coordinate
    y :: Coord
    }
    deriving stock (Eq, Show)

-- | A three-dimensional point
data Point3D = Point3D {
    -- | The X coordinate
    x :: Coord,

    -- | The Y coordinate
    y :: Coord,

    -- | The Z coordinate
    z :: Coord
    }
    deriving stock (Eq, Show)

-- | A two-dimensional aligned rectangle. A rectangle has an origin point in
-- three-dimensional space, and width and height in two-dimensional space.
--
-- Rectangles are created with the `makeRectangle`function.
data Rectangle = Rectangle {
    -- | The origin point
    origin :: Point3D,

    -- | Width of the rectangle
    width :: Distance,

    -- | Height of the rectangle
    height ::Distance
    }
    deriving stock (Eq, Show)

-- | Make a rectangle from a point and dimensions. If dimensions are negative,
-- the point and dimensions are adjusted such that the dimensions will be
-- positive.
makeRectangle
    :: Point3D -- ^ Origin of the rectangle
    -> Distance -- ^ Width of the rectangle
    -> Distance -- ^Height of the rectangle
    -> Rectangle
makeRectangle orig width height
    = let (x, width') = normalize orig.x width
          (y, height') = normalize orig.y height
      in Rectangle {
             origin = Point3D x y orig.z,
             width = width',
             height = height'
         }
    where
      -- | Normalize a coordinate and distance such that the distance is
      -- positive, possibly adjusting the coordinate accordingly.
      normalize :: Coord -> Distance -> (Coord, Distance)
      normalize coord dist
          | dist < 0 = (coord + dist, abs dist)
          | otherwise = (coord, dist)

-- | Make a new rectangle from given two points. The points need to represent
-- opposite corners of the rectangle, but it does not matter which corners
-- they otherwise are. Their order is also free.
--
-- If the points are equal, a rectangle of size 1 is returned.
--
-- If the points are not on the same Z plane, Nothing is returned.
makeRectangleFromCornerPoints
    :: Point3D -- ^ One corner of a rectangle
    -> Point3D -- ^ The opposite corner of the rectangle
    -> Maybe Rectangle -- ^ The resulting rectangle, if it is possible to create it
makeRectangleFromCornerPoints a b
    | a.z /= b.z = Nothing
    | otherwise
    = let x1 = min a.x b.x
          y1 = min a.y b.y
          x2 = max a.x b.x
          y2 = max a.y b.y
          width = x2 - x1 + 1
          height = y2 - y1 + 1
      in Just $! Rectangle {
             origin = Point3D x1 y1 a.z,
             width = width,
             height = height
         }

-- | Determine if a rectangle has no area.
nullRectangle :: Rectangle -> Bool
nullRectangle rect
    | rect.width == 0 || rect.height == 0 = True
    | otherwise = False

-- | Determine if two rectangles intersect. A rectangle that has no area is
-- considered to intersect with other rectangles (and itself). Rectangles do not intersect if their Z coordinate is not equal.
rectanglesIntersect :: Rectangle -> Rectangle -> Bool
rectanglesIntersect a b
    = (a.origin.z == b.origin.z)
      && (not (isOnLeft a b || isOnLeft b a))
      && (not (isAbove a b || isAbove b a))
  where
    -- | Determine if the first rectangle is completely on the left isde of
    -- the second rectangle.
    isOnLeft :: Rectangle -> Rectangle -> Bool
    isOnLeft a' b' = a'.origin.x + a.width < b'.origin.x

    -- | Determine if the first rectangle is completely above the second rectangle.
    isAbove :: Rectangle -> Rectangle -> Bool
    isAbove a' b' = a'.origin.y + a.height < b'.origin.y

-- | Determine if the first rectangle completely contains the second one.
containsRectangle
    :: Rectangle -- ^ The possible super rectangle
    -> Rectangle -- ^ The possible subrectangle
    -> Bool
containsRectangle super sub
    = let superLeft = super.origin.x
          superTop = super.origin.y
          superRight = superLeft + super.width
          superBottom = superTop + super.height

          subLeft = sub.origin.x
          subTop = sub.origin.y
          subRight = subLeft + sub.width
          subBottom = subTop + sub.height
      in super.origin.z == sub.origin.z
      && superLeft <= subLeft
      && superTop <= subTop
      && superRight >= subRight
      && superBottom >= subBottom

-- | Return an intersection of two rectangles. If the rectangles do not overlap, Nothing is returned.
rectangleIntersection :: Rectangle -> Rectangle -> Maybe Rectangle
rectangleIntersection a b
    | a.origin.z /= b.origin.z = Nothing
    | otherwise
    = let x1 = max a.origin.x b.origin.x
          y1 = max a.origin.y b.origin.y
          x2 = min (a.origin.x + a.width) (b.origin.x + b.width)
          y2 = min (a.origin.y + a.height) (b.origin.y + b.height)
          width = x2 - x1
          height = y2 - y1
      in if width < 0 || height < 0
      then Nothing
      else Just $! Rectangle {
               origin = Point3D x1 y1 a.origin.z,
               width = width,
               height = height
           }

-- | Split a super rectangle into subrectangles that surround a given
-- subrectangle contained completely within the super rectangle.
--
-- If the subrectangle is in the middle of the super rectangle and does not
-- touch any of the super's edges, this creates eight new subrectangles which
-- represent all the space on the top left, top middle, top right, middle
-- left, middle right, bottom left, bottom middle and bottom right sides of
-- the subrectangle.
--
-- The number of generated new rectangles will be smaller if the subrectangle
-- touches some of the super's edges. In other words this function does not
-- return nullary rectangles.
--
-- The subrectangle must not extend outisde the super's edges. If it does, Nothing is returned.
splitRectangleAround
    :: Rectangle -- ^ The super rectangle that is to be splitted
    -> Rectangle -- ^ A sub rectangle that will be "extracted out" of the super rectangle
    -> Maybe [Rectangle] -- ^ New subrectangles as a list embedded in Maybe
splitRectangleAround super sub
    | containsRectangle super sub = Nothing
    | otherwise = Just $! catMaybes $! rectanglesAround
  where
    -- | List of all possible rectangles around the subrectangle.
    rectanglesAround :: [Maybe Rectangle]
    rectanglesAround
        = let superLeft = super.origin.x
              superTop = super.origin.y
              superRight = superLeft + super.width
              superBottom = superTop + super.height

              subLeft = sub.origin.x
              subTop = sub.origin.y
              subRight = subLeft + sub.width
              subBottom = subTop + sub.height
      in [ -- Top row
           rectFromCoords superLeft superTop (subLeft - 1) (subTop - 1),
           rectFromCoords subLeft superTop subRight (subTop - 1),
           rectFromCoords (subRight + 1) superTop superRight (subTop - 1),

           -- Middle sides
           rectFromCoords superLeft subTop (subLeft - 1) subBottom,
           rectFromCoords (subRight + 1) subTop superRight subBottom,

           -- Bottom row
           rectFromCoords superLeft (subBottom + 1) (subLeft - 1) superBottom,
           rectFromCoords subLeft (subBottom + 1) subRight superBottom,
           rectFromCoords (subRight + 1) (subBottom + 1) superRight superBottom
         ]

    -- | Create a rectangle from top left and bottom right coordinates. If the
    -- rectangle is nullary or negative in size, return Nothing.
    rectFromCoords :: Coord -> Coord -> Coord -> Coord -> Maybe Rectangle
    rectFromCoords x1 y1 x2 y2
        = let width = x2 - x1 + 1
              height = y2 - y1 + 1
        in if width <= 0 || height <= 0
           then Nothing
           else Just $! Rectangle {
                    origin = Point3D x1 y1 super.origin.z,
                    width = width,
                    height = height
                }
