{-# LANGUAGE DeriveAnyClass #-}
-- | Data types and functions for dealing with world geometry

module FunHack.Geometry
    (
        -- * Coordinates, distance and directions
        Coord,
        Distance,

        -- * Direction
        Direction (..),
        Direction1D (..),
        directionToSteps,

        -- * Points
        Point (..),

        -- ** Distance calculation
        pointDistance,

        -- ** Finding adjacent points
        pointsAround,
        pointsAroundAligned,
        pointsAroundHorizontal,
        pointsAroundAlignedHorizontal,

        -- ** Point adjustment
        movePointBy,
        movePointTowards,

        -- * Boxes (a.k.a. rectangular cuboids)
        Box (origin, width, height, depth),

        -- ** Creating boxes
        makeBox,
        makeBoxFromCornerPoints,

        -- ** Box's coordinates
        north, east, south, west, top, bottom, centerX, centerY, centerZ,

        -- ** Properties
        boxesIntersect,
        containsBox,
        boxContainsPoint,
        pointsInBox,

        -- ** Box operations
        resizeBox,
        boxIntersection,
        splitBoxOut
    ) where

import Data.Hashable
import Data.Int (Int64)
import GHC.Generics (Generic)
import GHC.Stack

-- | A numeric coordinate (may be positive or negative)
type Coord = Int64

-- | A distance between two coordinate (may be positive or negative)
type Distance = Int64

-- | A direction of movement along a single axis.
--
-- How the value is interpreted depends on the axis: On X axis, decreasing
-- direction is towards West. On Y axis decreasing is towards South. On Z axis
-- decreasing direction is towards the center of the planet.
--
-- This might feel non-intuitive for people used to roguelike games, but will be familiar for those into cartography.
data Direction1D = Desc | None | Asc
    deriving stock (Bounded, Enum, Eq, Ord, Show)

-- | A complete direction value consists of combination of directions on all
-- three axes. The values are ordered in the same order as axes are listed
-- otherwise – X, Y and then Z. So for example `Direction None Asc None` means
-- "Northwards" and `Direction Asc None Desc` means downwards and east.
data Direction = Direction Direction1D Direction1D Direction1D
    deriving stock (Eq, Show)

-- | Convert a relative direction to relative steps along the axes. This
-- function returns a 3-tuple where each value is a relative step on X, Y and
-- Z axis respectively. THe value can be -1, 0 or 1.
directionToSteps :: Direction -> (Distance, Distance, Distance)
directionToSteps (Direction x y z)
    = (toSteps x, toSteps y, toSteps z)
  where
    toSteps :: Direction1D -> Distance
    toSteps Desc = -1
    toSteps None = 0
    toSteps Asc = 1

-- | Point is a three-dimensional location identifier relative to a game world.
data Point = Point {
    -- | The X coordinate
    x :: {-# UNPACK #-} Coord,

    -- | The Y coordinate
    y :: {-# UNPACK #-} Coord,

    -- | The Z coordinate
    z :: {-# UNPACK #-} Coord
    }
    deriving stock (Eq, Generic, Show)
    deriving anyclass Hashable

-- | Calculate the distance between two points using the Pythagorean theorem.
--
-- Note that the result value is a floating point number and not of the
-- `Distance` type. This is so that the distance can be presented exactly.
pointDistance :: Floating a => Point -> Point -> a
pointDistance !a !b
    = let dx = fromIntegral (a.x - b.x)
          dy = fromIntegral (a.y - b.y)
          dz = fromIntegral (a.z - b.z)
      in sqrt $! (dx ^ (2 :: Int)) + (dy ^ (2 :: Int)) + (dz ^ (2 :: Int))

-- | List of horizontal steps including and around a center point, moving clockwise.
horizontalStepsOrdered :: [(Distance, Distance)]
horizontalStepsOrdered = [ (0, 0), (0, 1), (1, 1), (1, 0), (1, -1), (0, -1), (-1, -1), (-1, 0), (-1, 1) ]

-- | Return a list of all points around the center point given as a parameter.
-- This includes points towards all of the directions representable by the
-- 'Direction' type. The result list always contains 26 elements, as the
-- center point is not included.
--
-- The order of directions is clockwise, starting from North. On Z axis horizontal level comes first, then upper, then lower.
pointsAround :: Point -> [Point]
pointsAround !p
    = -- The self point is the first returned one, and will need to be skipped
      tail $! [ Point (p.x + x) (p.y + y) (p.z + z)
              | z <- [ 0, 1, -1 ],
                (x, y) <- horizontalStepsOrdered ]

-- | Return a list of all points around the center point that are aligned along the axes. In other words diagonally reachable points are excluded. The result list always contains six poitns.
--
-- The order of returned points is North, East, South, West, Up, Down.
pointsAroundAligned :: Point -> [Point]
pointsAroundAligned (Point { x, y, z })
    = [ Point x (y - 1) z,
        Point (x + 1) y z,
        Point x (y + 1) z,
        Point (x - 1) y z,
        Point x y (z + 1),
        Point x y (z - 1)
      ]

-- | Return a list of points horizontally around the center point.
--
-- The list will be in order starting from North and moving clockwise.
pointsAroundHorizontal :: Point -> [Point]
pointsAroundHorizontal !p
    = tail $! [ Point (p.x + x) (p.y + y) p.z
              | (x, y) <- horizontalStepsOrdered ]

-- | Return the four points horizontally around the center point and aligned along the X and Y axis.
--
-- The order will be clockwise startig from North.
pointsAroundAlignedHorizontal :: Point -> [Point]
pointsAroundAlignedHorizontal (Point { x,  y,  z })
    = [ Point x (y - 1) z,
        Point (x + 1) y z,
        Point x (y + 1) z,
        Point (x - 1) y z
      ]

-- | Adjust a point's location relatively by steps.
movePointBy
    :: Point -- ^ The point to move
    -> Distance -- ^ Distance on X axis
    -> Distance -- ^ Distance on Y axis
    -> Distance -- ^ Distance on Z axis
    -> Point
movePointBy !p !x !y !z = Point (p.x + x) (p.y + y) (p.z + z)

-- | Adjust a point's location relatively towards a given direction by the given number of steps.
movePointTowards
    :: Point -- ^ The point to adjust
    -> Direction -- ^ The direction towards to adjust
    -> Distance -- ^ The number of steps
    -> Point
movePointTowards !point !dir !steps
    = let (x, y, z) = directionToSteps dir
      in movePointBy point (x * steps) (y * steps) (z * steps)

-- | Box is a rectangular cuboid aligned along the axes. A box has an origin
-- point (the bottom southwestern corner point), and width, height and depth.
--
-- Boxes are created with the 'makeBox' function.
data Box = Box {
    -- | The origin point which is the bottom southwestern point of the box.
    origin :: {-# UNPACK #-} Point,

    -- | Width of the box, eastwards form the origin
    width :: {-# UNPACK #-} Distance,

    -- | Height of the box, northwards from the origin
    height :: {-# UNPACK #-} Distance,

    -- | Depth of the box, upwards from the origin
    depth :: {-# UNPACK #-} Distance
    }
    deriving stock (Eq, Show)

-- | Make a box from a point and dimensions. If dimensions are negative, the
-- point and dimensions are adjusted such that the dimensions will be
-- positive.
--
-- If the volume of the resulting box would be zero (i.e. one of width, height
-- or depth is zero), 'error' will be called with a very unhelpful message.
makeBox
    :: HasCallStack
    => Point -- ^ Origin of the box
    -> Distance -- ^ Width of the box
    -> Distance -- ^ Height of the box
    -> Distance -- ^ Depth of the box
    -> Box
makeBox !orig !width !height !depth
    | width == 0 || height == 0 || depth == 0
    = error "Attempt to create a null box."

    | otherwise
    = let (x, width') = normalize orig.x width
          (y, height') = normalize orig.y height
          (z, depth') = normalize orig.y depth
      in Box {
             origin = Point x y z,
             width = width',
             height = height',
             depth = depth'
         }
    where
      -- Normalize a coordinate and distance such that the distance is
      -- positive, possibly adjusting the coordinate accordingly.
      normalize :: Coord -> Distance -> (Coord, Distance)
      normalize !coord !dist
          | dist < 0 = (coord + dist, abs dist)
          | otherwise = (coord, dist)

-- | Make a new box from given two points. The points need to represent
-- opposite corners of the box, but it does not matter which corners they are.
-- Their order is also free.
--
-- If the points are equal, a box of size 1 is returned.
makeBoxFromCornerPoints
    :: Point -- ^ One corner of a box
    -> Point -- ^ The opposite corner of the box
    -> Box
makeBoxFromCornerPoints !a !b
    = let x1 = min a.x b.x
          y1 = min a.y b.y
          z1 = min a.z b.z
          x2 = max a.x b.x
          y2 = max a.y b.y
          z2 = max a.z b.z
          width = x2 - x1 + 1
          height = y2 - y1 + 1
          depth = z2 - z1 + 1
      in Box {
             origin = Point x1 y1 z1,
             width = width,
             height = height,
             depth = depth
         }

-- | Return the northernmost Y coordinate of a box.
north :: Box -> Coord
north !r = r.origin.y + r.height - 1

-- | Return the easternmost X coordinate of a box.
east :: Box -> Coord
east !r = r.origin.x + r.width - 1

-- | Return the southernmost Y coordinate of a box.
south :: Box -> Coord
south !r = r.origin.y

-- | Return the westernmost X coordinate of a box.
west :: Box -> Coord
west !r = r.origin.x

-- | Return the topmost Z coordinate of a box.
top :: Box -> Coord
top !r = r.origin.z + r.depth - 1

-- | Return the undermost Z coordinate of a box.
bottom :: Box -> Coord
bottom !r = r.origin.z

-- | Return the center X coordinate of a box.
centerX :: Box -> Coord
centerX !r = r.origin.x + (r.width `div` 2)

-- | Return the center Y coordinate of a box.
centerY :: Box -> Coord
centerY !r = r.origin.y + (r.height `div` 2)

-- | Return the center Z coordinate of a box.
centerZ :: Box -> Coord
centerZ !r = r.origin.z + (r.depth `div` 2)

-- | Determine if two boxes intersect.
boxesIntersect :: Box -> Box -> Bool
boxesIntersect !a !b
    = west a <= east b && east a >= west b
      && south a <= north b && north a >= south b
      && bottom a <= top b && top a >= bottom b

-- | Determine if the first box completely contains the second
-- one.
containsBox
    :: Box -- ^ The possible super box
    -> Box -- ^ The possible subbox
    -> Bool
containsBox !super !sub
    = west super <= west sub && east super >= east sub
      && south super <= south sub && north super >= north sub
      && bottom super <= bottom sub && top super >= top sub

-- | Determine if a box contains a specific point.
boxContainsPoint :: Box -> Point -> Bool
boxContainsPoint !r !p
    = west r <= p.x && east r >= p.x
      && south r <= p.y && north r >= p.y
      && bottom r <= p.z && top r >= p.z

-- | Return a list of all points contained within a box. The
-- points are not in a parrticular order.
pointsInBox :: Box -> [Point]
pointsInBox !r
    = [ Point x y z
      | z <- [ bottom r .. top r ],
        y <- [ south r .. north r],
        x <- [ west r .. east r ] ]

-- | Resize a box by a given amount of steps per axis. The new
-- box will have the same center point as the original one. If the
-- resulting box would have volume of zero or less, Nothing is returned.
--
-- The resizing amounts are given as relative deltas from the original size.
resizeBox
    :: Distance -- ^ Amount to resize on X axis
    -> Distance -- ^ Amount to resize on Y axis
    -> Distance -- ^ Amount to resize on Z axis
    -> Box -- ^ The box to resize
    -> Maybe Box -- ^ The possible new box
resizeBox !dx !dy !dz !r
    = let (x, width) = resizeEdge r.origin.x r.width dx
          (y, height) = resizeEdge r.origin.y r.height dy
          (z, depth) = resizeEdge r.origin.z r.depth dz
    in if width <= 0 || height <= 0 || depth <= 0
       then Nothing
       else Just $! Box {
                origin = (Point x y z),
                width = width,
                height = height,
                depth = depth
            }
  where
    -- Resize a box's edge along one axis.
    --
    -- Given a center coordinate, a length of the corresponding edge in the
    -- original box and the difference of the new desired edge length
    -- respective to the original, return the new origin coordinate and the
    -- new edge length.
    resizeEdge :: Coord -> Distance -> Distance -> (Coord, Distance)
    resizeEdge !start !len !delta
        = let len' = len + delta
              adj = (len `div` 2) - (len' `div` 2)
        in (start + adj, len')

-- | Return an intersection of two boxes. If the boxes do not overlap, Nothing is returned.
boxIntersection :: Box -> Box -> Maybe Box
boxIntersection !a !b
    = let x1 = max (west a) (west b)
          y1 = max (south a) (south b)
          z1 = max (bottom a) (bottom b)
          x2 = min (east a) (east b)
          y2 = min (north a) (north b)
          z2 = min (top a) (top b)
          width = x2 - x1 + 1
          height = y2 - y1 + 1
          depth = z2 - z1 + 1
      in if width < 0 || height < 0 || depth < 0
      then Nothing
      else Just $! Box {
               origin = Point x1 y1 z1,
               width = width,
               height = height,
               depth = depth
           }

-- | Split a box into subboxes that surround a given
-- subbox contained completely within the super box.
--
-- If the subbox is in the middle of the super box and does not
-- touch any of the super's edges, this creates 26 new subboxes.
--
-- The number of generated new boxes will be smaller if the
-- subbox touches some of the super's edges.
--
-- The given subbox must not extend outisde the super's edges. If it does,
-- Nothing is returned.
--
-- The result list is generated lazily.
splitBoxOut
    :: Box -- ^ The super box that is to be splitted into parts
    -> Box -- ^ The subbox that is to be "extracted out" of the super
    -> Maybe [Box]
splitBoxOut !super !sub
    | containsBox super sub = Nothing
    | otherwise = Just $! boxesAround
  where
    -- List of all boxes around the subbox
    boxesAround :: [Box]
    boxesAround
        = [ Box (Point x y z) w h d
          | (x, w) <- find west east,
            (y, h) <- find south north,
            (z, d) <- find bottom top,
            not (x == sub.origin.x && y == sub.origin.y && z == sub.origin.z)
          ]

    -- Find all positions and lengths of a possible subbox on one axis
    -- using the given functions
    find :: (Box -> Coord) -> (Box -> Coord) -> [(Coord, Distance)]
    find minim maxim
        = let xs = [ (minim super, (minim sub) - 1),
                     (minim sub, maxim sub),
                     ((maxim sub) + 1, maxim super) ]
          in fmap (\(p1, p2) -> (p1, p2 - p1 + 1))
             . filter (uncurry (<=))
             $ xs
