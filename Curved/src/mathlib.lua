-- mathlib.lua

--[[
	Maths extension library for use in Corona SDK by Matthew Webster.
	All work derived from referenced sources.
	Many of these functions are useful for trigonometry and geometry because they have developed for use within graphical user interfaces.
	Much of these are useful when building physics games
	
	twitter: @horacebury
	blog: http://springboardpillow.blogspot.co.uk/2012/04/sample-code.html
	code exchange: http://code.coronalabs.com/search/node/HoraceBury
	github: https://gist.github.com/HoraceBury
]]--
 
--[[
	References:
		http://stackoverflow.com/questions/385305/efficient-maths-algorithm-to-calculate-intersections
		http://stackoverflow.com/questions/4543506/algorithm-for-intersection-of-2-lines
		http://community.topcoder.com/tc?module=Static&d1=tutorials&d2=geometry2#reflection
		http://gmc.yoyogames.com/index.php?showtopic=433577
		http://local.wasp.uwa.edu.au/~pbourke/geometry/
		http://alienryderflex.com/polygon/
		http://alienryderflex.com/polygon_fill/
		http://www.amazon.com/dp/1558607323/?tag=stackoverfl08-20
		http://www.amazon.co.uk/s/ref=nb_sb_noss_1?url=search-alias%3Daps&field-keywords=Real-Time+Collision+Detection
		http://en.wikipedia.org/wiki/Line-line_intersection
		http://developer.coronalabs.com/forum/2010/11/17/math-helper-functions-distancebetween-and-anglebetween
		http://www.mathsisfun.com/algebra/vectors-dot-product.html
		http://www.mathsisfun.com/algebra/vector-calculator.html
		http://lua-users.org/wiki/PointAndComplex
		http://www.math.ntnu.no/~stacey/documents/Codea/Library/Vec3.lua
		http://www.iforce2d.net/forums/viewtopic.php?f=4&t=79&sid=b9ecd62533361594e321de04b3929d4f
		http://rosettacode.org/wiki/Dot_product#Lua
		http://chipmunk-physics.net/forum/viewtopic.php?f=1&t=2215
		http://www.fundza.com/vectors/normalize/index.html
		http://www.mathopenref.com/coordpolygonarea2.html
		http://stackoverflow.com/questions/2705542/returning-the-nearest-multiple-value-of-a-number
		http://members.tripod.com/c_carleton/dotprod.html/
		http://www.1728.org/density.htm
		http://www.wikihow.com/Find-the-Angle-Between-Two-Vectors
		http://stackoverflow.com/questions/563198/how-do-you-detect-where-two-line-segments-intersect
]]--

--[[
	Deprecated functions (see revisions for code):
		rad = convertDegreesToRadians( degrees )
		deg = convertRadiansToDegrees( radians )
		polygonFill( points, closed, perPixel, width, height, col )
]]--

--[[
	Multiplication & Fractions Functions:
		
]]--

--[[
	Point Functions:
		
]]--

--[[
	Angle Functions:
		
]]--

--[[
	Line Functions:
		
]]--

--[[
	Polygon Functions:
		
]]--

--[[
	Point Functions:
		
]]--

--[[
	Fractions
]]--

-- rounds up to the nearest multiple of the number
local function nearest( number, multiple )
	return math.round( (number / multiple) ) * multiple
end
math.nearest = nearest

-- Returns b represented as a fraction of a.
-- Eg: If a is 1000 and b is 900 the returned value is 0.9
-- Often the returned value would be used in a multiplication of another value, usually a distance value.
local function fractionOf( a, b )
        return b / a
end
math.fractionOf = fractionOf

-- Returns b represented as a percentage of a.
-- Eg: If a is 1000 and b is 900 the returned value is 90
-- Use: This is useful in determining how far something should be moved to complete a certain distance.
-- Often the returned value would be used in a division of another value, usually a distance value.
local function percentageOf( a, b )
        return fractionOf(a, b) * 100
end
math.percentageOf = percentageOf

-- return a value clamped between a range
local function clamp( val, low, high )
	if (val < low) then return low end
	if (val > high) then return high end
	return val
end
math.clamp = clamp

--[[
	Angles
]]--

-- rotates point around the centre by degrees
-- rounds the returned coordinates using math.round() if round == true
-- returns new coordinates object
local function rotateAboutPoint( point, degrees, centre )
	local pt = { x=point.x - centre.x, y=point.y - centre.y }
	pt = math.rotateTo( pt, degrees )
	pt.x, pt.y = pt.x + centre.x, pt.y + centre.y
	return pt
end
math.rotateAboutPoint = rotateAboutPoint

-- rotates a point around the (0,0) point by degrees
-- returns new point object
-- center: optional
local function rotateTo( point, degrees, center )
	if (center ~= nil) then
		return rotateAboutPoint( point, degrees, center )
	else
		local x, y = point.x, point.y

		local theta = math.rad( degrees )

		local pt = {
			x = x * math.cos(theta) - y * math.sin(theta),
			y = x * math.sin(theta) + y * math.cos(theta)
		}
		
		return pt
	end
end
math.rotateTo = rotateTo

--[[ Support values for angles ]]--
local PI = (4*math.atan(1))
local quickPI = 180 / PI
math.PI, math.quickPI = PI, quickPI

--[[
	Returns the angle.
	
	Params:
		a : Returns the angle of the point at a relative to (0,0) (east is the virtual base)
		a, b Params: Returns the angle of b relative to a
		a, b, c Params: Returns the angle found at a for between b and c
]]--
local function angleOf( ... )
	local a, b, c = arg[1], arg[2], arg[3]
	
	if (#arg == 1) then
		-- angle of a relative to (0,0)
		return math.atan2( a.y, a.x ) * quickPI -- 180 / PI -- math.pi
	elseif (#arg == 2) then
		-- angle of b relative to a
		return math.atan2( b.y - a.y, b.x - a.x ) * quickPI -- 180 / PI -- math.pi
	elseif (#arg == 3) then
		-- angle between b and c found at a
		local deg = angleOf( a, b ) - angleOf( a, c ) -- target - source
		
		if (deg > 180) then
			deg = deg - 360
		elseif (deg < -180) then
			deg = deg + 360
		end
		
		return deg
	end
	
	-- wrong set of parameters
	return nil
end
math.angleOf = angleOf

-- Brent Sorrentino
-- Returns the angle between the objects
local function angleBetween( srcObj, dstObj )
	local xDist = dstObj.x - srcObj.x
	local yDist = dstObj.y - srcObj.y
	local angleBetween = math.deg( math.atan( yDist / xDist ) )
	if ( srcObj.x < dstObj.x ) then
		angleBetween = angleBetween + 90
	else
		angleBetween = angleBetween - 90
	end
	return angleBetween
end
math.angleBetween = angleBetween

--[[
	Calculate the angle between two lines.
	
	Params:
		lineA - The first line { a={x,y}, b={x,y} }
		lineA - The first line { a={x,y}, b={x,y} }
]]--
local function angleBetweenLines( lineA, lineB )
	local angle1 = math.atan2( lineA.a.y - lineA.b.y, lineA.a.x - lineA.b.x )
	local angle2 = math.atan2( lineB.a.y - lineB.b.y, lineB.a.x - lineB.b.x )
	return math.deg( angle1 - angle2 )
end
math.angleBetweenLines = angleBetweenLines

-- returns the smallest angle between the two angles
-- ie: the difference between the two angles via the shortest distance
-- returned value is signed: clockwise is negative, anticlockwise is positve
-- returned value wraps at +/-180
-- Example code to rotate a display object by touch:
--[[
	-- called in the "moved" phase of touch event handler
	local a = mathlib.angleBetweenPoints( target, target.prevevent )
	local b = mathlib.angleBetweenPoints( target, event )
	local d = mathlib.smallestAngleDiff( a, b )
	target.prev = event
	target.rotation = target.rotation - d
]]--
local function smallestAngleDiff( target, source )
	local a = target - source

	if (a > 180) then
		a = a - 360
	elseif (a < -180) then
		a = a + 360
	end

	return a
end
math.smallestAngleDiff = smallestAngleDiff

-- Returns the angle in degrees between the first and second points, measured at the centre
-- Always a positive value
local function angleAt( centre, first, second )
	local a, b, c = centre, first, second
	local ab = math.lengthOf( a, b )
	local bc = math.lengthOf( b, c )
	local ac = math.lengthOf( a, c )
	local angle = math.deg( math.acos( (ab*ab + ac*ac - bc*bc) / (2 * ab * ac) ) )
	return angle
end
math.angleAt = angleAt

-- Returns true if the point is within the angle at centre measured between first and second
local function isPointInAngle( centre, first, second, point )
	local range = math.angleAt( centre, first, second )
	local a = math.angleAt( centre, first, point )
	local b = math.angleAt( centre, second, point )
	-- print(range,a+b)
	return math.round(range) >= math.round(a + b)
end
math.isPointInAngle = isPointInAngle

-- Forces to apply based on total force and desired angle
-- http://developer.anscamobile.com/code/virtual-dpadjoystick-template
local function forcesByAngle(totalForce, angle)
	local forces = {}
	local radians = -math.rad(angle)

	forces.x = math.cos(radians) * totalForce
	forces.y = math.sin(radians) * totalForce

	return forces
end
math.forcesByAngle = forcesByAngle

--[[
	Lines and Vectors
]]--

--[[
	Returns the length of a line.
	
	Takes either:
		- two parameters with the end location of a line as ( x, y ) and (0,0) as the start
		- one parameter with the end location of a line as {x,y} and {x=0,y=0} as the start
		- two parameters as the start and end of the line as {x,y} and {x,y}
		- four parameters as the start and end of the line as ( x, y, x, y )
	
	Returns:
		The length of the line.
]]--
local function lengthOf( ... )
	local a, b
	
	if (#arg == 4) then
		-- four parameters spelling out x, y, x, y
		a = { x=arg[1], y=arg[2] }
		b = { x=arg[3], y=arg[4] }
		
	elseif (#arg == 2 and type(arg[1]) == "number") then
		-- two parameters spelling out x and y of one end
		a = { x=arg[1], y=arg[2] }
		b = { x=0, y=0 }
		
	elseif (#arg == 1 and arg[1].a ~= nil and arg[1].b ~= nil) then
		-- one parameter containing a and b as the ends of the line
		a = arg[1].a
		b = arg[1].b
		
	elseif (#arg == 1 and arg[1].x ~= nil) then
		-- one parameter as the x,y end
		a = arg[1]
		b = { x=0, y=0 }
		
	else
		-- two parameters as {x,y} for each end
		a = arg[1]
		b = arg[2]
	end
	
	local width, height = b.x-a.x, b.y-a.y
	return (width*width + height*height)^0.5 -- math.sqrt(width*width + height*height)
	-- nothing wrong with math.sqrt, but I believe the ^.5 is faster
end
math.lengthOf = lengthOf

--[[
	Description:
		Extends the point away from or towards the origin to the length of len.
	
	Params:
		max =
			If param max is nil then the lenOrMin value is the distance to calculate the point's location
			If param max is not nil then the lenOrMin value is the minimum clamping distance to extrude to
		lenOrMin = the length or the minimum length to extrude the point's distance to
		max = the maximum length to extrude to
	
	Returns:
		{x,y} = extruded point
]]--
local function extrudeToLen( origin, point, lenOrMin, max )
	local length = lengthOf( origin, point )
	if (length == 0) then
		return origin.x, origin.y
	end
	local len = lenOrMin
	if (max ~= nil) then
		if (length < lenOrMin) then
			len = lenOrMin
		elseif (length > max) then
			len = max
		else -- the point is within the min/max clamping range
			return point.x, point.y
		end
	end
	local factor = len / length
	local x, y = (point.x - origin.x) * factor, (point.y - origin.y) * factor
	return x + origin.x, y + origin.y, x, y
end
math.extrudeToLen = extrudeToLen

-- returns true when the point is on the right of the line formed by the north/south points
local function isOnRight( north, south, point )
	local a, b, c = north, south, point
	local factor = (b.x - a.x)*(c.y - a.y) - (b.y - a.y)*(c.x - a.x)
	return factor > 0, factor
end
math.isOnRight = isOnRight

-- reflect point across line from north to south
local function reflect( north, south, point )
        local x1, y1, x2, y2 = north.x, north.y, south.x, south.y
        local x3, y3 = point.x, point.y
        local x4, y4 = 0, 0 -- reflected point
        local dx, dy, t, d
 
        dx = y2 - y1
        dy = x1 - x2
        t = dx * (x3 - x1) + dy * (y3 - y1)
        t = t / (dx * dx  +  dy * dy)
 
        x = x3 - 2 * dx * t
        y = y3 - 2 * dy * t
 
        return { x=x, y=y }
end
math.reflect = reflect

--[[
	Shows that the lines intersect.
	
	Parameters:
		a, b: Lines with a and b ends, each end with x,y coords.
		a, b, c, d: Line ends for a-b and c-d, each end having x,y coords.
		ax, ay, bx, by, cx, cy, dx, dy: Full points of two lines
	
	Returns:
		true, x, y: If the lines intersect
		false: If the lines do not intersect
	
	Ref:
		http://stackoverflow.com/questions/563198/how-do-you-detect-where-two-line-segments-intersect
]]--
local function getLineIntersection( ... ) -- , *i_x, *i_y)
	local p0_x, p0_y, p1_x, p1_y, p2_x, p2_y, p3_x, p3_y
	
	-- separate parameters
	if (#arg == 2) then
		p0_x, p0_y, p1_x, p1_y = arg[1].a.x, arg[1].a.y, arg[1].b.x, arg[1].b.y
		p2_x, p2_y, p3_x, p3_y = arg[2].a.x, arg[2].a.y, arg[2].b.x, arg[2].b.y
	elseif (#arg == 4) then
		p0_x, p0_y, p1_x, p1_y = arg[1].x, arg[1].y, arg[2].x, arg[2].y
		p2_x, p2_y, p3_x, p3_y = arg[3].x, arg[3].y, arg[4].x, arg[4].y
	elseif (#arg == 8) then
		p0_x, p0_y, p1_x, p1_y, p2_x, p2_y, p3_x, p3_y = unpack( arg )
	end
	
	local i_x, i_y -- output
	
    local s1_x, s1_y, s2_x, s2_y
    s1_x = p1_x - p0_x
    s1_y = p1_y - p0_y
    
    s2_x = p3_x - p2_x
    s2_y = p3_y - p2_y

    local s, t
    s = (-s1_y * (p0_x - p2_x) + s1_x * (p0_y - p2_y)) / (-s2_x * s1_y + s1_x * s2_y)
    t = ( s2_x * (p0_y - p2_y) - s2_y * (p0_x - p2_x)) / (-s2_x * s1_y + s1_x * s2_y)

    if (s >= 0 and s <= 1 and t >= 0 and t <= 1) then
        -- Collision detected
    	i_x = p0_x + (t * s1_x)
    	i_y = p0_y + (t * s1_y)
        return true, i_x, i_y
    end

    return false -- ; // No collision
end
math.getLineIntersection = getLineIntersection

-- This is based off an explanation and expanded math presented by Paul Bourke:
-- It takes two lines as inputs and returns true if they intersect, false if they don't.
-- If they do, ptIntersection returns the point where the two lines intersect.
-- params a, b = first line
-- params c, d = second line
-- param ptIntersection: The point where both lines intersect (if they do)
-- http://local.wasp.uwa.edu.au/~pbourke/geometry/lineline2d/
-- http://paulbourke.net/geometry/pointlineplane/
local function doLinesIntersect( a, b, c, d )
	-- parameter conversion
	local L1 = {X1=a.x,Y1=a.y,X2=b.x,Y2=b.y}
	local L2 = {X1=c.x,Y1=c.y,X2=d.x,Y2=d.y}

	-- Denominator for ua and ub are the same, so store this calculation
	local d = (L2.Y2 - L2.Y1) * (L1.X2 - L1.X1) - (L2.X2 - L2.X1) * (L1.Y2 - L1.Y1)

	-- Make sure there is not a division by zero - this also indicates that the lines are parallel.
	-- If n_a and n_b were both equal to zero the lines would be on top of each
	-- other (coincidental).  This check is not done because it is not
	-- necessary for this implementation (the parallel check accounts for this).
	if (d == 0) then
		return false
	end

	-- n_a and n_b are calculated as seperate values for readability
	local n_a = (L2.X2 - L2.X1) * (L1.Y1 - L2.Y1) - (L2.Y2 - L2.Y1) * (L1.X1 - L2.X1)
	local n_b = (L1.X2 - L1.X1) * (L1.Y1 - L2.Y1) - (L1.Y2 - L1.Y1) * (L1.X1 - L2.X1)

	-- Calculate the intermediate fractional point that the lines potentially intersect.
	local ua = n_a / d
	local ub = n_b / d

	-- The fractional point will be between 0 and 1 inclusive if the lines
	-- intersect.  If the fractional calculation is larger than 1 or smaller
	-- than 0 the lines would need to be longer to intersect.
	if (ua >= 0 and ua <= 1 and ub >= 0 and ub <= 1) then
		local x = L1.X1 + (ua * (L1.X2 - L1.X1))
		local y = L1.Y1 + (ua * (L1.Y2 - L1.Y1))
		return true, {x=x, y=y}
	end

	return false
end
math.doLinesIntersect = doLinesIntersect

-- returns the closest point on the line between A and B from point P
local function GetClosestPoint( A,  B,  P, segmentClamp )
    local AP = { x=P.x - A.x, y=P.y - A.y }
    local AB = { x=B.x - A.x, y=B.y - A.y }
    local ab2 = AB.x*AB.x + AB.y*AB.y
    local ap_ab = AP.x*AB.x + AP.y*AB.y
    local t = ap_ab / ab2
 
    if (segmentClamp or true) then
         if (t < 0.0) then
                t = 0.0
         elseif (t > 1.0) then
                t = 1.0
         end
    end
 
    local Closest = { x=A.x + AB.x * t, y=A.y + AB.y * t }
 
    return Closest
end
math.GetClosestPoint = GetClosestPoint

--[[
	Performs unit normalisation of a vector.
	
	Description:
		Unit normalising is basically converting the length of a line to be a fraction of 1.0
		This function modified the vector value passed in and returns the length as returned by lengthOf()
	
	Note:
		Can also be performed like this:
		function Normalise(vector)
			local x,y = 	x/(x^2 + y^2)^(1/2), y/(x^2 + y^2)^(1/2)
			local unitVector = {x=x,y=y}
			return unitVector
		end
	
	Ref:
		http://www.fundza.com/vectors/normalize/index.html
]]--
local function normalise( vector )
	local len = math.lengthOf( vector )
	vector.x = vector.x / len
	vector.y = vector.y / len
	return len
end
math.normalise = normalise

--[[
	Polygons
]]--

--[[
	Calculates the area of a polygon.
	Will not calculate area for self-intersecting polygons (where vertices cross each other)
	
	Parameters:
		points: table of {x,y} points or list of {x,y,x,y...} coords
	
	Ref:
		http://www.mathopenref.com/coordpolygonarea2.html
]]--
local function polygonArea( points )
	if (type(points[1]) == "number") then
		points = math.tableToPoints( points )
	end
	
	local count = #points
	if (points.numChildren) then
		count = points.numChildren
	end
	
	local area = 0 -- Accumulates area in the loop
	local j = count -- The last vertex is the 'previous' one to the first
	
	for i=1, count do
		area = area +  (points[j].x + points[i].x) * (points[j].y - points[i].y)
		j = i -- j is previous vertex to i
	end
	
	return math.abs(area/2)
end
math.polygonArea = polygonArea

--[[
	Calculates the area of a table of polygons and also returns the sum of the areas.
	Overlapping intersecting areas are not accounted for.
	
	Parameters:
		polygons: table of polygons - see polygonArea()
	
	Returns:
		Table of { polygon, area } tables, sum of areas.
]]--
local function polygonAreas( polygons )
	local tbl = {}
	local sum = 0
	
	for i=1, #tbl do
		local polygon = polygons[i]
		local entry = { polygon=polygon, area=polygonArea( polygon ) }
		tbl[ #tbl+1 ] = entry
		sum = sum + entry.area
	end
	
	return tbl, sum
end
math.polygonAreas = polygonAreas

--[[
	Returns true if the dot {x,y} is within the polygon defined by points table { {x,y},{x,y},{x,y},... }
	Accepts coordinates list {x,y,x,y,...} or points {x,y} table or display group.
	
	Parameters:
		points: table of points or list of coordinates of polygon
		dot: point to check for being inside or outside the bounds of the polygon
	
	Return:
		true if the dot is inside the polygon
]]--
local function isPointInPolygon( points, dot )
	local count = points.numChildren
	
	if (count == nil) then
		points = math.ensurePointsTable( points )
		count = #points
	end
	
	local i, j = count, count
	local oddNodes = false
	
	for i=1, count do
		if ((points[i].y < dot.y and points[j].y>=dot.y
			or points[j].y< dot.y and points[i].y>=dot.y) and (points[i].x<=dot.x
			or points[j].x<=dot.x)) then
			if (points[i].x+(dot.y-points[i].y)/(points[j].y-points[i].y)*(points[j].x-points[i].x)<dot.x) then
				oddNodes = not oddNodes
			end
		end
		j = i
	end
	
	return oddNodes
end
math.isPointInPolygon = isPointInPolygon

--[[
	Return true if the dot { x,y } is within any of the polygons in the list.
	
	Parameters:
		polygons: table of polygons
		dot: point to check for being inside the polygons
	
	Return:
		true if the point is inside any of the polygons, the polygon containing the point
]]--
local function isPointInPolygons( polygons, dot )
	for i=1, #polygons do
		if (isPointInPolygon( polygons[i], dot )) then
			return true, polygons[i]
		end
	end
	return false
end
math.isPointInPolygons = isPointInPolygons
 
-- Returns true if the points in the polygon wind clockwise
-- Does not consider that the vertices may intersect (lines between points might cross over)
local function isPolygonClockwise( pointList )
        local area = 0
        
        if (type(pointList[1]) == "number") then
        	pointList = math.pointsToTable( pointList )
        	print("#pointList",#pointList)
        end
        
        for i = 1, #pointList-1 do
                local pointStart = { x=pointList[i].x - pointList[1].x, y=pointList[i].y - pointList[1].y }
                local pointEnd = { x=pointList[i + 1].x - pointList[1].x, y=pointList[i + 1].y - pointList[1].y }
                area = area + (pointStart.x * -pointEnd.y) - (pointEnd.x * -pointStart.y)
        end
        
        return (area < 0)
end
math.isPolygonClockwise = isPolyClockwise

--[[
	Returns true if the point has less than 180 degrees between the neighbouring points.
	
	Parameters:
		point: the {x,y} point to check the angle at
		b: the {x,y} point preceding the angle point
		c: the {x,y} point following the angle point
	
	Returns:
		true if the point's angle is less than 180 degrees.
]]--
local function isPointConcave( a, b, c )
	local small = smallestAngleDiff( math.angleOf(b,a), math.angleOf(b,c) )

	if (small < 0) then
		return false
	else
		return true
	end
end
math.isPointConcave = isPointConcave

--[[
	Returns true if the polygon is concave.
	Returns nil if there are not enough points ( < 3 )
	Can accept a display group.
	
	Parameters:
		points: table of {x,y} points or list of {x,y,x,y,...} coords
	
	Returns:
		true if the polygon is not convex.
]]--
local function isPolygonConcave( points )
	-- is points a display group?
	local count = points.numChildren
	if (count == nil) then
		-- points is not a display group...
		-- ensure table of points
		points = math.ensurePointsTable( points )
		count = #points
	end
	
	-- cannot check if input is not a polygon
	if (count < 3) then
		return nil
	end
	
	local isConcave = true
	
	for i=1, count do
		if (i == 1) then
			isConcave = isPointConcave( points[count],points[1],points[2] )
		elseif (i == count) then
			isConcave = isPointConcave( points[count-1],points[count],points[1] )
		else
			isConcave = isPointConcave( points[i-1], points[i], points[i+1] )
		end
		
		if (not isConcave) then
			return false
		end
	end
	
	return true
end
math.isPolygonConcave = isPolygonConcave

-- returns list of points where a polygon intersects with the line a,b
-- assumes polygon is standard display format: { x,y,x,y,x,y,x,y, ... }
-- returns collection of intersection points with the polygon line's index {x,y,lineIndex}
-- sort: true to sort the points into order from a to b
local function polygonLineIntersection( polygon, a, b, sort )
	local points = {}
	
	for i=1, #polygon-3, 2 do
		local success, pt = math.doLinesIntersect( a, b, { x=polygon[i], y=polygon[i+1] }, { x=polygon[i+2], y=polygon[i+3] } )
		
		if (success) then
			pt.lineIndex = i
			points[ #points+1 ] = pt
		end
	end
	
	if (sort and #points > 1) then
		table.sort( points, function(f,g) return math.lengthOf(a,f) < math.lengthOf(a,g) end )
	end
	
	return points
end
math.polygonLineIntersection = polygonLineIntersection

--[[
	Description:
		Calculates the average of all the x's and all the y's and returns the average centre of all points.
		Works with a display group or table proceeding { {x,y}, {x,y}, ... }
	
	Params:
		pts = list of {x,y} points to get the average middle point from
	
	Returns:
		x, y = average centre location of all the points
]]--
local function midPoint( ... )
	local pts = arg
	
	local x, y, c = 0, 0, #pts
	if (pts.numChildren and pts.numChildren > 0) then c = pts.numChildren end
	
	for i=1, c do
		x = x + pts[i].x
		y = y + pts[i].y
	end
	return x/c, y/c
end
math.midPoint = midPoint

--[[
	Calculates the middle of a polygon's bounding box - as if drawing a square around the polygon and finding the middle.
	Also calculates the width and height of the bounding box.
	
	Parameters:
		Polygon coordinates as a table of points, display group or list of coordinates.
	
	Returns:
		Centroid (centre) x, y
		Bounding box width, height
]]--
local function getBoundingCentroid( pts )
	pts = math.ensurePointsTable( pts )
	
	local xMin, xMax, yMin, yMax = 100000, 0, 100000, 0
	
	for i=1, #pts do
		local pt = pts[i]
		if (pt.x < xMin) then
			xMin = pt.x
		elseif (pt.x > xMax) then
			xMax = pt.x
		end
		if (pt.y < yMin) then
			yMin = pt.y
		elseif (pt.y > yMax) then
			yMax = pt.y
		end
	end
	
	local width, height = xMax-xMin, yMax-yMin
	local cx, cy = xMin+(width/2), yMin+(height/2)
	
	local output = {
		centroid = { x=cx, y=cy },
		width = width,
		height = height,
		bounding = { xMin=xMin, xMax=xMax, yMin=yMin, yMax=yMax },
	}
	
	return output
end
math.getBoundingCentroid = getBoundingCentroid

--[[
	Produces an adjusted polygon so that the vertices are centred on the bounding centroid (square bounding box middle.)
	
	Parameters:
		List of coordinates, display group or table of points of the polygon.
	
	Returns:
		Table of points for the adjusted polygon.
		x, y of the centre of the polygon.
]]--
local function centerPoly( pts, cx, cy )
	pts = math.ensurePointsTable( pts )
	local output = {}
	
	local x, y
	local minx, maxx, miny, maxy = 100000000, -100000000, 100000000, -100000000
	
	-- get dimensions
	for i=1, #pts do
		x, y = pts[i].x, pts[i].y
		if (x < minx) then minx = x end
		if (x > maxx) then maxx = x end
		if (y < miny) then miny = y end
		if (y > maxy) then maxy = y end
	end
	
	-- get bounds
	local width, height = maxx-minx, maxy-miny
	
	-- get centre
	if (cx and cy) then
		x, y = cx, cy
	else
		x, y = minx+(width/2), miny+(height/2)
	end
	
	-- centre
	for i=1, #pts do
		output[#output+1] = {x=pts[i].x-x,y=pts[i].y-y}
	end
	
	return output, { x=x, y=y, width=width, height=height, minx=minx, miny=miny, maxx=maxx, maxy=maxy }
end
--local function centrePolygon( pts )
--	pts = math.ensurePointsTable( pts )
--	local bounding = math.getBoundingCentroid( pts )
--	local x, y = bounding.centroid.x, bounding.centroid.y
--	
--	for i=1, #pts do
--		pts[i].x = pts[i].x - x
--		pts[i].y = pts[i].y - y
--	end
--	
--	return pts, x, y
--end
math.centrePolygon = centerPoly

--[[
	Description:
		Calculates the average of all the x's and all the y's and returns the average centre of all points.
		Works with a table proceeding {x,y,x,y,...} as used with display.newLine or physics.addBody
	
	Params:
		pts = table of x,y values in sequence
	
	Returns:
		x, y = average centre location of all points
]]--
local function midPointOfShape( pts )
	local x, y, c, t = 0, 0, #pts, #pts/2
	for i=1, c-1, 2 do
		x = x + pts[i]
		y = y + pts[i+1]
	end
	return x/t, y/t
end
math.midPointOfShape = midPointOfShape

--[[
	Description:
		Takes two polygons of the form {{x,y},{x,y},...} and determines if they intersect.
		Accepts parameters as display groups, tables of points {x,y} or lists of coords {x,y,x,y,...}
	
	Parameters:
		subjectPolygon: first polygon to intersect with the second
		clipPolygon: second polygon to intersect with the first
	
	Returns:
		Polygon of points of intersection between the two input polygons.
		True if the two do intersect, false if they are not touching.
	
	Example:
		subjectPolygon = {{x=50, y=150}, {x=200, y=50}, {x=350, y=150}, {x=350, y=300}, {x=250, y=300}, {x=200, y=250}, {x=150, y=350}, {x=100, y=250}, {x=100, y=200}}
		clipPolygon = {{x=100, y=100}, {x=300, y=100}, {x=300, y=300}, {x=100, y=300}}
		outputList, intersects = clip(subjectPolygon, clipPolygon)
	
	Ref:
		http://rosettacode.org/wiki/Sutherland-Hodgman_polygon_clipping#Lua
]]--
local function getPolygonIntersection( subjectPolygon, clipPolygon )
	local subjectPolygon = math.copyToPointsTable( subjectPolygon )
	local clipPolygon = math.copyToPointsTable( clipPolygon )
	
	local function inside(p, cp1, cp2)
		return (cp2.x-cp1.x)*(p.y-cp1.y) > (cp2.y-cp1.y)*(p.x-cp1.x)
	end
	
	local function intersection(cp1, cp2, s, e)
		local dcx, dcy = cp1.x-cp2.x, cp1.y-cp2.y
		local dpx, dpy = s.x-e.x, s.y-e.y
		local n1 = cp1.x*cp2.y - cp1.y*cp2.x
		local n2 = s.x*e.y - s.y*e.x
		local n3 = 1 / (dcx*dpy - dcy*dpx)
		local x = (n1*dpx - n2*dcx) * n3
		local y = (n1*dpy - n2*dcy) * n3
		return {x=x, y=y}
	end
	
	local outputList = subjectPolygon
	local cp1 = clipPolygon[#clipPolygon]
	
	for _, cp2 in ipairs(clipPolygon) do  -- WP clipEdge is cp1,cp2 here
		local inputList = outputList
		outputList = {}
		local s = inputList[#inputList]
		for _, e in ipairs(inputList) do
			if inside(e, cp1, cp2) then
				if not inside(s, cp1, cp2) then
					outputList[#outputList+1] = intersection(cp1, cp2, s, e)
				end
				outputList[#outputList+1] = e
			elseif inside(s, cp1, cp2) then
				outputList[#outputList+1] = intersection(cp1, cp2, s, e)
			end
			s = e
		end
		cp1 = cp2
	end
	
	return outputList, #outputList > 0
end
math.getPolygonIntersection = getPolygonIntersection

--[[
	Products
]]--

--[[
        Calculates the dot product of two lines.
        This function implements the simple form of the dot product calculation: a · b = ax × bx + ay × by
        The lines can be provided in 3 forms:
        
        Parameters:
        	a: {x,y}
        	b: {x,y}
        
        Example:
        	print( dotProduct( {x=10,y=10}, {x=-10,y=10} ) )
        
        Parameters:
        	a: {a,b}
        	b: {a,b}
        
        Example:
                print( dotProduct(
                        { a={x=10,y=10}, b={x=101,y=5} },
                        { a={x=10,y=-10}, b={x=51,y=10} }
                ))
        
		Params:
			lenA: Length A
			lenB: Length B
			deg: Angle between points A and B in degrees
		
		Example:
			print( dotProduct( 23, 10, 90 ) )
		
        Ref:
		http://www.mathsisfun.com/algebra/vectors-dot-product.html
		http://members.tripod.com/c_carleton/dotprod.html/
		http://www.mathsisfun.com/algebra/vector-calculator.html
]]--
local function dotProduct( ... )
	local ax, ax, bx, by
	
	if (#arg == 2 and arg[1].a == nil) then
		-- two vectors - get the vectors
		ax, ay = arg[1].x, arg[1].y
		bx, by = arg[2].x, arg[2].y
		
	elseif (#arg == 2 and a.x == nil) then
		-- two lines - calculate the vectors
		ax = arg[1].b.x - arg[1].a.x
		ay = arg[1].b.y - arg[1].a.y
		bx = arg[2].b.x - arg[2].a.x
		by = arg[2].b.y - arg[2].a.y
		
	elseif (#arg == 3 and type(arg[1]) == "number") then
		-- two lengths and an angle: lenA * lenB * math.cos( deg )
		return arg[1] * arg[2] * math.cos( arg[3] )
		
	elseif (#arg == 4 and type(arg[1]) == "number") then
		-- two lines, params are (x,y,x,y) - get the vectors
		ax, ay = arg[1], arg[2]
		bx, by = arg[3], arg[4]
	end
	
	-- multiply the x's, multiply the y's, then add
	local dot = ax * bx + ay * by
	return dot
end
math.dotProduct = dotProduct

--[[
        Description:
                Calculates the cross product of a vector.
        
        Ref:
                http://www.math.ntnu.no/~stacey/documents/Codea/Library/Vec3.lua
]]--
local function crossProduct( a, b )
        local x, y, z
        x = a.y * (b.z or 0) - (a.z or 0) * b.y
        y = (a.z or 0) * b.x - a.x * (b.z or 0)
        z = a.x * b.y - a.y * b.x
        return { x=x, y=y, z=z }
end
math.crossProduct = crossProduct

--[[
        Description:
                Perform the cross product on two vectors. In 2D this produces a scalar.
        
        Params:
                a: {x,y}
                b: {x,y}
        
        Ref:
                http://www.iforce2d.net/forums/viewtopic.php?f=4&t=79&sid=b9ecd62533361594e321de04b3929d4f
]]--
local function b2CrossVectVect( a, b )
        return a.x * b.y - a.y * b.x;
end
math.b2CrossVectVect = b2CrossVectVect

--[[
        Description:
                Perform the cross product on a vector and a scalar. In 2D this produces a vector.
        
        Params:
                a: {x,y}
                b: float
        
        Ref:
                http://www.iforce2d.net/forums/viewtopic.php?f=4&t=79&sid=b9ecd62533361594e321de04b3929d4f
]]--
local function b2CrossVectFloat( a, s )
        return { x = s * a.y, y = -s * a.x }
end
math.b2CrossVectFloat = b2CrossVectFloat

--[[
        Description:
                Perform the cross product on a scalar and a vector. In 2D this produces a vector.
        
        Params:
                a: float
                b: {x,y}
        
        Ref:
                http://www.iforce2d.net/forums/viewtopic.php?f=4&t=79&sid=b9ecd62533361594e321de04b3929d4f
]]--
local function b2CrossFloatVect( s, a )
        return { x = -s * a.y, y = s * a.x }
end
math.b2CrossFloatVect = b2CrossFloatVect

--[[
	Point Collections
]]--

-- converts a table of {x,y,x,y,...} to points {x,y}
local function tableToPoints( tbl )
	local pts = {}
	
	for i=1, #tbl-1, 2 do
		pts[#pts+1] = { x=tbl[i], y=tbl[i+1] }
	end
	
	return pts
end
math.tableToPoints = tableToPoints

-- converts a list of points {x,y} to a table of coords {x,y,x,y,...}
local function pointsToTable( pts )
	local tbl = {}
	
	for i=1, #pts do
		tbl[#tbl+1] = pts[i].x
		tbl[#tbl+1] = pts[i].y
	end
	
	return tbl
end
math.pointsToTable = pointsToTable

-- ensures that a list of coordinates is converted to a table of {x,y} points
-- returns a table of {x,y} points and the number of points, whether a display group or not
local function ensurePointsTable( tbl )
	if (type(tbl[1]) == "number") then
		-- list contains {x,y,x,y,...} coordinates - convert to table of {x,y} 
		tbl = tableToPoints( tbl )
		return tbl, #tbl
	else
		-- table is already in {x,y} point format...
		-- check for display group
		local count = tbl.numChildren
		if (count == nil) then
			count = #tbl
		end
		return tbl, count
	end
end
math.ensurePointsTable = ensurePointsTable

-- copies the points from a list of coords, table of points or display group into a new table of {x,y} points
local function copyToPointsTable( points )
	local tbl = {}
	
	local count = points.numChildren
	if (count == nil) then
		count = #points
	end
	
	local isCoords = (type(points[1]) == "number")
	local step = 1
	if (isCoords) then
		step = 2
	end
	
	for i=1, count, step do
		if (isCoords) then
			tbl[#tbl+1] = {x=points[i],y=points[i+1]}
		else
			tbl[#tbl+1] = {x=points[i].x,y=points[i].y}
		end
	end
	
	return tbl
end
math.copyToPointsTable = copyToPointsTable

--[[
	Removes points in found in sequence at the same location.
	Eg: two points both at {x=10,y=21} will be deduped to just one point.
	
	Parameters:
		points: The list of points - can be list of coords but will convert to points on return.
		maxdist: If nil the points are directly compared. If provided, points will be deduped if they are closer than this distance.
	
	Returns:
		List of points where no two points exist at the same location.
	
	Comments:
		Requires the table.lua library file: https://gist.github.com/HoraceBury/9307117
]]--
local function dedupePoints( points, maxdist )
	-- ensure points list not coords
	points = math.ensurePointsTable( points )
	
	-- create output list and copy first point
	local pts = {}
	pts[1] = points[1]
	
	-- compare points from second to last against previous
	for i=2, #points do
		if (maxdist) then
			if (math.lengthOf( pts[#pts], points[i] ) > maxdist) then
				pts[#pts+1] = points[i]
			end
		else
			if (not table.compare( pts[#pts], points[i] )) then
				pts[#pts+1] = points[i]
			end
		end
	end
	
	-- compare first and last points
	if (maxdist) then
		if (math.lengthOf( pts[1], pts[#pts] ) <= maxdist) then
			pts[#pts] = nil
		end
	else
		if (table.compare( pts[1], pts[#pts] )) then
			pts[#pts] = nil
		end
	end
	
	return pts
end
math.dedupePoints = dedupePoints
