-- slicer proof

-- http://www.mathopenref.com/polygonconvex.html

local isDebug = false

display.setStatusBar( display.HiddenStatusBar )
display.setDefault( "background", 1,1,1 )

require("physics")
physics.start()
physics.setGravity(0,0)
physics.setDrawMode( "hybrid" )

require("tablelib") -- adds support functions to the table.* api
require("mathlib") -- adds support for extended maths functions

local utils = require("utils")
local stage, sWidth, sHeight, pWidth, pHeight, centerX, centerY = utils.stage, utils.sWidth, utils.sHeight, utils.pWidth, utils.pHeight, utils.centerX, utils.centerY

local imagefile = "fish.small.red.png"

--local fish = display.newImage( imagefile )
--fish.x, fish.y = fish.width*.5, fish.height*.5
--fish.xScale, fish.yScale = 6, 6

local scale, shift = 6, 150

local outline = graphics.newOutline( 2, imagefile )
for i=1, #outline do
	outline[i] = outline[i]*scale -- +shift
end
--for i=1, #outline-1, 2 do
--	if (i>2) then
--		display.newLine(outline[i-2],outline[i-1],outline[i],outline[i+1]).stroke={0,0,0}
--	end
--	display.newCircle( outline[i], outline[i+1], 3 ).fill = {0,0,1}
--end
--a, b = math.centrePolygon( outline )
--b.x,b.y = display.contentCenterX,display.contentCenterY*.5
--display.newCircle( b.x, b.y, 5 ).fill = {0,0,0}
--local r = display.newRect( b.x,b.y,b.width,b.height )
--r.stroke={0,1,0}
--r.fill = {0,0,0,0}
--r.strokeWidth=1
--print(b.minx,b.miny,b.maxx,b.maxy)
--print(b.x,b.y)


local lines, dots = display.newGroup(), display.newGroup()

-- copies the first point to the end of the table
-- (avoids annoying code when looping through the polygon points)
local function closePolygonAsPoints( tbl )
	local points = math.ensurePointsTable( tbl )
	
	if (points[1].x ~= points[#points].x or points[1].y ~= points[#points].y) then
		points[#points+1] = { x=points[1].x, y=points[1].y }
	end
	
	return points
end

local d
local ps = closePolygonAsPoints( outline )
ps = math.dedupePoints( ps )
ps, d = math.centrePolygon( ps )
ps = math.pointsToTable( ps )
outline = ps
local first = display.newPolygon( display.contentCenterX,display.contentCenterY*.5 , outline )
first.fill = {0,1,0,.5}
first.fill = { type="image", filename=imagefile }
first.stroke = {1,0,0}
first.strokeWidth = 0

local function render( points, clear )
	if (clear == nil or clear) then
		lines:removeSelf(); dots:removeSelf()
		lines, dots = display.newGroup(), display.newGroup()
	end
	for i=1, #points-1 do
		local line = display.newLine( lines, points[i].x+first.x, points[i].y+first.y, points[i+1].x+first.x, points[i+1].y+first.y )
		line.stroke = {0,0,1}
		line.strokeWidth = 2
		display.newCircle( dots, points[i].x+first.x, points[i].y+first.y, 4 ).fill = {1,0,0}
	end
end
render( closePolygonAsPoints( outline ) )

-- checks for intersections between the lines in the path against the lines in the polygon points and adds intermediary points to the polygon
-- to ensure that no line in the polygon has more than one intersection
-- call this either on every touch event (with only the last path segment) or once before the dissection occurs (with the entire path)
local function preProcessAndReduceIntersections( points, path )
	-- for each line in the path...
	for p=1, #path-1 do
		local pathA, pathB = path[p], path[p+1]
		
		-- check against each line in the polygon
		for i=1, #points-1 do
			local pointA, pointB = points[i], points[i+1]
			
			-- check for an intersection between polygon line and path line
			local success, pt = math.doLinesIntersect( pointA, pointB, pathA, pathB )
			
			-- if there is an intersection, add it to the polygon's point's intersection list
			if (success) then
				local list = pointA.intersectionList
				
				-- if the point doesn't have a list, create one
				if (list) then
					-- add the intersection point to the polygon point's intersection list
					list[ #list+1 ] = pt
				else
					pointA.intersectionList = { pt }
				end
				
			end
		end
	end
	
	-- create new points in the polygon which segregate the intersection points to their own line
	local i = 1
	while (i <= #points-1) do
		local ptA, ptB = points[i], points[i+1]
		
		-- if a point has a list of intersections then subdivide it's line by those points
		if (ptA.intersectionList) then
			local list = ptA.intersectionList
			
			-- get the points on the line (start, intersections and end) into a table, ordered properly from start to end point
			table.sort( list, function(a,b) return math.lengthOf(ptA,a) < math.lengthOf(ptA,b) end )
			--print("#list",#list)
			list = table.copy( { ptA }, list, { ptB } )
			
--			display.newCircle( ptA.x+first.x, ptA.y+first.y-50, 7 ).fill = {1,0,1}
--			display.newCircle( ptB.x+first.x, ptB.y+first.y-50, 7 ).fill = {1,0,1}
			
			-- add points to the polygon between each of the intersection points
			for t=1, #list-1 do
				
				-- insert the mid point between the intersections into the polygon
				local x, y = math.midPoint( list[t], list[t+1] )
				table.insert( points, i+1, {x=x, y=y} )
				
				i = i + 1
			end
		end
		
		i = i + 1
	end
end

-- if b exists then the parameters are a straight line
-- b does not exit then a is a series of points on a curve
local function getIntersections( points, a, b )
	-- finds intersecting points starting at first point which cuts into the shape and returns pairs of points which enter then exit in that order, always in pairs
	local function line( points, a, b )
		local intersects = {}
		
		for i=1, #points-1 do
			local success, pt = math.doLinesIntersect( a,b, points[i], points[i+1] )
			if (success) then
				pt.index = i
				intersects[#intersects+1] = pt
			end
		end
		
		if (#intersects == 1) then
			return {}
		end
		
		table.sort( intersects, function(x,y) return math.lengthOf( a, x ) < math.lengthOf( a, y ) end )
		
		if (#intersects > 0 and math.isPointInPolygon( points, a )) then
			table.remove( intersects, 1 )
		end
		
		if (#intersects % 2 ~= 0) then
			intersects[#intersects] = nil
		end
		
		if (#intersects == 1) then
			return {}
		end
		
		for i=1, #intersects, 2 do
			intersects[i].pairIndex = i
			intersects[i+1].pairIndex = i
		end
		--print("INTER LINE")
		return intersects
	end
	
	-- returns distance-sorted list of intersections between any line in the polygon points and the line a,b
	local function checkIntersections( points, a, b )
		local intersects = {}
		local i = 1
		while (i <= #points-1) do
			local success, pt = math.doLinesIntersect( a,b, points[i], points[i+1] )
			if (success) then
				pt.index = i
				intersects[#intersects+1] = pt
				if (isDebug) then display.newCircle( pt.x+first.x, pt.y+first.y, 7 ).fill = {0,1,0} end
			end
			i = i + 1
		end
		table.sort( intersects, function(x,y) return math.lengthOf( a, x ) < math.lengthOf( a, y ) end )
		return intersects
	end
	
	-- returns sequences of curves with the start of each cutting into the shape and the last exiting the shape
	-- first and last points in each returned sequence are the intersecting points
	local function curve( points, seq )
		local intersects, paths = {}, { {} }
		local isInPoly = math.isPointInPolygon( points, seq[1] )
		
		for i=1, #seq-1 do
			if (isDebug) then
			display.newCircle(seq[i].x+first.x,seq[i].y+first.y,4).fill={0,0,0}
			local l = display.newLine(seq[i].x+first.x, seq[i].y+first.y, seq[i+1].x+first.x,seq[i+1].y+first.y)
			l.stroke={0,0,1}
			l.strokeWidth = 1
			end
			
			local intersections = checkIntersections( points, seq[i], seq[i+1] )
			
			local currentPath = paths[#paths]
			
			if (isInPoly and #intersects > 0) then
				currentPath[#currentPath+1] = seq[i]
			end
			
			for i=1, #intersections do
				if (isInPoly) then
					if (#intersects > 0) then
						-- already in polygon - shifting out of polygon
						intersects[#intersects+1] = intersections[i]
						intersects[#intersects].pathIndex = #paths
						currentPath = {}
						paths[#paths+1] = currentPath
					end
				else
					-- already outside polygon - shifting into polygon
					intersects[#intersects+1] = intersections[i]
					intersects[#intersects].pathIndex = #paths
				end
				isInPoly = not isInPoly -- shifting in/out of polygon
			end
		end
		
		-- sequence ends within the polygon so remove the last path
		if (isInPoly) then
			intersects[#intersects] = nil
			paths[#paths] = nil
		end
		
		for i=1, #intersects, 2 do
			intersects[i].pairIndex = i
			intersects[i+1].pairIndex = i
		end
		
		paths[#paths] = nil
		return intersects, paths
	end
	
	if (b == nil) then
		return curve( points, a )
	else
		return line( points, a, b )
	end
end

-- inserts the found intersection points into the polygon
local function inflate( points, sections )
	local pts = {}
	
	table.sort( sections, function(a,b) return a.index < b.index end )
	
	local nextIndex = sections[1].index
	
	for i=1, #points do
		pts[#pts+1] = points[i]
		if (i == nextIndex) then
			pts[#pts+1] = table.pop( sections, 1 )
			if (#sections == 0) then
				nextIndex = nil
			else
				nextIndex = sections[1].index
			end
		end
	end
	
	return pts
end

-- finds the next intersection point location for tye intersection pair
local function findFirst( points, pairIndex )
	for i=1, #points do
		if (pairIndex) then
			if (points[i].pairIndex == pairIndex) then
				return i, points[i].pairIndex
			end
		else
			if (points[i].pairIndex) then
				return i, points[i].pairIndex
			end
		end
	end
	return nil, pairIndex
end

-- returns the two polygons defined by a pair of intersection points
local function extractPairPoints( points, startIndex, pairIndex, paths )
	local output, second = {}, {}
	local index = startIndex
	
	-- output first sequence of the pair including start and end
	local count = 0
	while (count < 2) do
		output[#output+1] = points[index]
		if (points[index].pairIndex == pairIndex) then
			count = count + 1
		end
		if (count < 2) then
			index = index + 1
		end
		if (index > #points) then
			index = 1
		end
	end
	
	-- output second sequence of the pair including start and end
	count = 0
	while (count < 2) do
		second[#second+1] = points[index]
		if (points[index].pairIndex == pairIndex) then
			count = count + 1
		end
		if (count < 2) then
			index = index + 1
		end
		if (index > #points) then
			index = 1
		end
	end
	
	-- append the sequence paths to the halves
	if (paths) then
		local oc, sc = #output, #second
		
		local path = paths[output[1].pathIndex]
		
		if (math.lengthOf( output[#output], path[1] ) < math.lengthOf( output[#output], path[#path] )) then
			output = table.copy( output, path )
		else
			output = table.copy( output, table.reverse( path ) )
		end
		
		if (math.lengthOf( second[#second], path[1] ) < math.lengthOf( second[#second], path[#path] )) then
			second = table.copy( second, path )
		else
			second = table.copy( second, table.reverse( path ) )
		end
		
		print("===========",oc,"============")
		for i=1, #output do
			--print(i,output[i].x,output[i].y)
			--if (i==oc) then print("  ") end
			
			--timer.performWithDelay( 3000+i*1000, function() display.newCircle(output[i].x+first.x,output[i].y+first.y,7).fill={1,0,1} end, 1)
		end
		print("============",sc,"===========")
		for i=1, #second do
			--print(i,second[i].x,second[i].y)
			--if (i==sc) then print("  ") end
			
			--timer.performWithDelay( #output*1000+5000+i*1000, function() display.newCircle(second[i].x+first.x,second[i].y+first.y,7).fill={1,1,0} end, 1)
		end
		print("=======================")
	end
	
	return output, second
end

local function dissect( points, touchStart, touchEnd )
	points = closePolygonAsPoints( points, scale, shift )
	
	-- returns a series of curves if touchStart is a curved slice and not a straight line
	local intersections, paths = getIntersections( points, touchStart, touchEnd )
	
--	for i=1, #intersections do
--		display.newCircle( intersections[i].x+first.x, intersections[i].y+first.y, 3 ).fill = {1,0,0}
--	end
	
	if (#intersections < 2) then
		return {}
	end
	
	points = inflate( points, intersections )
	
	local startIndex, pairIndex = findFirst( points )
	
	local firsthalf, secondhalf -- = extractPairPoints( points, startIndex, pairIndex )
	
	local polygons = { points }
	local startIndex, pairIndex = 1, 1
	local i = 1
	
	while (i <= #polygons) do
		startIndex, pairIndex = findFirst( polygons[i], pairIndex )
		if (startIndex) then
			local firsthalf, secondhalf = extractPairPoints( polygons[i], startIndex, pairIndex, paths )
			table.remove( polygons, i )
			polygons[#polygons+1] = firsthalf
			polygons[#polygons+1] = secondhalf
			i = 0
			pairIndex = pairIndex + 2
		end
		i = i + 1
	end
	
	return polygons
end

local g = display.newGroup()
local h = display.newGroup()

local o, original = math.centrePolygon( outline )

local function lineSliceTouch(e)
	g:removeSelf()
	g = display.newGroup()
	
	local line = display.newLine( g, e.x, e.y, e.xStart, e.yStart )
	line.stroke = {0,0,0}
	line.strokeWidth = 2
	
	local lineend = {x=e.xStart-first.x,y=e.yStart-first.y}
	e.x, e.y = e.x-first.x, e.y-first.y
	
	local polygons = dissect( outline, e, lineend )
	
	if (#polygons>0) then
		local x = 0
		for i=1, #polygons do
			local pts = math.centrePolygon( math.dedupePoints( polygons[i] ) )
			
			local t, dim = math.centrePolygon( math.dedupePoints( polygons[i] ) )
			t = math.pointsToTable( t )
			local p = display.newPolygon( g, 0,600, t )
			p.strokeWidth = 2
			x = x + dim.width/2
			p.x = x
			x = x + dim.width/2
			
			local r = display.newRect( g,p.x,p.y,dim.width,dim.height )
			r.fill = {0,0,0,0}
			r.stroke = {0,0,1}
			r.strokeWidth = 2
			
			p.fill={1,0,0,.3}
			p.stroke = {1,0,0}
			p.strokeWidth = 2
			
			p.fill = { type="image", filename=imagefile }
			
			p.fill.scaleX = original.width / dim.width
			p.fill.scaleY = original.height / dim.height
			
			p.fill.x = (dim.x - original.x) / original.width
			p.fill.y = (dim.y - original.y) / original.height
		end
	end
	return true
end

local curve = nil

-- removes any points between the current point and intersecting line from the path (does not produce a new list)
local function removeIntersectionConflicts( path, e )
	if (#path >= 3) then
		for i=#path-2, 1, -1 do
			local theyDo = math.doLinesIntersect( path[i], path[i+1], path[#path], e )
			if (theyDo) then
				while (#path > i) do
					path[#path] = nil
				end
				return
			end
		end
	end
end

local draw = display.newGroup()

local function redraw( path )
	if (#path < 3) then return end
	
	draw:removeSelf()
	
	draw = display.newLine( path[1].x, path[1].y, path[2].x, path[2].y )
	draw.stroke = {0,0,0}
	draw.strokeWidth = 2
	
	for i=3, #path do
		draw:append( path[i].x, path[i].y )
	end
end

local function curveSliceTouch(e)
	if (e.phase == "began") then
		g:removeSelf()
		g = display.newGroup()
		
		g.curve = { e }
		
	elseif (e.phase == "moved") then
		removeIntersectionConflicts( g.curve, e )
		if (math.lengthOf( g.curve[ #g.curve ], e ) > 2) then
			g.curve[ #g.curve+1 ] = e
		end
		redraw( g.curve )
	else
		removeIntersectionConflicts( g.curve, e )
		if (math.lengthOf( g.curve[ #g.curve ], e ) <= 2) then
			g.curve[ #g.curve ] = nil
		end
		g.curve[ #g.curve+1 ] = e
		redraw( g.curve )
		
		g.curve = math.centrePolygon( g.curve, first.x, first.y )
		
		local polygon = closePolygonAsPoints( outline )
		
		if (isDebug) then display.newRect( display.contentCenterX, display.contentCenterY*.5, display.contentWidth, display.contentHeight*.3 ).alpha = .9 end
		
		print("pre",#polygon)
		preProcessAndReduceIntersections( polygon, g.curve )
		print("post",#polygon)
		
		polygon = math.dedupePoints( polygon )
		
		if (isDebug) then 
		for i=1, #polygon do
			timer.performWithDelay(i*500,function()
				display.newCircle( first.x+polygon[i].x, first.y+polygon[i].y, 6 ).fill = {1,0,0}
			end,1)
		end
		end
		
		local polygons = dissect( polygon, g.curve )
		
		if (#polygons>0) then
			local x = 0
			for i=1, #polygons do
				local polygon = polygons[i]
				
				polygon = math.dedupePoints( polygon )
				
--				print("======"..i.."========"..#polygon.."======")
--				for t=1, #polygon do
--					print(t,polygon[t].x,polygon[t].y)
--				end
				
				local pts = math.centrePolygon( polygon )
				
				local t, dim = math.centrePolygon( math.dedupePoints( polygon ) )
				t = math.pointsToTable( t )
				local p = display.newPolygon( g, 0,600, t )
				p.strokeWidth = 2
				x = x + dim.width/2
				p.x = x
				x = x + dim.width/2
				
				local r = display.newRect( g,p.x,p.y,dim.width,dim.height )
				r.fill = {0,0,0,0}
				r.stroke = {0,0,1}
				r.strokeWidth = 2
				
				p.fill={1,0,0,.3}
				p.stroke = {1,0,0}
				p.strokeWidth = 2
				
				p.fill = { type="image", filename=imagefile }
				
				p.fill.scaleX = original.width / dim.width
				p.fill.scaleY = original.height / dim.height
				
				p.fill.x = (dim.x - original.x) / original.width
				p.fill.y = (dim.y - original.y) / original.height
			end
		end
	end
	return true
end

Runtime:addEventListener("touch",lineSliceTouch)
