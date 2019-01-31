-- utils

local utils = {}

-- display values
local stage = display.getCurrentStage()
local sWidth, sHeight = display.contentWidth, display.contentHeight
local pWidth, pHeight = display.actualContentWidth, display.actualContentHeight
local centerX, centerY = pWidth*.5, pHeight*.5

utils.stage, utils.sWidth, utils.sHeight, utils.pWidth, utils.pHeight, utils.centerX, utils.centerY = stage, sWidth, sHeight, pWidth, pHeight, centerX, centerY

string.urlencode = function(str)
	if (str) then
		str = string.gsub (str, "\n", "\r\n")
--		str = string.gsub (str, "([^%w ])",	function ( c ) return string.format ("%%%02X", string.byte( c )) end)
		str = string.gsub (str, " ", "%%20")
	end
	return str
end

-- overrides network.download
local download = network.download
network.download = function( url, method, listener, params, filename, baseDirectory )
	print("network.download:url",urlencode( url ))
	local function onComplete(e)
		print("download phase",e.phase)
		if (e.phase == "ended") then
			native.setActivityIndicator( false )
		end
		listener(e)
	end
	native.setActivityIndicator( true )
	local function doit()
		local v = download( urlencode( url ), method, onComplete, params, filename, baseDirectory )
		return v
	end
	local status, result = pcall(doit)
	print("status,result",status,result)
	return v
end

-- creates a series of display groups
display.newGroups = function( parent, count, x, y )
	local list = {}
	for i=1, count do
		local group = display.newGroup()
		list[ #list+1 ] = group
		parent:insert( group )
		if (x) then group.x=x end
		if (y) then group.y=y end
	end
	return unpack(list)
end

-- adds parent and position parameters to the newGroup function
-- parameters: [parent, ][x, y]
local newgroup = display.newGroup
display.newGroup = function( ... )
	local parent, x, y = stage, 0, 0
	local group = newgroup()
	
	if (#arg == 1 or #arg == 3) then
		-- includes parent as first param
		arg[1]:insert( group )
	end
	
	if (#arg > 1) then
		-- includes x and y
		group.x, group.y = arg[#arg-1], arg[#arg]
	end
	
	return group
end

-- dumps all table content to the console
function dump( tbl )
	print("=== DUMP ---------")
	if (tbl) then
		for k,v in pairs(tbl) do
			print(k,v)
		end
	else
		print("nil")
	end
	print("------------------")
end

--[[
-- deep dumps the contents of the table and it's contents' contents
function deepdump( tbl )
	local checklist = {}
	local function innerdump( tbl, indent )
		checklist[ tostring(tbl) ] = true
		for k,v in pairs(tbl) do
			print(indent..k,v,type(v),checklist[ tostring(tbl) ])
			if (type(v) == "table" and not checklist[ tostring(v) ]) then innerdump(v,indent.."    ") end
		end
	end
	print("=== DEEPDUMP -----")
	checklist[ tostring(tbl) ] = true
	innerdump( tbl, "" )
	print("------------------")
end
]]--

-- replaces placeholders {1}, {2}, etc with args
-- eg: "hello banana" = strReplace("{1} {2}", "hello", "banana")
local function strReplace( str, ... )
	local function inner()
		for i=1, #arg do
			str = string.gsub( str, "{"..i.."}", arg[i] or "" )
		end
		return str
	end
	local status, result = pcall(inner)
	if (status) then
		return result
	else
		print("strReplace fail: ", result, str, unpack( arg ) )
		return nil
	end
end
string.strReplace = strReplace

-- splits a string into a table
-- str: the sring to split
-- sep: the separating char
local function strSplit( str, sep )
	sep = sep or "/" -- default for urls
	local tbl = {}
	
	-- get first separator
	local index = string.find( str, "/", 1, true )
	
	-- perform separations while there are substrings
	while (index ~= nil) do
		-- get string up to the separator
		local sub = string.sub( str, 1, index-1 )
		-- if the string is 1 chr or more (could be two adjacent separators)
		if (string.len( sub ) > 0) then
			tbl[ #tbl+1 ] = sub
		end
		-- get the rest of the string after the separator
		str = string.sub( str, index+1 )
		-- find the next separator
		index = string.find( str, "/", 1, true )
	end
	-- store the last substring
	if (string.len( str ) > 0) then
		tbl[ #tbl+1 ] = str
	end
	
	return tbl
end
string.strSplit = strSplit

-- takes two tables of strings, eg: {"a","c","e"} and {"b","d"} and interleaves them like a zipper to produce one table, then converts it into a string using table.concat
-- tblA must have at least the same number of elements as tblB
string.strZip = function( tblA, tblB )
	local tbl = {}
	for i=1, #tblA do
		tbl[ #tbl+1 ] = tblA[i]
		if (tblB[i]) then tbl[ #tbl+1 ] = tblB[i] end
	end
	return table.concat( tbl )
end

-- string replacement
-- if pattern is a table it is iterated, otherwise normal functionality is provided
local gsub = string.gsub
string.gsub = function( s, pattern, repl, n )
	if (type(pattern) == "string") then
		return gsub( s, pattern, repl, n )
	else
		for k,v in pairs(pattern) do
			s = string.gsub( s, k, v )
		end
		return s
	end
end

-- override print function to improve performance when running on device
print("utils.environment: ",system.getInfo("environment"))
if (system.getInfo("environment") == "device") then
	print("disabling print()")
	print = function() end
end

-- rounds up to the nearest multiple
math.nearest = function( number, multiple )
	return math.round( (number / multiple) ) * multiple
end

-- cancels multiple timers
local _timerCancel = timer.cancel
timer.cancel = function( ... )
	for i=1, #arg do
		if (arg[i]) then
			_timerCancel( arg[i] )
		end
	end
end

return utils
