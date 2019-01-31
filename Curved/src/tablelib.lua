-- table library extensions

-- allows the table.copy function to return a shallow copy of a range of the table items
-- Example:
--local t={1,2,3,4,5,6,7,8,9,10}
--dump(table.copy(t,3,8))
--prints out 3,4,5,6,7,8
local table_copy = table.copy
table.copy = function( t, ... )
	if (type(arg[1]) == "number" and (arg[2] == nil or type(arg[2]) == "number")) then
		local startIndex, endIndex = arg[1], arg[2]
		if (endIndex == nil) then endIndex = #t end
		local output = {}
		for i=startIndex, endIndex do
			output[ #output+1 ] = t[i]
		end
		return output
	else
		return table_copy( t, unpack( arg ) )
	end
end

-- like table.copy except this copies entries returned by pairs()
-- multiple tables can be combined but beware of overwriting entries
table.clone = function( ... )
	local output = {}
	for i=1, #arg do
		for k,v in pairs(arg[i]) do
			output[ k ] = v
		end
	end
	return output
end

table.deepclone = function( tbl )
	local output = {}
	for k,v in pairs(tbl) do
		if (type(v) == "table") then
			output[k] = table.deepclone(v)
		else
			output[k] = v
		end
	end
	return output
end

-- returns a new table containing the elements of the passed table which the compare function returned true for
-- works with display groups
table.find = function( t, compare )
	local newT = {}
	local count = t.numChildren
	
	if (count == nil) then
		count = #t
	end
	
	for i=1, count do
		local item = t[i]
		if (compare(item)) then
			newT[#newT+1] = item
		end
	end
	
	return newT
end

-- if sep is a table its items are used as separators for table t
-- Example:
--local a,b = {1,3,5,7,9}, {2,4,6,8,10}
--print(table.concat(a,b))
-- prints 12345678910
local table_concat = table.concat
table.concat = function( t, sep, i, j )
	if (i == nil and j == nil and type(sep) == "table") then
		return strZip( t, sep )
	else
		return table_concat( t, sep, i, j )
	end
end

-- extends the table.indexOf to work with display groups
local _indexof = table.indexOf
table.indexOf = function( tbl, element )
	if (tbl.numChildren == nil) then
		return _indexof( tbl, element )
	else
		for i=1, tbl.numChildren do
			if (tbl[i] == element) then
				return i
			end
		end
		return nil
	end
end

-- returns shallow copy of table elements from index for count of size (or to end if size is nil)
table.range = function( tbl, index, size )
	if (index == nil or index < 1) then return nil end
	size = size or #tbl-index+1
	local output = {}
	for i=index, index+size-1 do
		output[i] = tbl[i]
		dump(tbl[i])
		dump(output[i])
	end
	return output
end

-- extends table.remove to remove objects directly, without requiring table.indexOf
local _remove = table.remove
table.remove = function( t, pos )
	if (type(pos) == "number") then
		return _remove( t, pos )
	else
		pos = table.indexOf( t, pos )
		return _remove( t, pos )
	end
end

-- replaces entries of old with new
-- returns number of entries replaced
table.replace = function( tbl, old, new )
	local index = table.indexOf( tbl, old )
	local count = 0
	while (index) do
		count = count + 1
		tbl[index] = new
		index = table.indexOf( tbl, old )
	end
	return count
end

-- implements #+1 for speed when no index is passed
local _insert = table.insert
table.insert = function( t, pos, value )
	if (value == nil) then
		-- for speed
		t[ #t+1 ] = pos
	else
		-- original slow insert (good for middle inserts)
		_insert( t, pos, value )
	end
end

-- allows display groups to be sorted
local sort = table.sort
table.sort = function( t, compare )
	if (t.numChildren == nil) then
		sort( t, compare )
	else
		local tbl = {}
		for i=1, t.numChildren do
			tbl[#tbl+1] = t[i]
		end
		sort( tbl, compare )
		for i=1, #tbl do
			t:insert( tbl[i] )
		end
		return t
	end
end

-- reverses the order of the items in the table
local function reverse( t )
	local tbl = {}
	for i=#t, 1, -1 do
		tbl[#tbl+1] = t[i]
	end
	return tbl
end
table.reverse = reverse

-- pops # items off the top of the table (top == [1])
table.pop = function( tbl, count )
	local output = {}
	for i=1, count do
		output[#output+1] = table.remove(tbl,1)
	end
	return unpack(output)
end

-- pushes items onto tbl at bottom ([#tbl])
table.push = function( tbl, ... )
	for i=1, #arg do
		tbl[#tbl+1] = arg[i]
	end
end

-- concatenates the key value pairs of a table into a single string with optional separator
-- sep: separator between pairs, default: ', '
-- equ: separator key and value, default: '='
-- incKeys: false to exclude keys, default: true
-- incVals: false to exclude values, default: true
table.concatPairs = function( tbl, sep, equ, incKeys, incVals )
	local str = ""
	
	if (sep == nil) then sep = ', ' end
	if (equ == nil) then equ = '=' end
	if (incKeys == nil) then incKeys = true end
	if (incVals == nil) then incVals = true end
	
	for k,v in pairs(tbl) do
		if (str ~= "") then
			str = str .. sep
		end
		if (incKeys and incVals) then
			str = str .. k .. equ .. v
		elseif (incKeys) then
			str = str .. k
		elseif (incVals) then
			str = str .. v
		end
	end
	
	return str
end

--[[
	Compares two tables and returns true if they contain the same values, false if not.
	Does not do a deep comparison - only using pairs() function to retrieve keys.
	
	Parameters:
		a: first table to compare
		b: second table to compare
	
	Returns:
		true if both contain the same values
]]--
local function compare( a, b )
	for k,v in pairs(a) do
		if (a[k] ~= b[k]) then
			return false
		end
	end
	for k,v in pairs(b) do
		if (b[k] ~= a[k]) then
			return false
		end
	end
	return true
end
table.compare = compare
