local Functions = {}
local syn = syn or {request = request}
local getgc = getgc
local old = tick()
local Players = game:GetService("Players")
local LocalPlayer = Players.LocalPlayer
local json = {}
local sha256 = { }
local issentinelclosure = issentinelclosure or is_synapse_function

local MOD = 2^32
local MODM = MOD-1

local function memoize(f)
	local mt = {}
	local t = setmetatable({}, mt)
	function mt:__index(k)
		local v = f(k)
		t[k] = v
		return v
	end
	return t
end

local function make_bitop_uncached(t, m)
	local function bitop(a, b)
		local res,p = 0,1
		while a ~= 0 and b ~= 0 do
			local am, bm = a % m, b % m
			res = res + t[am][bm] * p
			a = (a - am) / m
			b = (b - bm) / m
			p = p*m
		end
		res = res + (a + b) * p
		return res
	end
	return bitop
end

local function make_bitop(t)
	local op1 = make_bitop_uncached(t,2^1)
	local op2 = memoize(function(a) return memoize(function(b) return op1(a, b) end) end)
	return make_bitop_uncached(op2, 2 ^ (t.n or 1))
end

local bxor1 = make_bitop({[0] = {[0] = 0,[1] = 1}, [1] = {[0] = 1, [1] = 0}, n = 4})

local function bxor(a, b, c, ...)
	local z = nil
	if b then
		a = a % MOD
		b = b % MOD
		z = bxor1(a, b)
		if c then z = bxor(z, c, ...) end
		return z
	elseif a then return a % MOD
	else return 0 end
end

local function band(a, b, c, ...)
	local z
	if b then
		a = a % MOD
		b = b % MOD
		z = ((a + b) - bxor1(a,b)) / 2
		if c then z = bit32_band(z, c, ...) end
		return z
	elseif a then return a % MOD
	else return MODM end
end

local function bnot(x) return (-1 - x) % MOD end

local function rshift1(a, disp)
	if disp < 0 then return lshift(a,-disp) end
	return math.floor(a % 2 ^ 32 / 2 ^ disp)
end

local function rshift(x, disp)
	if disp > 31 or disp < -31 then return 0 end
	return rshift1(x % MOD, disp)
end

local function lshift(a, disp)
	if disp < 0 then return rshift(a,-disp) end 
	return (a * 2 ^ disp) % 2 ^ 32
end

local function rrotate(x, disp)
    x = x % MOD
    disp = disp % 32
    local low = band(x, 2 ^ disp - 1)
    return rshift(x, disp) + lshift(low, 32 - disp)
end

local k = {
	0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5,
	0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5,
	0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3,
	0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174,
	0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc,
	0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
	0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7,
	0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967,
	0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13,
	0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85,
	0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3,
	0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
	0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5,
	0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
	0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208,
	0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2,
}

local function str2hexa(s)
	return (string.gsub(s, ".", function(c) return string.format("%02x", string.byte(c)) end))
end

local function num2s(l, n)
	local s = ""
	for i = 1, n do
		local rem = l % 256
		s = string.char(rem) .. s
		l = (l - rem) / 256
	end
	return s
end

local function s232num(s, i)
	local n = 0
	for i = i, i + 3 do n = n*256 + string.byte(s, i) end
	return n
end

local function preproc(msg, len)
	local extra = 64 - ((len + 9) % 64)
	len = num2s(8 * len, 8)
	msg = msg .. "\128" .. string.rep("\0", extra) .. len
	assert(#msg % 64 == 0)
	return msg
end

local function initH256(H)
	H[1] = 0x6a09e667
	H[2] = 0xbb67ae85
	H[3] = 0x3c6ef372
	H[4] = 0xa54ff53a
	H[5] = 0x510e527f
	H[6] = 0x9b05688c
	H[7] = 0x1f83d9ab
	H[8] = 0x5be0cd19
	return H
end

local function digestblock(msg, i, H)
	local w = {}
	for j = 1, 16 do w[j] = s232num(msg, i + (j - 1)*4) end
	for j = 17, 64 do
		local v = w[j - 15]
		local s0 = bxor(rrotate(v, 7), rrotate(v, 18), rshift(v, 3))
		v = w[j - 2]
		w[j] = w[j - 16] + s0 + w[j - 7] + bxor(rrotate(v, 17), rrotate(v, 19), rshift(v, 10))
	end

	local a, b, c, d, e, f, g, h = H[1], H[2], H[3], H[4], H[5], H[6], H[7], H[8]
	for i = 1, 64 do
		local s0 = bxor(rrotate(a, 2), rrotate(a, 13), rrotate(a, 22))
		local maj = bxor(band(a, b), band(a, c), band(b, c))
		local t2 = s0 + maj
		local s1 = bxor(rrotate(e, 6), rrotate(e, 11), rrotate(e, 25))
		local ch = bxor (band(e, f), band(bnot(e), g))
		local t1 = h + s1 + ch + k[i] + w[i]
		h, g, f, e, d, c, b, a = g, f, e, d + t1, c, b, a, t1 + t2
	end

	H[1] = band(H[1] + a)
	H[2] = band(H[2] + b)
	H[3] = band(H[3] + c)
	H[4] = band(H[4] + d)
	H[5] = band(H[5] + e)
	H[6] = band(H[6] + f)
	H[7] = band(H[7] + g)
	H[8] = band(H[8] + h)
end
local function sha256(msg)
	msg = preproc(msg, #msg)
	local H = initH256({})
    for i = 1, #msg, 64 do 
        digestblock(msg, i, H) 
    end
	return str2hexa(num2s(H[1], 4) .. num2s(H[2], 4) .. num2s(H[3], 4) .. num2s(H[4], 4) .. num2s(H[5], 4) .. num2s(H[6], 4) .. num2s(H[7], 4) .. num2s(H[8], 4))
end

local function HMAC(Data, Key) 
    return sha256(Key .. Data .. Key)
end

local http_request = http_request;
if syn then
	http_request = syn.request
elseif SENTINEL_V2 then
	function http_request(tb)
		return {
			StatusCode = 200;
			Body = request(tb.Url, tb.Method, (tb.Body or ''))
		}
	end
end

if (not http_request) then
	return game:GetService('Players').LocalPlayer:Kick('Unable to find proper request function')
end
local function Get(url) 
	if issentinelclosure(tick) then return end
    local OldTick = tick()
    local Results = http_request({Url = url, Method = "GET"}).Body
	if tick() - OldTick <= 0.01 then 
		setreadonly(getrawmetatable(game), false)
		getrawmetatable(game).__gc = false
		return wait(9e9)
    end
    return Results
end
json = { _version = "0.1.2" }
local encode

local escape_char_map = {
	[ "\\" ] = "\\\\",
	[ "\"" ] = "\\\"",
	[ "\b" ] = "\\b",
	[ "\f" ] = "\\f",
	[ "\n" ] = "\\n",
	[ "\r" ] = "\\r",
	[ "\t" ] = "\\t",
}

local escape_char_map_inv = { [ "\\/" ] = "/" }
for k, v in pairs(escape_char_map) do
	escape_char_map_inv[v] = k
end


local function escape_char(c)
	return escape_char_map[c] or string.format("\\u%04x", c:byte())
end


local function encode_nil(val)
	return "null"
end


local function encode_table(val, stack)
	local res = {}
	stack = stack or {}

	-- Circular reference?
	if stack[val] then error("circular reference") end

	stack[val] = true

	if rawget(val, 1) ~= nil or next(val) == nil then
		-- Treat as array -- check keys are valid and it is not sparse
		local n = 0
		for k in pairs(val) do
			if type(k) ~= "number" then
				error("invalid table: mixed or invalid key types")
			end
			n = n + 1
		end
		if n ~= #val then
			error("invalid table: sparse array")
		end
		-- Encode
		for i, v in ipairs(val) do
			table.insert(res, encode(v, stack))
		end
		stack[val] = nil
		return "[" .. table.concat(res, ",") .. "]"
	else
		-- Treat as an object
		for k, v in pairs(val) do
			if type(k) ~= "string" then
				error("invalid table: mixed or invalid key types")
			end
			table.insert(res, encode(k, stack) .. ":" .. encode(v, stack))
		end
		stack[val] = nil
		return "{" .. table.concat(res, ",") .. "}"
	end
end


local function encode_string(val)
	return '"' .. val:gsub('[%z\1-\31\\"]', escape_char) .. '"'
end


local function encode_number(val)
	-- Check for NaN, -inf and inf
	if val ~= val or val <= -math.huge or val >= math.huge then
		error("unexpected number value '" .. tostring(val) .. "'")
	end
	return string.format("%.14g", val)
end


local type_func_map = {
	[ "nil"     ] = encode_nil,
	[ "table"   ] = encode_table,
	[ "string"  ] = encode_string,
	[ "number"  ] = encode_number,
	[ "boolean" ] = tostring,
}


encode = function(val, stack)
	local t = type(val)
	local f = type_func_map[t]
	if f then
		return f(val, stack)
	end
	error("unexpected type '" .. t .. "'")
end


local function json_encode(val)
	return ( encode(val) )
end


local parse

local function create_set(...)
	local res = {}
	for i = 1, select("#", ...) do
		res[ select(i, ...) ] = true
	end
	return res
end

local space_chars   = create_set(" ", "\t", "\r", "\n")
local delim_chars   = create_set(" ", "\t", "\r", "\n", "]", "}", ",")
local escape_chars  = create_set("\\", "/", '"', "b", "f", "n", "r", "t", "u")
local literals      = create_set("true", "false", "null")

local literal_map = {
	[ "true"  ] = true,
	[ "false" ] = false,
	[ "null"  ] = nil,
}


local function next_char(str, idx, set, negate)
	for i = idx, #str do
		if set[str:sub(i, i)] ~= negate then
			return i
		end
	end
	return #str + 1
end


local function decode_error(str, idx, msg)
	local line_count = 1
	local col_count = 1
	for i = 1, idx - 1 do
		col_count = col_count + 1
		if str:sub(i, i) == "\n" then
			line_count = line_count + 1
			col_count = 1
		end
	end
	error( string.format("%s at line %d col %d", msg, line_count, col_count) )
end


local function codepoint_to_utf8(n)
	-- http://scripts.sil.org/cms/scripts/page.php?site_id=nrsi&id=iws-appendixa
	local f = math.floor
	if n <= 0x7f then
		return string.char(n)
	elseif n <= 0x7ff then
		return string.char(f(n / 64) + 192, n % 64 + 128)
	elseif n <= 0xffff then
		return string.char(f(n / 4096) + 224, f(n % 4096 / 64) + 128, n % 64 + 128)
	elseif n <= 0x10ffff then
		return string.char(f(n / 262144) + 240, f(n % 262144 / 4096) + 128,
						f(n % 4096 / 64) + 128, n % 64 + 128)
	end
	error( string.format("invalid unicode codepoint '%x'", n) )
end


local function parse_unicode_escape(s)
	local n1 = tonumber( s:sub(3, 6),  16 )
	local n2 = tonumber( s:sub(9, 12), 16 )
	-- Surrogate pair?
	if n2 then
		return codepoint_to_utf8((n1 - 0xd800) * 0x400 + (n2 - 0xdc00) + 0x10000)
	else
		return codepoint_to_utf8(n1)
	end
end


local function parse_string(str, i)
	local has_unicode_escape = false
	local has_surrogate_escape = false
	local has_escape = false
	local last
	for j = i + 1, #str do
		local x = str:byte(j)

		if x < 32 then
			decode_error(str, j, "control character in string")
		end

		if last == 92 then -- "\\" (escape char)
			if x == 117 then -- "u" (unicode escape sequence)
				local hex = str:sub(j + 1, j + 5)
				if not hex:find("%x%x%x%x") then
					decode_error(str, j, "invalid unicode escape in string")
				end
				if hex:find("^[dD][89aAbB]") then
					has_surrogate_escape = true
				else
					has_unicode_escape = true
				end
			else
				local c = string.char(x)
				if not escape_chars[c] then
					decode_error(str, j, "invalid escape char '" .. c .. "' in string")
				end
				has_escape = true
			end
			last = nil
		elseif x == 34 then -- '"' (end of string)
			local s = str:sub(i + 1, j - 1)
			if has_surrogate_escape then
				s = s:gsub("\\u[dD][89aAbB]..\\u....", parse_unicode_escape)
			end
			if has_unicode_escape then
				s = s:gsub("\\u....", parse_unicode_escape)
			end
			if has_escape then
				s = s:gsub("\\.", escape_char_map_inv)
			end
			return s, j + 1
		else
			last = x
		end
	end
	decode_error(str, i, "expected closing quote for string")
end


local function parse_number(str, i)
	local x = next_char(str, i, delim_chars)
	local s = str:sub(i, x - 1)
	local n = tonumber(s)
	if not n then
		decode_error(str, i, "invalid number '" .. s .. "'")
	end
	return n, x
end


local function parse_literal(str, i)
	local x = next_char(str, i, delim_chars)
	local word = str:sub(i, x - 1)
	if not literals[word] then
		decode_error(str, i, "invalid literal '" .. word .. "'")
	end
	return literal_map[word], x
end


local function parse_array(str, i)
	local res = {}
	local n = 1
	i = i + 1
	while 1 do
		local x
		i = next_char(str, i, space_chars, true)
		-- Empty / end of array?
		if str:sub(i, i) == "]" then
			i = i + 1
			break
		end
		-- Read token
		x, i = parse(str, i)
		res[n] = x
		n = n + 1
		-- Next token
		i = next_char(str, i, space_chars, true)
		local chr = str:sub(i, i)
		i = i + 1
		if chr == "]" then break end
		if chr ~= "," then decode_error(str, i, "expected ']' or ','") end
	end
	return res, i
end


local function parse_object(str, i)
	local res = {}
	i = i + 1
	while 1 do
		local key, val
		i = next_char(str, i, space_chars, true)
		-- Empty / end of object?
		if str:sub(i, i) == "}" then
			i = i + 1
			break
		end
		-- Read key
		if str:sub(i, i) ~= '"' then
			decode_error(str, i, "expected string for key")
		end
		key, i = parse(str, i)
		-- Read ':' delimiter
		i = next_char(str, i, space_chars, true)
		if str:sub(i, i) ~= ":" then
			decode_error(str, i, "expected ':' after key")
		end
		i = next_char(str, i + 1, space_chars, true)
		-- Read value
		val, i = parse(str, i)
		-- Set
		res[key] = val
		-- Next token
		i = next_char(str, i, space_chars, true)
		local chr = str:sub(i, i)
		i = i + 1
		if chr == "}" then break end
		if chr ~= "," then decode_error(str, i, "expected '}' or ','") end
	end
	return res, i
end


local char_func_map = {
	[ '"' ] = parse_string,
	[ "0" ] = parse_number,
	[ "1" ] = parse_number,
	[ "2" ] = parse_number,
	[ "3" ] = parse_number,
	[ "4" ] = parse_number,
	[ "5" ] = parse_number,
	[ "6" ] = parse_number,
	[ "7" ] = parse_number,
	[ "8" ] = parse_number,
	[ "9" ] = parse_number,
	[ "-" ] = parse_number,
	[ "t" ] = parse_literal,
	[ "f" ] = parse_literal,
	[ "n" ] = parse_literal,
	[ "[" ] = parse_array,
	[ "{" ] = parse_object,
}


parse = function(str, idx)
	local chr = str:sub(idx, idx)
	local f = char_func_map[chr]
	if f then
		return f(str, idx)
	end
	decode_error(str, idx, "unexpected character '" .. chr .. "'")
end


local function json_decode(str)
	if type(str) ~= "string" then
		error("expected argument of type string, got " .. type(str))
	end
	local res, idx = parse(str, next_char(str, 1, space_chars, true))
	idx = next_char(str, idx, space_chars, true)
	if idx <= #str then
		decode_error(str, idx, "trailing garbage")
	end
	return res
end
local pseudo_random = {}
function pseudo_random:new()
    local _new = {}

    local serialization = {}
    function serialization:new()
        local funcs = {}
        function funcs:serialize(s)
            local cs = {}
            for c in s:gmatch(".") do
                table.insert(cs, string.byte(c))
            end
            return cs
        end
        function funcs:deserialize(cs)
            local n_cs = {}
            for i, v in next, cs do
                table.insert(n_cs, string.char(v))
            end
            return table.concat(n_cs)
        end
        return funcs
    end

    local function new_offset(l)
        l = l or 12
        local function g() return tostring({}):match("table%p 0x(.+)") end
        local offset = ""
        repeat
            offset = offset .. g()
        until #offset >= l
        return offset:sub(1, l)
    end

    local ser = serialization:new()

    function _new:random(a, b)
        local offset = new_offset(b)
        local serialized = ser:serialize(offset)

        local n = 0
        table.foreach(serialized, function(index, value)
            n = n + value
        end)

        return (n % (b - a)) + a
    end

    function _new:randomstring(l)
		l = l or 8
        local cs = {}

        do -- Insert chars
            for i = ("a"):byte(), ("z"):byte() do
                table.insert(cs, string.char(i))
            end
            for i = ("A"):byte(), ("Z"):byte() do
                table.insert(cs, string.char(i))
            end
            for i = ("1"):byte(), ("9"):byte() do
                table.insert(cs, string.char(i))
            end
        end

        local n_cs = {}
        for i = 1, l do 
            table.insert(n_cs, cs[self:random(1, #cs)])
        end
        return table.concat(n_cs)
    end

    return _new
end
local function Get(Url) 
    return syn.request({
        ["Url"] = Url,
        ["Method"] = "GET",
        ["Headers"] = {
            Username = Players.LocalPlayer.Name
        }
    }).Body
end
local function CreateEvent() 
    local Event = {}
    Event.EventTable = {}
    Event.Callbacks = {}
    function Event.EventTable:Connect(func) 
        table.insert(Event.Callbacks, func)
    end
    function Event.EventTable:connect(func) 
        table.insert(Event.Callbacks, func)
    end
    function Event:Fire(...)
        for i, v in next, Event.Callbacks do 
            v(...)
        end
    end
    return Event
end
local RandomLib = pseudo_random:new()
local function GetClient() 
    local Client = {}
    local Events = {}
    Events.OnStatusChange = CreateEvent()
    Events.Completed = CreateEvent()
    Client.Events = {}
    for i, v in next, Events do 
        Client.Events[i] = v.EventTable
    end
    function Client:CheckWhitelist(Key) 
        Events.OnStatusChange:Fire("Checking whitelist")
        local VerificationString = RandomLib:randomstring(16)
        local HashToSend = sha256(VerificationString)
        local Response = Get("http://seataps.com/Whitelist/Verify?Key=" .. Key .. "&Hash=" .. HashToSend)
        local ResponseTable = json_decode(Response)
        local ServerHash = HMAC(HashToSend, "mDtdtFuD7wcSf3L7")
        if ResponseTable.Success then 
            if ResponseTable.Hash == ServerHash then 
                Events.Completed:Fire()
            else
                Events.OnStatusChange:Fire("Verification hash check failed")
            end
        else
            Events.OnStatusChange:Fire(ResponseTable.Reason)
        end
        wait(1)
        Events.OnStatusChange:Fire("")
    end
    return Client
end