
local noise={perm={}}

local floor=math.floor

local function fade(t)
	return t*t*t*(t*(t*6-15)+10)
end

local function lerp(t,a,b)
	return a+t*(b-a)
end

local function grad1d(hash,x)
	local h=bit.band(hash,15)
	return (bit.band(h,1)==0 and x or -x)
end

function noise.perlin1d(x)
	local X=bit.band(floor(x),255) x=x-floor(x)
	return lerp(fade(x),grad1d(noise.perm[X],x),grad1d(noise.perm[X+1],x-1))*2
end

local function grad2d(hash,x,y)
	local h=bit.band(hash,15)
	local u,v=h<8 and x or y,h<4 and y or x
	return (bit.band(h,1)==0 and u or -u)+(bit.band(h,2)==0 and v or -v)
end

function noise.perlin2d(x,y)
	local X,Y=bit.band(floor(x),255),bit.band(floor(y),255) x,y=x-floor(x),y-floor(y)
	local u,v=fade(x),fade(y)
	local A=noise.perm[X]+Y
	local B=noise.perm[X+1]+Y
	local a1=lerp(u,grad2d(noise.perm[A],x,y),grad2d(noise.perm[B],x-1,y))
	local a2=lerp(u,grad2d(noise.perm[A+1],x,y-1),grad2d(noise.perm[B+1],x-1,y-1))
	return lerp(v,a1,a2)
end

local function grad3d(hash,x,y,z)
	local h=bit.band(hash,15)
	local u,v=h<8 and x or y,h<4 and y or ((h==12 or h==14) and x or z)
	return (bit.band(h,1)==0 and u or -u)+(bit.band(h,2)==0 and v or -v)
end

function noise.perlin3d(x,y,z)
	local X,Y,Z=bit.band(floor(x),255),bit.band(floor(y),255),bit.band(floor(z),255)
	x,y,z=x-floor(x),y-floor(y),z-floor(z)
	local u,v,w=fade(x),fade(y),fade(z)
	local A=noise.perm[X]+Y
	local AA=noise.perm[A]+Z
	local AB=noise.perm[A+1]+Z
	local B=noise.perm[X+1]+Y
	local BA=noise.perm[B]+Z
	local BB=noise.perm[B+1]+Z
	local a1=lerp(u,grad3d(noise.perm[AA],x,y,z),grad3d(noise.perm[BA],x-1,y,z))
	local a2=lerp(u,grad3d(noise.perm[AB],x,y-1,z),grad3d(noise.perm[BB],x-1,y-1,z))
	local b1=lerp(u,grad3d(noise.perm[AA+1],x,y,z-1),grad3d(noise.perm[BA+1],x-1,y,z-1))
	local b2=lerp(u,grad3d(noise.perm[AB+1],x,y-1,z-1),grad3d(noise.perm[BB+1],x-1,y-1,z-1))
	return lerp(w,lerp(v,a1,a2),lerp(v,b1,b2))
end

function noise.fbm(x,y,z,func,octaves,lacunarity,gain)
	octaves=octaves or 8
	lacunarity=lacunarity or 2
	gain=gain or 0.5 func=func or noise.perlin3d
	local amplitude=1.0
	local frequency=1.0
	local sum=0.0 local max=0
	for _=0,octaves do
		sum=sum+amplitude*func(x*frequency,y*frequency,z*frequency)
		max=max+amplitude amplitude=amplitude*gain
		frequency=frequency*lacunarity
	end
	return sum/max
end

function noise.get_perm(seed)
	if seed then math.randomseed(seed) end
	for i=0,255 do
		noise.perm[i]=math.random(0,255) noise.perm[i+256]=noise.perm[i]
	end
end

function noise.initialize()
	noise.get_perm()
end

noise.initialize()








_G.noise=noise
return _G.noise

