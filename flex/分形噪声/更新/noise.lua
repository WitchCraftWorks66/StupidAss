
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

--[[
0: basic
1: turbulent smooth
2: turbulent basic
3: turbulent sharp
4: dynamic
]]
noise.fractal_type=0

--[[
0: block
1: linear
2: softlinear
3: spline
]]
noise.type=3

noise.rnd_seed=233

function noise.set_seed(seed)
	if seed then math.randomseed(seed) end noise.rnd_seed=math.random(23333)*math.random()
end

noise.invert=false

noise.contrast=1

noise.comp_val=0.5

noise.brightness=0

--[[
0: clip
1: soft clamp
2: wrap back
3: allow hdr results
]]
noise.overflow=0

noise.rotation=0

noise.complexity=5

noise.sub_influence=0.7

noise.sub_scale=0.57

noise.sub_rotation=0

noise.sub_xoff=0

noise.sub_yoff=0

noise.center_subscale=false

noise.evo=0

noise.cycle=false


local function mod(x,y) return x-y*math.floor(x/y) end

local function fract(n,_)
	if type(n)=="table" then
		for i=1,#n do
			_,n[i]=math.modf(n[i]) n[i]=n[i]<0 and n[i]+1 or n[i]
		end
	else _,n=math.modf(n) n=n<0 and n+1 or n
	end
	return n
end

local function hash33(v)
	local v=fract({v[1]*0.1031,v[2]*0.1030,v[3]*0.0973})
	local n=(v[2]+23.33)*v[1]+(v[1]+23.33)*v[2]+(v[3]+23.33)*v[3]
	v={v[1]+n,v[2]+n,v[3]+n}
	return fract({(v[1]+v[2])*v[3],v[1]*2*v[2],(v[1]+v[2])*v[1]})
end

local function bicubic(a,b,c,d,f)
	local f2=1-f
	local w0=1/6*f2^3
	local w1=2/3-0.5*f^2*(2-f)
	local w2=2/3-0.5*f2^2*(1+f)
	local w3=1/6*f^3
	return w0*a+w1*b+w2*c+w3*d
end

local function wrap(value,min,max)
	return mod(value-min,max-min)+min
end

local function tur_basic(f)
	return math.abs((f-0.5)*1.2)*2
end

local function tur_smooth(f)
	local t=tur_basic(f)
	return t^2
end

local function tur_sharp(f)
	local t=tur_basic(f)
	return t^0.5
end

local function select_fractal(f,typ)
	if typ==0 then
		return f
	elseif typ==1 then
		return tur_smooth(f)
	elseif typ==2 then
		return tur_basic(f)
	elseif typ==3 then
		return tur_sharp(f)
	else return f
	end
end

local function select_cycle(value,base,periode)
	local ret=0
	if noise.cycle then
		ret=wrap(value,base,base+periode)
	else ret=value
	end
	return ret
end

local function block(x,y,depth,evo,cycle,center_subscale)
	local rnd=noise.rnd_seed if center_subscale then rnd=rnd+depth end
	local hash=hash33({floor(x),floor(y),rnd}) local freq,periode=hash[1],1
	if cycle then
		freq=periode/cycle
	end
	evo=evo*freq+hash[2] local e,f=rnd+floor(evo),fract(evo)
	local a=hash33({floor(x),floor(y),select_cycle(e-1,rnd,periode)})
	local b=hash33({floor(x),floor(y),select_cycle(e,rnd,periode)})
	local c=hash33({floor(x),floor(y),select_cycle(e+1,rnd,periode)})
	local d=hash33({floor(x),floor(y),select_cycle(e+2,rnd,periode)})
	return bicubic(a[1],b[1],c[1],d[1],f)
end

local function linear(x,y,depth,evo,cycle,center_subscale)
	local tl=block(x,y,depth,evo,cycle,center_subscale)
	local tr=block(x+1,y,depth,evo,cycle,center_subscale)
	local bl=block(x,y+1,depth,evo,cycle,center_subscale)
	local br=block(x+1,y+1,depth,evo,cycle,center_subscale)
	local fx,fy=fract(x),fract(y)
	return lerp(fy,lerp(fx,tl,tr), lerp(fx,bl,br))
end

local function soft_linear(x,y,depth,evo,cycle,center_subscale)
	local tl=block(x,y,depth,evo,cycle,center_subscale)
	local tr=block(x+1,y,depth,evo,cycle,center_subscale)
	local bl=block(x,y+1,depth,evo,cycle,center_subscale)
	local br=block(x+1,y+1,depth,evo,cycle,center_subscale)
	local fx,fy=fract(x),fract(y) fx,fy=3*fx^2-2*fx^3,3*fy^2-2*fy^3
	return lerp(fy,lerp(fx,tl,tr), lerp(fx,bl,br))
end

local function spline(x,y,depth,evo,cycle,center_subscale)
	local ttll=block(x-1,y-1,depth,evo,cycle,center_subscale)
	local ttl=block(x,y-1,depth,evo,cycle,center_subscale)
	local ttr=block(x+1,y-1,depth,evo,cycle,center_subscale)
	local ttrr=block(x+2,y-1,depth,evo,cycle,center_subscale)

	local tll=block(x-1,y,depth,evo,cycle,center_subscale)
	local tl=block(x,y,depth,evo,cycle,center_subscale)
	local tr=block(x+1,y,depth,evo,cycle,center_subscale)
	local trr=block(x+2,y,depth,evo,cycle,center_subscale)

	local bll=block(x-1,y+1,depth,evo,cycle,center_subscale)
	local bl=block(x,y+1,depth,evo,cycle,center_subscale)
	local br=block(x+1,y+1,depth,evo,cycle,center_subscale)
	local brr=block(x+2,y+1,depth,evo,cycle,center_subscale)

	local bbll=block(x-1,y+2,depth,evo,cycle,center_subscale)
	local bbl=block(x,y+2,depth,evo,cycle,center_subscale)
	local bbr=block(x+1,y+2,depth,evo,cycle,center_subscale)
	local bbrr=block(x+2,y+2,depth,evo,cycle,center_subscale)
	local fx,fy=fract(x),fract(y)
	local tt=bicubic(ttll,ttl,ttr,ttrr,fx) local t=bicubic(tll,tl,tr,trr,fx)
	local b=bicubic(bll,bl,br,brr,fx) local bb=bicubic(bbll,bbl,bbr,bbrr,fx)
	local ret=bicubic(tt,t,b,bb,fy)
	return (ret-0.5)*1.5+0.5
end

local function clip(value)
	return value<0 and 0 or value>1 and 1 or value
end

local function soft_clamp(value)
	return 1/(1+math.exp(2-4*value))
end

local function wrap_back(value)
	return math.abs(value-2*floor(value*0.5+0.5))
end

local function allow_hdr(value)
	return value
end

local function select_overflow(value,overflow)
	if overflow==0 then
		return clip(value)
	elseif overflow==1 then
		return soft_clamp(value)
	elseif overflow==1 then
		return wrap_back(value)
	else
		return allow_hdr(value)
	end
end

local function layer(fractal_type,noise_type,x,y,depth,evo,cycle,center_subscale)
	local ret=0
	if noise_type==0 then
		ret=block(x,y,depth,evo,cycle,center_subscale)
	elseif noise_type==1 then
		ret=linear(x,y,depth,evo,cycle,center_subscale)
	elseif noise_type==2 then
		ret=soft_linear(x,y,depth,evo,cycle,center_subscale)
	elseif noise_type==3 then
		ret=spline(x,y,depth,evo,cycle,center_subscale)
	else
		ret=spline(x,y,depth,evo,cycle,center_subscale)
	end
	return select_fractal(ret,fractal_type)
end

function noise.fractal(x,y,scale_x,scale_y,depth,evo,cycle)
	local evo=evo or noise.evo local cycle=cycle or noise.cycle
	local center_subscale=noise.center_subscale local depth=depth or noise.complexity
	local fractal_type,noise_type=noise.fractal_type,noise.type
	local x1=x*math.cos(math.rad(noise.rotation))-y*math.sin(math.rad(noise.rotation))
	local y1=x*math.sin(math.rad(noise.rotation))+y*math.cos(math.rad(noise.rotation))
	local val,tot_weight=0,0 local fsc,xoff,yoff,roll,weight=1,0,0,0,1 local new_x,new_y
	for i=1,depth do
		x1,y1=(x1+xoff)/fsc,(y1+yoff)/fsc
		new_x=x1*math.cos(math.rad(roll))-y1*math.sin(math.rad(roll))
		new_y=x1*math.sin(math.rad(roll))+y1*math.cos(math.rad(roll))
		val=val+layer(fractal_type,noise_type,new_x/scale_x,new_y/scale_y,i,evo,cycle,center_subscale)*weight
		xoff,yoff=xoff+noise.sub_xoff,yoff+noise.sub_yoff
		fsc=fsc*noise.sub_scale roll=noise.sub_rotation+roll
		tot_weight=tot_weight+weight weight=weight*noise.sub_influence
	end
	local f=fract(depth)
	x1,y1=(x1+xoff)/fsc,(y1+yoff)/fsc
	new_x=x1*math.cos(math.rad(roll))-y1*math.sin(math.rad(roll))
	new_y=x1*math.sin(math.rad(roll))+y1*math.cos(math.rad(roll))
	val=val+layer(fractal_type,noise_type,new_x/scale_x,new_y/scale_y,floor(depth)+1,evo,cycle,center_subscale)*weight*f
	tot_weight=tot_weight+weight*f val=val/tot_weight if noise.invert then val=1-val end
	val=(val-noise.comp_val)*noise.contrast+noise.comp_val val=select_overflow(val+noise.brightness,noise.overflow)
	return (val-0.5)*2,val
end

function noise.re_map_range(val,a,b,new_a,new_b)
	return (val-a)/(b-a)*(new_b-new_a)+new_a
end










_G.noise=noise
return _G.noise

