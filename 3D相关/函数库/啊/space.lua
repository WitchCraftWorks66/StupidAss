
--代码写的很烂，基本没有更改过，希望大家不要嫌弃
--函数库介绍和字幕示例见：https://space.bilibili.com/346816900/channel/detail?cid=81879


local space={}

space.math={}

function space.abs_sin(angle,h)
	if h then
		return h*math.abs(math.sin(angle))
	end
	return math.abs(math.sin(angle))
end

function space.abs_sin2(angle,h)
	if h then
		return h*math.abs(math.sin(math.rad(angle)))
	end
	return math.abs(math.sin(math.rad(angle)))
end

function space.abs_cos(angle,h)
	if h then
		return h*math.abs(math.cos(angle))
	end
	return math.abs(math.cos(angle))
end

function space.abs_cos2(angle,h)
	if h then
		return h*math.abs(math.cos(math.rad(angle)))
	end
	return math.abs(math.cos(math.rad(angle)))
end

function space.math.round(n,digit)
	if digit and digit>=1 then
		digit=10^math.floor(digit)
		return math.floor(n*digit+0.5)/digit
		else return math.floor(n+0.5)
	end
end

local math_round=space.math.round

function space.v_nor(v)
	return (v[1]^2+v[2]^2+v[3]^2)^0.5
end

function space.matrix_mul(tbl,tbl2)
	local mul={}
	for i=1,#tbl do
		mul[i]=0
		for p=1,#tbl2 do
			mul[i]=mul[i]+tbl[i][p]*tbl2[p]
		end
	end
	return mul
end

local matrix_mul=space.matrix_mul

function space.points_int(points)
	for i=1,#points do
		for p=1,3 do
			points[i][p]=math_round(points[i][p],1)
		end
	end
	return points
end

function space.dot_product(v1,v2)
	local sum=0
	for i=1,#v1 do
		sum=sum+v1[i]*v2[i]
	end
	return sum
end

function space.cross_product(v1,v2)
	local v={}
	v[1]=v1[2]*v2[3]-v1[3]*v2[2]
	v[2]=v1[3]*v2[1]-v1[1]*v2[3]
	v[3]=v1[1]*v2[2]-v1[2]*v2[1]
	return v
end

function space.v_degree(v1,v2)
	return math.deg(math.acos(space.dot_product(v1,v2)/(space.v_nor(v1)*space.v_nor(v2))))
end

function space.bounding(planes)
	local x,y,z={},{},{}
	for i=1,#planes do
		for p=1,#planes[i] do
			x[#x+1]=planes[i][p][1] y[#y+1]=planes[i][p][2] z[#z+1]=planes[i][p][3]
		end
	end
	local xmax,ymax,zmax=math.max(table.unpack(x)),math.max(table.unpack(y)),math.max(table.unpack(z))
	local xmin,ymin,zmin=math.min(table.unpack(x)),math.min(table.unpack(y)),math.min(table.unpack(z))
	return {math_round(xmax,3),math_round(ymax,3),math_round(zmax,3),math_round(xmin,3),math_round(ymin,3),math_round(zmin,3)}
end

function space.center_pos(points)
	local c=space.bounding(points)
	c={(c[1]+c[4])/2,(c[2]+c[5])/2,(c[3]+c[6])/2}
	return c
end

function space.vector(p,p2)
	return {p2[1]-p[1],p2[2]-p[2],p2[3]-p[3]}
end

function space.nor_v(points)
	local v1=space.vector(points[1],points[2])
	local v2=space.vector(points[2],points[3])
	local n=space.cross_product(v1,v2)
	return n
end

function space.unit_vector(v)
	local v_normal=space.v_nor(v)
	return {v[1]/v_normal,v[2]/v_normal,v[3]/v_normal}
end

function space.visibility(plane,center_pos)
	local lay=0 local n=space.nor_v(plane) local v=space.vector(center_pos,plane[1])
	local cos=space.dot_product(n,v)/(space.v_nor(n)*space.v_nor(v))
	if cos<0 then n={-n[1],-n[2],-n[3]} end if n[3]>0 then lay=1 else lay=0 end
	return lay,n
end

function space.frx(p,angle)
	local ang=math.rad(angle) local points={}
	local frx={{1,0,0},{0,math.cos(ang),-math.sin(ang)},{0,math.sin(ang),math.cos(ang)}}
	for i=1,#p do
		points[i]=matrix_mul(frx,p[i])
	end
	return points
end

function space.frz(p,angle)
	local ang=math.rad(-angle) local points={}
	local frz={{math.cos(ang),-math.sin(ang),0},{math.sin(ang),math.cos(ang),0},{0,0,1}}
	for i=1,#p do
		points[i]=matrix_mul(frz,p[i])
	end
	return points
end

function space.fry(p,angle)
	local ang=math.rad(-angle) local points={}
	local fry={{math.cos(ang),0,math.sin(ang)},{0,1,0},{-math.sin(ang),0,math.cos(ang)}}
	for i=1,#p do
		points[i]=matrix_mul(fry,p[i])
	end
	return points
end

function space.fr_l(points,angle,axis)
	local ang=math.rad(angle) local p={}
	local fr_line={
		{
			math.cos(ang)+(1-math.cos(ang))*axis[1]^2,
			(1-math.cos(ang))*axis[1]*axis[2]-math.sin(ang)*axis[3],
			(1-math.cos(ang))*axis[1]*axis[3]+math.sin(ang)*axis[2]
		},
		{
			(1-math.cos(ang))*axis[1]*axis[2]+math.sin(ang)*axis[3],
			math.cos(ang)+(1-math.cos(ang))*axis[2]^2,
			(1-math.cos(ang))*axis[2]*axis[3]-math.sin(ang)*axis[1]
		},
		{
			(1-math.cos(ang))*axis[1]*axis[3]-math.sin(ang)*axis[2],
			(1-math.cos(ang))*axis[2]*axis[3]+math.sin(ang)*axis[1],
			math.cos(ang)+(1-math.cos(ang))*axis[3]^2
		}
	}
	for i=1,#points do
		p[i]=matrix_mul(fr_line,points[i])
	end
	return p
end

function space.rotate(points,angle,mode)
	local p={} local angle=angle or 0
	local mode=mode or {0.57735026918963,0.57735026918963,0.57735026918963}
	if mode=="x" then
		p=space.frx(points,angle)
	elseif mode=="y" then
		p=space.fry(points,angle)
	elseif mode=="z" then
		p=space.frz(points,angle)
	else p=space.fr_l(points,angle,mode)
	end
	return p
end

function space.rotate_offset(points,angle,mode,offset,offset2)
	local p={} local angle=angle or 0
	local mode=mode or {0.57735026918963,0.57735026918963,0.57735026918963}
	if mode=="x" then
		p=space.translate(space.frx(points,angle),0,offset,offset2)
	elseif mode=="y" then
		p=space.translate(space.fry(points,angle),offset,0,offset2)
	elseif mode=="z" then
		p=space.translate(space.frz(points,angle),offset,offset2,0)
	end
	return p
end

function space.scale(points,x,y,z,orgx,orgy,orgz)
	local orgx=orgx or 0 local orgy=orgy or 0
	local orgz=orgz or 0 local x=x or 1
	local y=y or x local z=z or x local p={}
	for i=1,#points do
		p[i]={orgx+(points[i][1]-orgx)*x,orgy+(points[i][2]-orgy)*y,orgz+(points[i][3]-orgz)*z}
	end
	return p
end

function space.translate(points,x,y,z)
	local x=x or 0 local y=y or 0
	local z=z or 0 local p={}
	for i=1,#points do
		p[i]={points[i][1]+x,points[i][2]+y,points[i][3]+z}
	end
	return p
end

function space.restore(points)
	local s={} local p=space.points_int(points)
	for i=1,#p do
		s[i]="l "..p[i][1].." "..p[i][2].." "
	end
	s=table.concat(s):gsub("^l","m")
	return s
end

function space.shape2points(ass_shape)
	local p={}
	for x,y in ass_shape:gmatch("([-.%d]+) ([-.%d]+)") do
		p[#p+1]={x,y,0}
	end
	return p
end

function space.v2v_info(v1,v2)
	local axis=space.unit_vector(space.cross_product(v1,v2))
	local angle=space.v_degree(v1,v2)
	if angle==0 or angle==180 then
		axis={0,0,0}
	end
	return axis,angle
end

function space.add_z(ass_shape,z)
	local z=z or 0
	local ass_shape=ass_shape:gsub("([-.%d]+ [-.%d]+)",
		function(xy)
			return xy.." "..z
		end
	)
	return ass_shape
end

function space.remove_z(ass_shape)
	local ass_shape=ass_shape:gsub("([-.%d]+) ([-.%d]+) [-.%d]+",
		function(x,y)
			return math_round(x,1).." "..math_round(y,1)
		end
	)
	return ass_shape
end

function space.roll(ass_shape,angle,axis)
	local ass_shape=ass_shape:gsub("([-.%d]+) ([-.%d]+) ([-.%d]+)",
		function(x,y,z)
			local point={{x,y,z}} local p=space.rotate(point,angle,axis)
			return math_round(p[1][1],3).." "..math_round(p[1][2],3).." "..math_round(p[1][3],3)
		end
	)
	return ass_shape
end

function space.fsc(ass_shape,fscx,fscy,fscz,orgx,orgy,orgz)
	local ass_shape=ass_shape:gsub("([-.%d]+) ([-.%d]+) ([-.%d]+)",
		function(x,y,z)
			local point={{x,y,z}} local p=space.scale(point,fscx,fscy,fscz,orgx,orgy,orgz)
			return math_round(p[1][1],3).." "..math_round(p[1][2],3).." "..math_round(p[1][3],3)
		end
	)
	return ass_shape
end

function space.shift(ass_shape,x,y,z)
	local ass_shape=ass_shape:gsub("([-.%d]+) ([-.%d]+) ([-.%d]+)",
		function(x1,y1,z1)
			local point={{x1,y1,z1}} local p=space.translate(point,x,y,z)
			return math_round(p[1][1],3).." "..math_round(p[1][2],3).." "..math_round(p[1][3],3)
		end
	)
	return ass_shape
end

function space.roll_2d(ass_shape,angle,axis)
	local ass_shape=ass_shape:gsub("([-.%d]+) ([-.%d]+)",
		function(x,y)
			local point={{x,y,0}} local p=space.rotate(point,angle,axis)
			return math_round(p[1][1],3).." "..math_round(p[1][2],3)
		end
	)
	return ass_shape
end

function space.fsc_2d(ass_shape,fscx,fscy,orgx,orgy)
	local ass_shape=ass_shape:gsub("([-.%d]+) ([-.%d]+)",
		function(x,y)
			local point={{x,y,0}} local p=space.scale(point,fscx,fscy,1,orgx,orgy)
			return math_round(p[1][1],3).." "..math_round(p[1][2],3)
		end
	)
	return ass_shape
end

function space.shift_2d(ass_shape,x,y)
	local ass_shape=ass_shape:gsub("([-.%d]+) ([-.%d]+)",
		function(x1,y1)
			local point={{x1,y1,0}} local p=space.translate(point,x,y)
			return math_round(p[1][1],3).." "..math_round(p[1][2],3)
		end
	)
	return ass_shape
end

function space.rotate_solid(solid,angle,mode)
	local new={}
	for i=1,#solid do
		new[i]=space.rotate(solid[i],angle,mode)
	end
	return new
end

function space.shift_solid(solid,x,y,z)
	local new={}
	for i=1,#solid do
		new[i]=space.translate(solid[i],x,y,z)
	end
	return new
end

function space.fsc_solid(solid,fscx,fscy,fscz,orgx,orgy,orgz)
	local new={}
	for i=1,#solid do
		new[i]=space.scale(solid[i],fscx,fscy,fscz,orgx,orgy,orgz)
	end
	return new
end

function space.rotate_point(point,angle,mode)
	local new={}
	new=space.rotate({point},angle,mode)
	return new[1]
end

function space.shift_point(point,x,y,z)
	local new={}
	new=space.translate({point},x,y,z)
	return new[1]
end

function space.fsc_point(point,fscx,fscy,fscz,orgx,orgy,orgz)
	local new={}
	new=space.scale({point},fscx,fscy,fscz,orgx,orgy,orgz)
	return new[1]
end

function space.rotate_solids(solids,angle,mode)
	local new={}
	for i=1,#solids do
		new[i]=space.rotate_solid(solids[i],angle,mode)
	end
	return new
end

function space.shift_solids(solids,x,y,z)
	local new={}
	for i=1,#solids do
		new[i]=space.shift_solid(solids[i],x,y,z)
	end
	return new
end

function space.fsc_solids(solids,fscx,fscy,fscz,orgx,orgy,orgz)
	local new={}
	for i=1,#solids do
		new[i]=space.fsc_solid(solids[i],fscx,fscy,fscz,orgx,orgy,orgz)
	end
	return new
end

function space.filter(plane,func)
	local set=space.tbl_copy(plane)
	if func then
		for i=1,#set do
			local x,y,z=func(set[i][1],set[i][2],set[i][3]) set[i]={x,y,z}
		end
	end
	return set
end

function space.filter_solid(solid,func)
	local set={}
	if func then
		for i=1,#solid do
			set[i]=space.filter(solid[i],func)
		end
	end
	return set
end

function space.color_brightness(color,pct)
	local color=color or "&HFFFFFF&"
	local b,g,r=color:sub(-7,-6),color:sub(-5,-4),color:sub(-3,-2)
	b,g,r=tonumber(b,16),tonumber(g,16),tonumber(r,16)
	local pct=pct>1 and 1 or (pct<0 and 0 or pct) b=b*pct g=g*pct r=r*pct
	return ("&H%02X%02X%02X&"):format(b,g,r)
end

function space.interpolate_color(clr,pct)
	local b,g,r={},{},{} local copies=1/(#clr-1) local pct=pct>1 and 1 or (pct<0 and 0 or pct)
	if pct==0 then return clr[1] end
	for i=1,#clr do
		b[i]=(clr[i]):sub(-7,-6) g[i]=(clr[i]):sub(-5,-4) r[i]=(clr[i]):sub(-3,-2)
		b[i]=tonumber(b[i],16) g[i]=tonumber(g[i],16) r[i]=tonumber(r[i],16)
	end
	local at=math.ceil(pct/copies) pct=(pct-copies*(at-1))/copies
	b=b[at]+(b[at+1]-b[at])*pct g=g[at]+(g[at+1]-g[at])*pct r=r[at]+(r[at+1]-r[at])*pct
	return ("&H%02X%02X%02X&"):format(b,g,r)
end

function space.shade(color,light,plane)
	local light=light or {0,0,1} local n=space.nor_v(plane)
	local cos=space.dot_product(light,n)/(space.v_nor(n)*space.v_nor(light))
	if cos<0 then n={-n[1],-n[2],-n[3]} end local angle=space.v_degree(light,n)
	return space.color_brightness(color,1.3-angle/90)
end

function space.tbl_copy(tbl)
	local new={}
	for i=1,#tbl do
		new[i]=tbl[i]
	end
	new.cp=tbl.cp or nil
	return new
end

function space.tbl_deep_copy(object)
	local lookup_table={}
	local function _copy(object)
		if type(object)~="table" then
			return object
		elseif lookup_table[object] then
			return lookup_table[object]
		end
		local new_table={}
		lookup_table[object]=new_table
		for key,value in pairs(object) do
			new_table[_copy(key)]=_copy(value)
		end
		return setmetatable(new_table,getmetatable(object))
	end
	return _copy(object)
end

function space.tbl_sort(tbl)
	for i=1,#tbl do
		for i2=1,#tbl-i do
			if tbl[i2+1][1]>tbl[i2][1] then
				tbl[i2+1],tbl[i2]=tbl[i2],tbl[i2+1]
			end
		end
	end
	return tbl
end

function space.cube(len,angle,axis,fsc,x,y,z,color,light)
	local p={{-1,-1,-1},{1,-1,-1},{1,1,-1},{-1,1,-1}} local s={}
	s[1]=p s[6]=space.translate(p,0,0,2) s[2]=space.rotate(p,90,"x")
	for i=3,5 do
		s[i]=space.rotate(space.rotate(p,90,"x"),90*(i-2),"z")
	end
	local center_pos={0,0,0}
	for i=1,6 do
		s[i]=space.scale(space.scale(s[i],len/2),fsc)
	end
	return space.info(s,center_pos,angle,axis,fsc,x,y,z,color,light),s
end

function space.pyramid(n,h,r,angle,axis,fsc,x,y,z,color,light)
	local deg=math.rad(360/n) local p={{}} local center_pos={0,0,0}
	for i=1,n do
		p[1][i]={math.cos((i-1/2)*deg)*r,math.sin((i-1/2)*deg)*r,-h/2}
	end
	for i=1,n-1 do
		p[i+1]={p[1][i],{0,0,h/2},p[1][i+1]}
	end
	p[#p+1]={p[1][n],{0,0,h/2},p[1][1]}
	return space.info(p,center_pos,angle,axis,fsc,x,y,z,color,light),p
end

function space.prism(n,h,r,angle,axis,fsc,x,y,z,color,light)
	local deg=math.rad(360/n) local p={{}} local center_pos={0,0,0}
	for i=1,n do
		p[1][i]={math.cos((i-1/2)*deg)*r,math.sin((i-1/2)*deg)*r,-h/2}
	end
	for i=1,n-1 do
		p[i+1]={p[1][i],{p[1][i][1],p[1][i][2],h/2},{p[1][i+1][1],p[1][i+1][2],h/2},p[1][i+1]}
	end
	p[#p+1]={p[1][n],{p[1][n][1],p[1][n][2],h/2},{p[1][1][1],p[1][1][2],h/2},p[1][1]}
	p[#p+1]=space.translate(p[1],0,0,h)
	return space.info(p,center_pos,angle,axis,fsc,x,y,z,color,light),p
end

function space.diamond(n,h1,h2,r1,r2,angle,axis,fsc,x,y,z,color,light)
	local deg=math.rad(360/n) local p={{}} local center_pos={0,0,0} local p2={{}}
	for i=1,n do
		p[1][i]={math.cos((i-1/2)*deg)*r1,math.sin((i-1/2)*deg)*r1,(h1+h2)/2}
		p2[1][i]={math.cos((i-1/8)*deg)*r2,math.sin((i-1/8)*deg)*r2,(h2-h1)/2}
	end
	for i=1,n-1 do
		p[#p+1]={p[1][i],p[1][i+1],p2[1][i]} p[#p+1]={p2[1][i],p[1][i+1],p2[1][i+1]}
		p[#p+1]={p2[1][i],p2[1][i+1],{0,0,-(h1+h2)/2}}
	end
	p[#p+1]={p[1][n],p[1][1],p2[1][n]} p[#p+1]={p2[1][n],p[1][1],p2[1][1]}
	p[#p+1]={p2[1][n],p2[1][1],{0,0,-(h1+h2)/2}}
	return space.info(p,center_pos,angle,axis,fsc,x,y,z,color,light),p
end

function space.bipyramid(n,h,r,angle,axis,fsc,x,y,z,color,light)
	local deg=math.rad(360/n) local s,p={},{} local cp={0,0,0}
	for i=1,n do
		s[i]={math.cos((i-1/2)*deg)*r,math.sin((i-1/2)*deg)*r,0}
	end
	for i=1,n-1 do
		p[#p+1]={s[i],{0,0,-h/2},s[i+1]} p[#p+1]={s[i],{0,0,h/2},s[i+1]}
	end
	p[#p+1]={s[n],{0,0,-h/2},s[1]} p[#p+1]={s[n],{0,0,h/2},s[1]}
	return space.info(p,cp,angle,axis,fsc,x,y,z,color,light),p
end

function space.frustum(n,h,r1,r2,angle,axis,fsc,x,y,z,color,light)
	local deg=math.rad(360/n) local p={{},{}} local center_pos={0,0,0}
	for i=1,n do
		p[1][i]={math.cos((i-1/2)*deg)*r1,math.sin((i-1/2)*deg)*r1,h/2}
		p[2][i]={math.cos((i-1/2)*deg)*r2,math.sin((i-1/2)*deg)*r2,-h/2}
	end
	for i=1,n-1 do
		p[#p+1]={p[1][i],p[1][i+1],p[2][i+1],p[2][i]}
	end
	p[#p+1]={p[1][n],p[1][1],p[2][1],p[2][n]}
	return space.info(p,center_pos,angle,axis,fsc,x,y,z,color,light),p
end

function space.bifrustum(n,h,r1,r2,angle,axis,fsc,x,y,z,color,light)
	local deg=math.rad(360/n) local p={{},{}} local cp,s={0,0,0},{} local r1=r1>r2 and r2 or r1
	for i=1,n do
		p[1][i]={math.cos((i-1/2)*deg)*r1,math.sin((i-1/2)*deg)*r1,-h/2}
		p[2][i]={math.cos((i-1/2)*deg)*r1,math.sin((i-1/2)*deg)*r1,h/2}
		s[i]={math.cos((i-1/2)*deg)*r2,math.sin((i-1/2)*deg)*r2,0}
	end
	for i=1,n-1 do
		p[#p+1]={p[1][i],p[1][i+1],s[i+1],s[i]} p[#p+1]={s[i],s[i+1],p[2][i+1],p[2][i]}
	end
	p[#p+1]={p[1][n],p[1][1],s[1],s[n]} p[#p+1]={s[n],s[1],p[2][1],p[2][n]}
	return space.info(p,cp,angle,axis,fsc,x,y,z,color,light),p
end

function space.icosahedron(r,angle,axis,fsc,x,y,z,color,light)
	local p={} local m=(50-10*5^0.5)^0.5/10*r local n=(50+10*5^0.5)^0.5/10*r local center_pos={0,0,0}
	local points={{m,0,n},{m,0,-n},{-m,0,n},{-m,0,-n},{0,n,m},{0,-n,m},{0,n,-m},{0,-n,-m},{n,m,0},{-n,m,0},{n,-m,0},{-n,-m,0}}
	p[1]={points[7],points[5],points[9]} p[2]={points[10],points[5],points[7]} p[3]={points[7],points[4],points[10]}
	p[4]={points[7],points[2],points[4]} p[5]={points[7],points[9],points[2]} p[6]={points[9],points[11],points[2]}
	p[7]={points[9],points[1],points[11]} p[8]={points[9],points[5],points[1]} p[9]={points[5],points[3],points[1]}
	p[10]={points[5],points[10],points[3]} p[11]={points[10],points[12],points[3]} p[12]={points[10],points[4],points[12]}
	p[13]={points[4],points[2],points[8]} p[14]={points[2],points[11],points[8]} p[15]={points[11],points[1],points[6]}
	p[16]={points[1],points[3],points[6]} p[17]={points[3],points[12],points[6]} p[18]={points[4],points[8],points[12]}
	p[19]={points[6],points[12],points[8]} p[20]={points[11],points[6],points[8]}
	return space.info(p,center_pos,angle,axis,fsc,x,y,z,color,light),p
end

function space.subdivide(r,p)
	local new={}
	for i=1,#p do
		local p1={(p[i][1][1]+p[i][2][1])/2,(p[i][1][2]+p[i][2][2])/2,(p[i][1][3]+p[i][2][3])/2} local l1=space.v_nor(p1)
		local p2={(p[i][1][1]+p[i][3][1])/2,(p[i][1][2]+p[i][3][2])/2,(p[i][1][3]+p[i][3][3])/2} local l2=space.v_nor(p2)
		local p3={(p[i][3][1]+p[i][2][1])/2,(p[i][3][2]+p[i][2][2])/2,(p[i][3][3]+p[i][2][3])/2} local l3=space.v_nor(p3)
		p1={p1[1]*r/l1,p1[2]*r/l1,p1[3]*r/l1} p2={p2[1]*r/l2,p2[2]*r/l2,p2[3]*r/l2} p3={p3[1]*r/l3,p3[2]*r/l3,p3[3]*r/l3}
		new[#new+1]={p[i][1],p1,p2} new[#new+1]={p1,p3,p2} new[#new+1]={p1,p[i][2],p3} new[#new+1]={p2,p3,p[i][3]}
	end
	return new
end

function space.build_sphere_subdivide(n,r)
	local p={} local _,solid=space.icosahedron(r,{},{})
	if n<1 then
		return solid
	elseif n==1 then
		p=space.subdivide(r,solid)
	else solid=space.build_sphere_subdivide(n-1,r) p=space.subdivide(r,solid)
	end
	return p
end

function space.sphere_subdivide(n,r,angle,axis,fsc,x,y,z,color,light)
	local center_pos={0,0,0} local p=space.build_sphere_subdivide(n,r)
	return space.info(p,center_pos,angle,axis,fsc,x,y,z,color,light),p
end

function space.to_subdivide(r,pct,p)
	local new={} local pct=pct<0 and 0 or (pct>1 and 1 or pct)
	for i=1,#p do
		local p1={(p[i][1][1]+p[i][2][1])/2,(p[i][1][2]+p[i][2][2])/2,(p[i][1][3]+p[i][2][3])/2} local l1=space.v_nor(p1)
		local p2={(p[i][1][1]+p[i][3][1])/2,(p[i][1][2]+p[i][3][2])/2,(p[i][1][3]+p[i][3][3])/2} local l2=space.v_nor(p2)
		local p3={(p[i][3][1]+p[i][2][1])/2,(p[i][3][2]+p[i][2][2])/2,(p[i][3][3]+p[i][2][3])/2} local l3=space.v_nor(p3)
		local r1=l1+(r-l1)*pct local r2=l2+(r-l2)*pct local r3=l3+(r-l3)*pct
		p1={p1[1]*r1/l1,p1[2]*r1/l1,p1[3]*r1/l1} p2={p2[1]*r2/l2,p2[2]*r2/l2,p2[3]*r2/l2} p3={p3[1]*r3/l3,p3[2]*r3/l3,p3[3]*r3/l3}
		new[#new+1]={p[i][1],p1,p2} new[#new+1]={p1,p3,p2} new[#new+1]={p1,p[i][2],p3} new[#new+1]={p2,p3,p[i][3]}
	end
	return new
end

function space.build_to_sphere_subdivide(n,r,pct)
	local p=space.build_sphere_subdivide(n,r) local solid=space.to_subdivide(r,pct,p)
	return solid
end

function space.build_sphere_thorn(n,r,len)
	local new,center_pos={},{} local p=space.build_sphere_subdivide(n,r)
	for i=1,#p do
		local cp=space.plane_center(p[i]) local l1=space.v_nor(cp) center_pos[i]={cp[1]*(l1+len/2)/l1,cp[2]*(l1+len/2)/l1,cp[3]*(l1+len/2)/l1}
		cp={cp[1]*(l1+len)/l1,cp[2]*(l1+len)/l1,cp[3]*(l1+len)/l1}
		new[#new+1]={{p[i][1],p[i][2],cp},{p[i][3],cp,p[i][2]},{p[i][1],cp,p[i][3]}}
	end
	new.cp=center_pos
	return new
end

function space.build_face_thorn(solid,len)
	local new,center_pos={},{}
	for i=1,#solid do
		local cp=space.plane_center(solid[i]) local l1=space.v_nor(cp) center_pos[i]={cp[1]*(l1+len/2)/l1,cp[2]*(l1+len/2)/l1,cp[3]*(l1+len/2)/l1}
		cp={cp[1]*(l1+len)/l1,cp[2]*(l1+len)/l1,cp[3]*(l1+len)/l1} new[#new+1]={}
		for i2=1,#solid[i] do
			local idx=i2==#solid[i] and 1 or i2+1 new[#new][i2]={solid[i][i2],solid[i][idx],cp}
		end
	end
	new.cp=center_pos
	return new
end

function space.build_face_platform(solid,len,pct)
	local new,center_pos={},{}
	for i=1,#solid do
		local cp=space.plane_center(solid[i]) local l1=space.v_nor(cp) local vertex,p={},{cp[1]*(l1+len)/l1,cp[2]*(l1+len)/l1,cp[3]*(l1+len)/l1}
		for i2=1,#solid[i] do
			vertex[i2]=space.vector(cp,solid[i][i2]) vertex[i2]={vertex[i2][1]*pct,vertex[i2][2]*pct,vertex[i2][3]*pct}
			vertex[i2]={vertex[i2][1]+p[1],vertex[i2][2]+p[2],vertex[i2][3]+p[3]}
		end
		center_pos[i]={cp[1]*(l1+len/2)/l1,cp[2]*(l1+len/2)/l1,cp[3]*(l1+len/2)/l1} new[i]={}
		for i2=1,#solid[i] do
			local idx=i2==#solid[i] and 1 or i2+1
			new[i][i2]={solid[i][i2],solid[i][idx],vertex[idx],vertex[i2]}
		end
		new[i][#solid[i]+1]=vertex
	end
	new.cp=center_pos
	return new
end

function space.build_face_platform_free(tbl,solid,len,pct)
	local new,center_pos={},{} local pct=pct<0 and 0 or (pct>1 and 1 or pct)
	for i=1,#tbl do
		local cp=space.plane_center(solid[tbl[i]]) local l1=space.v_nor(cp) local coor,p={},{cp[1]*(l1+len)/l1,cp[2]*(l1+len)/l1,cp[3]*(l1+len)/l1}
		for i2=1,#solid[tbl[i]] do
			coor[i2]=space.vector(cp,solid[tbl[i]][i2]) coor[i2]={coor[i2][1]*pct,coor[i2][2]*pct,coor[i2][3]*pct}
			coor[i2]={coor[i2][1]+p[1],coor[i2][2]+p[2],coor[i2][3]+p[3]}
		end
		center_pos[i]={cp[1]*(l1+len/2)/l1,cp[2]*(l1+len/2)/l1,cp[3]*(l1+len/2)/l1} new[i]={}
		if pct==0 then
			for i2=1,#solid[tbl[i]] do
				local idx=i2==#solid[tbl[i]] and 1 or i2+1 new[i][i2]={solid[tbl[i]][i2],solid[tbl[i]][idx],p}
			end
		else new[i][1]=coor
			for i2=1,#solid[tbl[i]] do
				local idx=i2==#solid[tbl[i]] and 1 or i2+1
				new[i][i2+1]={solid[tbl[i]][i2],solid[tbl[i]][idx],coor[idx],coor[i2]}
			end
		end
	end
	center_pos[#center_pos+1]={0,0,0} new[#new+1]=solid new.cp=center_pos
	return new
end

function space.dodecahedron(r,angle,axis,fsc,x,y,z,color,light)
	local a=1.61803 local p={} local center_pos={0,0,0}
	local vertex={{1/a,a,0},{-1/a,a,0},{1/a,-a,0},{-1/a,-a,0},{a,0,1/a},{-a,0,1/a},{a,0,-1/a},{-a,0,-1/a},
	{0,1/a,a},{0,-1/a,a},{0,1/a,-a},{0,-1/a,-a},{1,1,1},{1,-1,1},{1,1,-1},{-1,1,1},{-1,-1,1},{-1,1,-1},{-1,-1,-1},{1,-1,-1}}
	local face={{14,10,17,4,3},{6,8,19,4,17},{9,16,6,17,10},{5,13,9,10,14},{14,3,20,7,5},{3,4,19,12,20},{8,18,11,12,19},
	{16,2,18,8,6},{7,20,12,11,15},{2,1,15,11,18},{13,5,7,15,1},{9,13,1,2,16}}
	for i=1,12 do
		p[i]=space.scale({vertex[face[i][1]],vertex[face[i][2]],vertex[face[i][3]],vertex[face[i][4]],vertex[face[i][5]]},r/1.73205)
	end
	return space.info(p,center_pos,angle,axis,fsc,x,y,z,color,light),p
end

function space.dodecahedron_subdivide(n,r)
	local p={} local _,solid=space.dodecahedron(r,{},{})
	if n<1 then
		return solid
	elseif n==1 then
		for i=1,#solid do
			local cp=space.plane_center(solid[i]) local l1=space.v_nor(cp) local new={cp[1]*r/l1,cp[2]*r/l1,cp[3]*r/l1}
			for i2=1,#solid[i] do
				local idx=i2==#solid[i] and 1 or i2+1 p[#p+1]={solid[i][i2],solid[i][idx],new}
			end
		end
	else solid=space.dodecahedron_subdivide(n-1,r) p=space.subdivide(r,solid)
	end
	return p
end

function space.to_dodecahedron_subdivide(n,r,pct)
	local p={} local _,solid=space.dodecahedron(r,{},{})
	if n<0 then
		return solid
	elseif n==0 then
		for i=1,#solid do
			local cp=space.plane_center(solid[i]) local l1=space.v_nor(cp)
			local new={cp[1]*(l1+(r-l1)*pct)/l1,cp[2]*(l1+(r-l1)*pct)/l1,cp[3]*(l1+(r-l1)*pct)/l1}
			for i2=1,#solid[i] do
				local idx=i2==#solid[i] and 1 or i2+1 p[#p+1]={solid[i][i2],solid[i][idx],new}
			end
		end
	else solid=space.dodecahedron_subdivide(n,r) p=space.to_subdivide(r,pct,solid)
	end
	return p
end

function space.ball_subdivide(n,r,angle,axis,fsc,x,y,z,color,light)
	local center_pos={0,0,0} local p=space.dodecahedron_subdivide(n,r)
	return space.info(p,center_pos,angle,axis,fsc,x,y,z,color,light),p
end

function space.ball(r,horizontal,vertical,ass_shape,angle,axis,fsc,x,y,z,color,light)
	local h=math.ceil(r*math.pi*horizontal)
	local p={space.translate(space.shape2points(ass_shape),0,0,r)} local center_pos={0,0,0}
	for i=1,h do
		local dh=i*180/h local rv=r*math.sin(math.rad(dh))
		local v=math.ceil(rv*math.pi*vertical*2)
		for ii=1,v do
			p[#p+1]=space.rotate(space.rotate(p[1],dh,"x"),360*(ii-1)/v,"z")
		end
	end
	return space.info(p,center_pos,angle,axis,fsc,x,y,z,color,light),p
end

function space.ellipsoid(rx,ry,rz,horizontal,vertical,ass_shape,angle,axis,fsc,x,y,z,color,light)
	local r=math.max(rx,ry,rz) local h=math.ceil(r*math.pi*horizontal)
	local p={space.translate(space.shape2points(ass_shape),0,0,r)} local center_pos={0,0,0}
	for i=1,h do
		local dh=i*180/h local rv=r*math.sin(math.rad(dh))
		local v=math.ceil(rv*math.pi*vertical*2)
		for ii=1,v do
			p[#p+1]=space.rotate(space.rotate(p[1],dh,"x"),360*(ii-1)/v,"z")
			p[#p]=space.scale(p[#p],rx/r,ry/r,rz/r)
		end
	end
	p[1]=space.scale(p[1],rx/r,ry/r,rz/r)
	return space.info(p,center_pos,angle,axis,fsc,x,y,z,color,light),p
end

function space.digonal_antiprism(w,h,angle,axis,fsc,x,y,z,color,light)
	local p={} local center_pos={0,0,0}
	p[1]={{0,w/2,-h/2},{-w/2,0,h/2},{w/2,0,h/2}} p[2]={{0,w/2,-h/2},{w/2,0,h/2},{0,-w/2,-h/2}}
	p[3]={{0,-w/2,-h/2},{w/2,0,h/2},{-w/2,0,h/2}} p[4]={{0,-w/2,-h/2},{-w/2,0,h/2},{0,w/2,-h/2}}
	return space.info(p,center_pos,angle,axis,fsc,x,y,z,color,light),p
end

function space.octahedron(r,angle,axis,fsc,x,y,z,color,light)
	local p,cp={},{0,0,0} local face={{1,4,5},{1,3,5},{2,5,3},{2,5,4},{6,4,1},{6,3,1},{6,2,3},{6,2,4}}
	local vertex={{r,0,0},{-r,0,0},{0,r,0},{0,-r,0},{0,0,r},{0,0,-r}}
	for i=1,8 do
		p[i]={vertex[face[i][1]],vertex[face[i][2]],vertex[face[i][3]]}
	end
	return space.info(p,cp,angle,axis,fsc,x,y,z,color,light),p
end

function space.build_octahedron_subdivide(n,r)
	local p={} local _,solid=space.octahedron(r,{},{})
	if n<1 then
		return solid
	elseif n==1 then
		p=space.subdivide(r,solid)
	else solid=space.build_octahedron_subdivide(n-1,r) p=space.subdivide(r,solid)
	end
	return p
end

function space.octasphere(n,r,angle,axis,fsc,x,y,z,color,light)
	local center_pos={0,0,0} local p=space.build_octahedron_subdivide(n,r)
	return space.info(p,center_pos,angle,axis,fsc,x,y,z,color,light),p
end

function space.to_octasphere_subdivide(n,r,pct)
	local p=space.build_octahedron_subdivide(n,r) local solid=space.to_subdivide(r,pct,p)
	return solid
end

function space.build_octasphere_at_pct(r,pct)
	local pct=pct<0 and 0 or (pct>1 and 1 or pct) local p
	if pct<=1/3 then
		p=space.to_octasphere_subdivide(0,r,pct*3)
	elseif pct<=2/3 then
		p=space.to_octasphere_subdivide(1,r,(pct-1/3)*3)
	else p=space.to_octasphere_subdivide(2,r,(pct-2/3)*3)
	end
	return p
end

function space.octasphere_at_pct(r,pct,angle,axis,fsc,x,y,z,color,light)
	local p=space.build_octasphere_at_pct(r,pct) local center_pos={0,0,0}
	return space.info(p,center_pos,angle,axis,fsc,x,y,z,color,light),p
end

function space.rhombicuboctahedron(h,angle,axis,fsc,x,y,z,color,light)
	local a=h/(1+2^0.5) local p,center_pos,h1={},{0,0,0},a/2^0.5
	local vertex={
		{-a/2,a/2,h/2},{-a/2,-a/2,h/2},{a/2,-a/2,h/2},{a/2,a/2,h/2},{-a/2,a/2,-h/2},{-a/2,-a/2,-h/2},{a/2,-a/2,-h/2},{a/2,a/2,-h/2},
		{-h/2,a/2,h/2-h1},{-h/2,-a/2,h/2-h1},{-h/2+h1,-h/2,h/2-h1},{a/2,-h/2,h/2-h1},{h/2,-a/2,h/2-h1},{h/2,a/2,h/2-h1},
		{a/2,h/2,h/2-h1},{-a/2,h/2,h/2-h1},{-h/2,a/2,h/2-h1-a},{-h/2,-a/2,h/2-h1-a},{-h/2+h1,-h/2,h/2-h1-a},{a/2,-h/2,h/2-h1-a},
		{h/2,-a/2,h/2-h1-a},{h/2,a/2,h/2-h1-a},{a/2,h/2,h/2-h1-a},{-a/2,h/2,h/2-h1-a}
	}
	local face={
		{1,2,3,4},{16,15,23,24},{14,13,21,22},{9,10,18,17},{12,11,19,20},{5,6,7,8},{9,10,2,1},{15,16,1,4},{13,14,4,3},
		{11,12,3,2},{7,6,19,20},{8,7,21,22},{5,8,23,24},{6,5,17,18},{16,9,17,24},{15,14,22,23},{13,12,20,21},{11,10,18,19},
		{9,1,16},{15,4,14},{13,3,12},{11,2,10},{18,19,6},{20,21,7},{22,23,8},{24,17,5}
	}
	for i=1,18 do
		p[i]={vertex[face[i][1]],vertex[face[i][2]],vertex[face[i][3]],vertex[face[i][4]]}
	end
	for i=19,26 do
		p[i]={vertex[face[i][1]],vertex[face[i][2]],vertex[face[i][3]]}
	end
	return space.info(p,center_pos,angle,axis,fsc,x,y,z,color,light),p
end

function space.truncated_cube(a,len,angle,axis,fsc,x,y,z,color,light)
	local a=a>=len/2 and len/2-1 or a local p,center_pos={},{0,0,0} a=a<1 and 1 or a
	local vertex={
		{-len/2,len/2-a,len/2},{-len/2,a-len/2,len/2},{a-len/2,-len/2,len/2},{len/2-a,-len/2,len/2},{len/2,a-len/2,len/2},
		{len/2,len/2-a,len/2},{len/2-a,len/2,len/2},{a-len/2,len/2,len/2},{-len/2,len/2-a,-len/2},{-len/2,a-len/2,-len/2},
		{a-len/2,-len/2,-len/2},{len/2-a,-len/2,-len/2},{len/2,a-len/2,-len/2},{len/2,len/2-a,-len/2},{len/2-a,len/2,-len/2},
		{a-len/2,len/2,-len/2},{-len/2,len/2,len/2-a},{-len/2,-len/2,len/2-a},{len/2,-len/2,len/2-a},{len/2,len/2,len/2-a},
		{-len/2,len/2,a-len/2},{-len/2,-len/2,a-len/2},{len/2,-len/2,a-len/2},{len/2,len/2,a-len/2}
	}
	local face={
		{1,2,3,4,5,6,7,8},{7,8,17,21,16,15,24,20},{6,5,19,23,13,14,24,20},{2,1,17,21,9,10,22,18},{4,3,18,22,11,12,23,19},
		{9,10,11,12,13,14,15,16},{8,1,17},{3,2,18},{5,4,19},{7,6,20},{9,16,21},{11,10,22},{12,13,23},{14,15,24}
	}
	for i=1,6 do
		p[i]={
			vertex[face[i][1]],vertex[face[i][2]],vertex[face[i][3]],vertex[face[i][4]],
			vertex[face[i][5]],vertex[face[i][6]],vertex[face[i][7]],vertex[face[i][8]]
		}
	end
	for i=7,14 do
		p[i]={vertex[face[i][1]],vertex[face[i][2]],vertex[face[i][3]]}
	end
	return space.info(p,center_pos,angle,axis,fsc,x,y,z,color,light),p
end

function space.rhombic_dodecahedron(w,angle,axis,fsc,x,y,z,color,light)
	local p,center_pos={},{0,0,0}
	local vertex={{1,1,1},{1,1,-1},{1,-1,1},{1,-1,-1},{-1,1,1},{-1,1,-1},{-1,-1,-1},{-1,-1,1},{2,0,0},{-2,0,0},{0,2,0},{0,-2,0},{0,0,2},{0,0,-2}}
	local face={
		{13,1,11,5},{13,3,9,1},{13,8,12,3},{13,5,10,8},{1,9,2,11},{9,3,12,4},{9,4,14,2},{11,2,14,6},{14,4,12,7},{5,10,6,11},{10,8,12,7},{10,7,14,6}
	}
	for i=1,12 do
		p[i]=space.scale({vertex[face[i][1]],vertex[face[i][2]],vertex[face[i][3]],vertex[face[i][4]]},w/4)
	end
	return space.info(p,center_pos,angle,axis,fsc,x,y,z,color,light),p
end

function space.tetrahedron(len,angle,axis,fsc,x,y,z,color,light)
	local center_pos={0,0,0} local h=6^0.5/3*len
	local p={
		{{-len/2,len/2/3^0.5,-h/4},{0,-len/3^0.5,-h/4},{len/2,len/2/3^0.5,-h/4}},{{-len/2,len/2/3^0.5,-h/4},{0,-len/3^0.5,-h/4},{0,0,3*h/4}},
		{{0,-len/3^0.5,-h/4},{len/2,len/2/3^0.5,-h/4},{0,0,3*h/4}},{{-len/2,len/2/3^0.5,-h/4},{0,0,3*h/4},{len/2,len/2/3^0.5,-h/4}}
	}
	return space.info(p,center_pos,angle,axis,fsc,x,y,z,color,light),p
end

function space.truncated_tetrahedron(a,l,angle,axis,fsc,x,y,z,color,light)
	local p,center_pos={},{0,0,0} local r=l/2/3^0.5*2 local a=a>=l/3 and l/3 or (a<1 and 1 or a)
	local vertex={
		{r/2,l/2-a,-r/2},{r/2,-l/2+a,-r/2},{r/2-a/2*3^0.5,-l/2+a/2,-r/2},{-r+a/2*3^0.5,-a/2,-r/2},{-r+a/2*3^0.5,a/2,-r/2},
		{r/2-a/2*3^0.5,l/2-a/2,-r/2},{(r-a*3^0.5/3)/2,(r*3^0.5-a)/2,-r/2+a*6^0.5/3},{(r-a*3^0.5/3)/2,(a-r*3^0.5)/2,-r/2+a*6^0.5/3},
		{a*3^0.5/3-r,0,-r/2+a*6^0.5/3},{a*3^0.5/6,a/2,r-a*6^0.5/3},{a*3^0.5/6,-a/2,r-a*6^0.5/3},{-a*3^0.5/3,0,r-a*6^0.5/3}
	}
	local face={{1,2,3,4,5,6},{7,10,11,8,2,1},{8,11,12,9,4,3},{6,5,9,12,10,7},{6,7,1},{2,8,3},{4,9,5},{10,12,11}}
	for i=1,4 do
		p[i]={vertex[face[i][1]],vertex[face[i][2]],vertex[face[i][3]],vertex[face[i][4]],vertex[face[i][5]],vertex[face[i][6]]}
	end
	for i=5,8 do
		p[i]={vertex[face[i][1]],vertex[face[i][2]],vertex[face[i][3]]}
	end
	return space.info(p,center_pos,angle,axis,fsc,x,y,z,color,light),p
end

function space.cuboid(a,b,h,angle,axis,fsc,x,y,z,color,light)
	local p,center_pos={},{0,0,0} local face={{5,6,7,8},{2,1,5,6},{1,4,8,5},{2,3,7,6},{4,3,7,8},{1,2,3,4}}
	local vertex={{b/2,a/2,h/2},{-b/2,a/2,h/2},{-b/2,-a/2,h/2},{b/2,-a/2,h/2},{b/2,a/2,-h/2},{-b/2,a/2,-h/2},{-b/2,-a/2,-h/2},{b/2,-a/2,-h/2}}
	for i=1,6 do
		p[i]={vertex[face[i][1]],vertex[face[i][2]],vertex[face[i][3]],vertex[face[i][4]]}
	end
	return space.info(p,center_pos,angle,axis,fsc,x,y,z,color,light),p
end

function space.truncated_cuboid(t,a,b,h,angle,axis,fsc,x,y,z,color,light)
	local t=t>=math.min(a,b,h)/2 and math.min(a,b,h)/2-1 or t local p,center_pos={},{0,0,0}
	local vertex={
		{t-a/2,b/2,h/2},{-a/2,b/2-t,h/2},{-a/2,t-b/2,h/2},{t-a/2,-b/2,h/2},{a/2-t,-b/2,h/2},{a/2,t-b/2,h/2},{a/2,b/2-t,h/2},{a/2-t,b/2,h/2},
		{-a/2,b/2,h/2-t},{-a/2,-b/2,h/2-t},{a/2,-b/2,h/2-t},{a/2,b/2,h/2-t},{-a/2,b/2,t-h/2},{-a/2,-b/2,t-h/2},{a/2,-b/2,t-h/2},{a/2,b/2,t-h/2},
		{t-a/2,b/2,-h/2},{-a/2,b/2-t,-h/2},{-a/2,t-b/2,-h/2},{t-a/2,-b/2,-h/2},{a/2-t,-b/2,-h/2},{a/2,t-b/2,-h/2},{a/2,b/2-t,-h/2},{a/2-t,b/2,-h/2}
	}
	local face={
		{17,18,19,20,21,22,23,24},{18,13,9,2,3,10,14,19},{24,17,13,9,1,8,12,16},{20,14,10,4,5,11,15,21},{23,16,12,7,6,11,15,22},
		{1,2,3,4,5,6,7,8},{9,2,1},{10,3,4},{5,6,11},{12,8,7},{18,13,17},{24,16,23},{19,14,20},{21,15,22}
	}
	for i=1,6 do
		p[i]={
			vertex[face[i][1]],vertex[face[i][2]],vertex[face[i][3]],vertex[face[i][4]],
			vertex[face[i][5]],vertex[face[i][6]],vertex[face[i][7]],vertex[face[i][8]]
		}
	end
	for i=7,14 do
		p[i]={vertex[face[i][1]],vertex[face[i][2]],vertex[face[i][3]]}
	end
	return space.info(p,center_pos,angle,axis,fsc,x,y,z,color,light),p
end

function space.elongated_pyramid(n,h1,h2,r,angle,axis,fsc,x,y,z,color,light)
	local deg=math.rad(360/n) local p={{}} local center_pos={0,0,0}
	for i=1,n do
		p[1][i]={math.cos((i-1/2)*deg)*r,math.sin((i-1/2)*deg)*r,-(h1+h2)/2}
	end
	for i=1,n-1 do
		p[#p+1]={p[1][i],{p[1][i][1],p[1][i][2],(h1-h2)/2},{p[1][i+1][1],p[1][i+1][2],(h1-h2)/2},p[1][i+1]}
		p[#p+1]={{p[1][i][1],p[1][i][2],(h1-h2)/2},{0,0,(h1+h2)/2},{p[1][i+1][1],p[1][i+1][2],(h1-h2)/2}}
	end
	p[#p+1]={p[1][n],{p[1][n][1],p[1][n][2],(h1-h2)/2},{p[1][1][1],p[1][1][2],(h1-h2)/2},p[1][1]}
	p[#p+1]={{p[1][n][1],p[1][n][2],(h1-h2)/2},{0,0,(h1+h2)/2},{p[1][1][1],p[1][1][2],(h1-h2)/2}}
	return space.info(p,center_pos,angle,axis,fsc,x,y,z,color,light),p
end

function space.elongated_bipyramid(n,h1,h2,r,angle,axis,fsc,x,y,z,color,light)
	local deg=math.rad(360/n) local s,p={},{} local center_pos={0,0,0}
	for i=1,n do
		s[i]={math.cos((i-1/2)*deg)*r,math.sin((i-1/2)*deg)*r,h1/2}
	end
	for i=1,n-1 do
		p[#p+1]={s[i],{0,0,h1/2+h2},s[i+1]} p[#p+1]={{s[i][1],s[i][2],-h1/2},{0,0,-h1/2-h2},{s[i+1][1],s[i+1][2],-h1/2}}
		p[#p+1]={s[i],s[i+1],{s[i+1][1],s[i+1][2],-h1/2},{s[i][1],s[i][2],-h1/2}}
	end
	p[#p+1]={s[n],{0,0,h1/2+h2},s[1]} p[#p+1]={{s[n][1],s[n][2],-h1/2},{0,0,-h1/2-h2},{s[1][1],s[1][2],-h1/2}}
	p[#p+1]={s[n],s[1],{s[1][1],s[1][2],-h1/2},{s[n][1],s[n][2],-h1/2}}
	return space.info(p,center_pos,angle,axis,fsc,x,y,z,color,light),p
end

function space.pyramid_frustum(n,h1,h2,r1,r2,angle,axis,fsc,x,y,z,color,light)
	local deg=math.rad(360/n) local s,p={},{{}} local center_pos={0,0,0}
	for i=1,n do
		s[i]={math.cos((i-1/2)*deg)*r1,math.sin((i-1/2)*deg)*r1,0}
		p[1][i]={math.cos((i-1/2)*deg)*r2,math.sin((i-1/2)*deg)*r2,-h1}
	end
	for i=1,n-1 do
		p[#p+1]={s[i],{0,0,h2},s[i+1]} p[#p+1]={s[i],s[i+1],p[1][i+1],p[1][i]}
	end
	p[#p+1]={s[n],{0,0,h2},s[1]} p[#p+1]={s[n],s[1],p[1][1],p[1][n]}
	return space.info(p,center_pos,angle,axis,fsc,x,y,z,color,light),p
end

function space.crystal(n,h1,h2,r,angle,axis,fsc,x,y,z,color,light)
	local deg=math.rad(360/n) local s,p={},{} local center_pos,h3={0,0,0},h2/7
	for i=1,n do
		s[i]={math.cos((i-1/2)*deg)*r,math.sin((i-1/2)*deg)*r,h1/2}
	end
	for i=1,n do
		local ne=i+1>n and 1 or i+1 local be=i-1<1 and n or i-1
		p[#p+1]={s[i],{(s[be][1]+s[i][1])/2,(s[be][2]+s[i][2])/2,h1/2+h3},{0,0,h1/2+h2}}
		p[#p+1]={s[i],{0,0,h1/2+h2},{(s[ne][1]+s[i][1])/2,(s[ne][2]+s[i][2])/2,h1/2+h3}}
		p[#p+1]={{s[i][1],s[i][2],-h1/2},{(s[be][1]+s[i][1])/2,(s[be][2]+s[i][2])/2,-h1/2-h3},{0,0,-h1/2-h2}}
		p[#p+1]={{s[i][1],s[i][2],-h1/2},{0,0,-h1/2-h2},{(s[ne][1]+s[i][1])/2,(s[ne][2]+s[i][2])/2,-h1/2-h3}}
		p[#p+1]={
			s[i],{(s[be][1]+s[i][1])/2,(s[be][2]+s[i][2])/2,h1/2+h3},s[be],{s[be][1],s[be][2],-h1/2},
			{(s[be][1]+s[i][1])/2,(s[be][2]+s[i][2])/2,-h1/2-h3},{s[i][1],s[i][2],-h1/2}
		}
	end
	return space.info(p,center_pos,angle,axis,fsc,x,y,z,color,light),p
end

function space.oblique_prism(n,h,r,off_x,off_y,angle,axis,fsc,x,y,z,color,light)
	local deg=math.rad(360/n) local p={{}} local center_pos={0,0,0}
	for i=1,n do
		p[1][i]={math.cos((i-1/2)*deg)*r,math.sin((i-1/2)*deg)*r,-h/2}
	end
	p[1]=space.translate(p[1],off_x/2,off_y/2) p[n+2]=space.translate(p[1],-off_x,-off_y,h)
	for i=1,n-1 do
		p[i+1]={p[1][i],p[n+2][i],p[n+2][i+1],p[1][i+1]}
	end
	p[n+1]={p[1][n],p[n+2][n],p[n+2][1],p[1][1]}
	return space.info(p,center_pos,angle,axis,fsc,x,y,z,color,light),p
end

function space.oblique_pyramid(n,h,r,off_x,off_y,angle,axis,fsc,x,y,z,color,light)
	local deg=math.rad(360/n) local p={{}} local center_pos={0,0,0}
	for i=1,n do
		p[1][i]={math.cos((i-1/2)*deg)*r,math.sin((i-1/2)*deg)*r,-h/2}
	end
	p[1]=space.translate(p[1],-off_x/2,-off_y/2)
	for i=1,n-1 do
		p[i+1]={p[1][i],{off_x/2,off_y/2,h/2},p[1][i+1]}
	end
	p[#p+1]={p[1][n],{off_x/2,off_y/2,h/2},p[1][1]}
	return space.info(p,center_pos,angle,axis,fsc,x,y,z,color,light),p
end

function space.oblique_frustum(n,h,r1,r2,off_x,off_y,angle,axis,fsc,x,y,z,color,light)
	local deg=math.rad(360/n) local p={{},{}} local center_pos={0,0,0}
	for i=1,n do
		p[1][i]={math.cos((i-1/2)*deg)*r1,math.sin((i-1/2)*deg)*r1,h/2}
		p[2][i]={math.cos((i-1/2)*deg)*r2,math.sin((i-1/2)*deg)*r2,-h/2}
	end
	p[1]=space.translate(p[1],-off_x/2,-off_y/2) p[2]=space.translate(p[2],off_x/2,off_y/2)
	for i=1,n-1 do
		p[#p+1]={p[1][i],p[1][i+1],p[2][i+1],p[2][i]}
	end
	p[#p+1]={p[1][n],p[1][1],p[2][1],p[2][n]}
	return space.info(p,center_pos,angle,axis,fsc,x,y,z,color,light),p
end

function space.dipyramid(n,h1,h2,r,angle,axis,fsc,x,y,z,color,light)
	local deg=math.rad(360/n) local s={} local p,center_pos={},{0,0,0}
	for i=1,n do
		s[i]={math.cos((i-1/2)*deg)*r,math.sin((i-1/2)*deg)*r,0}
	end
	for i=1,n-1 do
		p[#p+1]={s[i],{0,0,-h1},s[i+1]} p[#p+1]={s[i],{0,0,h2},s[i+1]}
	end
	p[#p+1]={s[n],{0,0,-h1},s[1]} p[#p+1]={s[n],{0,0,h2},s[1]}
	return space.info(p,center_pos,angle,axis,fsc,x,y,z,color,light),p
end

function space.sphere(n,n1,r,angle,axis,fsc,x,y,z,color,light)
	local n=n%4==0 and n or math.ceil(n/4)*4 local deg1,deg2=math.rad(360/n),360/n1 local s,p={{}},{{},{}} local center_pos={0,0,0}
	for i=1,n/2 do
		s[1][i]={0,math.sin((i-1/2)*deg1)*r,math.cos((i-1/2)*deg1)*r}
	end
	for i=1,n1 do
		s[i]=space.rotate(s[1],deg2*(i-1),"z") p[1][i]=s[i][1] p[2][i]=s[i][n/2]
	end
	for i=1,n1 do
		for i2=1,n/2-1 do
			local num1=i==n1 and 1 or i+1 local num2=i2+1
			p[#p+1]={s[i][i2],s[i][num2],s[num1][num2],s[num1][i2]}
		end
	end
	return space.info(p,center_pos,angle,axis,fsc,x,y,z,color,light),p
end

function space.half_sphere(n,n1,r,angle,axis,fsc,x,y,z,color,light)
	local n=n%4==0 and n or math.ceil(n/4)*4 local deg1,deg2=math.rad(360/n),360/n1 local s,p={{}},{{},{}} local center_pos={0,0,0}
	for i=1,n/4 do
		s[1][i]={0,math.sin((i-1/2)*deg1)*r,math.cos((i-1/2)*deg1)*r}
	end
	table.insert(s[1],{0,r,0})
	for i=1,n1 do
		s[i]=space.rotate(s[1],deg2*(i-1),"z") p[1][i]=s[i][1] p[2][i]=s[i][#s[i]]
	end
	for i=1,n1 do
		for i2=1,n/4 do
			local num1=i==n1 and 1 or i+1 local num2=i2+1
			p[#p+1]={s[i][i2],s[i][num2],s[num1][num2],s[num1][i2]}
		end
	end
	p=space.shift_solid(p,0,0,-r/2)
	return space.info(p,center_pos,angle,axis,fsc,x,y,z,color,light),p
end

function space.capsule(n,n1,h,r,angle,axis,fsc,x,y,z,color,light)
	local n=n%4==0 and n or math.ceil(n/4)*4 local deg1,deg2=math.rad(360/n),360/n1 local s,p0={{}},{{}} local center_pos={0,0,0}
	for i=1,n/4 do
		s[1][i]={0,math.sin((i-1/2)*deg1)*r,math.cos((i-1/2)*deg1)*r}
	end
	table.insert(s[1],{0,r,0})
	for i=1,n1 do
		s[i]=space.rotate(s[1],deg2*(i-1),"z") p0[1][i]=s[i][1]
	end
	for i=1,n1 do
		for i2=1,n/4 do
			local num1=i==n1 and 1 or i+1 local num2=i2+1
			p0[#p0+1]={s[i][i2],s[i][num2],s[num1][num2],s[num1][i2]}
		end
	end
	p0=space.shift_solid(p0,0,0,h/2) local p1=space.rotate_solid(p0,180,"x") local p={}
	for i=1,#p0 do
		p[#p+1]=p0[i] p[#p+1]=p1[i]
	end
	for i=1,n1 do
		local i0=i==n1 and 1 or i+1 local i1=n/4+1
		p[#p+1]={
			{s[i][i1][1],s[i][i1][2],s[i][i1][3]+h/2},{s[i0][i1][1],s[i0][i1][2],s[i0][i1][3]+h/2},
			{s[i0][i1][1],s[i0][i1][2],-s[i0][i1][3]-h/2},{s[i][i1][1],s[i][i1][2],-s[i][i1][3]-h/2}
		}
	end
	return space.info(p,center_pos,angle,axis,fsc,x,y,z,color,light),p
end

function space.capsule2(n,h,r,angle,axis,fsc,x,y,z,color,light)
	local solid=space.build_dome_subdivide(n,r) local deg,p=math.rad(360/2^(n+2)) local center_pos={0,0,0}
	p=space.shift_solid(solid,0,0,h/2) solid=space.rotate_solid(p,180,"x")
	for i=1,#solid do
		p[#p+1]=solid[i]
	end
	for i=1,2^(n+2) do
		p[#p+1]={
			{math.cos((i-1)*deg)*r,math.sin((i-1)*deg)*r,h/2},{math.cos(i*deg)*r,math.sin(i*deg)*r,h/2},
			{math.cos(i*deg)*r,math.sin(i*deg)*r,-h/2},{math.cos((i-1)*deg)*r,math.sin((i-1)*deg)*r,-h/2}
		}
	end
	return space.info(p,center_pos,angle,axis,fsc,x,y,z,color,light),p
end

function space.ice_cream(n,n1,h,r,angle,axis,fsc,x,y,z,color,light)
	local n=n%4==0 and n or math.ceil(n/4)*4 local deg1,deg2=math.rad(360/n),360/n1 local s,p={{}},{{}} local center_pos={0,0,0}
	for i=1,n/4 do
		s[1][i]={0,math.sin((i-1/2)*deg1)*r,math.cos((i-1/2)*deg1)*r}
	end
	table.insert(s[1],{0,r,0})
	for i=1,n1 do
		s[i]=space.rotate(s[1],deg2*(i-1),"z") p[1][i]=s[i][1]
	end
	for i=1,n1 do
		for i2=1,n/4 do
			local num1=i==n1 and 1 or i+1 local num2=i2+1
			p[#p+1]={s[i][i2],s[i][num2],s[num1][num2],s[num1][i2]}
		end
	end
	p=space.shift_solid(p,0,0,(h-r)/2)
	for i=1,n1 do
		local i0=i==n1 and 1 or i+1
		p[#p+1]={space.shift_point(s[i][n/4+1],0,0,(h-r)/2),space.shift_point(s[i0][n/4+1],0,0,(h-r)/2),{0,0,-(h+r)/2}}
	end
	return space.info(p,center_pos,angle,axis,fsc,x,y,z,color,light),p
end

function space.ice_cream2(n,h,r,angle,axis,fsc,x,y,z,color,light)
	local solid=space.build_dome_subdivide(n,r) local deg,p=math.rad(360/2^(n+2)) local center_pos={0,0,0}
	p=space.shift_solid(solid,0,0,(h-r)/2)
	for i=1,2^(n+2) do
		p[#p+1]={{math.cos((i-1)*deg)*r,math.sin((i-1)*deg)*r,(h-r)/2},{math.cos(i*deg)*r,math.sin(i*deg)*r,(h-r)/2},{0,0,-(h+r)/2}}
	end
	return space.info(p,center_pos,angle,axis,fsc,x,y,z,color,light),p
end

function space.hemisphere_on_prism(n,n1,h,r,angle,axis,fsc,x,y,z,color,light)
	local n=n%4==0 and n or math.ceil(n/4)*4 local deg1,deg2=math.rad(360/n),360/n1 local s,p={{}},{{},{}} local center_pos={0,0,0}
	for i=1,n/4 do
		s[1][i]={0,math.sin((i-1/2)*deg1)*r,math.cos((i-1/2)*deg1)*r}
	end
	table.insert(s[1],{0,r,0})
	for i=1,n1 do
		s[i]=space.rotate(s[1],deg2*(i-1),"z") p[1][i]=s[i][1] p[2][i]=s[i][n/4+1]
	end
	for i=1,n1 do
		for i2=1,n/4 do
			local num1=i==n1 and 1 or i+1 local num2=i2+1
			p[#p+1]={s[i][i2],s[i][num2],s[num1][num2],s[num1][i2]}
		end
	end
	p=space.shift_solid(p,0,0,(h-r)/2) p[2]=space.translate(p[2],0,0,-h)
	for i=1,n1 do
		local i0=i==n1 and 1 or i+1 local i1=n/4+1
		p[#p+1]={
			{s[i][i1][1],s[i][i1][2],(h-r)/2},{s[i0][i1][1],s[i0][i1][2],(h-r)/2},
			{s[i0][i1][1],s[i0][i1][2],(-h-r)/2},{s[i][i1][1],s[i][i1][2],(-h-r)/2}
		}
	end
	return space.info(p,center_pos,angle,axis,fsc,x,y,z,color,light),p
end

function space.dome_on_prism(n,h,r,angle,axis,fsc,x,y,z,color,light)
	local solid=space.build_dome_subdivide(n,r) local deg,p=math.rad(360/2^(n+2)) local center_pos={0,0,0}
	p=space.shift_solid(solid,0,0,(h-r)/2) local idx=#p p[idx+1]={}
	for i=1,2^(n+2) do
		p[idx+1][i]={math.cos((i-1)*deg)*r,math.sin((i-1)*deg)*r,-(h+r)/2}
		p[idx+1+i]={
			{math.cos((i-1)*deg)*r,math.sin((i-1)*deg)*r,(h-r)/2},{math.cos(i*deg)*r,math.sin(i*deg)*r,(h-r)/2},
			{math.cos(i*deg)*r,math.sin(i*deg)*r,-(h+r)/2},{math.cos((i-1)*deg)*r,math.sin((i-1)*deg)*r,-(h+r)/2}
		}
	end
	return space.info(p,center_pos,angle,axis,fsc,x,y,z,color,light),p
end

function space.spheroid(n,a,b,angle,axis,fsc,x,y,z,color,light)
	local n=n%4==0 and n or math.ceil(n/4)*4 local deg1,deg2=math.rad(360/n),360/n local s,p={{}},{} local center_pos={0,0,0}
	for i=1,n do
		s[1][i]={math.cos((i-1/2)*deg1)*a,0,math.sin((i-1/2)*deg1)*b}
	end
	for i=1,n+1 do
		s[i+1]=space.rotate(s[1],deg2*(1/2-i),"z")
	end
	table.remove(s,1)
	for i=1,n+1 do
		for i2=1,n do
			local num1=i>n and 1 or i+1 local num2=i2+1>n and 1 or i2+1
			p[#p+1]={s[i][i2],s[i][num2],s[num1][num2],s[num1][i2]}
		end
	end
	return space.info(p,center_pos,angle,axis,fsc,x,y,z,color,light),p
end

function space.trirectangular_tetrahedron(a,b,c,angle,axis,fsc,x,y,z,color,light)
	local p={} local center_pos={0,0,0}
	p[1]={{0,0,0},{a,0,0},{0,b,0}} p[2]={{0,0,0},{0,0,c},{a,0,0}} p[3]={{0,0,0},{0,b,0},{0,0,c}}
	p[4]={{a,0,0},{0,b,0},{0,0,c}} p=space.shift_solid(p,-a/4,-b/4,-c/4)
	return space.info(p,center_pos,angle,axis,fsc,x,y,z,color,light),p
end

function space.trirectangular_rect_nets(n1,n2,len,angle,axis,fsc,x,y,z,color,light)
	local a,b=len/n1,len/n2 local cp,p={0,0,0},{} local p0={{0,0,0},{a,0,0},{a,b,0},{0,b,0}}
	local p1={{0,0,0},{0,0,b},{a,0,b},{a,0,0}} local p2={{len,a,0},{len,a,b},{len,0,b},{len,0,0}}
	for i=1,n1 do
		for i2=1,n2 do
			p[#p+1]=space.translate(p0,(i-1)*a,(i2-1)*b)
			p[#p+1]=space.translate(p1,(i-1)*a,0,(i2-1)*b)
			p[#p+1]=space.translate(p2,0,(i-1)*a,(i2-1)*b)
		end
	end
	p=space.shift_solid(p,-len/2,-len/2,-len/2)
	return space.info(p,cp,angle,axis,fsc,x,y,z,color,light),p
end

function space.cube_to_dodecahedron(len,pct,angle,axis,fsc,x,y,z,color,light)
	local p,cp={},{0,0,0} local pct=pct<0 and 0 or (pct>0.9999 and 0.9999 or pct) local l1=len/2 local h2=pct*pct
	p[1]={{-l1,l1,-l1},{-l1+h2*l1,0,-l1-l1*pct},{l1-h2*l1,0,-l1-l1*pct},{l1,l1,-l1},{0,l1*(1+pct),-l1+l1*h2}}
	p[2]={{-l1+h2*l1,0,-l1-l1*pct},{-l1,-l1,-l1},{0,-l1*(1+pct),-l1+l1*h2},{l1,-l1,-l1},{l1-h2*l1,0,-l1-l1*pct}}
	p[3]={{l1-h2*l1,0,l1+l1*pct},{l1,-l1,l1},{l1+l1*pct,h2*l1-l1,0},{l1+l1*pct,l1-h2*l1,0},{l1,l1,l1}}
	p[4]={{l1+l1*pct,l1-h2*l1,0},{l1+l1*pct,h2*l1-l1,0},{l1,-l1,-l1},{l1-h2*l1,0,-l1-l1*pct},{l1,l1,-l1}}
	p[5]={{-l1,l1,l1},{0,l1*(1+pct),l1-l1*h2},{0,l1*(1+pct),l1*h2-l1},{-l1,l1,-l1},{-l1-l1*pct,l1-h2*l1,0}}
	p[6]={{0,l1*(1+pct),l1-l1*h2},{l1,l1,l1},{l1+l1*pct,l1-h2*l1,0},{l1,l1,-l1},{0,l1*(1+pct),l1*h2-l1}}
	p[7]={{-l1,-l1,l1},{0,-l1*(1+pct),l1-l1*h2},{0,-l1*(1+pct),l1*h2-l1},{-l1,-l1,-l1},{-l1-l1*pct,h2*l1-l1,0}}
	p[8]={{0,-l1*(1+pct),l1-l1*h2},{l1,-l1,l1},{l1+l1*pct,h2*l1-l1,0},{l1,-l1,-l1},{0,-l1*(1+pct),l1*h2-l1}}
	p[9]={{h2*l1-l1,0,l1+l1*pct},{-l1,-l1,l1},{-l1-l1*pct,h2*l1-l1,0},{-l1-l1*pct,l1-h2*l1,0},{-l1,l1,l1}}
	p[10]={{-l1-l1*pct,h2*l1-l1,0},{-l1,-l1,-l1},{h2*l1-l1,0,-l1-l1*pct},{-l1,l1,-l1},{-l1-l1*pct,l1-h2*l1,0}}
	p[11]={{-l1,l1,l1},{-l1+h2*l1,0,l1+l1*pct},{l1-h2*l1,0,l1+l1*pct},{l1,l1,l1},{0,l1*(1+pct),l1-l1*h2}}
	p[12]={{-l1+h2*l1,0,l1+l1*pct},{-l1,-l1,l1},{0,-l1*(1+pct),l1-l1*h2},{l1,-l1,l1},{l1-h2*l1,0,l1+l1*pct}}
	return space.info(p,cp,angle,axis,fsc,x,y,z,color,light),p
end

function space.antiprism(n,h,r,angle,axis,fsc,x,y,z,color,light)
	local deg=math.rad(360/n) local p={{},{}} local center_pos={0,0,0}
	for i=1,n do
		p[1][i]={math.cos((i-1/2)*deg)*r,math.sin((i-1/2)*deg)*r,-h/2}
		p[2][i]={math.cos(i*deg)*r,math.sin(i*deg)*r,h/2}
	end
	for i=1,n-1 do
		p[#p+1]={p[1][i],p[2][i],p[1][i+1]} p[#p+1]={p[1][i+1],p[2][i],p[2][i+1]}
	end
	p[#p+1]={p[1][n],p[2][n],p[1][1]} p[#p+1]={p[1][1],p[2][n],p[2][1]}
	return space.info(p,center_pos,angle,axis,fsc,x,y,z,color,light),p
end

function space.expanded_cube(len,a,angle,axis,fsc,x,y,z,color,light)
	local _,p=space.cuboid(len,len,len,{},{}) local center_pos={0,0,0} p[1]=space.translate(p[1],0,0,-a) p[2]=space.translate(p[2],0,a)
	p[3]=space.translate(p[3],a) p[4]=space.translate(p[4],-a) p[5]=space.translate(p[5],0,-a) p[6]=space.translate(p[6],0,0,a)
	local l1=len/2 local l2=len/2+a p[7]={{-l1,l1,l2},{l1,l1,l2},{l1,l2,l1},{-l1,l2,l1}} p[8]={{l1,l2,l1},{l1,l1,l2},{l2,l1,l1}}
	p[9]={{l1,l1,l2},{l1,-l1,l2},{l2,-l1,l1},{l2,l1,l1}} p[10]={{l2,-l1,l1},{l1,-l1,l2},{l1,-l2,l1}}
	p[11]={{l1,-l1,l2},{l1,-l2,l1},{-l1,-l2,l1},{-l1,-l1,l2}} p[12]={{-l1,-l1,l2},{-l1,-l2,l1},{-l2,-l1,l1}}
	p[13]={{-l2,-l1,l1},{-l1,-l1,l2},{-l1,l1,l2},{-l2,l1,l1}} p[14]={{-l2,l1,l1},{-l1,l1,l2},{-l1,l2,l1}}
	p[15]={{l1,l2,l1},{l2,l1,l1},{l2,l1,-l1},{l1,l2,-l1}} p[16]={{l2,-l1,l1},{l1,-l2,l1},{l1,-l2,-l1},{l2,-l1,-l1}}
	p[17]={{-l1,-l2,l1},{-l1,-l2,-l1},{-l2,-l1,-l1},{-l2,-l1,l1}} p[18]={{-l2,l1,l1},{-l1,l2,l1},{-l1,l2,-l1},{-l2,l1,-l1}}
	for i=1,8 do
		p[#p+1]=space.rotate(p[6+i],180,"y")
	end
	return space.info(p,center_pos,angle,axis,fsc,x,y,z,color,light),p
end

function space.build_sphere_at_percent(r,pct)
	local pct=pct<0 and 0 or (pct>1 and 1 or pct) local p
	if pct<=1/3 then
		p=space.build_to_sphere_subdivide(0,r,pct*3)
	elseif pct<=2/3 then
		p=space.build_to_sphere_subdivide(1,r,(pct-1/3)*3)
	else p=space.build_to_sphere_subdivide(2,r,(pct-2/3)*3)
	end
	return p
end

function space.sphere_at_percent(r,pct,angle,axis,fsc,x,y,z,color,light)
	local p=space.build_sphere_at_percent(r,pct) local center_pos={0,0,0}
	return space.info(p,center_pos,angle,axis,fsc,x,y,z,color,light),p
end

function space.build_sphere_at_pct(r,pct)
	local pct=pct<0 and 0 or (pct>1 and 1 or pct) local p
	if pct<=1/3 then
		p=space.to_dodecahedron_subdivide(0,r,pct*3)
	elseif pct<=2/3 then
		p=space.to_dodecahedron_subdivide(1,r,(pct-1/3)*3)
	else p=space.to_dodecahedron_subdivide(2,r,(pct-2/3)*3)
	end
	return p
end

function space.sphere_at_pct(r,pct,angle,axis,fsc,x,y,z,color,light)
	local p=space.build_sphere_at_pct(r,pct) local center_pos={0,0,0}
	return space.info(p,center_pos,angle,axis,fsc,x,y,z,color,light),p
end

function space.truncated_prism(n,h,r,deg,mode,angle,axis,fsc,x,y,z,color,light)
	local ang=math.rad(360/n) local p={{},{}} local center_pos={0,0,0} local deg1=math.deg(math.atan(h/r))
	local deg=deg>deg1 and deg1 or (deg<-deg1 and -deg1 or deg) local a=r/math.cos(math.rad(deg))
	for i=1,n do
		p[1][i]={math.cos((i-1/2)*ang)*r,math.sin((i-1/2)*ang)*r,-h/2}
		p[2][i]={math.cos((i-1/2)*ang)*a,math.sin((i-1/2)*ang)*r,0}
	end
	if mode==1 then
		p[2]=space.translate(space.rotate(p[2],deg,"y"),0,0,h/2)
	else p[2]=space.translate(space.rotate(p[2],-deg,"y"),0,0,h/2)
	end
	for i=1,n-1 do
		p[i+2]={p[1][i],p[2][i],p[2][i+1],p[1][i+1]}
	end
	p[n+2]={p[1][n],p[2][n],p[2][1],p[1][1]}
	return space.info(p,center_pos,angle,axis,fsc,x,y,z,color,light),p
end

function space.truncated_prism2(n,h,r,deg1,deg2,angle,axis,fsc,x,y,z,color,light)
	local n=n%4==0 and n or math.ceil(n/4)*4 local ang=math.rad(360/n) local p={{},{},{}} local center_pos={0,0,0}
	local deg=math.deg(math.atan(h/r)) local deg1=deg1>deg and deg or deg1 local a=r/math.cos(math.rad(deg1))
	local deg2=deg2>deg and deg or deg2 local a1=r/math.cos(math.rad(deg2)) local p1,p2={},{}
	for i=1,n do
		p[1][i]={math.cos((i-1/2)*ang)*r,math.sin((i-1/2)*ang)*r,-h/2}
		p1[i]={math.cos((i-1/2)*ang)*a,math.sin((i-1/2)*ang)*r,0}
		p2[i]={math.cos((i-1/2)*ang)*a1,math.sin((i-1/2)*ang)*r,0}
	end
	p1=space.translate(space.rotate(p1,deg1,"y"),0,0,h/2) p2=space.translate(space.rotate(p2,-deg2,"y"),0,0,h/2)
	for i=n/4+1,3*n/4 do
		p[2][#p[2]+1]=p1[i]
		if i<=n/2 then
			p[3][i-n/4]=p2[i+n/2]
		else p[3][i-n/4]=p2[i-n/2]
		end
	end
	table.insert(p[2],{0,-r*math.cos(ang/2),h/2}) table.insert(p[2],1,{0,r*math.cos(ang/2),h/2})
	table.insert(p[3],{0,r*math.cos(ang/2),h/2}) table.insert(p[3],1,{0,-r*math.cos(ang/2),h/2})
	for i=1,n do
		if i<n/4 then
			p[i+3]={p[1][i],p[3][i+n/4+1],p[3][i+n/4+2],p[1][i+1]}
		elseif i==n/4 then
			p[i+3]={p[1][i],p[3][i+n/4+1],p[3][i+n/4+2],p[2][2],p[1][i+1]}
		elseif i<3*n/4 then
			p[i+3]={p[1][i],p[2][i-n/4+1],p[2][i-n/4+2],p[1][i+1]}
		elseif i==3*n/4 then
			p[i+3]={p[1][i],p[2][i-n/4+1],p[2][i-n/4+2],p[3][2],p[1][i+1]}
		elseif i<n then
			p[i+3]={p[1][i],p[3][i-3*n/4+1],p[3][i-3*n/4+2],p[1][i+1]}
		else p[i+3]={p[1][i],p[3][i-3*n/4+1],p[3][i-3*n/4+2],p[1][1]}
		end
	end
	return space.info(p,center_pos,angle,axis,fsc,x,y,z,color,light),p
end

function space.bicylinder(n,r,angle,axis,fsc,x,y,z,color,light)
	local n=n%4==0 and n or math.ceil(n/4)*4 local ang=math.rad(360/n) local p={}
	local a=r/math.cos(math.rad(45)) local solid,p0,p1={},{},{{}} local center_pos={0,0,0}
	for i=1,n do
		p0[i]={math.cos((i-1/2)*ang)*a,0,math.sin((i-1/2)*ang)*r}
	end
	p0=space.rotate(p0,45,"z")
	for i=n/4+1,3*n/4 do
		p1[1][#p1[1]+1]=p0[i]
	end
	table.insert(p1[1],{0,0,-r*math.cos(ang/2)}) table.insert(p1[1],1,{0,0,r*math.cos(ang/2)})
	p1[2]=space.rotate(p1[1],90,"z") p1[3]=space.rotate(p1[1],180,"z") p1[4]=space.rotate(p1[1],270,"z")
	p[1]={p1[1][2],p1[2][2],p1[3][2],p1[4][2]} p[2]={p1[1][#p1[1]-1],p1[2][#p1[1]-1],p1[3][#p1[1]-1],p1[4][#p1[1]-1]}
	for i=1,4 do
		for i2=2,#p1[1]-2 do
			local idx=i==4 and 1 or i+1 p[#p+1]={p1[i][i2],p1[i][i2+1],p1[idx][i2+1],p1[idx][i2]}
		end
	end
	return space.info(p,center_pos,angle,axis,fsc,x,y,z,color,light),p
end

function space.pumpkin(n,n1,r,angle,axis,fsc,x,y,z,color,light)
	local n=n%4==0 and n or math.ceil(n/4)*4 local ang=math.rad(360/n) local p={{},{}} local n1=n1<3 and 3 or n1
	local deg=360/n1 local deg1=90-deg
	local a=r/math.cos(math.rad(deg1)) local solid,p0,p1={},{},{{}} local center_pos={0,0,0}
	for i=1,n do
		p0[i]={math.cos((i-1/2)*ang)*a,0,math.sin((i-1/2)*ang)*r}
	end
	p0=space.rotate(p0,deg1,"z")
	for i=n/4+1,3*n/4 do
		p1[1][#p1[1]+1]=p0[i]
	end
	for i=1,n1 do
		p1[i]=space.rotate(p1[1],deg*(i-1),"z") p[1][i]=p1[i][1] p[2][i]=p1[i][#p1[1]]
	end
	for i=1,n1 do
		for i2=1,#p1[1]-1 do
			local idx=i==n1 and 1 or i+1 p[#p+1]={p1[i][i2],p1[i][i2+1],p1[idx][i2+1],p1[idx][i2]}
		end
	end
	return space.info(p,center_pos,angle,axis,fsc,x,y,z,color,light),p
end

function space.cylindrical_wedge(n,h1,h2,r,deg1,deg2,mode,angle,axis,fsc,x,y,z,color,light)
	local ang=math.rad(360/n) local p={{},{}} local center_pos={0,0,0} local deg,deg0=math.deg(math.atan(h1/r)),math.deg(math.atan(h2/r))
	local deg1=deg1>deg and deg or (deg1<-deg and -deg or deg1) local deg2=deg2>deg0 and deg0 or (deg2<-deg0 and -deg0 or deg2)
	local a=r/math.cos(math.rad(deg1)) local a1=r/math.cos(math.rad(deg2))
	for i=1,n do
		p[1][i]={math.cos((i-1/2)*ang)*a,math.sin((i-1/2)*ang)*r,0} p[2][i]={math.cos((i-1/2)*ang)*a1,math.sin((i-1/2)*ang)*r,0}
	end
	if mode==11 then
		p[1]=space.translate(space.rotate(p[1],deg1,"y"),0,0,h1) p[2]=space.translate(space.rotate(p[2],deg2,"y"),0,0,-h2)
	elseif mode==12 then
		p[1]=space.translate(space.rotate(p[1],deg1,"y"),0,0,h1) p[2]=space.translate(space.rotate(p[2],-deg2,"y"),0,0,-h2)
	elseif mode==21 then
		p[1]=space.translate(space.rotate(p[1],-deg1,"y"),0,0,h1) p[2]=space.translate(space.rotate(p[2],deg2,"y"),0,0,-h2)
	else p[1]=space.translate(space.rotate(p[1],-deg1,"y"),0,0,h1) p[2]=space.translate(space.rotate(p[2],-deg2,"y"),0,0,-h2)
	end
	for i=1,n-1 do
		p[i+2]={p[1][i],p[2][i],p[2][i+1],p[1][i+1]}
	end
	p[n+2]={p[1][n],p[2][n],p[2][1],p[1][1]}
	return space.info(p,center_pos,angle,axis,fsc,x,y,z,color,light),p
end

function space.cylindrical_wedge2(n,h1,h2,r,deg1,deg2,mode,angle,axis,fsc,x,y,z,color,light)
	local n=n%4==0 and n or math.ceil(n/4)*4 local ang=math.rad(360/n) local p={{},{}} local center_pos={0,0,0}
	local deg,deg0=math.deg(math.atan(h1/r)),math.deg(math.atan(h2/r)) local deg1=deg1>deg and deg or (deg1<-deg and -deg or deg1)
	local deg2=deg2>deg0 and deg0 or (deg2<-deg0 and -deg0 or deg2) local a=r/math.cos(math.rad(deg1)) local a1=r/math.cos(math.rad(deg2))
	for i=1,n do
		p[1][i]={math.cos((i-1/2)*ang)*a,math.sin((i-1/2)*ang)*r,0} p[2][i]={math.sin((1/2-i)*ang)*r,math.cos((1/2-i)*ang)*a1,0}
	end
	if mode==11 then
		p[1]=space.translate(space.rotate(p[1],deg1,"y"),0,0,h1) p[2]=space.translate(space.rotate(p[2],deg2,"x"),0,0,-h2)
	elseif mode==12 then
		p[1]=space.translate(space.rotate(p[1],deg1,"y"),0,0,h1) p[2]=space.translate(space.rotate(p[2],-deg2,"x"),0,0,-h2)
	elseif mode==21 then
		p[1]=space.translate(space.rotate(p[1],-deg1,"y"),0,0,h1) p[2]=space.translate(space.rotate(p[2],deg2,"x"),0,0,-h2)
	else p[1]=space.translate(space.rotate(p[1],-deg1,"y"),0,0,h1) p[2]=space.translate(space.rotate(p[2],-deg2,"x"),0,0,-h2)
	end
	for i=1,n do
		local num1=i==n and 1 or i+1 local idx=i>n*1/4 and i-n/4 or i+n*3/4 local num2=i==1*n/4 and 1 or idx+1
		p[i+2]={p[1][i],p[2][idx],p[2][num2],p[1][num1]}
	end
	return space.info(p,center_pos,angle,axis,fsc,x,y,z,color,light),p
end

function space.tunnel(n,h1,h2,r,deg1,deg2,angle,axis,fsc,x,y,z,color,light)
	local ang=math.rad(360/n) local p={{},{}} local center_pos={0,0,0} local deg,deg0=math.deg(math.atan(h1/r)),math.deg(math.atan(h2/r))
	local deg1=deg1>deg and deg or (deg1<-deg and -deg or deg1) local deg2=deg2>deg0 and deg0 or (deg2<-deg0 and -deg0 or deg2)
	local a=r/math.cos(math.rad(deg1)) local a1=r/math.cos(math.rad(deg2)) local p1,p2={},{}
	for i=1,n do
		p1[i]={math.cos((i-1/2)*ang)*a,math.sin((i-1/2)*ang)*r,0}
		p2[i]={math.cos((i-1/2)*ang)*a1,math.sin((i-1/2)*ang)*r,0}
	end
	p1=space.translate(space.rotate(p1,deg1,"y"),0,0,h1) p2=space.translate(space.rotate(p2,-deg2,"y"),0,0,-h2)
	for i=n/4+1,3*n/4 do
		p[1][#p[1]+1]=p1[i] p[2][#p[2]+1]=p2[i]
	end
	table.insert(p[1],{0,-r*math.cos(ang/2),h1}) table.insert(p[1],1,{0,r*math.cos(ang/2),h1})
	table.insert(p[2],{0,-r*math.cos(ang/2),-h2}) table.insert(p[2],1,{0,r*math.cos(ang/2),-h2})
	for i=1,#p[1] do
		local idx=i==#p[1] and 1 or i+1 p[i+2]={p[1][i],p[1][idx],p[2][idx],p[2][i]}
	end
	p=space.shift_solid(p,r/2)
	return space.info(p,center_pos,angle,axis,fsc,x,y,z,color,light),p
end

function space.dome_on_truncated_prism(n,h,r,deg,angle,axis,fsc,x,y,z,color,light)
	local ang=math.rad(360/2^(n+2)) local center_pos={0,0,0} local deg1=math.deg(math.atan(h/r))
	local deg=deg>deg1 and deg1 or (deg<-deg1 and -deg1 or deg) local a=r/math.cos(math.rad(deg))
	local solid=space.build_dome_subdivide(n,r) local center_pos={0,0,0} local temp={}
	local p=space.rotate_solid(space.shift_solid(solid,0,0,(h-r)/2),180/2^(n+2),"z") local idx=#p p[idx+2^(n+2)+1]={}
	for i=1,2^(n+2) do
		p[idx+2^(n+2)+1][i]={math.cos((i-1/2)*ang)*a,math.sin((i-1/2)*ang)*r,0} temp[i]={math.cos((i-1/2)*ang)*r,math.sin((i-1/2)*ang)*r,(h-r)/2}
	end
	p[idx+2^(n+2)+1]=space.translate(space.rotate(p[idx+2^(n+2)+1],deg,"y"),0,0,-(h+r)/2)
	for i=1,2^(n+2) do
		local i2=i==2^(n+2) and 1 or i+1
		p[idx+i]={temp[i],temp[i2],p[idx+2^(n+2)+1][i2],p[idx+2^(n+2)+1][i]}
	end
	return space.info(p,center_pos,angle,axis,fsc,x,y,z,color,light),p
end

function space.difrustum(n,h,r1,r2,angle,axis,fsc,x,y,z,color,light)
	local s0,p0=space.frustum(n,h/2,r1,r2,{},{}) local solids={}
	local p1,p2={},{}
	for i=1,#p0 do
		p1[i]=space.translate(space.rotate(p0[i],180,"x"),0,0,-h/4)
		p2[i]=space.translate(p0[i],0,0,h/4)
	end
	solids[1]=p1 solids[2]=p2
	return space.concave(solids,angle,axis,fsc,x,y,z,color,light),solids
end

function space.double_pyramid(n,h,r,angle,axis,fsc,x,y,z,color,light)
	local _,p0=space.pyramid(n,h/2,r,{},{}) local solids,p1,p2={},{},{}
	for i=1,#p0 do
		p1[i]=space.translate(space.rotate(p0[i],180,"x"),0,0,h/4)
		p2[i]=space.translate(p0[i],0,0,-h/4)
	end
	solids[1]=p1 solids[2]=p2
	return space.concave(solids,angle,axis,fsc,x,y,z,color,light),solids
end

function space.double_pyramid2(n,h,r,angle,axis,fsc,x,y,z,color,light)
	local _,p0=space.pyramid(n,h/2,r,{},{}) local solids,p1={},{}
	for i=1,#p0 do
		p1[i]=space.translate(space.rotate(p0[i],180,"x"),0,0,h/4)
	end
	solids[1]=p1 solids[2]=space.shift_solid(p1,0,0,-h/2)
	return space.concave(solids,angle,axis,fsc,x,y,z,color,light),solids
end

function space.cross_simple(rw,len,h,angle,axis,fsc,x,y,z,color,light)
	local _,p0=space.cuboid(rw,h,(len-rw)/2,{},{}) local solids,p={},{}
	table.remove(p0,1) p[1]=space.shift_solid(p0,0,0,rw/2+(len-rw)/4)
	p[1]=space.rotate_solid(p[1],90,"y") p[2]=space.rotate_solid(p[1],180,"z") p[3]=space.rotate_solid(p[1],90,"z")
	p[4]=space.rotate_solid(p[3],180,"z") p[5]={} p[5][1]={{rw/2,rw/2,h/2},{rw/2,-rw/2,h/2},{-rw/2,-rw/2,h/2},{-rw/2,rw/2,h/2}}
	p[5][2]={{rw/2,rw/2,-h/2},{rw/2,-rw/2,-h/2},{-rw/2,-rw/2,-h/2},{-rw/2,rw/2,-h/2}}
	solids={p[1],p[2],p[3],p[4],p[5]}
	return space.concave(solids,angle,axis,fsc,x,y,z,color,light),solids
end

function space.cross(rw,h,angle,axis,fsc,x,y,z,color,light)
	local _,p0=space.cuboid(rw,rw,(h-rw)/2,{},{}) local solids,p={},{}
	table.remove(p0,1)
	p[1]=space.shift_solid(p0,0,0,rw/2+(h-rw)/4) p[2]=space.rotate_solid(p[1],180,"x") p[3]=space.rotate_solid(p[1],90,"x")
	p[4]=space.rotate_solid(p[3],180,"x") p[5]=space.rotate_solid(space.rotate_solid(p[1],90,"x"),90,"z") p[6]=space.rotate_solid(p[5],180,"z")
	solids={p[1],p[2],p[3],p[4],p[5],p[6]}
	return space.concave(solids,angle,axis,fsc,x,y,z,color,light),solids
end

function space.star(h,r,angle,axis,fsc,x,y,z,color,light)
	local p,vertex,solids={},{},{} local r2=r*math.cos(math.rad(72))/math.sin(math.rad(54))
	for i=1,5 do
		vertex[#vertex+1]={r*math.cos(math.rad(18+(i-1)*72)),r*math.sin(math.rad(18+(i-1)*72)),0}
		vertex[#vertex+1]={r2*math.cos(math.rad(54+(i-1)*72)),r2*math.sin(math.rad(54+(i-1)*72)),0}
	end
	for i=1,9 do
		p[#p+1]={vertex[i],{0,0,h/2},vertex[i+1]} p[#p+1]={vertex[i],{0,0,-h/2},vertex[i+1]}
	end
	p[#p+1]={vertex[10],{0,0,h/2},vertex[1]} p[#p+1]={vertex[10],{0,0,-h/2},vertex[1]} solids[1]={p[1],p[2],p[19],p[20]}
	for i=3,18,4 do
		solids[#solids+1]={p[i],p[i+1],p[i+2],p[i+3]}
	end
	return space.concave(solids,angle,axis,fsc,x,y,z,color,light),solids
end

function space.star_fat(h,r,angle,axis,fsc,x,y,z,color,light)
	local p,vertex,solids={},{},{} local r2=r/2/math.sin(math.rad(54))
	for i=1,5 do
		vertex[#vertex+1]={r*math.cos(math.rad(18+(i-1)*72)),r*math.sin(math.rad(18+(i-1)*72)),0}
		vertex[#vertex+1]={r2*math.cos(math.rad(54+(i-1)*72)),r2*math.sin(math.rad(54+(i-1)*72)),0}
	end
	for i=1,9 do
		p[#p+1]={vertex[i],{0,0,h/2},vertex[i+1]} p[#p+1]={vertex[i],{0,0,-h/2},vertex[i+1]}
	end
	p[#p+1]={vertex[10],{0,0,h/2},vertex[1]} p[#p+1]={vertex[10],{0,0,-h/2},vertex[1]} solids[1]={p[1],p[2],p[19],p[20]}
	for i=3,18,4 do
		solids[#solids+1]={p[i],p[i+1],p[i+2],p[i+3]}
	end
	return space.concave(solids,angle,axis,fsc,x,y,z,color,light),solids
end

function space.star_n(n,h,r1,r2,angle,axis,fsc,x,y,z,color,light)
	local deg=math.rad(180/n) local s,p,solids={},{},{} if r1<r2 then r1,r2=r2,r1 end
	for i=1,4*n do
		if i%2==1 then
			s[i]={math.cos((i-1/2)*deg)*r1,math.sin((i-1/2)*deg)*r1,0}
		else s[i]={math.cos((i-1/2)*deg)*r2,math.sin((i-1/2)*deg)*r2,0}
		end
	end
	for i=1,4*n-1 do
		p[#p+1]={s[i],{0,0,h/2},s[i+1]} p[#p+1]={s[i],{0,0,-h/2},s[i+1]}
	end
	p[#p+1]={s[4*n],{0,0,h/2},s[1]} p[#p+1]={s[4*n],{0,0,-h/2},s[1]} solids[1]={p[1],p[2],p[4*n-1],p[4*n]}
	for i=3,4*n-2,4 do
		solids[#solids+1]={p[i],p[i+1],p[i+2],p[i+3]}
	end
	return space.concave(solids,angle,axis,fsc,x,y,z,color,light),solids
end

function space.star_n_simple(n,h,r1,r2,angle,axis,fsc,x,y,z,color,light)
	local deg=math.rad(180/n) local s,p,solids={},{},{} if r1<r2 then r1,r2=r2,r1 end
	for i=1,4*n do
		if i%2==1 then
			s[i]={math.cos((i-1/2)*deg)*r1,math.sin((i-1/2)*deg)*r1,-h/2}
		else s[i]={math.cos((i-1/2)*deg)*r2,math.sin((i-1/2)*deg)*r2,-h/2}
		end
	end
	for i=1,2*n,2 do
		local i1=i==1 and 2*n or i-1 local i2=i==2*n and 1 or i+1
		p[#p+1]={s[i1],{0,0,-h/2},s[i2],s[i]} p[#p+1]=space.translate(p[#p],0,0,h)
		p[#p+1]={s[i1],{s[i1][1],s[i1][2],h/2},{s[i][1],s[i][2],h/2},s[i]} p[#p+1]={s[i],{s[i][1],s[i][2],h/2},{s[i2][1],s[i2][2],h/2},s[i2]}
	end
	for i=1,#p,4 do
		solids[#solids+1]={p[i],p[i+1],p[i+2],p[i+3]}
	end
	return space.concave(solids,angle,axis,fsc,x,y,z,color,light),solids
end

function space.star_n_platform(n,h,r0,r1,pct,angle,axis,fsc,x,y,z,color,light)
	local deg=math.rad(180/n) local s,s0,p,solids={},{},{},{} if r0<r1 then r0,r1=r1,r0 end local r2,r3=r0*pct,r1*pct local cp={}
	for i=1,4*n do
		if i%2==1 then
			s[i]={math.cos((i-1/2)*deg)*r0,math.sin((i-1/2)*deg)*r0,-h/2} s0[i]={math.cos((i-1/2)*deg)*r2,math.sin((i-1/2)*deg)*r2,h/2}
		else s[i]={math.cos((i-1/2)*deg)*r1,math.sin((i-1/2)*deg)*r1,-h/2} s0[i]={math.cos((i-1/2)*deg)*r3,math.sin((i-1/2)*deg)*r3,h/2}
		end
	end
	for i=1,2*n,2 do
		local i1=i==1 and 2*n or i-1 local i2=i==2*n and 1 or i+1
		p[#p+1]={s[i1],{0,0,-h/2},s[i2],s[i]} local p1=space.center_pos({p[#p]}) p[#p+1]={s0[i1],{0,0,h/2},s0[i2],s0[i]}
		local p2=space.center_pos({p[#p]}) p[#p+1]={s[i1],s0[i1],s0[i],s[i]} p[#p+1]={s[i],s0[i],s0[i2],s[i2]} cp[#cp+1]=space.center_pos({{p1,p2}})
	end
	for i=1,#p,4 do
		solids[#solids+1]={p[i],p[i+1],p[i+2],p[i+3]}
	end
	solids.cp=cp
	return space.concave(solids,angle,axis,fsc,x,y,z,color,light),solids
end

function space.star_n_platform2(n,h,r0,r1,pct,angle,axis,fsc,x,y,z,color,light)
	local deg=math.rad(180/n) local s,s0,p,solids={},{},{},{} if r0<r1 then r0,r1=r1,r0 end local r2,r3=r0*pct,r1*pct local cp={}
	for i=1,4*n do
		if i%2==1 then
			s[i]={math.cos((i-1/2)*deg)*r0,math.sin((i-1/2)*deg)*r0,0} s0[i]={math.cos((i-1/2)*deg)*r2,math.sin((i-1/2)*deg)*r2,h/2}
		else s[i]={math.cos((i-1/2)*deg)*r1,math.sin((i-1/2)*deg)*r1,0} s0[i]={math.cos((i-1/2)*deg)*r3,math.sin((i-1/2)*deg)*r3,h/2}
		end
	end
	for i=1,2*n,2 do
		local i1=i==1 and 2*n or i-1 local i2=i==2*n and 1 or i+1 local p0=space.center_pos({{s[i1],{0,0,0},s[i2],s[i]}})
		p[#p+1]={{s0[i1][1],s0[i1][2],-h/2},{0,0,-h/2},{s0[i2][1],s0[i2][2],-h/2},{s0[i][1],s0[i][2],-h/2}} local p1=space.center_pos({p[#p]})
		cp[#cp+1]=space.center_pos({{p0,p1}}) p[#p+1]={s[i1],{s0[i1][1],s0[i1][2],-h/2},{s0[i][1],s0[i][2],-h/2},s[i]}
		p[#p+1]={s[i],{s0[i][1],s0[i][2],-h/2},{s0[i2][1],s0[i2][2],-h/2},s[i2]} p[#p+1]={s0[i1],{0,0,h/2},s0[i2],s0[i]}
		local p2=space.center_pos({p[#p]}) cp[#cp+1]=space.center_pos({{p0,p2}})
		p[#p+1]={s[i],s0[i],s0[i2],s[i2]} p[#p+1]={s[i1],s0[i1],s0[i],s[i]}
	end
	for i=1,#p,3 do
		solids[#solids+1]={p[i],p[i+1],p[i+2]}
	end
	solids.cp=cp
	return space.concave(solids,angle,axis,fsc,x,y,z,color,light),solids
end

function space.thorn(rw,h,angle,axis,fsc,x,y,z,color,light)
	local _,p0=space.pyramid(4,(h-rw)/2,rw/2^0.5,{},{}) local solids,p={},{}
	table.remove(p0,1)
	p[1]=space.shift_solid(p0,0,0,rw/2+(h-rw)/4) p[2]=space.rotate_solid(p[1],180,"x") p[3]=space.rotate_solid(p[1],90,"x")
	p[4]=space.rotate_solid(p[3],180,"x") p[5]=space.rotate_solid(space.rotate_solid(p[1],90,"x"),90,"z") p[6]=space.rotate_solid(p[5],180,"z")
	solids={p[1],p[2],p[3],p[4],p[5],p[6]}
	return space.concave(solids,angle,axis,fsc,x,y,z,color,light),solids
end

function space.arrow(n,h1,h2,r1,r2,angle,axis,fsc,x,y,z,color,light)
	local _,p0=space.pyramid(n,h1,r1,{},{}) local _2,p1=space.prism(n,h2,r2,{},{}) local cp={{0,0,-(h1+h2)/2},{0,0,0}}
	local solids={space.shift_solid(space.rotate_solid(p0,180,"x"),0,0,-(h1+h2)/2),p1} solids.cp=cp
	return space.concave(solids,angle,axis,fsc,x,y,z,color,light),solids
end

function space.arrow2(n,h1,h2,r1,r2,angle,axis,fsc,x,y,z,color,light)
	local _,p0=space.pyramid(n,h1,r1,{},{}) local _2,p1=space.prism(n,h2,r2,{},{})
	local solids={space.shift_solid(space.rotate_solid(p0,180,"x"),0,0,-(h1+h2)/2),p1} solids[3]=space.rotate_solid(solids[1],180,"x")
	local cp={{0,0,-(h1+h2)/2},{0,0,0},{0,0,(h1+h2)/2}} solids.cp=cp
	return space.concave(solids,angle,axis,fsc,x,y,z,color,light),solids
end

function space.arrow_xyz(n,h1,h2,r1,r2,angle,axis,fsc,x,y,z,color,light)
	local _,p0=space.pyramid(n,h1,r1,{},{}) local _2,p1=space.prism(n,h2,r2,{},{}) local _3,cube=space.cuboid(2*r2,2*r2,2*r2,{},{})
	local solids={cube} solids[2]=space.shift_solid(space.rotate_solid(p0,180,"x"),0,0,-(h1+2*h2+2*r2)/2)
	solids[3]=space.shift_solid(p1,0,0,-(h2+2*r2)/2) local arrow_x=space.rotate_solids({solids[2],solids[3]},-90,"y")
	local arrow_y=space.rotate_solids({solids[2],solids[3]},-90,"x") solids[4]=arrow_y[1] solids[5]=arrow_y[2] solids[6]=arrow_x[1]
	solids[7]=arrow_x[2] local cp={{0,0,0},{0,0,-(h1+2*h2+2*r2)/2},{0,0,-(h2+2*r2)/2},{0,-(h1+2*h2+2*r2)/2,0},{0,-(h2+2*r2)/2,0}}
	cp[6]={-(h1+2*h2+2*r2)/2,0,0} cp[7]={-(h2+2*r2)/2,0,0}solids.cp=cp
	return space.concave(solids,angle,axis,fsc,x,y,z,color,light),solids
end

function space.mushroom(n,h1,h2,r1,r2,r3,angle,axis,fsc,x,y,z,color,light)
	local _,p0=space.frustum(n,h1,r1,r2,{},{}) local _2,p1=space.prism(n,h2,r3,{},{}) local cp={{0,0,-(h1+h2)/2},{0,0,0}}
	local solids={space.shift_solid(space.rotate_solid(p0,180,"x"),0,0,-(h1+h2)/2),p1} solids.cp=cp
	return space.concave(solids,angle,axis,fsc,x,y,z,color,light),solids
end

function space.mushroom2(n,h1,h2,r1,r2,r3,angle,axis,fsc,x,y,z,color,light)
	local _,p0=space.frustum(n,h1,r1,r2,{},{}) local _2,p1=space.prism(n,h2,r3,{},{})
	local solids={space.shift_solid(space.rotate_solid(p0,180,"x"),0,0,-(h1+h2)/2),p1} solids[3]=space.rotate_solid(solids[1],180,"x")
	local cp={{0,0,-(h1+h2)/2},{0,0,0},{0,0,(h1+h2)/2}} solids.cp=cp
	return space.concave(solids,angle,axis,fsc,x,y,z,color,light),solids
end

function space.quadruple_pyramid(n,h,r,angle,axis,fsc,x,y,z,color,light)
	local _,p0=space.pyramid(n,h/2,r,{},{}) local p={}
	p[1]=space.shift_solid(p0,0,0,-h/4) p[2]=space.rotate_solid(p[1],180,"x")
	p[3]=space.rotate_solid(p[1],90,"x") p[4]=space.rotate_solid(p[1],-90,"x")
	local solids={p[1],p[2],p[3],p[4]}
	return space.concave(solids,angle,axis,fsc,x,y,z,color,light),solids
end

function space.sextuple_pyramid(n,h,r,angle,axis,fsc,x,y,z,color,light)
	local _,p0=space.pyramid(n,h/2,r,{},{}) local p,cp={},{{0,0,-h/4},{0,0,h/4},{0,h/4,0},{0,-h/4,0},{-h/4,0,0},{h/4,0,0}}
	p[1]=space.shift_solid(p0,0,0,-h/4) p[2]=space.rotate_solid(p[1],180,"x") p[3]=space.rotate_solid(p[1],90,"x")
	p[4]=space.rotate_solid(p[1],-90,"x") p[5]=space.rotate_solid(p[4],90,"z") p[6]=space.rotate_solid(p[4],-90,"z")
	local solids={p[1],p[2],p[3],p[4],p[5],p[6]} solids.cp=cp
	return space.concave(solids,angle,axis,fsc,x,y,z,color,light),solids
end

function space.quadruple_dipyramid(n,h1,h2,r,angle,axis,fsc,x,y,z,color,light)
	local h2=h2<r and r or h2 local _,p0=space.dipyramid(n,h1,h2,r,{},{}) local p={}
	p[1]=space.shift_solid(p0,0,0,-h2) p[2]=space.rotate_solid(p[1],180,"x")
	p[3]=space.rotate_solid(p[1],90,"x") p[4]=space.rotate_solid(p[1],-90,"x")
	local solids={p[1],p[2],p[3],p[4]}
	return space.concave(solids,angle,axis,fsc,x,y,z,color,light),solids
end

function space.sextuple_dipyramid(n,h1,h2,r,angle,axis,fsc,x,y,z,color,light)
	local h2=h2<r and r or h2 local _,p0=space.dipyramid(n,h1,h2,r,{},{}) local p={}
	p[1]=space.shift_solid(p0,0,0,-h2) p[2]=space.rotate_solid(p[1],180,"x") p[3]=space.rotate_solid(p[1],90,"x")
	p[4]=space.rotate_solid(p[1],-90,"x") p[5]=space.rotate_solid(p[4],90,"z") p[6]=space.rotate_solid(p[4],-90,"z")
	local solids={p[1],p[2],p[3],p[4],p[5],p[6]}
	return space.concave(solids,angle,axis,fsc,x,y,z,color,light),solids
end

function space.quadruple_frustum(h,r,r2,angle,axis,fsc,x,y,z,color,light)
	local rw=2*r/2^0.5 local _,p0=space.frustum(4,(h-rw)/2,r2,r,{},{}) local p={} table.remove(p0,2)
	p[1]=space.shift_solid(p0,0,0,rw/2+(h-rw)/4) p[2]=space.rotate_solid(p[1],180,"x")
	p[3]=space.rotate_solid(p[1],90,"x") p[4]=space.rotate_solid(p[3],180,"x") p[5]={}
	p[5][1]={{rw/2,rw/2,rw/2},{rw/2,-rw/2,rw/2},{rw/2,-rw/2,-rw/2},{rw/2,rw/2,-rw/2}}
	p[5][2]={{-rw/2,rw/2,rw/2},{-rw/2,-rw/2,rw/2},{-rw/2,-rw/2,-rw/2},{-rw/2,rw/2,-rw/2}}
	local solids={p[1],p[2],p[3],p[4],p[5]}
	return space.concave(solids,angle,axis,fsc,x,y,z,color,light),solids
end

function space.sextuple_frustum(h,r,r2,angle,axis,fsc,x,y,z,color,light)
	local rw=2*r/2^0.5 local _,p0=space.frustum(4,(h-rw)/2,r2,r,{},{}) local p={} table.remove(p0,2)
	p[1]=space.shift_solid(p0,0,0,rw/2+(h-rw)/4) p[2]=space.rotate_solid(p[1],180,"x") p[3]=space.rotate_solid(p[1],90,"x")
	p[4]=space.rotate_solid(p[3],180,"x") p[5]=space.rotate_solid(space.rotate_solid(p[1],90,"x"),90,"z") p[6]=space.rotate_solid(p[5],180,"z")
	local solids={p[1],p[2],p[3],p[4],p[5],p[6]}
	return space.concave(solids,angle,axis,fsc,x,y,z,color,light),solids
end

function space.elongated_pyramid4(h1,h2,r,angle,axis,fsc,x,y,z,color,light)
	local _,p0=space.elongated_pyramid(4,h1,h2,r,{},{}) local p,len={},r*2^0.5/2 table.remove(p0,1)
	p[1]=space.rotate_solid(space.shift_solid(p0,0,0,(h1+h2+r*2^0.5)/2),90,"x") p[2]=space.rotate_solid(p[1],180,"z")
	p[3]=space.rotate_solid(p[1],90,"z") p[4]=space.rotate_solid(p[1],-90,"z") p[5]={{{-len,len,len},{len,len,len},{len,-len,len},{-len,-len,len}}}
	p[5][2]={{-len,len,-len},{len,len,-len},{len,-len,-len},{-len,-len,-len}}
	local solids={p[1],p[2],p[3],p[4],p[5]}
	return space.concave(solids,angle,axis,fsc,x,y,z,color,light),solids
end

function space.elongated_pyramid6(h1,h2,r,angle,axis,fsc,x,y,z,color,light)
	local _,p0=space.elongated_pyramid(4,h1,h2,r,{},{}) local p,len={},r*2^0.5/2 table.remove(p0,1)
	p[1]=space.shift_solid(p0,0,0,(h1+h2+r*2^0.5)/2) p[2]=space.rotate_solid(p[1],180,"x") p[3]=space.rotate_solid(p[1],90,"x")
	p[4]=space.rotate_solid(p[1],-90,"x") p[5]=space.rotate_solid(p[2],-90,"y") p[6]=space.rotate_solid(p[2],90,"y")
	local solids={p[1],p[2],p[3],p[4],p[5],p[6]}
	return space.concave(solids,angle,axis,fsc,x,y,z,color,light),solids
end

function space.tower(h,dh,r,dr,angle,axis,fsc,x,y,z,color,light)
	local cnt,mul=0,math.floor(r/dr+1-1) local solids,p0,hi,middle={},{},{},math.floor(h+dh*(mul+1)/2)
	for i=1,mul do
		hi[i]=h+dh*(i-1) cnt=cnt+hi[i]
		_,p0[i]=space.frustum(4,hi[i],r-dr*(i-1),r-dr*i,{},{}) solids[i]=space.shift_solid(p0[i],0,0,hi[i]/2-cnt)
	end
	solids=space.shift_solids(solids,0,0,cnt/2)
	return space.concave(solids,angle,axis,fsc,x,y,z,color,light),solids
end

function space.pyramiding(n,h,r,d,mul,angle,axis,fsc,x,y,z,color,light)
	local mul=r-d*(mul-1)>0 and mul or math.floor(r/d+1)
	local solids,p0,middle={},{},math.floor((mul+1)/2) local cp={}
	for i=1,mul do
		_,p0[i]=space.prism(n,h,r-d*(i-1),{},{}) cp[i]={0,0,h*(middle-i)}
		solids[i]=space.shift_solid(p0[i],0,0,h*(middle-i))
	end
	solids.cp=cp
	return space.concave(solids,angle,axis,fsc,x,y,z,color,light),solids
end

function space.pyramiding2(n,h,r,d,mul,angle,axis,fsc,x,y,z,color,light)
	local mul=r-d*(mul-1)>0 and mul or math.floor(r/d+1) local solids,p0={},{} local cp={}
	for i=1,mul do
		_,p0[i]=space.prism(n,h,r-d*(i-1),{},{}) cp[i]={0,0,h*(i-1)}
		solids[i]=space.shift_solid(p0[i],0,0,h*(i-1))
	end
	for i=1,mul-1 do
		_,p0[i]=space.prism(n,h,r-d*i,{},{}) cp[#cp+1]={0,0,-h*i}
		solids[#solids+1]=space.shift_solid(p0[i],0,0,-h*i)
	end
	solids.cp=cp
	return space.concave(solids,angle,axis,fsc,x,y,z,color,light),solids
end

function space.oblique_pyramid2(n,h,r,off_x,off_y,angle,axis,fsc,x,y,z,color,light)
	local _,p0=space.oblique_pyramid(n,h,r,off_x,off_y,{},{}) table.remove(p0,1) local cp={{0,0,h/2},{0,0,-h/2}}
	local solids={space.shift_solid(p0,0,0,h/2)} solids[2]=space.rotate_solid(solids[1],180,"x") solids.cp=cp
	return space.concave(solids,angle,axis,fsc,x,y,z,color,light),solids
end

function space.oblique_frustum2(n,h,r1,r2,off_x,off_y,angle,axis,fsc,x,y,z,color,light)
	local _,p0=space.oblique_frustum(n,h,r1,r2,off_x,off_y,{},{}) table.remove(p0,2) local cp={{0,0,h/2},{0,0,-h/2}}
	local solids={space.shift_solid(p0,0,0,h/2)} solids[2]=space.rotate_solid(solids[1],180,"x") solids.cp=cp
	return space.concave(solids,angle,axis,fsc,x,y,z,color,light),solids
end

function space.build_ring(n,h,r,d)
	local n=n<4 and 4 or n local deg=math.rad(360/n) local p={{}} local solids={}
	for i=1,n+1 do
		p[1][i]={math.cos((i-1/2)*deg)*r,math.sin((i-1/2)*deg)*r,-h/2}
		p[1][i+n+1]={math.cos((1/2-i)*deg)*(r-d),math.sin((1/2-i)*deg)*(r-d),-h/2}
	end
	for i=1,n-1 do
		local s=2*n+2-i local e=2*n+1-i solids[i]={}
		solids[i][1]={p[1][i],{p[1][i][1],p[1][i][2],h/2},{p[1][i+1][1],p[1][i+1][2],h/2},p[1][i+1]}
		solids[i][2]={p[1][s],{p[1][s][1],p[1][s][2],h/2},{p[1][e][1],p[1][e][2],h/2},p[1][e]}
		solids[i][3]={{p[1][i][1],p[1][i][2],h/2},{p[1][s][1],p[1][s][2],h/2},{p[1][e][1],p[1][e][2],h/2},{p[1][i+1][1],p[1][i+1][2],h/2}}
		solids[i][4]=space.translate(solids[i][3],0,0,-h)
	end
	solids[n]=space.rotate_solid(solids[1],360/n,"z")
	return solids
end

function space.ring(n,h,r,d,angle,axis,fsc,x,y,z,color,light)
	local solids=space.build_ring(n,h,r,d)
	return space.concave(solids,angle,axis,fsc,x,y,z,color,light),solids
end

function space.build_rings(n,h,r,d1,d2,mul)
	local mul=(r-d1)-(d1+d2)*(mul-1)<0 and (r-d1)/(d1+d2)+1 or mul local ring,solids={},{}
	for i=1,mul do
		ring[i]=space.build_ring(n,h,r-(d1+d2)*(i-1),d1)
		for i2=1,#ring[i] do
			solids[#solids+1]=ring[i][i2]
		end
	end
	return solids
end

function space.rings(n,h,r,d1,d2,mul,angle,axis,fsc,x,y,z,color,light)
	local solids=space.build_rings(n,h,r,d1,d2,mul)
	return space.concave(solids,angle,axis,fsc,x,y,z,color,light),solids
end

function space.build_donut(n1,n2,r1,r2)
	local n2=n2<5 and 5 or n2 local deg1,deg2=math.rad(360/n1),360/n2 local s,p={{}},{} local solids={}
	for i=1,n1 do
		s[1][i]={math.cos((i-1/2)*deg1)*r1,0,math.sin((i-1/2)*deg1)*r1}
	end
	s[1]=space.translate(s[1],r2)
	for i=1,n2+1 do
		s[i+1]=space.rotate(s[1],deg2*(i-1),"z")
	end
	for i=1,n2+1 do
		solids[i]={}
		for i2=1,n1 do
			local num1=i>n2 and 1 or i+1 local num2=i2+1>n1 and 1 or i2+1
			p[#p+1]={s[i][i2],s[i][num2],s[num1][num2],s[num1][i2]} solids[i][i2]=p[#p]
		end
	end
	return solids
end

function space.donut(n1,n2,r1,r2,angle,axis,fsc,x,y,z,color,light)
	local solids=space.build_donut(n1,n2,r1,r2)
	return space.concave(solids,angle,axis,fsc,x,y,z,color,light),solids
end

function space.build_donuts(n1,n2,r1,r2,d,mul)
	local mul=(r2-d-r1*2)-(r1*2+d)*(mul-1)<0 and (r2-d-r1*2)/(r1*2+d)+1 or mul local donut,solids={},{}
	for i=1,mul do
		donut[i]=space.build_donut(n1,n2,r1,r2-(r1*2+d)*(i-1))
		for i2=1,#donut[i] do
			solids[#solids+1]=donut[i][i2]
		end
	end
	return solids
end

function space.donuts(n1,n2,r1,r2,d,mul,angle,axis,fsc,x,y,z,color,light)
	local solids=space.build_donuts(n1,n2,r1,r2,d,mul)
	return space.concave(solids,angle,axis,fsc,x,y,z,color,light),solids
end

function space.build_half_donut(n1,n2,r1,r2,deg)
	local n1=n1%4==0 and n1 or math.ceil(n1/4)*4 local n2=n2<5 and 5 or n2 local s,p={{}},{}
	local deg1,deg2=math.rad(360/n1),360/n2 local solids={}
	for i=1,n1/2 do
		s[1][i]={math.cos((i-1/2)*deg1)*r1,0,math.sin((i-1/2)*deg1)*r1}
	end
	table.insert(s[1],{-r1*math.cos(deg1/2),0,0}) table.insert(s[1],1,{r1*math.cos(deg1/2),0,0})
	local deg=deg<0 and 0 or (deg>180 and 180 or deg) s[1]=space.translate(space.rotate(s[1],-deg,"y"),r2)
	for i=1,n2+1 do
		s[i+1]=space.rotate(s[1],deg2*(i-1),"z")
	end
	for i=1,n2+1 do
		solids[i]={}
		for i2=1,n1/2 do
			local num1=i>n2 and 1 or i+1 local num2=i2+1>n1/2 and 1 or i2+1
			p[#p+1]={s[i][i2],s[i][num2],s[num1][num2],s[num1][i2]} solids[i][i2]=p[#p]
		end
	end
	return solids
end

function space.half_donut(n1,n2,r1,r2,deg,angle,axis,fsc,x,y,z,color,light)
	local solids=space.build_half_donut(n1,n2,r1,r2,deg)
	return space.concave(solids,angle,axis,fsc,x,y,z,color,light),solids
end

function space.build_chain(n1,n2,r1,r2,len)
	local n2=n2%2==0 and n2 or n2+1 local deg1,deg2=math.rad(360/n1),360/n2 local s,p={{}},{} local solids={}
	for i=1,n1 do
		s[1][i]={math.cos((i-1/2)*deg1)*r1,0,math.sin((i-1/2)*deg1)*r1}
	end
	s[1]=space.translate(s[1],r2)
	for i=1,n2+1 do
		if i<=n2/2 then
			s[i+1]=space.translate(space.rotate(s[1],deg2*(i-1/2),"z"),0,-len/2)
		else s[i+1]=space.translate(space.rotate(s[1],deg2*(i-1/2),"z"),0,len/2)
		end
	end
	table.remove(s,1)
	for i=1,n2 do
		solids[i]={}
		for i2=1,n1 do
			local num1=i==n2 and 1 or i+1 local num2=i2+1>n1 and 1 or i2+1
			p[#p+1]={s[i][i2],s[i][num2],s[num1][num2],s[num1][i2]} solids[i][i2]=p[#p]
		end
	end
	return solids
end

function space.chain(n1,n2,r1,r2,len,angle,axis,fsc,x,y,z,color,light)
	local solids=space.build_chain(n1,n2,r1,r2,len)
	return space.concave(solids,angle,axis,fsc,x,y,z,color,light),solids
end

function space.build_half_chain(n1,n2,r1,r2,len,deg)
	local n1=n1%4==0 and n1 or math.ceil(n1/4)*4 local n2=n2%2==0 and n2 or n2+1
	local deg1,deg2=math.rad(360/n1),360/n2 local s,p={{}},{} local solids={}
	for i=1,n1/2 do
		s[1][i]={math.cos((i-1/2)*deg1)*r1,0,math.sin((i-1/2)*deg1)*r1}
	end
	table.insert(s[1],{-r1*math.cos(deg1/2),0,0}) table.insert(s[1],1,{r1*math.cos(deg1/2),0,0})
	local deg=deg<0 and 0 or (deg>180 and 180 or deg) s[1]=space.translate(space.rotate(s[1],-deg,"y"),r2)
	for i=1,n2+1 do
		if i<=n2/2 then
			s[i+1]=space.translate(space.rotate(s[1],deg2*(i-1/2),"z"),0,-len/2)
		else s[i+1]=space.translate(space.rotate(s[1],deg2*(i-1/2),"z"),0,len/2)
		end
	end
	table.remove(s,1)
	for i=1,n2 do
		solids[i]={}
		for i2=1,n1/2 do
			local num1=i==n2 and 1 or i+1 local num2=i2+1>n1/2 and 1 or i2+1
			p[#p+1]={s[i][i2],s[i][num2],s[num1][num2],s[num1][i2]} solids[i][i2]=p[#p]
		end
	end
	return solids
end

function space.half_chain(n1,n2,r1,r2,len,deg,angle,axis,fsc,x,y,z,color,light)
	local solids=space.build_half_chain(n1,n2,r1,r2,len,deg)
	return space.concave(solids,angle,axis,fsc,x,y,z,color,light),solids
end

function space.build_elliptic_torus(n1,n2,a,b,a1,b1)
	local n1=n1<4 and 4 or n1 local n2=n2<5 and 5 or n2 local deg1,deg2=math.rad(360/n1),360/n2 local s,p={{}},{} local solids={}
	for i=1,n1 do
		s[1][i]={math.cos((i-1/2)*deg1)*a,0,math.sin((i-1/2)*deg1)*b}
	end
	for i=1,n2+1 do
		s[i+1]=space.rotate(s[1],deg2*(1/2-i),"z")
		s[i+1]=space.translate(s[i+1],a1*math.cos(math.rad((i-1/2)*deg2)),b1*math.sin(math.rad((i-1/2)*deg2)))
	end
	table.remove(s,1)
	for i=1,n2+1 do
		solids[i]={}
		for i2=1,n1 do
			local num1=i>n2 and 1 or i+1 local num2=i2+1>n1 and 1 or i2+1
			p[#p+1]={s[i][i2],s[i][num2],s[num1][num2],s[num1][i2]} solids[i][i2]=p[#p]
		end
	end
	return solids
end

function space.elliptic_torus(n1,n2,a,b,a1,b1,angle,axis,fsc,x,y,z,color,light)
	local solids=space.build_elliptic_torus(n1,n2,a,b,a1,b1)
	return space.concave(solids,angle,axis,fsc,x,y,z,color,light),solids
end

function space.build_horn_cyclide(n1,n2,r1,r2,pct)
	local n2=n2<5 and 5 or n2 local deg1,deg2=math.rad(360/n1),360/n2 local s,p={{}},{} local solids={}
	for i=1,n1 do
		s[1][i]={math.cos((i-1/2)*deg1)*r1,0,math.sin((i-1/2)*deg1)*r1}
	end
	for i=1,n2+1 do
		s[i+1]=space.scale(s[1],0.1+math.abs(math.sin(math.rad(deg2/2*(i-1)*pct))))
		s[i+1]=space.rotate(space.translate(s[i+1],r2),deg2*(i-1),"z")
	end
	table.remove(s,1)
	for i=1,n2+1 do
		solids[i]={}
		for i2=1,n1 do
			local num1=i>n2 and 1 or i+1 local num2=i2+1>n1 and 1 or i2+1
			p[#p+1]={s[i][i2],s[i][num2],s[num1][num2],s[num1][i2]} solids[i][i2]=p[#p]
		end
	end
	return solids
end

function space.horn_cyclide(n1,n2,r1,r2,pct,angle,axis,fsc,x,y,z,color,light)
	local solids=space.build_horn_cyclide(n1,n2,r1,r2,pct)
	return space.concave(solids,angle,axis,fsc,x,y,z,color,light),solids
end

function space.build_cyclide_free(n1,n2,r1,r2,func)
	local n2=n2<5 and 5 or n2 local deg1,deg2=math.rad(360/n1),360/n2 local s,p={{}},{} local solids={}
	for i=1,n1 do
		s[1][i]={math.cos((i-1/2)*deg1)*r1,0,math.sin((i-1/2)*deg1)*r1}
	end
	for i=1,n2+1 do
		local fsc=func(deg2,i-1,n1,n2,r1,r2)
		s[i+1]=space.scale(s[1],fsc)
		s[i+1]=space.rotate(space.translate(s[i+1],r2),deg2*(i-1),"z")
	end
	table.remove(s,1)
	for i=1,n2+1 do
		solids[i]={}
		for i2=1,n1 do
			local num1=i>n2 and 1 or i+1 local num2=i2+1>n1 and 1 or i2+1
			p[#p+1]={s[i][i2],s[i][num2],s[num1][num2],s[num1][i2]} solids[i][i2]=p[#p]
		end
	end
	return solids
end

function space.cyclide_free(n1,n2,r1,r2,func,angle,axis,fsc,x,y,z,color,light)
	local solids=space.build_cyclide_free(n1,n2,r1,r2,func)
	return space.concave(solids,angle,axis,fsc,x,y,z,color,light),solids
end

function space.mul_cube(coor,len,angle,axis,fsc,x,y,z,color,light)
	local _,cube=space.cuboid(len,len,len,{},{}) local solids={}
	for i=1,#coor do
		solids[i]=space.shift_solid(cube,-coor[i][1]*len,-coor[i][2]*len,-coor[i][3]*len)
	end
	return space.concave(solids,angle,axis,fsc,x,y,z,color,light),solids
end

function space.sword_simple(n,h1,h2,r,w,thickness,angle,axis,fsc,x,y,z,color,light)
	local p={} p[1]={{-w/2,0,0},{0,0,-thickness/2},{w/2,0,0},{0,0,thickness/2}} p[2]={{-w/2,0,0},{0,-h1,0},{0,0,thickness/2}}
	p[3]={{-w/2,0,0},{0,-h1,0},{0,0,-thickness/2}} p[4]={{w/2,0,0},{0,0,thickness/2},{0,-h1,0}} p[5]={{w/2,0,0},{0,0,-thickness/2},{0,-h1,0}}
	local _,p1=space.prism(n,h2,r,{},{}) local solids={p,space.shift_solid(space.rotate_solid(p1,90,"x"),0,h2/2)}
	return space.concave(solids,angle,axis,fsc,x,y,z,color,light),solids
end

function space.six_solids(l1,l2,angle,axis,fsc,x,y,z,color,light)
	local l2=l2<1 and 1 or l2 local r1,r2=(l1/2)*2^0.5,(l2/2)*2^0.5 local h,solids=(l1-l2)/2,{} local _,p=space.frustum(4,h,r2,r1,{},{})
	solids[1]=space.shift_solid(p,0,0,-(l2+h)/2) solids[2]=space.rotate_solid(solids[1],90,"x") solids[3]=space.rotate_solid(solids[2],-90,"z")
	solids[4]=space.rotate_solid(solids[2],90,"z") solids[5]=space.rotate_solid(solids[1],270,"x") solids[6]=space.rotate_solid(solids[1],180,"x")
	return space.concave(solids,angle,axis,fsc,x,y,z,color,light),solids
end

function space.n6_solids(n,l1,l2,angle,axis,fsc,x,y,z,color,light)
	local l2=l2<1 and 1 or l2 local r1,r2=(l1/2)*2^0.5,(l2/2)*2^0.5 local h,solids=(l1-l2)/2,{} local _,p=space.frustum(4,h,r2,r1,{},{}) local tbl={}
	solids[1]=space.shift_solid(p,0,0,-(l2+h)/2) solids[2]=space.rotate_solid(solids[1],90,"x") solids[3]=space.rotate_solid(solids[2],-90,"z")
	solids[4]=space.rotate_solid(solids[2],90,"z") solids[5]=space.rotate_solid(solids[1],270,"x") solids[6]=space.rotate_solid(solids[1],180,"x")
	for i=1,#n do
		tbl[i]=solids[n[i]]
	end
	return space.concave(tbl,angle,axis,fsc,x,y,z,color,light),tbl
end

function space.cube_pyramid(l1,angle,axis,fsc,x,y,z,color,light)
	local r,solids=(l1/2)*2^0.5,{} local _,p=space.pyramid(4,l1/2,r,{},{})
	solids[1]=space.shift_solid(p,0,0,-l1/4) solids[2]=space.rotate_solid(solids[1],90,"x") solids[3]=space.rotate_solid(solids[2],-90,"z")
	solids[4]=space.rotate_solid(solids[2],90,"z") solids[5]=space.rotate_solid(solids[1],270,"x") solids[6]=space.rotate_solid(solids[1],180,"x")
	return space.concave(solids,angle,axis,fsc,x,y,z,color,light),solids
end

function space.two_cylinders(n,h0,h1,h2,h3,r,angle,axis,fsc,x,y,z,color,light)
	local solids={} local h0,h1=h0<r and r or h0,h1<r and r or h1 local h2,h3=h2<r and r or h2,h3<r and r or h3
	local _,p0=space.truncated_prism2(n,h0,r,45,45,{},{}) local _,p1=space.truncated_prism2(n,h1,r,45,45,{},{})
	local _,p2=space.truncated_prism2(n,h2,r,45,45,{},{}) local _,p3=space.truncated_prism2(n,h3,r,45,45,{},{}) table.remove(p0,3)
	table.remove(p0,2) table.remove(p1,3) table.remove(p1,2) table.remove(p2,3) table.remove(p2,2) table.remove(p3,3) table.remove(p3,2)
	solids[1]=space.shift_solid(p0,0,0,-h0/2) solids[2]=space.shift_solid(space.rotate_solid(p1,180,"x"),0,0,h1/2)
	solids[3]=space.shift_solid(space.rotate_solid(p2,-90,"y"),-h2/2) solids[4]=space.shift_solid(space.rotate_solid(p3,90,"y"),h3/2)
	return space.concave(solids,angle,axis,fsc,x,y,z,color,light),solids
end

function space.two_truncated_prisms(n,h1,h2,r,deg,angle,axis,fsc,x,y,z,color,light)
	local deg0=90-deg/2 local deg1=math.deg(math.atan(h1/r)) local deg2=math.deg(math.atan(h2/r))
	deg0=deg0>math.min(deg1,deg2) and math.min(deg1,deg2) or deg0 deg0=deg0<-math.min(deg1,deg2) and -math.min(deg1,deg2) or deg0 
	local deg=180-2*deg0 local solids,cp={},{{0,0,-9*math.min(h1,h2)/10},{0,0,-9*math.min(h1,h2)/10}}
	local _,p1=space.truncated_prism(n,h1,r,deg0,1,{},{}) local _,p2=space.truncated_prism(n,h2,r,deg0,2,{},{})
	table.remove(p1,2) table.remove(p2,2) solids[1]=space.shift_solid(p1,0,0,-h1/2)
	solids[2]=space.rotate_solid(space.shift_solid(p2,0,0,-h2/2),-deg,"y") cp[2]=space.rotate_point(cp[2],-deg,"y") solids.cp=cp
	return space.concave(solids,angle,axis,fsc,x,y,z,color,light),solids
end

function space.three_truncated_prisms(n,h0,h1,h2,h3,r,ang1,ang2,angle,axis,fsc,x,y,z,color,light)
	local d0=90-ang1/2 local d1=math.deg(math.atan(h0/r)) local d2=math.deg(math.atan(h2/r)) d0=d0>math.min(d1,d2) and math.min(d1,d2) or d0
	d0=d0<-math.min(d1,d2) and -math.min(d1,d2) or d0 local ang1=180-2*d0 local deg0=90-ang2/2
	local deg1=math.deg(math.atan(h1/r)) local deg2=math.deg(math.atan(h3/r)) deg0=deg0>math.min(deg1,deg2) and math.min(deg1,deg2) or deg0
	deg0=deg0<-math.min(deg1,deg2) and -math.min(deg1,deg2) or deg0 local ang2=2*deg0 local h=math.min(h0,h1,h2,h3)
	local solids,cp={},{{0,0,(h0-h1)/2},{0,0,-9*h/10},{0,0,-9*h/10}} local _,p1=space.cylindrical_wedge2(n,h0,h1,r,d0,deg0,11,{},{})
	local _,p2=space.truncated_prism(n,h2,r,d0,2,{},{}) local _,p3=space.truncated_prism(n,h3,r,deg0,1,{},{})
	table.remove(p1,2) table.remove(p1,1) table.remove(p2,2) table.remove(p3,2) p3=space.rotate_solid(p3,90,"z")
	solids[1]=p1 solids[2]=space.shift_solid(space.rotate_solid(space.shift_solid(p2,0,0,-h2/2),-ang1,"y"),0,0,h0)
	cp[2]=space.shift_point(space.rotate_point(cp[2],-ang1,"y"),0,0,h0) cp[3]=space.shift_point(space.rotate_point(cp[3],ang2,"x"),0,0,-h1)
	solids[3]=space.shift_solid(space.rotate_solid(space.shift_solid(p3,0,0,-h3/2),ang2,"x"),0,0,-h1) solids.cp=cp
	return space.concave(solids,angle,axis,fsc,x,y,z,color,light),solids
end

function space.domes_on_two_truncated_prisms(n,h1,h2,r,deg,angle,axis,fsc,x,y,z,color,light)
	local deg0=90-deg/2 local deg1=math.deg(math.atan(h1/r)) local deg2=math.deg(math.atan(h2/r))
	deg0=deg0>math.min(deg1,deg2) and math.min(deg1,deg2) or deg0 deg0=deg0<-math.min(deg1,deg2) and -math.min(deg1,deg2) or deg0 
	local deg=180-2*deg0 local solids,cp={},{{0,0,(math.min(h1,h2)-r)/2},{0,0,(math.min(h1,h2)-r)/2}}
	local _,p1=space.dome_on_truncated_prism(n,h1,r,deg0,{},{}) local _,p2=space.dome_on_truncated_prism(n,h2,r,-deg0,{},{})
	table.remove(p1) table.remove(p2) solids[1]=space.shift_solid(space.rotate_solid(p1,180,"x"),0,0,-(h1+r)/2)
	solids[2]=space.rotate_solid(space.shift_solid(space.rotate_solid(p2,180,"x"),0,0,-(h2+r)/2),deg,"y")
	cp[2]=space.rotate_point(space.shift_point(space.rotate_point(cp[2],180,"x"),0,0,-(h2+r)/2),deg,"y")
	cp[1]=space.shift_point(space.rotate_point(cp[1],180,"x"),0,0,-(h1+r)/2) solids.cp=cp
	return space.concave(solids,angle,axis,fsc,x,y,z,color,light),solids
end

function space.domes_on_3_truncated_prisms(n,h0,h1,h2,h3,r,ang1,ang2,angle,axis,fsc,x,y,z,color,light)
	local d0=90-ang1/2 local d1=math.deg(math.atan(h0/r)) local d2=math.deg(math.atan(h2/r)) d0=d0>math.min(d1,d2) and math.min(d1,d2) or d0
	d0=d0<-math.min(d1,d2) and -math.min(d1,d2) or d0 local ang1=180-2*d0 local deg0=90-ang2/2
	local deg1=math.deg(math.atan(h1/r)) local deg2=math.deg(math.atan(h3/r)) deg0=deg0>math.min(deg1,deg2) and math.min(deg1,deg2) or deg0
	deg0=deg0<-math.min(deg1,deg2) and -math.min(deg1,deg2) or deg0 local ang2=2*deg0 local h=math.min(h0,h1,h2,h3)
	local solids,cp={},{{0,0,(h0-h1)/2},{0,0,-9*(h+r)/10},{0,0,-9*(h+r)/10}} local _,p1=space.cylindrical_wedge2(2^(n+2),h0,h1,r,d0,deg0,11,{},{})
	local _,p2=space.dome_on_truncated_prism(n,h2,r,d0,{},{}) local _,p3=space.dome_on_truncated_prism(n,h3,r,deg0,{},{})
	table.remove(p1,2) table.remove(p1,1) table.remove(p2) table.remove(p3) p3=space.rotate_solid(p3,90,"z")
	p2=space.rotate_solid(p2,180,"x") p3=space.rotate_solid(p3,180,"x")
	solids[1]=p1 solids[2]=space.shift_solid(space.rotate_solid(space.shift_solid(p2,0,0,-(h2+r)/2),-ang1,"y"),0,0,h0)
	cp[2]=space.shift_point(space.rotate_point(cp[2],-ang1,"y"),0,0,h0) cp[3]=space.shift_point(space.rotate_point(cp[3],ang2,"x"),0,0,-h1)
	solids[3]=space.shift_solid(space.rotate_solid(space.shift_solid(p3,0,0,-(h3+r)/2),ang2,"x"),0,0,-h1) solids.cp=cp
	return space.concave(solids,angle,axis,fsc,x,y,z,color,light),solids
end

function space.mul_cylinders(n,n1,h,r,angle,axis,fsc,x,y,z,color,light)
	local deg=360/n1 local deg1=(180-deg)/2 local cp={{0,0,-9*h/10}} local h=h<math.tan(math.rad(deg1))*r and math.tan(math.rad(deg1))*r or h
	local _,p0=space.truncated_prism2(n,h,r,deg1,deg1,{},{}) table.remove(p0,3) table.remove(p0,2) local solids={space.shift_solid(p0,0,0,-h/2)}
	for i=2,n1 do
		solids[i]=space.rotate_solid(solids[1],deg*(i-1),"y") cp[i]=space.rotate_point(cp[1],deg*(i-1),"y")
	end
	solids.cp=cp
	return space.concave(solids,angle,axis,fsc,x,y,z,color,light),solids
end

function space.build_dome_subdivide(n,r)
	local p={} local _,solid=space.octahedron(r,{},{}) for i=8,5,-1 do table.remove(solid,i) end
	if n<1 then
		return solid
	elseif n==1 then
		p=space.subdivide(r,solid)
	else solid=space.build_dome_subdivide(n-1,r) p=space.subdivide(r,solid)
	end
	return p
end

function space.dome(n,r,angle,axis,fsc,x,y,z,color,light)
	local solid=space.build_dome_subdivide(n,r) local p,deg={},math.rad(360/2^(n+2))
	for i=1,2^(n+2) do
		p[i]={math.cos((i-1)*deg)*r,math.sin((i-1)*deg)*r,0}
	end
	table.insert(solid,1,p) local solids={solid}
	return space.concave(solids,angle,axis,fsc,x,y,z,color,light),solids
end

function space.double_pyramid_frustum(n,h1,h2,r1,r2,angle,axis,fsc,x,y,z,color,light)
	local _,p1=space.pyramid(n,h1,r1,{},{}) local _,p2=space.frustum(n,h2,r1,r2,{},{}) local solids={} table.remove(p1,1)
	table.remove(p2,2)  table.remove(p2,1) solids[1]=space.shift_solid(p1,0,0,h1/2+h2) solids[2]=space.shift_solid(p2,0,0,h2/2)
	solids[3]=space.rotate_solid(solids[2],180,"x") solids[4]=space.rotate_solid(solids[1],180,"x")
	solids.cp={{0,0,h1/2+h2},{0,0,h2/2},{0,0,-h2/2},{0,0,-h1/2-h2}}
	return space.concave(solids,angle,axis,fsc,x,y,z,color,light),solids
end

function space.simple_to_3d(ass_shape,h,angle,axis,fsc,x,y,z,color,light)
	local s,cnt={},-1 local x=x or 0 local y=y or 0 local z=z or 0 local cp={}
	for m in ass_shape:gmatch("(m [^m]+)") do
		local pts=space.shape2points(m)
		local p={space.translate(pts,0,0,h/2)}
		for i=1,#p[1]-1 do
			p[#p+1]={p[1][i],space.translate(p[1],0,0,h/2)[i],space.translate(p[1],0,0,h/2)[i+1],p[1][i+1]}
		end
		p[#p+1]={p[1][#p],space.translate(p[1],0,0,h/2)[#p],space.translate(p[1],0,0,h/2)[1],p[1][1]}
		p[#p+1]=space.translate(p[1],0,0,h/2)
		s[#s+1]=p cp[#cp+1]=space.plane_center(pts)
	end
	s.cp=cp
	return space.concave(s,angle,axis,fsc,x,y,z,color,light),s
end

function space.plane_to_3d(ass_shape,h)
	local nor_v={{0,0,-1},{0,0,1}}
	local p={space.translate(space.shape2points(ass_shape),0,0,-h/2)} p[2]=space.translate(p[1],0,0,h)
	for str in ass_shape:gmatch("(m [^m]+)") do
		local m={space.translate(space.shape2points(str),0,0,-h/2)} m[2]=space.translate(m[1],0,0,h)
		for i=1,#m[1]-1 do
			p[#p+1]={m[1][i+1],m[2][i+1],m[2][i],m[1][i]} nor_v[#nor_v+1]=space.nor_v(p[#p])
		end
		p[#p+1]={m[1][1],m[2][1],m[2][#m[1]],m[1][#m[1]]} nor_v[#nor_v+1]=space.nor_v(p[#p])
	end
	return {p,nor_v,ass_shape}
end

function space.info(plane,center_pos,angle,axis,fsc,x,y,z,color,light)
	local s={} local p=plane local color=color or {}
	for i=1,#p do
		local info=space.plane_info(p[i],center_pos,angle,axis,fsc,x,y,z,color[i],nil,nil,light)
		s[i]={s=info.s,lay=info.lay,c=info.c}
	end
	return s
end

function space.action(set,s_time,e_time,interval,s_ang,e_ang,axis,fsc,x,y,z,color,surface,light)
	local n=#set local loop=math.ceil((e_time-s_time)/interval) local s={n={}}
	local color=color or {} local cp={0,0,0}
	if #color<n then
		for i=1,n-#color do
			color[#color+1]=color[1] and color[math.random(#color)] or "&HFFFFFF&"
		end
	end
	for i=1,n do
		s.n[i]=loop s[i]={}
		for ms=1,loop do
			local angle={}
			for a=1,#s_ang do
				angle[a]=s_ang[a]+(e_ang[a]-s_ang[a])*(ms-1)/loop
			end
			if surface and surface[i] then
				local p=space.plane_info(set[i],cp,angle,axis,1+(fsc-1)*ms/loop,1+(x-1)*ms/loop,1+(y-1)*ms/loop,1+(z-1)*ms/loop,color[i],surface[i],surface.c[i],nil,light)
				s[i][ms]={s=p.s,lay=p.lay,c=p.c} s[n+i]=ms==1 and {} or s[n+i] s.n[n+i]=loop
				s[n+i][ms]={s=p.sur,lay=p.sur_lay,c=p.sur_c,clip=p.clip}
			else local p=space.plane_info(set[i],cp,angle,axis,1+(fsc-1)*ms/loop,1+(x-1)*ms/loop,1+(y-1)*ms/loop,1+(z-1)*ms/loop,color[i],nil,nil,nil,light)
				s[i][ms]={s=p.s,lay=p.lay,c=p.c}
			end
		end
	end
	return s
end

function space.freemove(set,s_time,e_time,interval,roll_fsc_pos,color,surface,sur_filter,light)
	local n=#set local loop=math.ceil((e_time-s_time)/interval) local s={n={}}
	local color=color or {} local cp,cnt={0,0,0},0
	if #color<n then
		for i=1,n-#color do
			color[#color+1]=color[1] and color[math.random(#color)] or "&HFFFFFF&"
		end
	end
	for i=1,n do
		s.n[i]=loop s[i]={}
		for ms=1,loop do
			local action=roll_fsc_pos and roll_fsc_pos(ms,loop) or {{-110},{"x"},1,0,0,0}
			if surface and surface[i] then
				cnt=ms==1 and cnt+1 or cnt local c=surface.c[i] and surface.c[i] or "&HFFFFFF&" local filter=sur_filter and sur_filter(ms,loop) or nil
				local p=space.plane_info(set[i],cp,action[1],action[2],action[3],action[4],action[5],action[6],color[i],surface[i],c,filter,light)
				s[i][ms]={s=p.s,lay=p.lay,c=p.c} s[n+cnt]=ms==1 and {} or s[n+cnt] s.n[n+cnt]=loop
				s[n+cnt][ms]={s=p.sur,lay=p.sur_lay,c=p.sur_c,clip=p.clip}
			else local p=space.plane_info(set[i],cp,action[1],action[2],action[3],action[4],action[5],action[6],color[i],nil,nil,nil,light)
				s[i][ms]={s=p.s,lay=p.lay,c=p.c}
			end
		end
	end
	return s
end

function space.plane_info(plane,center_pos,angle,axis,fsc,x,y,z,color,surface,surface_color,sur_filter,light)
	local s,p={} local c=space.plane_center(plane) local _,norm=space.visibility(plane,center_pos)
	for i=1,#axis do
		plane=space.rotate(plane,angle[i],axis[i])
	end
	if type(fsc)=="table" then
		p=space.translate(space.scale(plane,fsc[1],fsc[2],fsc[3]),x,y,z)
	else p=space.translate(space.scale(plane,fsc),x,y,z)
	end
	s.s=space.restore(p) s.lay=space.visibility(plane,center_pos) s.c=space.shade(color,light,plane)
	if surface then
		if sur_filter then
			surface=space.shift_2d(space.fsc_2d(space.roll_2d(surface,sur_filter[1],"z"),sur_filter[2][1],sur_filter[2][2],sur_filter[2][3],sur_filter[2][4]),sur_filter[3][1],sur_filter[3][2])
		end
		local sur_axis,ang=space.v2v_info({0,0,1},norm)
		local sur=space.shift(space.roll(space.add_z(surface),ang,sur_axis),c[1],c[2],c[3])
		for i=1,#axis do
			sur=space.roll(sur,angle[i],axis[i])
		end
		if type(fsc)=="table" then
			sur=space.shift(space.fsc(sur,fsc[1],fsc[2],fsc[3]),x,y,z)
		else sur=space.shift(space.fsc(sur,fsc),x,y,z)
		end
		s.sur=space.remove_z(sur) s.sur_lay=s.lay==0 and 0 or 2 s.clip=s.s
		if surface_color then
			s.sur_c=space.shade(surface_color,light,p)
		end
	end
	return s
end

function space.plane_center(plane)
	local cx,cy,cz=0,0,0
	for i=1,#plane do
		cx=cx+plane[i][1] cy=cy+plane[i][2] cz=cz+plane[i][3]
	end
	return {cx/#plane,cy/#plane,cz/#plane}
end

function space.concave(set,angle,axis,fsc,x,y,z,color,light)
	local cp={} local color=color or {} local cp_z,s,cnt={},{},1
	local solids=space.tbl_copy(set) local lay={}
	for i=1,#solids do
		if solids.cp then
			cp[i]=solids.cp[i]
		else cp[i]=space.center_pos(solids[i])
		end
		for roll=1,#axis do
			solids[i]=space.rotate_solid(solids[i],angle[roll],axis[roll])
			cp[i]=space.rotate_point(cp[i],angle[roll],axis[roll])
		end
		if type(fsc)=="table" then
			solids[i]=space.shift_solid(space.fsc_solid(solids[i],fsc[1],fsc[2],fsc[3]),x,y,z)
			cp[i]=space.shift_point(space.fsc_point(cp[i],fsc[1],fsc[2],fsc[3]),x,y,z)
		else solids[i]=space.shift_solid(space.fsc_solid(solids[i],fsc),x,y,z)
			cp[i]=space.shift_point(space.fsc_point(cp[i],fsc),x,y,z)
		end
		cp_z[i]={cp[i][3],i}
	end
	cp_z=space.tbl_sort(cp_z)
	for i=1,#cp do
		lay[cp_z[i][2]]=(#solids-i)*2
	end
	for i=1,#solids do
		for p=1,#solids[i] do
			local layer=space.visibility(solids[i][p],cp[i])
			s[#s+1]={s=space.restore(solids[i][p]),lay=layer==1 and layer+lay[i] or 0,c=space.shade(color[cnt],light,solids[i][p])}
			cnt=cnt+1
		end
	end
	return s
end

function space.concave_move(solids,s_time,e_time,interval,roll_fsc_pos,color,light)
	local n=0 local loop=math.ceil((e_time-s_time)/interval) local s={n={}}
	local color=color or {}
	for i=1,#solids do
		n=n+#solids[i]
	end
	if #color<n then
		for i=1,n-#color do
			color[#color+1]=color[1] and color[math.random(#color)] or "&HFFFFFF&"
		end
	end
	for i=1,n do
		s.n[i]=loop s[i]={}
	end
	for ms=1,loop do
		local action=roll_fsc_pos and roll_fsc_pos(ms,loop) or {{-110},{"x"},1,0,0,0}
		local tr=space.concave(solids,action[1],action[2],action[3],action[4],action[5],action[6],color,light)
		for i=1,n do
			s[i][ms]=tr[i]
		end
	end
	return s
end

function space.concave_freemove(solids,s_time,e_time,interval,roll_fsc_pos,color,light)
	local n=0 local loop=math.ceil((e_time-s_time)/interval) local s={n={}}
	local color=color or {}
	for i=1,#solids do
		n=n+#solids[i]
	end
	if #color<n then
		for i=1,n-#color do
			color[#color+1]=color[1] and color[math.random(#color)] or "&HFFFFFF&"
		end
	end
	for i=1,n do
		s.n[i]=loop s[i]={}
	end
	for ms=1,loop do
		local action=roll_fsc_pos and roll_fsc_pos(ms,loop) or {{-110},{"x"},1,0,0,0} local tr
		if type(action[1][1])=="table" then
			for i=1,#solids do
				for roll=1,#action[1][1] do
					solids[i]=space.rotate_solid(solids[i],action[1][1][roll],action[1][2][roll])
				end
				solids[i]=space.shift_solid(solids[i],action[1][3],action[1][4],action[1][5])
			end
			tr=space.concave(solids,action[2],action[3],action[4],action[5],action[6],action[7],color,light)
		else tr=space.concave(solids,action[1],action[2],action[3],action[4],action[5],action[6],color,light)
		end
		for i=1,n do
			s[i][ms]=tr[i]
		end
	end
	return s
end

function space.plane_zmax(plane)
	local zmax=plane[1][3]
	for i=2,#plane do
		zmax=math.max(zmax,plane[i][3])
	end
	return zmax
end

function space.surface_restore(ass_shape,plane)
	local cnt=0 local p=space.points_int(plane)
	local ass_shape=ass_shape:gsub('[-.%d]+ [-.%d]+',
		function (point)
			cnt=cnt+1
			return p[cnt][1]..' '..p[cnt][2]
		end
	)
	return ass_shape
end

function space.to_3d_info(set,angle,axis,fsc,x,y,z,color,light)
	local color=color or {} local zmax,s,lay={},{},{} local solid,nor_v,ass_shape=set[1],set[2],set[3] local light=light or {0,0,1}
	for roll=1,#axis do
		solid=space.rotate_solid(solid,angle[roll],axis[roll])
		nor_v=space.rotate(nor_v,angle[roll],axis[roll])
	end
	if type(fsc)=="table" then
		solid=space.shift_solid(space.fsc_solid(solid,fsc[1],fsc[2],fsc[3]),x,y,z)
	else solid=space.shift_solid(space.fsc_solid(solid,fsc),x,y,z)
	end
	for i=1,#solid do
		if nor_v[i][3]>0 then
			zmax[#zmax+1]={space.plane_center(solid[i])[3]+space.plane_zmax(solid[i]),i}
		end
	end
	table.sort(zmax,function (z1,z2) return z1[1]<z2[1] end)
	for i=1,#zmax do
		if zmax[i][2]==1 or zmax[i][2]==2 then
			lay[zmax[i][2]]=#zmax+1
		else lay[zmax[i][2]]=i
		end
	end
	for i=1,#solid do
		local ang=space.v_degree(light,nor_v[i]) local shade=space.color_brightness(color[i],1.3-ang/90)
		if i<3 then
			s[#s+1]={s=space.surface_restore(ass_shape,solid[i]),lay=lay[i] and lay[i] or 0,c=shade}
		else
			s[#s+1]={s=space.restore(solid[i]),lay=lay[i] and lay[i] or 0,c=shade}
		end
	end
	return s
end

function space.to_3d(set,s_time,e_time,interval,roll_fsc_pos,color,mode,light)
	local n=#set[1] local loop=math.ceil((e_time-s_time)/interval) local s={n={}}
	local color=color or {}
	if #color<n then
		for i=1,n-#color do
			color[#color+1]=color[1] and color[math.random(#color)] or "&HFFFFFF&"
		end
	end
	if mode then
		s.n={{loop,n}}
		for ms=1,loop do
			s[ms]={} local action=roll_fsc_pos and roll_fsc_pos(ms,loop) or {{-110},{"x"},1,0,0,0}
			local tr=space.to_3d_info(set,action[1],action[2],action[3],action[4],action[5],action[6],color,light)
			for i=1,n do
				s[ms][i]=tr[i]
			end
		end
	else
		for ms=1,loop do
			s[ms]={} local action=roll_fsc_pos and roll_fsc_pos(ms,loop) or {{-110},{"x"},1,0,0,0}
			local tr=space.to_3d_info(set,action[1],action[2],action[3],action[4],action[5],action[6],color,light)
			for i=1,n do
				if tr[i].lay>0 then
					s[ms][#s[ms]+1]=tr[i]
				end
			end
			s.n[ms]=#s[ms]
		end
	end
	return s
end

function space.filter_move(solid,s_time,e_time,interval,filter,roll_fsc_pos,light)
	local n=#solid local loop=math.ceil((e_time-s_time)/interval) local s={n={}} local light=light or {0,0,1}
	for i=1,n do
		s.n[i]=loop s[i]={}
	end
	for ms=1,loop do
		local planes=space.filter_solid(solid,function(x,y,z) return filter(x,y,z,ms,loop) end)
		local action=roll_fsc_pos and roll_fsc_pos(ms,loop) or {{-110},{"x"},1,0,0,0}
		for i=1,#action[1] do
			planes=space.rotate_solid(planes,action[1][i],action[2][i])
		end
		if type(action[3])=="table" then
			planes=space.shift_solid(space.fsc_solid(planes,action[3][1],action[3][2],action[3][3]),action[4],action[5],action[6])
		else planes=space.shift_solid(space.fsc_solid(planes,action[3]),action[4],action[5],action[6])
		end
		for i=1,n do
			local nor_v=space.nor_v(planes[i]) local cos=space.dot_product(light,nor_v)/(space.v_nor(nor_v)*space.v_nor(light))
			s[i][ms]={p={x=math_round(planes[i][1][1],1),y=math_round(planes[i][1][2],1)},s=space.restore(planes[i]),a=ass_alpha(255*math.abs(cos))}
		end
	end
	return s
end

function space.lift4fm(x,y,z,i,n,t)
	return x,y,z+(x^2+y^2)^0.5*(i-1)/(n-1)*t
end

function space.sin_lift4fm(x,y,z,i,n,t,w)
	return x,y,z+(x^2+y^2)^0.5*(i-1)/(n-1)*t*math.sin(math.rad(i)*w)
end

function space.sin_little_lift4fm(x,y,z,i,n,t,w)
	return x,y,z+(x^2+y^2)^0.5*t*math.sin(math.rad(i)*w)
end

function space.cosx_cosy4fm(x,y,z,i,n,t,w)
	return x,y,math.cos(x)*math.cos(y)*t*math.sin(math.rad(i)*w)
end

function space.waves4fm(x,y,z,i,n,t,w)
	return x,y,z+t*math.sin(x+y+w*math.rad(i))
end

function space.hyperbolic_paraboloid4fm(x,y,z,i,n,a,b,t,w,mode)
	local mode=mode or 1 local t=t or 1
	if mode==0 then
		return x,y,z+(x^2/a^2-y^2/b^2)*t
	elseif mode==1 then
		return x,y,z+(x^2/a^2-y^2/b^2)*(i-1)/(n-1)*t
	elseif mode==2 then
		return x,y,z+(x^2/a^2-y^2/b^2)*t*math.sin(math.rad(i)*w)
	elseif mode==3 then
		return x,y,z+(x^2/a^2-y^2/b^2)*(i-1)/(n-1)*t*math.sin(math.rad(i)*w)
	end
end

function space.fifty_fifty4fm(x,y,z,i,n,t,w,mode)
	local mode=mode or 1 local t=t or 1
	if mode==0 then
		return x,y,z+45*(1-math.exp(x+y))/(1+math.exp(x+y))*t
	elseif mode==1 then
		return x,y,z+45*(1-math.exp(x+y))/(1+math.exp(x+y))*t*(i-1)/(n-1)
	elseif mode==2 then
		return x,y,z+45*(1-math.exp(x+y))/(1+math.exp(x+y))*t*math.sin(math.rad(i-1)*w)
	elseif mode==3 then
		return x,y,z+45*(1-math.exp(x+y))/(1+math.exp(x+y))*t*math.sin(math.rad(i-1)*w)*(i-1)/(n-1)
	end
end

function space.gaussian_func4fm(x,y,z,i,n,orgx,orgy,ax,ay,t,w,mode)
	local mode=mode or 1 local t=t or 1 local ax,ay=ax/10000,ay/10000
	if mode==0 then
		return x,y,z+t*math.exp(-((x-orgx)^2/2*ax^2+(y-orgy)^2/2*ay^2))
	elseif mode==1 then
		return x,y,z+(t*math.exp(-((x-orgx)^2/2*ax^2+(y-orgy)^2/2*ay^2))-t)*math.sin(math.rad(i-1)*w)
	elseif mode==2 then
		return x,y,z+(t*math.exp(-((x-orgx)^2/2*(ax*(i-1)/(n-1))^2+(y-orgy)^2/2*(ay*(i-1)/(n-1))^2))-t)*math.sin(math.rad(i-1)*w)
	elseif mode==3 then
		return x,y,t*math.exp(-((x-orgx)^2/2*(ax*(i-1)/(n-1))^2+(y-orgy)^2/2*(ay*(i-1)/(n-1))^2))-t
	elseif mode==4 then
		return x,y,z+t*math.exp(-((x-orgx*(i-1)/(n-1))^2/2*ax^2+(y-orgy*(i-1)/(n-1))^2/2*ay^2))-t
	elseif mode==5 then
		return x,y,z+t*math.exp(-((x-orgx*math.sin(math.rad(i-1)*w))^2/2*ax^2+(y-orgy*math.sin(math.rad(i-1)*w))^2/2*ay^2))-t
	elseif mode==6 then
		return x,y,z+(t*math.exp(-((x-orgx*(i-1)/(n-1))^2/2*ax^2+(y-orgy*(i-1)/(n-1))^2/2*ay^2))-t)*math.sin(math.rad(i-1)*w)
	elseif mode==7 then
		return x,y,z+(t*math.exp(-((x-orgx*(i-1)/(n-1))^2/2*(ax*(i-1)/(n-1))^2+(y-orgy*(i-1)/(n-1))^2/2*(ay*(i-1)/(n-1))^2))-t)*math.sin(math.rad(i-1)*w)
	elseif mode==8 then
		return x,y,z+(t*math.exp(-((x-orgx*math.sin(math.rad(i-1)*w))^2/2*ax^2+(y-orgy*math.sin(math.rad(i-1)*w))^2/2*ay^2))-t)*math.sin(math.rad(i-1)*w)
	elseif mode==9 then
		return x,y,z+(t*math.exp(-((x-orgx*math.sin(math.rad(i-1)*w))^2/2*(ax*(i-1)/(n-1))^2+(y-orgy*math.sin(math.rad(i-1)*w))^2/2*(ay*(i-1)/(n-1))^2))-t)*math.sin(math.rad(i-1)*w)
	elseif mode==10 then
		local sin=math.sin(math.rad(i-1)*w)
		return x,y,z+(t*math.exp(-((x-orgx*sin)^2/2*(ax*sin)^2+(y-orgy*sin)^2/2*(ay*sin)^2))-t)*sin
	end
end

function space.rand4fm(x,y,z,i,n,rand_tbl,mul)
	local loop,cnt=math.ceil(n/mul),0 local at=math.ceil(i/loop) local newx,newy,newz={x},{y},{z}
	for i0=1,at do
		math.randomseed((x+y+z)*i0)
		local randx=rand_tbl[1] local randy=rand_tbl[2] and rand_tbl[2] or 0 local randz=rand_tbl[3] or 0
		local a,b,c=math.random(-randx,randx),math.random(-randy,randy),math.random(-randz,randz)
		newx[#newx+1]=x+a newy[#newy+1]=y+b newz[#newz+1]=z+c
	end
	local i2=i%loop==0 and loop or i%loop local x1=newx[#newx-1]+(newx[#newx]-newx[#newx-1])*i2/loop
	local y1=newy[#newy-1]+(newy[#newy]-newy[#newy-1])*i2/loop local z1=newz[#newz-1]+(newz[#newz]-newz[#newz-1])*i2/loop
	return x1,y1,z1
end

function space.concave2(set,angle,axis,fsc,x,y,z,color,light)
	local cp={} local color=color or {} local cp_z,s,cnt={},{},1
	local solids=space.tbl_copy(set) local lay={}
	for i=1,#solids do
		if solids.cp then
			cp[i]=solids.cp[i]
		else cp[i]=space.center_pos(solids[i])
		end
		for roll=1,#axis do
			solids[i]=space.rotate_solid(solids[i],angle[roll],axis[roll])
			cp[i]=space.rotate_point(cp[i],angle[roll],axis[roll])
		end
		if type(fsc)=="table" then
			solids[i]=space.shift_solid(space.fsc_solid(solids[i],fsc[1],fsc[2],fsc[3]),x,y,z)
			cp[i]=space.shift_point(space.fsc_point(cp[i],fsc[1],fsc[2],fsc[3]),x,y,z)
		else solids[i]=space.shift_solid(space.fsc_solid(solids[i],fsc),x,y,z)
			cp[i]=space.shift_point(space.fsc_point(cp[i],fsc),x,y,z)
		end
		cp_z[i]={cp[i][3],i}
	end
	cp_z=space.tbl_sort(cp_z)
	for i=1,#cp do
		lay[cp_z[i][2]]=(#solids-i)*2
	end
	for i=1,#solids do
		for p=1,#solids[i] do
			s[#s+1]={s=space.restore(solids[i][p]),lay=space.visibility(solids[i][p],cp[i])+lay[i],c=space.shade(color[cnt],light,solids[i][p])}
			cnt=cnt+1
		end
	end
	return s
end

function space.spread(solids,s_time,e_time,interval,spread_func,roll_fsc_pos,color,light)
	local n,cp,len,temp=0,{},{},{} local loop=math.ceil((e_time-s_time)/interval) local s={n={}} local color=color or {} local p1={}
	for i=1,#solids do
		if solids.cp then
			cp[i]=solids.cp[i]
		else cp[i]=space.center_pos(solids[i])
		end
		n=n+#solids[i] len[i]=space.v_nor(cp[i])
	end
	if type(color[1])=="table" then
		if #color[1]<#solids then
			for i=1,#solids-#color[1] do
				color[1][#color[1]+1]=color[1][1] and color[1][math.random(#color[1])] or "&HFFFFFF&"
			end
		end
		local c=color[1] color={}
		for i=1,#solids do
			for i2=1,#solids[i] do
				color[#color+1]=c[i]
			end
		end
	else if #color<n then
			for i=1,n-#color do
				color[#color+1]=color[1] and color[math.random(#color)] or "&HFFFFFF&"
			end
		end
	end
	for i=1,n do
		s.n[i]=loop s[i]={}
	end
	for ms=1,loop do
		for i=1,#solids do
			local spread=spread_func and spread_func(i,#solids,ms,loop) or {{},{},1,0}
			local k=len[i]==0 and 0 or (spread[4]+len[i])/len[i] temp[i]=space.tbl_copy(solids[i])
			temp[i]=space.shift_solid(temp[i],-cp[i][1],-cp[i][2],-cp[i][3]) local v={cp[i][1]*k,cp[i][2]*k,cp[i][3]*k} p1[i]=v
			for roll=1,#spread[1] do
				temp[i]=space.rotate_solid(temp[i],spread[1][roll],spread[2][roll])
			end
			if type(spread[3])=="table" then
				temp[i]=space.shift_solid(space.fsc_solid(temp[i],spread[3][1],spread[3][2],spread[3][3]),v[1],v[2],v[3])
			else temp[i]=space.shift_solid(space.fsc_solid(temp[i],spread[3]),v[1],v[2],v[3])
			end
		end
		local action=roll_fsc_pos and roll_fsc_pos(ms,loop) or {{-110},{"x"},1,0,0,0} local tr temp.cp=p1
		if type(action[1][1])=="table" then
			for i=1,#solids do
				for roll=1,#action[1][1] do
					temp[i]=space.rotate_solid(temp[i],action[1][1][roll],action[1][2][roll])
				end
				temp[i]=space.shift_solid(temp[i],action[1][3],action[1][4],action[1][5])
			end
			tr=space.concave2(temp,action[2],action[3],action[4],action[5],action[6],action[7],color,light)
		else tr=space.concave2(temp,action[1],action[2],action[3],action[4],action[5],action[6],color,light)
		end
		for i=1,n do
			s[i][ms]=tr[i]
		end
	end
	return s
end

function space.spread_rand(solids,s_time,e_time,interval,spread_func,rand_tbl,roll_fsc_pos,color,light)
	local n,cp,len,temp,nums=0,{},{},{},{} local loop=math.ceil((e_time-s_time)/interval) local s={n={}} local color=color or {} local p1={}
	for i=1,#solids do
		if solids.cp then
			cp[i]=solids.cp[i]
		else cp[i]=space.center_pos(solids[i])
		end
		n=n+#solids[i] len[i]=space.v_nor(cp[i])
	end
	if rand_tbl then
		if #rand_tbl<#solids then
			for i=1,#solids-#rand_tbl do
				rand_tbl[#rand_tbl+1]=rand_tbl[1] and rand_tbl[math.random(#rand_tbl)] or {0,180}
			end
		end
		for i=1,#rand_tbl do
			nums[i]=math.random(rand_tbl[i][1],rand_tbl[i][2])
		end
	end
	if type(color[1])=="table" then
		if #color[1]<#solids then
			for i=1,#solids-#color[1] do
				color[1][#color[1]+1]=color[1][1] and color[1][math.random(#color[1])] or "&HFFFFFF&"
			end
		end
		local c=color[1] color={}
		for i=1,#solids do
			for i2=1,#solids[i] do
				color[#color+1]=c[i]
			end
		end
	else if #color<n then
			for i=1,n-#color do
				color[#color+1]=color[1] and color[math.random(#color)] or "&HFFFFFF&"
			end
		end
	end
	for i=1,n do
		s.n[i]=loop s[i]={}
	end
	for ms=1,loop do
		for i=1,#solids do
			local spread=spread_func and spread_func(i,#solids,ms,loop,nums) or {{},{},1,0}
			local k=len[i]==0 and 0 or (spread[4]+len[i])/len[i] temp[i]=space.tbl_copy(solids[i])
			temp[i]=space.shift_solid(temp[i],-cp[i][1],-cp[i][2],-cp[i][3]) local v={cp[i][1]*k,cp[i][2]*k,cp[i][3]*k} p1[i]=v
			for roll=1,#spread[1] do
				temp[i]=space.rotate_solid(temp[i],spread[1][roll],spread[2][roll])
			end
			if type(spread[3])=="table" then
				temp[i]=space.shift_solid(space.fsc_solid(temp[i],spread[3][1],spread[3][2],spread[3][3]),v[1],v[2],v[3])
			else temp[i]=space.shift_solid(space.fsc_solid(temp[i],spread[3]),v[1],v[2],v[3])
			end
		end
		local action=roll_fsc_pos and roll_fsc_pos(ms,loop) or {{-110},{"x"},1,0,0,0} local tr temp.cp=p1
		if type(action[1][1])=="table" then
			for i=1,#solids do
				for roll=1,#action[1][1] do
					temp[i]=space.rotate_solid(temp[i],action[1][1][roll],action[1][2][roll])
				end
				temp[i]=space.shift_solid(temp[i],action[1][3],action[1][4],action[1][5])
			end
			tr=space.concave2(temp,action[2],action[3],action[4],action[5],action[6],action[7],color,light)
		else tr=space.concave2(temp,action[1],action[2],action[3],action[4],action[5],action[6],color,light)
		end
		for i=1,n do
			s[i][ms]=tr[i]
		end
	end
	return s
end

function space.rings_rand(n1,h,r,d1,d2,mul,s_time,e_time,interval,ra,rand_tbl,roll_fsc_pos,color,light)
	local deg=math.rad(180/n1) local d2=d2<r*(1-math.cos(deg))*n1/2.5 and r*(1-math.cos(deg))*n1/2.5 or d2
	local h=h>r/5-d2*math.cos(deg)*math.cos(deg)/5 and r/5-d2*math.cos(deg)*math.cos(deg)/5 or h
	local _,solids=space.rings(n1,h,r,d1,d2,mul,{},{})
	local n,temp,nums=4*#solids,{},{} local loop=math.ceil((e_time-s_time)/interval) local s={n={}} local color=color or {}
	if rand_tbl then
		if #rand_tbl<n1*4 then
			for i=1,n1*4-#rand_tbl do
				rand_tbl[#rand_tbl+1]=rand_tbl[1] and rand_tbl[math.random(#rand_tbl)] or {0,180}
			end
		end
		for i=1,#rand_tbl do
			nums[i]=math.random(rand_tbl[i][1],rand_tbl[i][2])
		end
	end
	if type(color[1])=="table" then
		if #color[1]<#solids then
			for i=1,#solids-#color[1] do
				color[1][#color[1]+1]=color[1][1] and color[1][math.random(#color[1])] or "&HFFFFFF&"
			end
		end
		local c=color[1] color={}
		for i=1,#solids do
			for i2=1,#solids[i] do
				color[#color+1]=c[i]
			end
		end
	else if #color<n then
			for i=1,n-#color do
				color[#color+1]=color[1] and color[math.random(#color)] or "&HFFFFFF&"
			end
		end
	end
	for i=1,n do
		s.n[i]=loop s[i]={}
	end
	for ms=1,loop do
		for i=1,#solids do
			local idx=math.ceil(i/n1) local x,y,z=nums[idx]+ra*ms,nums[idx]+ra*ms,nums[idx]+ra*ms local tbl={{x,y,z},{"x","y","z"}}
			temp[i]=space.tbl_copy(solids[i])
			for roll=1,#tbl[1] do
				temp[i]=space.rotate_solid(temp[i],tbl[1][roll],tbl[2][roll])
			end
		end
		local action=roll_fsc_pos and roll_fsc_pos(ms,loop) or {{-110},{"x"},1,0,0,0} local tr
		if type(action[1][1])=="table" then
			for i=1,#solids do
				for roll=1,#action[1][1] do
					temp[i]=space.rotate_solid(temp[i],action[1][1][roll],action[1][2][roll])
				end
				temp[i]=space.shift_solid(temp[i],action[1][3],action[1][4],action[1][5])
			end
			tr=space.concave2(temp,action[2],action[3],action[4],action[5],action[6],action[7],color,light)
		else tr=space.concave2(temp,action[1],action[2],action[3],action[4],action[5],action[6],color,light)
		end
		for i=1,n do
			s[i][ms]=tr[i]
		end
	end
	return s
end

function space.cube_n(n,len,color,inside_clr)
	local _,cube=space.cuboid(len,len,len,{},{}) local all={} local p=n%2==1 and math.floor(n/2)*len or (n-1)/2*len
	local color,inside_clr=color or {},inside_clr or "&HFFECA8&" local c={}
	if #color<6 then
		for i=1,6-#color do
			color[#color+1]=color[1] and color[math.random(#color)] or "&HFFFFFF&"
		end
	end
	for i=1,n do
		for i2=1,n do
			for i3=1,n do
				local x,y,z=(i-1)*len-p,(i2-1)*len-p,(i3-1)*len-p all[#all+1]=space.shift_solid(cube,x,y,z)
				c[#c+1]=z==-p and color[1] or inside_clr
				c[#c+1]=y==p and color[2] or inside_clr
				c[#c+1]=x==p and color[3] or inside_clr
				c[#c+1]=x==-p and color[4] or inside_clr
				c[#c+1]=y==-p and color[5] or inside_clr
				c[#c+1]=z==p and color[6] or inside_clr
			end
		end
	end
	return all,c
end

function space.cube_blast(si,sn,i,n,nums,len,ra,copies)
	local copies=copies or 3 local step1=math_round(n/copies) local x,y,z local a,ra=a or 1,ra or 7
	if i<step1 then
		x=nums[si]*(i-1)/step1 y=nums[sn+1-si]*(i-1)/step1
	else x=nums[si]+ra*(i-step1) y=nums[sn+1-si]+ra*(i-step1)
	end
	return {{x,y},{"x","y"},1,len*(i-1)/(n-1)}
end

function space.solids_to_rhombic_12(si,sn,i,n,len,s_time,e_time)
	local s_time,e_time=s_time and s_time or 0,e_time and e_time or 0 local n1=math.ceil((n-s_time-e_time)/3)
	local l1=len*(i-s_time-1)/n1 local x,y=0,0
	if i<s_time+1 then
		return {{0},{"z"},1,0}
	elseif i<s_time+n1 then
		return {{x,y},{"x","y"},1,l1}
	elseif i<s_time+2*n1 then
		x=180*(i-s_time+n1)/n1 y=x
		if si==3 or si==4 then
			return {{y},{"y"},1,len}
		else return {{x},{"x"},1,len}
		end
	elseif i<s_time+3*n1 then
		if si==3 or si==4 then
			return {{180},{"y"},1,len*(1-(i-s_time-2*n1)/n1/2)}
		else return {{180},{"x"},1,len*(1-(i-s_time-2*n1)/n1/2)}
		end
	else if si==3 or si==4 then
			return {{180},{"y"},1,len/2}
		else return {{180},{"x"},1,len/2}
		end
	end
end

function space.sketch(set,s_time,e_time,interval,jitter,roll_fsc_pos,color,surface,sur_filter,light)
	local n=#set local loop=math.ceil((e_time-s_time)/interval) local s={n={}} local cp,cnt={0,0,0},0
	local color=color or {} local x,y,z=math.random(-jitter[1],jitter[1]),math.random(-jitter[2],jitter[2]),math.random(-jitter[3],jitter[3])
	if #color<n then
		for i=1,n-#color do
			color[#color+1]=color[1] and color[math.random(#color)] or "&HFFFFFF&"
		end
	end
	for i=1,n do
		s.n[i]=loop s[i]={}
		for ms=1,loop do
			local action=roll_fsc_pos and roll_fsc_pos(ms,loop) or {{-110},{"x"},1,0,0,0}
			if surface and surface[i] then
				cnt=ms==1 and cnt+1 or cnt local c=surface.c[i] and surface.c[i] or "&HFFFFFF&" local filter=sur_filter and sur_filter(ms,loop) or nil
				local p=space.plane_info(space.translate(set[i],x,y,z),cp,action[1],action[2],action[3],action[4],action[5],action[6],color[i],surface[i],c,filter,light)
				s[i][ms]={s=p.s,lay=p.lay,c=p.c} s[n+cnt]=ms==1 and {} or s[n+cnt] s.n[n+cnt]=loop
				s[n+cnt][ms]={s=p.sur,lay=p.sur_lay,c=p.sur_c,clip=p.clip}
			else local p=space.plane_info(space.translate(set[i],x,y,z),cp,action[1],action[2],action[3],action[4],action[5],action[6],color[i],nil,nil,nil,light)
				s[i][ms]={s=p.s,lay=p.lay,c=p.c}
			end
		end
	end
	return s
end

function space.jitter(set,s_time,e_time,interval,jitter,roll_fsc_pos,color,surface,sur_filter,light)
	local n=#set local loop=math.ceil((e_time-s_time)/interval) local s={n={}} local cp,cnt={0,0,0},0 local color=color or {}
	if #color<n then
		for i=1,n-#color do
			color[#color+1]=color[1] and color[math.random(#color)] or "&HFFFFFF&"
		end
	end
	for i=1,n do
		s.n[i]=loop s[i]={}
		for ms=1,loop do
			local action=roll_fsc_pos and roll_fsc_pos(ms,loop) or {{-110},{"x"},1,0,0,0}
			local x,y,z=math.random(-jitter[1],jitter[1]),math.random(-jitter[2],jitter[2]),math.random(-jitter[3],jitter[3])
			if surface and surface[i] then
				cnt=ms==1 and cnt+1 or cnt local c=surface.c[i] and surface.c[i] or "&HFFFFFF&" local filter=sur_filter and sur_filter(ms,loop) or nil
				local p=space.plane_info(space.translate(set[i],x,y,z),cp,action[1],action[2],action[3],action[4],action[5],action[6],color[i],surface[i],c,filter,light)
				s[i][ms]={s=p.s,lay=p.lay,c=p.c} s[n+cnt]=ms==1 and {} or s[n+cnt] s.n[n+cnt]=loop
				s[n+cnt][ms]={s=p.sur,lay=p.sur_lay,c=p.sur_c,clip=p.clip}
			else local p=space.plane_info(space.translate(set[i],x,y,z),cp,action[1],action[2],action[3],action[4],action[5],action[6],color[i],nil,nil,nil,light)
				s[i][ms]={s=p.s,lay=p.lay,c=p.c}
			end
		end
	end
	return s
end

function space.move_fixed_axis(i,x,y,z,angle,mode)
	local mode=mode or "z" local axis={0,0,1}
	if mode=="y" then
		axis={0,1,0}
	elseif mode=="x" then
		axis={1,0,0}
	else axis=mode
	end
	return {{angle*i,x,y,z},{axis,"x","y","z"}}
end

function space.fixed_axis_xyz(i,x,y,z,angle,mode)
	local mode=mode or "z" local axis={0,0,1}
	if mode=="y" then
		axis={0,1,0}
	elseif mode=="x" then
		axis={1,0,0}
	else axis=mode
	end
	return {{angle*i,x*i,y*i,z*i},{axis,"x","y","z"}}
end

function space.cube2thorn(len,s_time,e_time,interval,func,color,light)
	local n=24 local loop=math.ceil((e_time-s_time)/interval) local s={n={}}
	local color=color or {}
	if #color<n then
		for i=1,n-#color do
			color[#color+1]=color[1] and color[math.random(#color)] or "&HFFFFFF&"
		end
	end
	for i=1,n do
		s.n[i]=loop s[i]={}
	end
	for ms=1,loop do
		local action=func and func(ms,loop) or {{-110},{"x"},1,0,0,0} local _,solids=space.thorn(len,len+1+action[1],{},{})
		local tr=space.concave(solids,action[2],action[3],action[4],action[5],action[6],action[7],color,light)
		for i=1,n do
			s[i][ms]=tr[i]
		end
	end
	return s
end

function space.to_sphere(r,s_time,e_time,interval,func,color,light)
	local loop=math.ceil((e_time-s_time)/interval) local s={n={}} local color=color or {}
	if #color<1280 then
		for i=1,1280-#color do
			color[#color+1]=color[1] and color[math.random(#color)] or "&HFFFFFF&"
		end
	end
	for ms=1,loop do
		local action=func and func(ms,loop) or {{-110},{"x"},1,0,0,0} s[ms]={}
		local solid=space.build_to_sphere_subdivide(action[1],r,action[2])
		local tr=space.concave({solid},action[3],action[4],action[5],action[6],action[7],action[8],color,light)
		for i=1,#solid do
			s.n[ms]=#solid s[ms][i]=tr[i]
		end
	end
	return s
end

function space.tosphere(r,s_time,e_time,interval,func,color,light)
	local loop=math.ceil((e_time-s_time)/interval) local s={n={}} local color=color or {}
	if #color<960 then
		for i=1,960-#color do
			color[#color+1]=color[1] and color[math.random(#color)] or "&HFFFFFF&"
		end
	end
	for ms=1,loop do
		local action=func and func(ms,loop) or {{-110},{"x"},1,0,0,0} s[ms]={}
		local solid=space.to_dodecahedron_subdivide(action[1],r,action[2])
		local tr=space.concave({solid},action[3],action[4],action[5],action[6],action[7],action[8],color,light)
		for i=1,#solid do
			s.n[ms]=#solid s[ms][i]=tr[i]
		end
	end
	return s
end

function space.sphere_0to2(ms,loop)
	local n,pct
	if ms<math.ceil(loop/3) then
		n=0 pct=(ms-1)/math.ceil(loop/3)
	elseif ms<math.ceil(loop*2/3) then
		n=1 pct=(ms-math.ceil(loop/3))/math.ceil(loop/3)
	else n=2 pct=(ms-math.ceil(loop*2/3))/math.ceil(loop/3)
	end
	return {n,pct,{80,30,0},{"x","y","z"}}
end

function space.sphere_0to1_back(ms,loop)
	local n,pct
	if ms<math.ceil(loop/4) then
		n=0 pct=(ms-1)/math.ceil(loop/4)
	elseif ms<math.ceil(loop*2/4) then
		n=1 pct=(ms-math.ceil(loop/4))/math.ceil(loop/4)
	elseif ms<math.ceil(loop*3/4) then
		n=1 pct=1-(ms-math.ceil(loop*2/4))/math.ceil(loop/4)
	else n=0 pct=1-(ms-math.ceil(loop*3/4))/math.ceil(loop/4)
	end
	return {n,pct,{80,30,0},{"x","y","z"}}
end

function space.sphere2thorn(n1,r,s_time,e_time,interval,func,color,light)
	local n=20*4^n1*3 local loop=math.ceil((e_time-s_time)/interval) local s={n={}}
	local color=color or {}
	if #color<n then
		for i=1,n-#color do
			color[#color+1]=color[1] and color[math.random(#color)] or "&HFFFFFF&"
		end
	end
	for i=1,n do
		s.n[i]=loop s[i]={}
	end
	for ms=1,loop do
		local action=func and func(ms,loop) or {{-110},{"x"},1,0,0,0} local solids=space.build_sphere_thorn(n1,r,action[1]+1)
		local tr=space.concave(solids,action[2],action[3],action[4],action[5],action[6],action[7],color,light)
		for i=1,n do
			s[i][ms]=tr[i]
		end
	end
	return s
end

function space.faces2thorn(solid,s_time,e_time,interval,func,color,light)
	local n=0 local loop=math.ceil((e_time-s_time)/interval) local s={n={}} local color=color or {}
	for i=1,#solid do
		n=n+#solid[i]
	end
	if #color<n then
		for i=1,n-#color do
			color[#color+1]=color[1] and color[math.random(#color)] or "&HFFFFFF&"
		end
	end
	for i=1,n do
		s.n[i]=loop s[i]={}
	end
	for ms=1,loop do
		local action=func and func(ms,loop) or {{-110},{"x"},1,0,0,0} local solids=space.build_face_thorn(solid,action[1]+1)
		local tr=space.concave(solids,action[2],action[3],action[4],action[5],action[6],action[7],color,light)
		for i=1,n do
			s[i][ms]=tr[i]
		end
	end
	return s
end

function space.faces2platform(solid,s_time,e_time,interval,func,color,light)
	local n=0 local loop=math.ceil((e_time-s_time)/interval) local s={n={}} local color=color or {}
	for i=1,#solid do
		n=n+#solid[i]+1
	end
	if #color<n then
		for i=1,n-#color do
			color[#color+1]=color[1] and color[math.random(#color)] or "&HFFFFFF&"
		end
	end
	for i=1,n do
		s.n[i]=loop s[i]={}
	end
	for ms=1,loop do
		local action=func and func(ms,loop) or {{-110},{"x"},1,0,0,0} local solids=space.build_face_platform(solid,action[1]+1,action[2])
		local tr=space.concave(solids,action[3],action[4],action[5],action[6],action[7],action[8],color,light)
		for i=1,n do
			s[i][ms]=tr[i]
		end
	end
	return s
end

function space.face2platform(solid,s_time,e_time,interval,func,color,light)
	local n1=#solid*10 local loop=math.ceil((e_time-s_time)/interval) local s={n={}} local color=color or {}
	if #color<n1 then
		for i=1,n1-#color do
			color[#color+1]=color[1] and color[math.random(#color)] or "&HFFFFFF&"
		end
	end
	for ms=1,loop do
		local action=func and func(ms,loop,#solid) or {{-110},{"x"},1,0,0,0} s[ms]={} local cnt,n=0,0
		local solids=space.build_face_platform_free(action[3],solid,action[1]+1,action[2])
		local tr=space.concave(solids,action[4],action[5],action[6],action[7],action[8],action[9],color,light)
		for i=1,#solids do
			cnt=cnt+#solids[i]
			for i2=1,#solids[i] do
				n=n+1 s[ms][n]=tr[n]
			end
			s.n[ms]=cnt
		end
	end
	return s
end

function space.to_star_n(n1,r,s_time,e_time,interval,func,color,light)
	local n=n1*4 local loop=math.ceil((e_time-s_time)/interval) local s={n={}}
	local color=color or {}
	if #color<n then
		for i=1,n-#color do
			color[#color+1]=color[1] and color[math.random(#color)] or "&HFFFFFF&"
		end
	end
	for i=1,n do
		s.n[i]=loop s[i]={}
	end
	for ms=1,loop do
		local action=func and func(ms,loop) or {{-110},{"x"},1,0,0,0} local _,solids=space.star_n(n1,action[1],r+action[2],r,{},{})
		local tr=space.concave(solids,action[3],action[4],action[5],action[6],action[7],action[8],color,light)
		for i=1,n do
			s[i][ms]=tr[i]
		end
	end
	return s
end

function space.to_star_n_simple(n1,r,s_time,e_time,interval,func,color,light)
	local n=n1*4 local loop=math.ceil((e_time-s_time)/interval) local s={n={}}
	local color=color or {}
	if #color<n then
		for i=1,n-#color do
			color[#color+1]=color[1] and color[math.random(#color)] or "&HFFFFFF&"
		end
	end
	for i=1,n do
		s.n[i]=loop s[i]={}
	end
	for ms=1,loop do
		local action=func and func(ms,loop) or {{-110},{"x"},1,0,0,0} local _,solids=space.star_n_simple(n1,action[1],r+action[2],r,{},{})
		local tr=space.concave(solids,action[3],action[4],action[5],action[6],action[7],action[8],color,light)
		for i=1,n do
			s[i][ms]=tr[i]
		end
	end
	return s
end

function space.to_dice(len,s_time,e_time,interval,roll_fsc_pos,color,surface,sur_filter,light)
	local n=14 local loop=math.ceil((e_time-s_time)/interval) local s={n={}}
	local color=color or {} local cp,cnt={0,0,0},0
	if #color<n then
		for i=1,n-#color do
			color[#color+1]=color[1] and color[math.random(#color)] or "&HFFFFFF&"
		end
	end
	for i=1,n do
		s.n[i]=loop s[i]={}
		for ms=1,loop do
			local action=roll_fsc_pos and roll_fsc_pos(ms,loop) or {{-110},{"x"},1,0,0,0}
			local _,set=space.truncated_cube(action[1],len,{},{})
			if surface and surface[i] then
				cnt=ms==1 and cnt+1 or cnt local c=surface.c[i] and surface.c[i] or "&HFFFFFF&" local filter=sur_filter and sur_filter(ms,loop) or nil
				local p=space.plane_info(set[i],cp,action[2],action[3],action[4],action[5],action[6],action[7],color[i],surface[i],c,filter,light)
				s[i][ms]={s=p.s,lay=p.lay,c=p.c} s[n+cnt]=ms==1 and {} or s[n+cnt] s.n[n+cnt]=loop
				s[n+cnt][ms]={s=p.sur,lay=p.sur_lay,c=p.sur_c,clip=p.clip}
			else local p=space.plane_info(set[i],cp,action[2],action[3],action[4],action[5],action[6],action[7],color[i],nil,nil,nil,light)
				s[i][ms]={s=p.s,lay=p.lay,c=p.c}
			end
		end
	end
	return s
end

function space.cube_to_12(len,s_time,e_time,interval,roll_fsc_pos,color,surface,sur_filter,light)
	local n=12 local loop=math.ceil((e_time-s_time)/interval) local s={n={}}
	local color=color or {} local cp,cnt={0,0,0},0
	if #color<n then
		for i=1,n-#color do
			color[#color+1]=color[1] and color[math.random(#color)] or "&HFFFFFF&"
		end
	end
	for i=1,n do
		s.n[i]=loop s[i]={}
		for ms=1,loop do
			local action=roll_fsc_pos and roll_fsc_pos(ms,loop) or {{-110},{"x"},1,0,0,0}
			local _,set=space.cube_to_dodecahedron(len,action[1],{},{})
			if surface and surface[i] then
				cnt=ms==1 and cnt+1 or cnt local c=surface.c[i] and surface.c[i] or "&HFFFFFF&" local filter=sur_filter and sur_filter(ms,loop) or nil
				local p=space.plane_info(set[i],cp,action[2],action[3],action[4],action[5],action[6],action[7],color[i],surface[i],c,filter,light)
				s[i][ms]={s=p.s,lay=p.lay,c=p.c} s[n+cnt]=ms==1 and {} or s[n+cnt] s.n[n+cnt]=loop
				s[n+cnt][ms]={s=p.sur,lay=p.sur_lay,c=p.sur_c,clip=p.clip}
			else local p=space.plane_info(set[i],cp,action[2],action[3],action[4],action[5],action[6],action[7],color[i],nil,nil,nil,light)
				s[i][ms]={s=p.s,lay=p.lay,c=p.c}
			end
		end
	end
	return s
end

function space.freemove2(set,s_time,e_time,interval,roll_fsc_pos,color,surface,sur_filter,light)
	local n=#set local loop=math.ceil((e_time-s_time)/interval) local s={n={}}
	local color=color or {} local cp,cnt={0,0,0},0
	if #color<n then
		for i=1,n-#color do
			color[#color+1]=color[1] and color[math.random(#color)] or "&HFFFFFF&"
		end
	end
	for ms=1,loop do
		local action=roll_fsc_pos and roll_fsc_pos(ms,loop) or {{-110},{"x"},1,0,0,0} s[ms]={}
		for i=1,n do
			if surface and surface[i] then
				cnt=ms==1 and cnt+1 or cnt local c=surface.c[i] and surface.c[i] or "&HFFFFFF&" local filter=sur_filter and sur_filter(ms,loop) or nil
				local p=space.plane_info(set[i],cp,action[1],action[2],action[3],action[4],action[5],action[6],color[i],surface[i],c,filter,light)
				if action[7] then
					s[ms][i]={s=p.s,lay=p.lay,c=p.c} s[ms][n+cnt]={s=p.sur,lay=p.sur_lay,c=p.sur_c,clip=p.clip}
				else if p.lay==1 then
						s[ms][#s[ms]+1]={s=p.s,lay=p.lay,c=p.c} s[ms][#s[ms]+1]={s=p.sur,lay=p.sur_lay,c=p.sur_c,clip=p.clip}
					end
				end
			else local p=space.plane_info(set[i],cp,action[1],action[2],action[3],action[4],action[5],action[6],color[i],nil,nil,nil,light)
				if action[7] then
					s[ms][i]={s=p.s,lay=p.lay,c=p.c}
				else if p.lay==1 then
					s[ms][#s[ms]+1]={s=p.s,lay=p.lay,c=p.c}
					end
				end
			end
		end
		s.n[ms]=#s[ms]
	end
	return s
end

function space.concave_move2(solids,s_time,e_time,interval,roll_fsc_pos,color,light)
	local n=0 local loop=math.ceil((e_time-s_time)/interval) local s={n={}}
	local color=color or {}
	for i=1,#solids do
		n=n+#solids[i]
	end
	if #color<n then
		for i=1,n-#color do
			color[#color+1]=color[1] and color[math.random(#color)] or "&HFFFFFF&"
		end
	end
	for ms=1,loop do
		local action=roll_fsc_pos and roll_fsc_pos(ms,loop) or {{-110},{"x"},1,0,0,0}
		local tr=space.concave(solids,action[1],action[2],action[3],action[4],action[5],action[6],color,light)
		for i=1,n do
			if tr[i].lay>0 then
				s[ms][#s[ms]+1]=tr[i]
			end
		end
		s.n[ms]=#s[ms]
	end
	return s
end

function space.concave_freemove2(solids,s_time,e_time,interval,roll_fsc_pos,color,light)
	local n=0 local loop=math.ceil((e_time-s_time)/interval) local s={n={}}
	local color=color or {}
	for i=1,#solids do
		n=n+#solids[i]
	end
	if #color<n then
		for i=1,n-#color do
			color[#color+1]=color[1] and color[math.random(#color)] or "&HFFFFFF&"
		end
	end
	for ms=1,loop do
		local action=roll_fsc_pos and roll_fsc_pos(ms,loop) or {{-110},{"x"},1,0,0,0} s[ms]={} local tr
		if type(action[1][1])=="table" then
			for i=1,#solids do
				for roll=1,#action[1][1] do
					solids[i]=space.rotate_solid(solids[i],action[1][1][roll],action[1][2][roll])
				end
				solids[i]=space.shift_solid(solids[i],action[1][3],action[1][4],action[1][5])
			end
			tr=space.concave(solids,action[2],action[3],action[4],action[5],action[6],action[7],color,light)
		else tr=space.concave(solids,action[1],action[2],action[3],action[4],action[5],action[6],color,light)
		end
		for i=1,n do
			if tr[i].lay>0 then
				s[ms][#s[ms]+1]=tr[i]
			end
		end
		s.n[ms]=#s[ms]
	end
	return s
end

function space.to_model(func,s_time,e_time,interval,roll_fsc_pos,color,surface,sur_filter,light)
	local loop=math.ceil((e_time-s_time)/interval) local s={n={}}
	local color=color or {} local cp,cnt={0,0,0},0
	for ms=1,loop do
		local action=roll_fsc_pos and roll_fsc_pos(ms,loop) or {{-110},{"x"},1,0,0,0} s[ms]={} local set=func(ms,loop) local n=#set
		for i=1,n do
			color[i]=color[i] and color[i] or color[math.random(#color)]
			if surface and surface[i] then
				cnt=ms==1 and cnt+1 or cnt local c=surface.c[i] and surface.c[i] or "&HFFFFFF&" local filter=sur_filter and sur_filter(ms,loop) or nil
				local p=space.plane_info(set[i],cp,action[1],action[2],action[3],action[4],action[5],action[6],color[i],surface[i],c,filter,light)
				if action[7] then
					s[ms][i]={s=p.s,lay=p.lay,c=p.c} s[ms][n+cnt]={s=p.sur,lay=p.sur_lay,c=p.sur_c,clip=p.clip}
				else if p.lay==1 then
						s[ms][#s[ms]+1]={s=p.s,lay=p.lay,c=p.c} s[ms][#s[ms]+1]={s=p.sur,lay=p.sur_lay,c=p.sur_c,clip=p.clip}
					end
				end
			else local p=space.plane_info(set[i],cp,action[1],action[2],action[3],action[4],action[5],action[6],color[i],nil,nil,nil,light)
				if action[7] then
					s[ms][i]={s=p.s,lay=p.lay,c=p.c}
				else if p.lay==1 then
					s[ms][#s[ms]+1]={s=p.s,lay=p.lay,c=p.c}
					end
				end
			end
		end
		s.n[ms]=#s[ms]
	end
	return s
end

function space.to_models(func,s_time,e_time,interval,roll_fsc_pos,color,light)
	local n=3000 local loop=math.ceil((e_time-s_time)/interval) local s={n={}}
	local color=color or {}
	if #color<n then
		for i=1,n-#color do
			color[#color+1]=color[1] and color[math.random(#color)] or "&HFFFFFF&"
		end
	end
	for ms=1,loop do
		local action=roll_fsc_pos and roll_fsc_pos(ms,loop) or {{-110},{"x"},1,0,0,0} s[ms]={} local solids=func(ms,loop) local tr
		if type(action[1][1])=="table" then
			for i=1,#solids do
				for roll=1,#action[1][1] do
					solids[i]=space.rotate_solid(solids[i],action[1][1][roll],action[1][2][roll])
				end
				solids[i]=space.shift_solid(solids[i],action[1][3],action[1][4],action[1][5])
			end
			tr=space.concave(solids,action[2],action[3],action[4],action[5],action[6],action[7],color,light)
		else tr=space.concave(solids,action[1],action[2],action[3],action[4],action[5],action[6],color,light)
		end
		for i=1,#tr do
			if action[8] then
				s[ms][#s[ms]+1]=tr[i]
			else if tr[i].lay>0 then
					s[ms][#s[ms]+1]=tr[i]
				end
			end
		end
		s.n[ms]=#s[ms]
	end
	return s
end

function space.jitter2(set,s_time,e_time,interval,jitter,roll_fsc_pos,color,surface,sur_filter,light)
	local n=#set local loop=math.ceil((e_time-s_time)/interval) local s={n={}} local cp,cnt={0,0,0},0 local color=color or {}
	if #color<n then
		for i=1,n-#color do
			color[#color+1]=color[1] and color[math.random(#color)] or "&HFFFFFF&"
		end
	end
	for ms=1,loop do
		local action=roll_fsc_pos and roll_fsc_pos(ms,loop) or {{-110},{"x"},1,0,0,0} s[ms]={}
		local x,y,z=math.random(-jitter[1],jitter[1]),math.random(-jitter[2],jitter[2]),math.random(-jitter[3],jitter[3])
		for i=1,n do
			if surface and surface[i] then
				cnt=ms==1 and cnt+1 or cnt local c=surface.c[i] and surface.c[i] or "&HFFFFFF&" local filter=sur_filter and sur_filter(ms,loop) or nil
				local p=space.plane_info(space.translate(set[i],x,y,z),cp,action[1],action[2],action[3],action[4],action[5],action[6],color[i],surface[i],c,filter,light)
				if action[7] then
					s[ms][i]={s=p.s,lay=p.lay,c=p.c} s[ms][n+cnt]={s=p.sur,lay=p.sur_lay,c=p.sur_c,clip=p.clip}
				else if p.lay==1 then
						s[ms][#s[ms]+1]={s=p.s,lay=p.lay,c=p.c} s[ms][#s[ms]+1]={s=p.sur,lay=p.sur_lay,c=p.sur_c,clip=p.clip}
					end
				end
			else local p=space.plane_info(space.translate(set[i],x,y,z),cp,action[1],action[2],action[3],action[4],action[5],action[6],color[i],nil,nil,nil,light)
				if action[7] then
					s[ms][i]={s=p.s,lay=p.lay,c=p.c}
				else if p.lay==1 then
					s[ms][#s[ms]+1]={s=p.s,lay=p.lay,c=p.c}
					end
				end
			end
		end
		s.n[ms]=#s[ms]
	end
	return s
end

function space.freemove2_plus(set,s_time,e_time,interval,roll_fsc_pos,color,surface,sur_filter,light)
	local n=#set local loop=math.ceil((e_time-s_time)/interval) local s={n={}}
	local color=color or {} local cp,cnt={0,0,0},0 local coor={n={}}
	if #color<n then
		for i=1,n-#color do
			color[#color+1]=color[1] and color[math.random(#color)] or "&HFFFFFF&"
		end
	end
	for ms=1,loop do
		local action=roll_fsc_pos and roll_fsc_pos(ms,loop) or {{-110},{"x"},1,0,0,0} s[ms]={} coor[ms]={}
		for i=1,n do
			if surface and surface[i] then
				cnt=ms==1 and cnt+1 or cnt local c=surface.c[i] and surface.c[i] or "&HFFFFFF&" local filter=sur_filter and sur_filter(ms,loop) or nil
				local p=space.plane_info(set[i],cp,action[1],action[2],action[3],action[4],action[5],action[6],color[i],surface[i],c,filter,light)
				if action[7] then
					s[ms][i]={s=p.s,p={x=(p.s):match("[-.%d]+"),y=(p.s):match("[-.%d]+ ([-.%d]+)")},lay=p.lay,c=p.c}
					s[ms][n+cnt]={s=p.sur,p={x=(p.s):match("[-.%d]+"),y=(p.s):match("[-.%d]+ ([-.%d]+)")},lay=p.sur_lay,c=p.sur_c,clip=p.clip}
				else if p.lay==1 then
						s[ms][#s[ms]+1]={s=p.s,p={x=(p.s):match("[-.%d]+"),y=(p.s):match("[-.%d]+ ([-.%d]+)")},lay=p.lay,c=p.c}
						s[ms][#s[ms]+1]={s=p.sur,p={x=(p.s):match("[-.%d]+"),y=(p.s):match("[-.%d]+ ([-.%d]+)")},lay=p.sur_lay,c=p.sur_c,clip=p.clip}
					end
				end
				for p1,p2 in (p.s):gmatch("([-.%d]+) ([-.%d]+)") do
					coor[ms][#coor[ms]+1]={x=p1,y=p2,lay=p.lay}
				end
			else local p=space.plane_info(set[i],cp,action[1],action[2],action[3],action[4],action[5],action[6],color[i],nil,nil,nil,light)
				if action[7] then
					s[ms][i]={s=p.s,p={x=(p.s):match("[-.%d]+"),y=(p.s):match("[-.%d]+ ([-.%d]+)")},lay=p.lay,c=p.c}
				else if p.lay==1 then
					s[ms][#s[ms]+1]={s=p.s,p={x=(p.s):match("[-.%d]+"),y=(p.s):match("[-.%d]+ ([-.%d]+)")},lay=p.lay,c=p.c}
					end
				end
				for p1,p2 in (p.s):gmatch("([-.%d]+) ([-.%d]+)") do
					coor[ms][#coor[ms]+1]={x=p1,y=p2,lay=p.lay}
				end
			end
		end
		s.n[ms]=#s[ms] coor.n[ms]=#coor[ms]
	end
	return s,coor
end

function space.to_model_plus(func,s_time,e_time,interval,roll_fsc_pos,color,surface,sur_filter,light)
	local loop=math.ceil((e_time-s_time)/interval) local s={n={}}
	local color=color or {} local cp,cnt={0,0,0},0 local coor={n={}}
	for ms=1,loop do
		local action=roll_fsc_pos and roll_fsc_pos(ms,loop) or {{-110},{"x"},1,0,0,0} s[ms]={} coor[ms]={} local set=func(ms,loop) local n=#set
		for i=1,n do
			color[i]=color[i] and color[i] or color[math.random(#color)]
			if surface and surface[i] then
				cnt=ms==1 and cnt+1 or cnt local c=surface.c[i] and surface.c[i] or "&HFFFFFF&" local filter=sur_filter and sur_filter(ms,loop) or nil
				local p=space.plane_info(set[i],cp,action[1],action[2],action[3],action[4],action[5],action[6],color[i],surface[i],c,filter,light)
				if action[7] then
					s[ms][i]={s=p.s,p={x=(p.s):match("[-.%d]+"),y=(p.s):match("[-.%d]+ ([-.%d]+)")},lay=p.lay,c=p.c}
					s[ms][n+cnt]={s=p.sur,p={x=(p.s):match("[-.%d]+"),y=(p.s):match("[-.%d]+ ([-.%d]+)")},lay=p.sur_lay,c=p.sur_c,clip=p.clip}
				else if p.lay==1 then
						s[ms][#s[ms]+1]={s=p.s,p={x=(p.s):match("[-.%d]+"),y=(p.s):match("[-.%d]+ ([-.%d]+)")},lay=p.lay,c=p.c}
						s[ms][#s[ms]+1]={s=p.sur,p={x=(p.s):match("[-.%d]+"),y=(p.s):match("[-.%d]+ ([-.%d]+)")},lay=p.sur_lay,c=p.sur_c,clip=p.clip}
					end
				end
				for p1,p2 in (p.s):gmatch("([-.%d]+) ([-.%d]+)") do
					coor[ms][#coor[ms]+1]={x=p1,y=p2,lay=p.lay}
				end
			else local p=space.plane_info(set[i],cp,action[1],action[2],action[3],action[4],action[5],action[6],color[i],nil,nil,nil,light)
				if action[7] then
					s[ms][i]={s=p.s,p={x=(p.s):match("[-.%d]+"),y=(p.s):match("[-.%d]+ ([-.%d]+)")},lay=p.lay,c=p.c}
				else if p.lay==1 then
					s[ms][#s[ms]+1]={s=p.s,p={x=(p.s):match("[-.%d]+"),y=(p.s):match("[-.%d]+ ([-.%d]+)")},lay=p.lay,c=p.c}
					end
				end
				for p1,p2 in (p.s):gmatch("([-.%d]+) ([-.%d]+)") do
					coor[ms][#coor[ms]+1]={x=p1,y=p2,lay=p.lay}
				end
			end
		end
		s.n[ms]=#s[ms] coor.n[ms]=#coor[ms]
	end
	return s,coor
end

function space.concave_freemove2_plus(solids,s_time,e_time,interval,roll_fsc_pos,color,light)
	local n=0 local loop=math.ceil((e_time-s_time)/interval) local s={n={}}
	local color=color or {} local coor={n={}}
	for i=1,#solids do
		n=n+#solids[i]
	end
	if #color<n then
		for i=1,n-#color do
			color[#color+1]=color[1] and color[math.random(#color)] or "&HFFFFFF&"
		end
	end
	for ms=1,loop do
		local action=roll_fsc_pos and roll_fsc_pos(ms,loop) or {{-110},{"x"},1,0,0,0} s[ms]={} coor[ms]={} local tr
		if type(action[1][1])=="table" then
			for i=1,#solids do
				for roll=1,#action[1][1] do
					solids[i]=space.rotate_solid(solids[i],action[1][1][roll],action[1][2][roll])
				end
				solids[i]=space.shift_solid(solids[i],action[1][3],action[1][4],action[1][5])
			end
			tr=space.concave(solids,action[2],action[3],action[4],action[5],action[6],action[7],color,light)
		else tr=space.concave(solids,action[1],action[2],action[3],action[4],action[5],action[6],color,light)
		end
		for i=1,n do
			if action[8] then
				s[ms][#s[ms]+1]=tr[i] s[ms][#s[ms]].p={x=(tr[i].s):match("[-.%d]+"),y=(tr[i].s):match("[-.%d]+ ([-.%d]+)")}
			else if tr[i].lay>0 then
					s[ms][#s[ms]+1]=tr[i] s[ms][#s[ms]].p={x=(tr[i].s):match("[-.%d]+"),y=(tr[i].s):match("[-.%d]+ ([-.%d]+)")}
				end
			end
			for p1,p2 in (tr[i].s):gmatch("([-.%d]+) ([-.%d]+)") do
				if action[8] then
					coor[ms][#coor[ms]+1]={x=p1,y=p2,lay=tr[i].lay}
				else if tr[i].lay>0 then
						coor[ms][#coor[ms]+1]={x=p1,y=p2,lay=tr[i].lay}
					end
				end
			end
		end
		s.n[ms]=#s[ms] coor.n[ms]=#coor[ms]
	end
	return s,coor
end

function space.to_models_plus(func,s_time,e_time,interval,roll_fsc_pos,color,light)
	local n=3000 local loop=math.ceil((e_time-s_time)/interval) local s={n={}}
	local color=color or {} local coor={n={}}
	if #color<n then
		for i=1,n-#color do
			color[#color+1]=color[1] and color[math.random(#color)] or "&HFFFFFF&"
		end
	end
	for ms=1,loop do
		local action=roll_fsc_pos and roll_fsc_pos(ms,loop) or {{-110},{"x"},1,0,0,0} s[ms]={} coor[ms]={} local solids=func(ms,loop) local tr
		if type(action[1][1])=="table" then
			for i=1,#solids do
				for roll=1,#action[1][1] do
					solids[i]=space.rotate_solid(solids[i],action[1][1][roll],action[1][2][roll])
				end
				solids[i]=space.shift_solid(solids[i],action[1][3],action[1][4],action[1][5])
			end
			tr=space.concave(solids,action[2],action[3],action[4],action[5],action[6],action[7],color,light)
		else tr=space.concave(solids,action[1],action[2],action[3],action[4],action[5],action[6],color,light)
		end
		for i=1,#tr do
			if action[8] then
				s[ms][#s[ms]+1]=tr[i] s[ms][#s[ms]].p={x=(tr[i].s):match("[-.%d]+"),y=(tr[i].s):match("[-.%d]+ ([-.%d]+)")}
			else if tr[i].lay>0 then
					s[ms][#s[ms]+1]=tr[i] s[ms][#s[ms]].p={x=(tr[i].s):match("[-.%d]+"),y=(tr[i].s):match("[-.%d]+ ([-.%d]+)")}
				end
			end
			for p1,p2 in (tr[i].s):gmatch("([-.%d]+) ([-.%d]+)") do
				if action[8] then
					coor[ms][#coor[ms]+1]={x=p1,y=p2,lay=tr[i].lay}
				else if tr[i].lay>0 then
						coor[ms][#coor[ms]+1]={x=p1,y=p2,lay=tr[i].lay}
					end
				end
			end
		end
		s.n[ms]=#s[ms] coor.n[ms]=#coor[ms]
	end
	return s,coor
end

function space.solids2solid(solids)
	local solid={}
	for i=1,#solids do
		for p=1,#solids[i] do
			solid[#solid+1]=solids[i][p]
		end
	end
	return solid
end

function space.simple_tri_nets(n,a,b,h)
	local x,y=a/n,b/n local p0,p={{{0,0,h},{x,y,h},{0,y,h}},{{0,0,h},{x,0,h},{x,y,h}}},{}
	for i=1,n do
		for i2=1,n do
			p[#p+1]=space.translate(p0[1],(i-1)*x,(i2-1)*y) p[#p+1]=space.translate(p0[2],(i-1)*x,(i2-1)*y)
		end
	end
	p=space.shift_solid(p,-a/2,-b/2)
	return p
end

function space.tri_nets(n,a,b,h)
	local points,p={},{}
	for i=1,n do
		points[i]={math.random(-a,a),math.random(-b,b),h}
	end
	for i=1,n-2 do
		p[i]={points[i],points[i+1],points[i+2]}
	end
	return p
end

function space.rect_nets(n1,n2,a,b,h)
	local x,y=a/n1,b/n2 local p0,p={{0,0,h},{x,0,h},{x,y,h},{0,y,h}},{}
	for i=1,n1 do
		for i2=1,n2 do
			p[#p+1]=space.translate(p0,(i-1)*x,(i2-1)*y)
		end
	end
	p=space.shift_solid(p,-a/2,-b/2)
	return p
end

function space.loose(solid,filter,pct)
	local w=filter[7] and filter[7] or 500 local h=filter[8] and filter[8] or 500
	local set=space.filter_solid(solid,
		function(x,y,z)
			return x+filter[1]*math.sin(pct*filter[4]+y*math.ceil(h/200)*math.pi/h),
			y+filter[2]*math.sin(pct*filter[5]+x*math.ceil(w/200)*math.pi/w),
			z+filter[3]*math.sin(pct*filter[6]+y*math.ceil(h/200)*math.pi/h+x*math.ceil(w/200)*math.pi/w)
		end
	)
	return set
end

function space.swing(solid,s_time,e_time,interval,filter,roll_fsc_pos,light)
	local n=#solid local loop=math.ceil((e_time-s_time)/interval) local s={n={}} local light=light or {0,0,1}
	for i=1,n do
		s.n[i]=loop s[i]={}
	end
	for ms=1,loop do
		local planes=space.loose(solid,filter,ms/loop)
		local action=roll_fsc_pos and roll_fsc_pos(ms,loop) or {{-110},{"x"},1,0,0,0}
		for i=1,#action[1] do
			planes=space.rotate_solid(planes,action[1][i],action[2][i])
		end
		if type(action[3])=="table" then
			planes=space.shift_solid(space.fsc_solid(planes,action[3][1],action[3][2],action[3][3]),action[4],action[5],action[6])
		else planes=space.shift_solid(space.fsc_solid(planes,action[3]),action[4],action[5],action[6])
		end
		for i=1,n do
			local nor_v=space.nor_v(planes[i]) local cos=space.dot_product(light,nor_v)/(space.v_nor(nor_v)*space.v_nor(light))
			s[i][ms]={p={x=math_round(planes[i][1][1],1),y=math_round(planes[i][1][2],1)},s=space.restore(planes[i]),a=ass_alpha(255*math.abs(cos))}
		end
	end
	return s
end

function space.or_syntax(n,...)
	local collect={...}
	for i=1,#collect do
		if n==collect[i] then
			return true
		end
	end
	return false
end

function space.mini_cube(len,operation,angle,axis,fsc,x,y,z,color,light)
	local solids=space.build_mini_cube(len,operation,color)
	return space.minicube_info(solids,angle,axis,fsc,x,y,z,light)
end

function space.minicube_info(set,angle,axis,fsc,x,y,z,light)
	local cp={} local cp_z,s,cnt={},{},1
	local solids=space.tbl_copy(set) local lay={}
	for i=1,#solids do
		if solids.cp then
			cp[i]=solids.cp[i]
		else cp[i]=space.center_pos(solids[i])
		end
		for roll=1,#axis do
			solids[i]=space.rotate_solid(solids[i],angle[roll],axis[roll])
			cp[i]=space.rotate_point(cp[i],angle[roll],axis[roll])
		end
		if type(fsc)=="table" then
			solids[i]=space.shift_solid(space.fsc_solid(solids[i],fsc[1],fsc[2],fsc[3]),x,y,z)
			cp[i]=space.shift_point(space.fsc_point(cp[i],fsc[1],fsc[2],fsc[3]),x,y,z)
		else solids[i]=space.shift_solid(space.fsc_solid(solids[i],fsc),x,y,z)
			cp[i]=space.shift_point(space.fsc_point(cp[i],fsc),x,y,z)
		end
		cp_z[i]={cp[i][3],i}
	end
	cp_z=space.tbl_sort(cp_z)
	for i=1,#cp do
		lay[cp_z[i][2]]=(#solids-i)*2
	end
	for i=1,#solids do
		for p=1,#solids[i] do
			local layer=space.visibility(solids[i][p],cp[i]) layer=layer>0 and layer+set[i].lay[p] or layer
			s[#s+1]={s=space.restore(solids[i][p]),lay=layer+lay[i],c=space.shade(set[i].c[p],light,solids[i][p])}
			cnt=cnt+1
		end
	end
	return s
end

function space.minicube_step(str)
	local step={}
	for s in str:gmatch("%a'?2?") do
		local deg=s:match("2") and 180 or 90 deg=s:match("'") and -deg or deg
		step[#step+1]={s:match("^%a"),deg}
	end
	return step
end

function space.minicube_step_reverse(str)
	local step=space.minicube_step(str) local new={}
	for i=1,#step do
		new[i]={step[#step+1-i][1],-step[#step+1-i][2]}
	end
	return new
end

function space.build_mini_cube(len,operation,color)
	local _,cube=space.cuboid(len,len,len,{},{}) local all={} local inside_clr=color[7] or "&H00FCFF&"
	local clr={
		{1,4},{1},{1,3},{1,2,4},{1,2},{1,2,3},{1,4,5},{1,5},{1,3,5},
		{4},{},{3},{2,4},{2},{2,3},{4,5},{5},{3,5},
		{4,6},{6},{3,6},{2,4,6},{2,6},{2,3,6},{4,5,6},{5,6},{3,5,6}
	}
	for i=1,27 do
		local x1=i%3==0 and len or (i%3-2)*len
		local y1=(math.ceil(i/3)%3-1)*len
		local z1=(math.ceil(i/9)-2)*len
		all[i]=space.shift_solid(cube,x1,y1,z1)
		if type(operation)=="table" then
			if type(operation[1])=="string" then
				all[i]=space.minicube_operation(len,all[i],{operation[1],operation[2]})
			else
				for i2=1,#operation do
					all[i]=space.minicube_operation(len,all[i],operation[i2])
				end
			end
		else local step=space.minicube_step(operation)
			for i2=1,#step do
				all[i]=space.minicube_operation(len,all[i],step[i2])
			end
		end
		all[i].c={} all[i].lay={}
		for p=1,#clr[i] do
			all[i].c[clr[i][p]]=color[clr[i][p]]
			all[i].lay[clr[i][p]]=54
		end
		for p=1,6 do
			all[i].c[p]=all[i].c[p] and all[i].c[p] or inside_clr
			all[i].lay[p]=all[i].lay[p] or 0
		end
	end
	return all
end

function space.minicube_operation(len,solid,operation)
	local cp=space.center_pos(solid)
	if operation[1]=="r" then
		if cp[1]==-len then
			solid=space.rotate_solid(solid,operation[2],"x")
		end
	elseif operation[1]=="l" then
		if cp[1]==len then
			solid=space.rotate_solid(solid,operation[2],"x")
		end
	elseif operation[1]=="u" then
		if cp[2]==len then
			solid=space.rotate_solid(solid,operation[2],"y")
		end
	elseif operation[1]=="d" then
		if cp[2]==-len then
			solid=space.rotate_solid(solid,operation[2],"y")
		end
	elseif operation[1]=="f" then
		if cp[3]==-len then
			solid=space.rotate_solid(solid,operation[2],"z")
		end
	elseif operation[1]=="b" then
		if cp[3]==len then
			solid=space.rotate_solid(solid,operation[2],"z")
		end
	end
	return solid
end

function space.minicube_justmove(len,s_time,e_time,interval,operation,roll_fsc_pos,color,light)
	local loop=math.ceil((e_time-s_time)/interval) local s={n={{162,loop}}}
	local color=color or {}
	if #color<6 then
		for i=1,6-#color do
			color[#color+1]=color[1] and color[math.random(#color)] or "&HFFFFFF&"
		end
	end
	for ms=1,loop do
		local action=roll_fsc_pos and roll_fsc_pos(ms,loop) or {{-110},{"x"},1,0,0,0}
		local tr=space.mini_cube(len,operation,action[1],action[2],action[3],action[4],action[5],action[6],color,light)
		for i=1,162 do
			s[i]=ms>1 and s[i] or {}
			s[i][ms]=tr[i]
		end
	end
	return s
end

function space.minicube2next(len,minicube,operation)
	local new,c,lay={},{},{}
	for i=1,#minicube do
		c=minicube[i].c lay=minicube[i].lay
		new[i]=space.minicube_operation(len,minicube[i],operation)
		new[i].c=c new[i].lay=lay
	end
	return new
end

function space.minicube_move_sequence(len,s_time,e_time,interval,operation,roll_fsc_pos,color,light)
	local step=space.minicube_step(operation) local cnt,cube=0,{}
	local loop=math.ceil((e_time-s_time)/interval/#step) local s={n={{162,#step*loop}}}
	local color=color or {}
	if #color<6 then
		for i=1,6-#color do
			color[#color+1]=color[1] and color[math.random(#color)] or "&HFFFFFF&"
		end
	end
	for i=1,#step do
		for ms=1,loop do
			cnt=cnt+1 local tbl={step[i][1]} tbl[2]=ms==1 and 0 or step[i][2]/(loop-1)
			local action=roll_fsc_pos and roll_fsc_pos(cnt,#step*loop) or {{-110},{"x"},1,0,0,0}
			cube[cnt]=cnt>1 and space.minicube2next(len,cube[cnt-1],tbl) or space.build_mini_cube(len,{"r",0},color)
			local tr=space.minicube_info(cube[cnt],action[1],action[2],action[3],action[4],action[5],action[6],light)
			for i2=1,162 do
				s[i2]=cnt>1 and s[i2] or {}
				s[i2][cnt]=tr[i2]
			end
		end
	end
	return s
end

function space.minicube_move_reverse(len,s_time,e_time,interval,operation,roll_fsc_pos,color,light)
	local step=space.minicube_step_reverse(operation) local cnt,cube=0,{space.build_mini_cube(len,{"r",0},color)}
	local loop=math.ceil((e_time-s_time)/interval/#step) local s={n={{162,#step*loop}}}
	local color=color or {} local step1=space.minicube_step(operation)
	if #color<6 then
		for i=1,6-#color do
			color[#color+1]=color[1] and color[math.random(#color)] or "&HFFFFFF&"
		end
	end
	for i=1,#step1 do
		cube[1]=space.minicube2next(len,cube[1],step1[i])
	end
	for i=1,#step do
		for ms=1,loop do
			cnt=cnt+1 local tbl={step[i][1]} tbl[2]=ms==1 and 0 or step[i][2]/(loop-1)
			local action=roll_fsc_pos and roll_fsc_pos(cnt,#step*loop) or {{-110},{"x"},1,0,0,0}
			cube[cnt]=cnt>1 and space.minicube2next(len,cube[cnt-1],tbl) or cube[1]
			local tr=space.minicube_info(cube[cnt],action[1],action[2],action[3],action[4],action[5],action[6],light)
			for i2=1,162 do
				s[i2]=cnt>1 and s[i2] or {}
				s[i2][cnt]=tr[i2]
			end
		end
	end
	return s
end

function space.build_unfold_cube(len,mode,pct)
	local p={} p[1]={{-1,-1,-1},{1,-1,-1},{1,1,-1},{-1,1,-1}} p[2]={{-1,0,1},{1,0,1},{1,0,-1},{-1,0,-1}}
	p[3]={{0,1,1},{0,-1,1},{0,-1,-1},{0,1,-1}} p[4]={{0,1,1},{0,-1,1},{0,-1,-1},{0,1,-1}}
	p[5]={{-1,0,1},{1,0,1},{1,0,-1},{-1,0,-1}} p[6]={{-1,-1,0},{1,-1,0},{1,1,0},{-1,1,0}}
	local pct=pct>1 and 1 or (pct<0 and 0 or pct)
	if mode==1 then
		if pct<=0.5 then
			local dy,dz=math.sin(math.rad(90*2*pct)),1-math.cos(math.rad(-90*2*pct))
			p[2]=space.translate(space.rotate(p[2],-90*2*pct,"x"),0,dy+1,-dz) p[5]=space.translate(space.rotate(p[5],90*2*pct,"x"),0,-dy-1,-dz)
			p[3]=space.translate(space.rotate(p[3],-90*2*pct,"y"),dy+1,0,-dz) p[4]=space.translate(space.rotate(p[4],90*2*pct,"y"),-dy-1,0,-dz)
			p[6]=space.translate(space.rotate(p[6],-90*2*pct,"x"),0,dy*2+dz,(1-dz)*2+dy-1)
		else p[2]=space.translate(space.rotate(p[2],-90,"x"),0,2,-1) p[5]=space.translate(space.rotate(p[5],90,"x"),0,-2,-1)
			p[3]=space.translate(space.rotate(p[3],-90,"y"),2,0,-1) p[4]=space.translate(space.rotate(p[4],90,"y"),-2,0,-1)
			p[6]=space.translate(space.rotate(p[6],-90*2*pct,"x"),0,3+math.sin(math.rad(90*2*(pct-0.5))),math.cos(math.rad(-90*2*(pct-0.5)))-1)
		end
	elseif mode==2 then
		if pct<=0.5 then
			local dy,dz=math.sin(math.rad(90*2*pct)),1-math.cos(math.rad(-90*2*pct))
			p[2]=space.translate(space.rotate(p[2],-90*2*pct,"x"),0,dy+1,-dz) p[5]=space.translate(space.rotate(p[5],90*2*pct,"x"),0,-dy-1,-dz)
			p[3]=space.translate(space.rotate(p[3],-90*2*pct,"y"),dy+1,0,-dz) p[4]={{-1,2*dy+1,1-2*dz},{-1,2*dy-1+2*dz,1-2*dz+2*dy},{-1,-1+2*dz,-1+2*dy},{-1,1,-1}}
			p[6]={{-1,2*dy-1+2*dz,1-2*dz+2*dy},{1-2*dz,2*dz+(2+2*dy)*dy-1,(2+2*dy)*(1-dz)-1+2*dy},{1-2*dz,1+(2+2*dy)*dy,(2+2*dy)*(1-dz)-1},{-1,2*dy+1,1-2*dz}}
		else p[2]=space.translate(space.rotate(p[2],-90,"x"),0,2,-1) p[5]=space.translate(space.rotate(p[5],90,"x"),0,-2,-1) p[3]=space.translate(space.rotate(p[3],-90,"y"),2,0,-1)
			p[4]={{-1,3,-1},{-2*math.sin(math.rad(90*2*(pct-0.5)))-1,3,2*math.cos(math.rad(90*2*(pct-0.5)))-1},{-2*math.sin(math.rad(90*2*(pct-0.5)))-1,1,2*math.cos(math.rad(90*2*(pct-0.5)))-1},{-1,1,-1}}
			p[6]={{-2*math.sin(math.rad(90*2*(pct-0.5)))-1,3,2*math.cos(math.rad(90*2*(pct-0.5)))-1},{-2*math.sin(math.rad(90*2*(pct-0.5)))-1,5,2*math.cos(math.rad(90*2*(pct-0.5)))-1},{-1,5,-1},{-1,3,-1}}
		end
	elseif mode==3 then
		if pct<=0.5 then
			local dy,dz=math.sin(math.rad(90*2*pct)),1-math.cos(math.rad(-90*2*pct))
			p[2]=space.translate(space.rotate(p[2],-90*2*pct,"x"),0,dy+1,-dz) p[3]=space.translate(space.rotate(p[3],90*2*pct,"z"),dy+1,-dz)
			p[4]=space.translate(space.rotate(p[4],90*2*pct,"y"),-dy-1,0,-dz) p[5]=space.translate(p[5],0,-1)
			p[6]=space.translate(space.rotate(p[6],-90*2*pct,"x"),0,dy*2+dz,(1-dz)*2+dy-1)
		else p[2]=space.translate(space.rotate(p[2],-90,"x"),0,2,-1) p[4]=space.translate(space.rotate(p[4],90,"y"),-2,0,-1)
			local e=pct-0.5 local dy,dz=math.sin(math.rad(90*2*e)),1-math.cos(math.rad(-90*2*e))
			p[3]=space.translate(space.rotate(p[5],90*2*e,"x"),2,-dy-1,-dz) p[5]=space.translate(space.rotate(p[5],90*2*e,"x"),0,-dy-1,-dz)
			p[6]=space.translate(space.rotate(p[6],-90*2*pct,"x"),0,3+math.sin(math.rad(90*2*(pct-0.5))),math.cos(math.rad(-90*2*(pct-0.5)))-1)
		end
	elseif mode==4 then
		if pct<=0.5 then
			local dy,dz=math.sin(math.rad(90*2*pct)),1-math.cos(math.rad(-90*2*pct)) p[6]=space.translate(space.rotate(p[6],90*2*pct,"x"),0,-dy*2-dz,(1-dz)*2+dy-1)
			p[2]=space.translate(space.rotate(p[2],-90*2*pct,"z"),-dz,dy+1,0) p[5]=space.translate(space.rotate(p[5],90*2*pct,"x"),0,-dy-1,-dz) p[4]=space.translate(p[4],-1,0,0)
			p[3]={{1-2*dz,1+2*dy,1},{1-2*dz+2*math.sin(math.rad(90*4*pct)),1+2*dy-2*math.cos(math.rad(90*4*pct)),1},{1-2*dz+2*math.sin(math.rad(90*4*pct)),1+2*dy-2*math.cos(math.rad(90*4*pct)),-1},{1-2*dz,1+2*dy,-1}}
		else local e=pct-0.5 local dy,dz=math.sin(math.rad(90*2*e)),1-math.cos(math.rad(-90*2*e)) p[5]=space.translate(space.rotate(p[5],90,"x"),0,-2,-1)
			p[2]={{-1-2*dy,1,1-2*dz},{-1-2*dy,3,1-2*dz},{-1,3,-1},{-1,1,-1}} p[4]=space.translate(space.rotate(p[4],90*2*e,"y"),-dy-1,0,-dz) p[3]={{-1-2*dy,3,1-2*dz},{-1-2*dy,5,1-2*dz},{-1,5,-1},{-1,3,-1}}
			p[6]=space.translate(space.rotate(p[6],90*2*pct,"x"),0,-3-math.sin(math.rad(90*2*(pct-0.5))),math.cos(math.rad(-90*2*(pct-0.5)))-1)
		end
	elseif mode==5 then
		if pct<=0.5 then
			local dy,dz=math.sin(math.rad(90*2*pct)),1-math.cos(math.rad(-90*2*pct)) p[2]=space.translate(p[2],0,1)
			p[3]=space.translate(space.rotate(p[3],-90*2*pct,"z"),dy+1,dz,0) p[5]=space.translate(space.rotate(p[5],90*2*pct,"x"),0,-dy-1,-dz)
			p[6]=space.translate(space.rotate(p[6],90*2*pct,"x"),0,-dy*2-dz,(1-dz)*2+dy-1) p[4]=space.translate(space.rotate(p[4],90*2*pct,"z"),-dy-1,dz,0)
		else local e=pct-0.5 local dy,dz=math.sin(math.rad(90*2*e)),1-math.cos(math.rad(-90*2*e))
			p[2]=space.translate(space.rotate(p[2],-90*2*e,"x"),0,dy+1,-dz) p[5]=space.translate(space.rotate(p[5],90,"x"),0,-2,-1)
			p[3]={{1,1+2*dy,1-2*dz},{3,1+2*dy,1-2*dz},{3,1,-1},{1,1,-1}} p[4]={{-1,1+2*dy,1-2*dz},{-3,1+2*dy,1-2*dz},{-3,1,-1},{-1,1,-1}}
			p[6]=space.translate(space.rotate(p[6],90*2*pct,"x"),0,-3-math.sin(math.rad(90*2*(pct-0.5))),math.cos(math.rad(-90*2*(pct-0.5)))-1)
		end
	elseif mode==6 then
		if pct<=0.5 then
			local dy,dz=math.sin(math.rad(90*2*pct)),1-math.cos(math.rad(-90*2*pct))
			p[2]=space.translate(space.rotate(p[2],-90*2*pct,"z"),-dz,dy+1,0) p[5]=space.translate(space.rotate(p[5],90*2*pct,"x"),0,-dy-1,-dz)
			p[3]=space.translate(space.rotate(p[3],-90*2*pct,"y"),dy+1,0,-dz) p[4]=space.translate(p[4],-1,0,0)
			p[6]=space.translate(space.rotate(p[6],90*2*pct,"x"),0,-dy*2-dz,(1-dz)*2+dy-1)
		else local e=pct-0.5 local dy,dz=math.sin(math.rad(90*2*e)),1-math.cos(math.rad(-90*2*e))
			p[2]={{-1-2*dy,1,1-2*dz},{-1-2*dy,3,1-2*dz},{-1,3,-1},{-1,1,-1}} p[5]=space.translate(space.rotate(p[5],90,"x"),0,-2,-1)
			p[3]=space.translate(space.rotate(p[3],-90,"y"),2,0,-1) p[4]=space.translate(space.rotate(p[4],90*2*e,"y"),-dy-1,0,-dz)
			p[6]=space.translate(space.rotate(p[6],90*2*pct,"x"),0,-3-math.sin(math.rad(90*2*(pct-0.5))),math.cos(math.rad(-90*2*(pct-0.5)))-1)
		end
	elseif mode==7 then
		if pct<=0.5 then
			local dy,dz=math.sin(math.rad(90*2*pct)),1-math.cos(math.rad(-90*2*pct)) p[4]=space.translate(p[4],-1)
			p[2]=space.translate(space.rotate(p[2],-90*2*pct,"x"),0,dy+1,-dz) p[3]=space.translate(space.rotate(p[3],90*2*pct,"z"),dy+1,-dz)
			p[5]=space.translate(p[5],0,-1) p[6]=space.translate(space.rotate(p[6],90*2*pct,"x"),0,-dz,dy+1)
		else local e=pct-0.5 local dy,dz=math.sin(math.rad(90*2*e)),1-math.cos(math.rad(-90*2*e))
			p[2]=space.translate(space.rotate(p[2],-90,"x"),0,2,-1) p[3]=space.translate(space.rotate(p[5],90*2*e,"x"),2,-dy-1,-dz)
			p[5]=space.translate(space.rotate(p[5],90*2*e,"x"),0,-dy-1,-dz) p[4]=space.translate(space.rotate(p[4],90*2*e,"y"),-dy-1,0,-dz)
			p[6]={{-1,-1-2*dy,1-2*dz},{1,-1-2*dy,1-2*dz},{1,-1-4*dy,3-4*dz},{-1,-1-4*dy,3-4*dz}}
		end
	elseif mode==8 then
		if pct<=0.5 then
			local dy,dz=math.sin(math.rad(90*2*pct)),1-math.cos(math.rad(-90*2*pct))
			p[2]=space.translate(space.rotate(p[2],-90*2*pct,"x"),0,dy+1,-dz) p[3]=space.translate(space.rotate(p[3],90*2*pct,"z"),dy+1,-dz)
			p[5]=space.translate(p[5],0,-1) p[4]={{-1,2*dy+1,1-2*dz},{-1,2*dy-1+2*dz,1-2*dz+2*dy},{-1,-1+2*dz,-1+2*dy},{-1,1,-1}}
			p[6]={{-1,2*dy-1+2*dz,1-2*dz+2*dy},{1-2*dz,2*dz+(2+2*dy)*dy-1,(2+2*dy)*(1-dz)-1+2*dy},{1-2*dz,1+(2+2*dy)*dy,(2+2*dy)*(1-dz)-1},{-1,2*dy+1,1-2*dz}}
		else local e=pct-0.5 local dy,dz=math.sin(math.rad(90*2*e)),1-math.cos(math.rad(-90*2*e))
			p[2]=space.translate(space.rotate(p[2],-90,"x"),0,2,-1) p[3]=space.translate(space.rotate(p[5],90*2*e,"x"),2,-dy-1,-dz) p[5]=space.translate(space.rotate(p[5],90*2*e,"x"),0,-dy-1,-dz)
			p[4]={{-1,3,-1},{-2*math.sin(math.rad(90*2*(pct-0.5)))-1,3,2*math.cos(math.rad(90*2*(pct-0.5)))-1},{-2*math.sin(math.rad(90*2*(pct-0.5)))-1,1,2*math.cos(math.rad(90*2*(pct-0.5)))-1},{-1,1,-1}}
			p[6]={{-2*math.sin(math.rad(90*2*(pct-0.5)))-1,3,2*math.cos(math.rad(90*2*(pct-0.5)))-1},{-2*math.sin(math.rad(90*2*(pct-0.5)))-1,5,2*math.cos(math.rad(90*2*(pct-0.5)))-1},{-1,5,-1},{-1,3,-1}}
		end
	elseif mode==9 then
		if pct<=0.5 then
			local dy,dz=math.sin(math.rad(90*2*pct)),1-math.cos(math.rad(-90*2*pct))
			p[2]=space.translate(p[2],0,1) p[4]=space.translate(space.rotate(p[4],90*2*pct,"z"),-dy-1,dz,0) p[5]=space.translate(space.rotate(p[5],90*2*pct,"x"),0,-dy-1,-dz)
			p[3]={{1,2*math.cos(math.pi*2*pct)-1-2*dy,1+2*math.sin(math.pi*2*pct)-2*dz},{1,-2*dy-1,1-2*dz},{1,2*math.sin(math.pi*2*pct)-1-2*dy,1-2*math.cos(math.pi*2*pct)-2*dz},{1,2*(math.cos(math.pi*2*pct)+math.sin(math.pi*2*pct))-1-2*dy,1-2*dz+2*(math.sin(math.pi*2*pct)-math.cos(math.pi*2*pct))}}
			p[6]={{-1,-2*dy-1,1-2*dz},{1,-2*dy-1,1-2*dz},{1,2*math.cos(math.pi*2*pct)-1-2*dy,1+2*math.sin(math.pi*2*pct)-2*dz},{-1,2*math.cos(math.pi*2*pct)-1-2*dy,1+2*math.sin(math.pi*2*pct)-2*dz}}
		else local e=pct-0.5 local dy,dz=math.sin(math.rad(90*2*e)),1-math.cos(math.rad(-90*2*e))
			p[2]=space.translate(space.rotate(p[2],-90*2*e,"x"),0,dy+1,-dz) p[5]=space.translate(space.rotate(p[5],90,"x"),0,-2,-1)
			p[3]={{1,-5,-1},{1,-3,-1},{1+2*dy,-3,1-2*dz},{1+2*dy,-5,1-2*dz}} p[4]={{-1,1+2*dy,1-2*dz},{-3,1+2*dy,1-2*dz},{-3,1,-1},{-1,1,-1}}
			p[6]=space.translate(space.rotate(p[6],180,"x"),0,-4,-1)
		end
	elseif mode==10 then
		if pct<=0.5 then
			local dy,dz=math.sin(math.rad(90*2*pct)),1-math.cos(math.rad(-90*2*pct))
			p[2]=space.translate(space.rotate(p[2],-90*2*pct,"x"),0,dy+1,-dz) p[5]=space.translate(space.rotate(p[5],-90*2*pct,"z"),dz,-dy-1)
			p[4]={{-1,2*dy+1,1-2*dz},{-1,2*dy-1+2*dz,1-2*dz+2*dy},{-1,-1+2*dz,-1+2*dy},{-1,1,-1}} p[3]=space.translate(p[3],1)
			p[6]={{-1,2*dy-1+2*dz,1-2*dz+2*dy},{1-2*dz,2*dz+(2+2*dy)*dy-1,(2+2*dy)*(1-dz)-1+2*dy},{1-2*dz,1+(2+2*dy)*dy,(2+2*dy)*(1-dz)-1},{-1,2*dy+1,1-2*dz}}
		else local e=pct-0.5 local dy,dz=math.sin(math.rad(90*2*e)),1-math.cos(math.rad(-90*2*e))
			p[2]=space.translate(space.rotate(p[2],-90,"x"),0,2,-1) p[3]=space.translate(space.rotate(p[3],-90*2*e,"y"),dy+1,0,-dz) p[5]={{1+2*dy,-3,1-2*dz},{1+2*dy,-1,1-2*dz},{1,-1,-1},{1,-3,-1}}
			p[4]={{-1,3,-1},{-2*math.sin(math.rad(90*2*(pct-0.5)))-1,3,2*math.cos(math.rad(90*2*(pct-0.5)))-1},{-2*math.sin(math.rad(90*2*(pct-0.5)))-1,1,2*math.cos(math.rad(90*2*(pct-0.5)))-1},{-1,1,-1}}
			p[6]={{-2*math.sin(math.rad(90*2*(pct-0.5)))-1,3,2*math.cos(math.rad(90*2*(pct-0.5)))-1},{-2*math.sin(math.rad(90*2*(pct-0.5)))-1,5,2*math.cos(math.rad(90*2*(pct-0.5)))-1},{-1,5,-1},{-1,3,-1}}
		end
	elseif mode==11 then
		if pct<=0.5 then
			local dy,dz=math.sin(math.rad(90*2*pct)),1-math.cos(math.rad(-90*2*pct)) p[6]={{-1,-1,1},{1,-1,1},{1,1-2*dz,1+2*dy},{-1,1-2*dz,1+2*dy}}
			p[2]=space.translate(space.rotate(p[2],-90*2*pct,"x"),0,dy+1,-dz) p[3]=space.translate(space.rotate(p[3],90*2*pct,"z"),dy+1,-dz)
			p[5]=space.translate(p[5],0,-1) p[4]={{-1,2*dy+1,1-2*dz},{-1,2*dy-1+2*dz,1-2*dz+2*dy},{-1,-1+2*dz,-1+2*dy},{-1,1,-1}}
		else local e=pct-0.5 local dy,dz=math.sin(math.rad(90*2*e)),1-math.cos(math.rad(-90*2*e)) p[6]={{-1,-1-2*dy,1-2*dz},{1,-1-2*dy,1-2*dz},{1,-1-4*dy,3-4*dz},{-1,-1-4*dy,3-4*dz}}
			p[2]=space.translate(space.rotate(p[2],-90,"x"),0,2,-1) p[3]=space.translate(space.rotate(p[5],90*2*e,"x"),2,-dy-1,-dz) p[5]=space.translate(space.rotate(p[5],90*2*e,"x"),0,-dy-1,-dz)
			p[4]={{-1,3,-1},{-2*math.sin(math.rad(90*2*(pct-0.5)))-1,3,2*math.cos(math.rad(90*2*(pct-0.5)))-1},{-2*math.sin(math.rad(90*2*(pct-0.5)))-1,1,2*math.cos(math.rad(90*2*(pct-0.5)))-1},{-1,1,-1}}
		end
	end
	for i=1,6 do
		p[i]=space.scale(p[i],len/2)
	end
	return p
end

function space.unfold_cube(len,mode,pct,angle,axis,fsc,x,y,z,color,light)
	local p=space.build_unfold_cube(len,mode,pct) local cp={0,0,0}
	return space.info(p,cp,angle,axis,fsc,x,y,z,color,light),p
end

function space.unfold_cube_move(len,transform_tbl,s_time,e_time,interval,s_ang,e_ang,axis,fsc,x,y,z,color,light)
	local n=math.ceil((e_time-s_time)/interval/#transform_tbl/2) local loop=n*#transform_tbl*2 local s={{},{},{},{},{},{},n={{6,loop}}}
	local color=color or {} local cp={0,0,0} local cnt=0
	if #color<6 then
		for i=1,6-#color do
			color[#color+1]=color[1] and color[math.random(#color)] or "&HFFFFFF&"
		end
	end
	for i=1,#transform_tbl*2 do
		if i%2==1 then
			for ii=1,n do
				local per=ii==1 and 0 or ii/n local ang={} local pct=i==1 and transform_tbl[math.ceil(i/2)][2]+(1-transform_tbl[math.ceil(i/2)][2])*per or per
				cnt=cnt+1 local e=cnt==1 and 0 or cnt/(#transform_tbl*2*n)
				local p=space.build_unfold_cube(len,transform_tbl[math.ceil(i/2)][1],pct)
				for a=1,#s_ang do
					ang[a]=s_ang[a]+(e_ang[a]-s_ang[a])*e
				end
				for pl=1,6 do
					local info=space.plane_info(p[pl],cp,ang,axis,1+(fsc-1)*e,1+(x-1)*e,1+(y-1)*e,1+(z-1)*e,color[pl],light)
					s[pl][#s[pl]+1]={s=info.s,lay=info.lay,c=info.c}
				end
			end
		else for ii=1,n do
				local per=ii==1 and 0 or ii/n local ang={} local pct=i==#transform_tbl*2 and 1+(transform_tbl[math.ceil(i/2)][2]-1)*per or 1-per
				cnt=cnt+1 local e=cnt/(#transform_tbl*2*n)
				local p=space.build_unfold_cube(len,transform_tbl[math.ceil(i/2)][1],pct)
				for a=1,#s_ang do
					ang[a]=s_ang[a]+(e_ang[a]-s_ang[a])*e
				end
				for pl=1,6 do
					local info=space.plane_info(p[pl],cp,ang,axis,1+(fsc-1)*e,1+(x-1)*e,1+(y-1)*e,1+(z-1)*e,color[pl],light)
					s[pl][#s[pl]+1]={s=info.s,lay=info.lay,c=info.c}
				end
			end
		end
	end
	return s
end

function space.unfold_cube_freemove(len,transform_tbl,s_time,e_time,interval,roll_fsc_pos,color,light)
	local n=math.ceil((e_time-s_time)/interval/#transform_tbl/2) local loop=n*#transform_tbl*2 local s={{},{},{},{},{},{},n={{6,loop}}}
	local color=color or {} local cp={0,0,0} local cnt=0
	if #color<6 then
		for i=1,6-#color do
			color[#color+1]=color[1] and color[math.random(#color)] or "&HFFFFFF&"
		end
	end
	for i=1,#transform_tbl*2 do
		if i%2==1 then
			for ii=1,n do
				local per=ii==1 and 0 or ii/n local pct=i==1 and transform_tbl[math.ceil(i/2)][2]+(1-transform_tbl[math.ceil(i/2)][2])*per or per
				cnt=cnt+1 local action=roll_fsc_pos and roll_fsc_pos(cnt) or {{-10,30},{"x","y"},1,0,0,0}
				local p=space.build_unfold_cube(len,transform_tbl[math.ceil(i/2)][1],pct)
				for pl=1,6 do
					local info=space.plane_info(p[pl],cp,action[1],action[2],action[3],action[4],action[5],action[6],color[pl],light)
					s[pl][#s[pl]+1]={s=info.s,lay=info.lay,c=info.c}
				end
			end
		else for ii=1,n do
				local per=ii==1 and 0 or ii/n local pct=i==#transform_tbl*2 and 1+(transform_tbl[math.ceil(i/2)][2]-1)*per or 1-per
				cnt=cnt+1 local action=roll_fsc_pos and roll_fsc_pos(cnt) or {{-10,30},{"x","y"},1,0,0,0}
				local p=space.build_unfold_cube(len,transform_tbl[math.ceil(i/2)][1],pct)
				for pl=1,6 do
					local info=space.plane_info(p[pl],cp,action[1],action[2],action[3],action[4],action[5],action[6],color[pl],light)
					s[pl][#s[pl]+1]={s=info.s,lay=info.lay,c=info.c}
				end
			end
		end
	end
	return s
end




_G.space = space
return _G.space