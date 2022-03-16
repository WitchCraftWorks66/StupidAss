
local polyb={}

local function math_round(n,digit)
	if digit and digit>=1 then
		digit=10^math.floor(digit)
		return math.floor(n*digit+0.5)/digit
		else return math.floor(n+0.5)
	end
end


local ffi=require "ffi"
local C=ffi.load "polyc"

ffi.cdef[[
typedef struct __CPoint { int32_t x; int32_t y; } CPoint;
typedef struct __CRect { int32_t left, top, right, bottom; } CRect;
typedef struct __CPath CPath;
typedef struct __CPaths CPaths;
typedef struct __CClipper CClipper;
typedef struct __CClipperOffset CClipperOffset;
typedef struct __CClipperTri CClipperTri;

CPoint* CPointNew(int32_t x, int32_t y);
void CPointDelete(CPoint* self);
int CPointInPolygon(const CPoint* self, const CPath* path);

CPath* CPathNew();
void CPathDelete(CPath* self);
CPoint* CPathGet(CPath* self, int i);
void CPathAdd(CPath* self, CPoint* pt);
int CPathSize(CPath* self);
double CPathArea(const CPath* self);
bool CPathOrientation(const CPath* self);
void CPathReverse(CPath* self);
CPaths* CPathSimplify(CPath* self, int fillRule);

CPaths* CPathsNew();
void CPathsDelete(CPaths* self);
CPath* CPathsGet(CPaths* self, int i);
void CPathsAdd(CPaths* self, CPath* path);
int CPathsSize(CPaths* self);
void CPathsReverse(CPaths* self);
CPaths* CPathsSimplify(CPaths* self, int fillRule);
CPaths* CPathsOffset(CPaths* self, double delta, int joinType, int endType, double miterLimit, double arcTolerance);
CPaths* CPathsTriangulate(CPaths* self, int fillRule);

CClipper* CClipperNew();
void CClipperDelete(CClipper* self);
void CClipperAddPath(CClipper* self, CPath* path, int type, bool open);
void CClipperAddPaths(CClipper* self, CPaths* paths, int type, bool open);
CPaths* CClipperExecute(CClipper* self, int clipType, int fillRule);

CClipperOffset* COffsetNew(double miterLimit, double arcTolerance);
void COffsetDelete(CClipperOffset* self);
void COffsetAddPath(CClipperOffset* self, CPath* path, int joinType, int endType);
void COffsetAddPaths(CClipperOffset* self, CPaths* path, int joinType, int endType);
CPaths* COffsetExecute(CClipperOffset* self, double delta);

CClipperTri* CTriangulateNew();
void CTriangulateDelete(CClipperTri* self);
void CTriangulateAddPath(CClipperTri* self, CPath* path);
void CTriangulateAddPaths(CClipperTri* self, CPaths* paths);
CPaths* CTriangulateExecute(CClipperTri* self, int fillRule);
]]

_G.ClipType={None=0, Intersection=1, Union=2, Difference=3, Xor=4}
_G.FillRule={EvenOdd=0, NonZero=1, Positive=2, Negative=3}
_G.PathType={Subject=0, Clip=1}
_G.JoinType={Square=0, Round=1, Mitter=2}
_G.EndType={Polygon=0, OpenJoined=1, OpenButt=2, OpenSquare=3, OpenRound=4}

local Point={}

function Point.New(x, y)
	return ffi.gc(C.CPointNew(x, y), C.CPointDelete)
end

function Point:InPolygon(path)
	return C.CPointInPolygon(self, path)
end

local Path={}

function Path.New()
	return ffi.gc(C.CPathNew(), C.CPathDelete)
end

function Path:Get(i)
	return C.CPathGet(self,i-1)
end

function Path:Add(pt)
	return C.CPathAdd(self,pt)
end

function Path:Size()
	return C.CPathSize(self)
end

function Path:Area()
	return C.CPathArea(self)
end

function Path:Orientation()
	return C.CPathOrientation(self)
end

function Path:Reverse()
	return C.CPathReverse(self)
end

function Path:Simplify(fillRule)
	local fillRule=fillRule or 0
	return C.CPathSimplify(self, fillRule)
end

local Paths={}

function Paths.New()
	return ffi.gc(C.CPathsNew(),C.CPathsDelete)
end

function Paths:Get(i)
	return C.CPathsGet(self,i-1)
end

function Paths:Add(path)
	return C.CPathsAdd(self,path)
end

function Paths:Size()
	return C.CPathsSize(self)
end

function Paths:Reverse()
	return C.CPathsReverse(self)
end

function Paths:Simplify(fillRule)
	local fillRule=fillRule or 0
	return C.CPathsSimplify(self, fillRule)
end

function Paths:Offset(delta,joinType,endType,miterLimit,arcTolerance)
	local joinType=joinType or 1
	local endType=endType or 0
	local miterLimit=miterLimit or 2.0
	local arcTolerance=arcTolerance or 0.0
	return C.CPathsOffset(self,delta,joinType,endType,miterLimit,arcTolerance)
end

function Paths:Triangulate(fillRule)
	local fillRule=fillRule or 0
	return C.CPathsTriangulate(self,fillRule)
end

local Clipper={}

function Clipper.New()
	return ffi.gc(C.CClipperNew(), C.CClipperDelete)
end

function Clipper:AddPath(path, pathType, open)
	local open=open or false
	C.CClipperAddPath(self, path, pathType, open)
end

function Clipper:AddPaths(paths, pathType, open)
	local open=open or false
	C.CClipperAddPaths(self, paths, pathType, open)
end

function Clipper:Execute(clipType, fillRule)
	local clipType=clipType or 1
	local fillRule=fillRule or 0
	return C.CClipperExecute(self, clipType, fillRule)
end

local ClipperOffset={}

function ClipperOffset.New(miterLimit, arcTolerance)
	local miterLimit=miterLimit or 2.0
	local arcTolerance=arcTolerance or 0.0
	return ffi.gc(C.COffsetNew(miterLimit, arcTolerance), C.COffsetDelete)
end

function ClipperOffset:AddPath(path, joinType, endType)
	local joinType=joinType or 1
	local endType=endType or 0
	C.COffsetAddPath(self, path, joinType, endType)
end

function ClipperOffset:AddPaths(paths, joinType, endType)
	local joinType=joinType or 1
	local endType=endType or 0
	C.COffsetAddPaths(self, paths, joinType, endType)
end

function ClipperOffset:Execute(delta)
	return C.COffsetExecute(self, delta)
end

local ClipperTri={}

function ClipperTri.New()
	return ffi.gc(C.CTriangulateNew(), C.CTriangulateDelete)
end

function ClipperTri:AddPath(path)
	C.CTriangulateAddPath(self, path)
end

function ClipperTri:AddPaths(paths)
	C.CTriangulateAddPath(self, paths)
end

function ClipperTri:Execute(fillRule)
	local fillRule=fillRule or 0
	return C.CTriangulateExecute(self, fillRule)
end

ffi.metatype("CPoint", {__index=Point})
ffi.metatype("CPath", {__index=Path})
ffi.metatype("CPaths", {__index=Paths})
ffi.metatype("CClipper", {__index=Clipper})
ffi.metatype("CClipperOffset", {__index=ClipperOffset})
ffi.metatype("CClipperTri", {__index=ClipperTri})

local Clib={
	Point=Point.New,
	Path=Path.New,
	Paths=Paths.New,
	Clipper=Clipper.New,
	ClipperOffset=ClipperOffset.New,
	ClipperTri=ClipperTri.New
}

function polyb.simplify(ass_shape,accur,mode)
	local paths=Clib.Paths() local accur=accur or 2 local mode=mode or 1
	for m in ass_shape:gmatch('m[^m]+') do
		local s=Clib.Path()
		for x,y in m:gmatch('([-.%d]+) ([-.%d]+)') do
			s:Add(Clib.Point(x*10^accur,y*10^accur))
		end
		paths:Add(s)
	end
	local obj=paths:Simplify(mode) local new={}
	for i=1,obj:Size() do
		local m=obj:Get(i) local s={}
		for i2=1,m:Size() do
			local pt=m:Get(i2)
			s[i2]=math_round(pt.x/10^accur,accur)..' '..math_round(pt.y/10^accur,accur)..' '
		end
		new[i]='m '..table.concat(s,'l ')
	end
	return table.concat(new)
end

function polyb.get_poly_path(ass_shape,accur)
	local path=Clib.Path() local accur=accur or 2
	for x,y in ass_shape:gmatch('([-.%d]+) ([-.%d]+)') do
		path:Add(Clib.Point(x*10^accur,y*10^accur))
	end
	return path
end

local function bezier_flat(pt,x1,y1,x2,y2,x3,y3,x4,y4)
	local function check_deg(px1,py1,px2,py2,px3,py3)
		local v1={px2-px1,py2-py1} local v2={px2-px3,py2-py3} local len1,len2=(v1[1]^2+v1[2]^2)^0.5,(v2[1]^2+v2[2]^2)^0.5
		if len1==0 or len2==0 then
			return true
		else return math.abs((v1[1]*v2[1]+v2[2]*v1[2])/(len1*len2))>0.9995
		end
	end
	local function flat(x1,y1,x2,y2,x3,y3,x4,y4)
		if check_deg(x1,y1,x2,y2,x3,y3) and check_deg(x2,y2,x3,y3,x4,y4) then
			pt:Add(Clib.Point(x4,y4))
		else
			local f_x2,f_y2,f_x3,f_y3,x,y,s_x2,s_y2,s_x3,s_y3=Xshape.split_bezier_into_two(x1,y1,x2,y2,x3,y3,x4,y4)
			flat(x1,y1,f_x2,f_y2,f_x3,f_y3,x,y)
			flat(x,y,s_x2,s_y2,s_x3,s_y3,x4,y4)
		end
	end
	flat(x1,y1,x2,y2,x3,y3,x4,y4)
end

function polyb.get_paths(ass_shape,accur)
	local paths,accur=Clib.Paths(),accur or 2 local s
	local xmin,ymin,xmax,ymax=99999999999,99999999999,-99999999999,-99999999999
	for cmd,p in ass_shape:gmatch('([mlb])([-. %d]+)') do
		if cmd=='m' then
			if s then paths:Add(s) end s=Clib.Path()
			for x,y in p:gmatch('([-.%d]+) ([-.%d]+)') do
				s:Add(Clib.Point(x*10^accur,y*10^accur))
				xmin,ymin,xmax,ymax=math.min(xmin,x),math.min(ymin,y),math.max(xmax,x),math.max(ymax,y)
			end
		elseif cmd=='l' then
			for x,y in p:gmatch('([-.%d]+) ([-.%d]+)') do
				s:Add(Clib.Point(x*10^accur,y*10^accur))
				xmin,ymin,xmax,ymax=math.min(xmin,x),math.min(ymin,y),math.max(xmax,x),math.max(ymax,y)
			end
		else
			local pt=s:Get(s:Size()) local x1,y1,cnt=pt.x,pt.y,0
			for x2,y2,x3,y3,x4,y4 in p:gmatch('([-.%d]+) ([-.%d]+) ([-.%d]+) ([-.%d]+) ([-.%d]+) ([-.%d]+)') do
				xmin,ymin,xmax,ymax=math.min(xmin,x2,x3,x4),math.min(ymin,y2,y3,y4),math.max(xmax,x2,x3,x4),math.max(ymax,y2,y3,y4)
				x4,y4=x4*10^accur,y4*10^accur cnt=1
				bezier_flat(s,x1,y1,x2*10^accur,y2*10^accur,x3*10^accur,y3*10^accur,x4,y4)
				if cnt==1 then x1,y1=x4,y4 end
			end
		end
	end
	paths:Add(s)
	return paths,xmin,ymin,xmax,ymax,xmax-xmin,ymax-ymin,(xmin+xmax)/2,(ymin+ymax)/2
end

function polyb.get_poly_paths(ass_shape,accur)
	local paths=Clib.Paths() local accur=accur or 2
	for m in ass_shape:gmatch('m[^m]+') do
		local s=Clib.Path()
		for x,y in m:gmatch('([-.%d]+) ([-.%d]+)') do
			s:Add(Clib.Point(x*10^accur,y*10^accur))
		end
		paths:Add(s)
	end
	return paths
end

function polyb.get_poly_paths_and_bounding(ass_shape,accur)
	local paths,x1,y1,x2,y2,accur=Clib.Paths(),99999999999,99999999999,-99999999999,-99999999999,accur or 2
	for m in ass_shape:gmatch('m[^m]+') do
		local s=Clib.Path()
		for x,y in m:gmatch('([-.%d]+) ([-.%d]+)') do
			s:Add(Clib.Point(x*10^accur,y*10^accur))
			x1,y1=math.min(x1,x),math.min(y1,y) x2,y2=math.max(x2,x),math.max(y2,y)
		end
		paths:Add(s)
	end
	return paths,x1,y1,x2,y2,x2-x1,y2-y1,(x1+x2)/2,(y1+y2)/2
end

function polyb.paths_to_shape(paths,accur)
	local s={} local accur=accur or 2
	for i=1,paths:Size() do
		local m=paths:Get(i) local p={}
		for i2=1,m:Size() do
			local pt=m:Get(i2)
			p[i2]=math_round(pt.x/10^accur,accur)..' '..math_round(pt.y/10^accur,accur)..' '
		end
		s[i]='m '..table.concat(p,'l ')
	end
	return table.concat(s)
end

function polyb.paths_to_shape_align(paths,accur)
	local s,accur,x1,y1,x2,y2={},accur or 2,99999999999,99999999999,-99999999999,-99999999999
	for i=1,paths:Size() do
		local m=paths:Get(i) local p={}
		for i2=1,m:Size() do
			local pt=m:Get(i2) local x,y=math_round(pt.x/10^accur,accur),math_round(pt.y/10^accur,accur)..' '
			p[i2]={x,y} x1,y1=math.min(x1,x),math.min(y1,y) x2,y2=math.max(x2,x),math.max(y2,y)
		end
		s[i]=p
	end
	local cx,cy=(x1+x2)/2,(y1+y2)/2
	for i=1,#s do
		for i2=1,#s[i] do
			s[i][i2]=math_round(s[i][i2][1]-cx,2)..' '..math_round(s[i][i2][2]-cy,2)..' '
		end
		s[i]='m '..table.concat(s[i],'l ')
	end
	return table.concat(s),math_round(cx,1),math_round(cy,1)
end



function polyb.shape_mesh_shape(ass_shape1,ass_shape2)--绘图1是否包含绘图2,绘图2是否包含绘图1
	local x1,y1=ass_shape1:match('m ([-.%d]+) ([-.%d]+) *$') local x2,y2=ass_shape2:match('m ([-.%d]+) ([-.%d]+) *$')
	if y2 then
		return Xshape.pt_in_shape(ass_shape1,x2,y2),false
	elseif y1 then
		return false,Xshape.pt_in_shape(ass_shape2,x1,y1)
	end
	local s1=polyb.simplify(ass_shape1) local s2=polyb.simplify(ass_shape2) local AND=polyb.intersection(ass_shape1,ass_shape2)
	return AND==s2,AND==s1
end



function polyb.intersection(ass_shape1,ass_shape2,accur,mode)
	local paths1=polyb.get_paths(ass_shape1,accur) local paths2=polyb.get_paths(ass_shape2,accur) local c=Clib.Clipper()
	local mode=mode or 1 c:AddPaths(paths1,0,false) c:AddPaths(paths2,1,false)
	local solution=c:Execute(1,mode)
	return polyb.paths_to_shape(solution,accur)
end

function polyb.union(ass_shape1,ass_shape2,accur,mode)
	local paths1=polyb.get_paths(ass_shape1,accur) local paths2=polyb.get_paths(ass_shape2,accur) local c=Clib.Clipper()
	local mode=mode or 1 c:AddPaths(paths1,0,false) c:AddPaths(paths2,1,false)
	local solution=c:Execute(2,mode)
	return polyb.paths_to_shape(solution,accur)
end

function polyb.difference(ass_shape1,ass_shape2,accur,mode)
	local paths1=polyb.get_paths(ass_shape1,accur) local paths2=polyb.get_paths(ass_shape2,accur) local c=Clib.Clipper()
	local mode=mode or 1 c:AddPaths(paths1,0,false) c:AddPaths(paths2,1,false)
	local solution=c:Execute(3,mode)
	return polyb.paths_to_shape(solution,accur)
end

function polyb.xor(ass_shape1,ass_shape2,accur,mode)
	local paths1=polyb.get_paths(ass_shape1,accur) local paths2=polyb.get_paths(ass_shape2,accur) local c=Clib.Clipper()
	local mode=mode or 1 c:AddPaths(paths1,0,false) c:AddPaths(paths2,1,false)
	local solution=c:Execute(4,mode)
	return polyb.paths_to_shape(solution,accur)
end

function polyb.intersections(ass_shapes,accur,mode)
	local mode=mode or 1 local solution
	for i=1,#ass_shapes-1 do
		local paths1=solution or polyb.get_paths(ass_shapes[i],accur) local paths2=polyb.get_paths(ass_shapes[i+1],accur)
		local c=Clib.Clipper() c:AddPaths(paths1,0,false) c:AddPaths(paths2,1,false) solution=c:Execute(1,mode)
	end
	return polyb.paths_to_shape(solution,accur)
end

function polyb.unions(ass_shapes,accur,mode)
	local mode=mode or 1 local solution
	for i=1,#ass_shapes-1 do
		local paths1=solution or polyb.get_paths(ass_shapes[i],accur) local paths2=polyb.get_paths(ass_shapes[i+1],accur)
		local c=Clib.Clipper() c:AddPaths(paths1,0,false) c:AddPaths(paths2,1,false) solution=c:Execute(2,mode)
	end
	return polyb.paths_to_shape(solution,accur)
end

function polyb.differences(ass_shapes,accur,mode)
	local mode=mode or 1 local solution
	for i=1,#ass_shapes-1 do
		local paths1=solution or polyb.get_paths(ass_shapes[i],accur) local paths2=polyb.get_paths(ass_shapes[i+1],accur)
		local c=Clib.Clipper() c:AddPaths(paths1,0,false) c:AddPaths(paths2,1,false) solution=c:Execute(3,mode)
	end
	return polyb.paths_to_shape(solution,accur)
end

function polyb.xors(ass_shapes,accur,mode)
	local mode=mode or 1 local solution
	for i=1,#ass_shapes-1 do
		local paths1=solution or polyb.get_paths(ass_shapes[i],accur) local paths2=polyb.get_paths(ass_shapes[i+1],accur)
		local c=Clib.Clipper() c:AddPaths(paths1,0,false) c:AddPaths(paths2,1,false) solution=c:Execute(4,mode)
	end
	return polyb.paths_to_shape(solution,accur)
end

function polyb.paths_intersection(paths1,paths2,mode)
	local c=Clib.Clipper() c:AddPaths(paths1,0,false) c:AddPaths(paths2,1,false)
	local mode=mode or 1 local solution=c:Execute(1,mode)
	return solution
end

function polyb.paths_union(paths1,paths2,mode)
	local c=Clib.Clipper() c:AddPaths(paths1,0,false) c:AddPaths(paths2,1,false)
	local mode=mode or 1 local solution=c:Execute(2,mode)
	return solution
end

function polyb.paths_difference(paths1,paths2,mode)
	local c=Clib.Clipper() c:AddPaths(paths1,0,false) c:AddPaths(paths2,1,false)
	local mode=mode or 1 local solution=c:Execute(3,mode)
	return solution
end

function polyb.paths_xor(paths1,paths2,mode)
	local c=Clib.Clipper() c:AddPaths(paths1,0,false) c:AddPaths(paths2,1,false)
	local mode=mode or 1 local solution=c:Execute(4,mode)
	return solution
end

function polyb.offset(ass_shape,d,jointype,endtype,accur,miter_limit,arc_tolerance)
	local accur=accur or 2 local paths1=polyb.get_paths(ass_shape,accur) local jointype=jointype or 1 local endtype=endtype or 4
	local miter_limit=miter_limit or 2 local arc_tolerance=arc_tolerance or 0 local paths2=paths1:Offset(d*10^accur,jointype,endtype,miter_limit,arc_tolerance)
	return polyb.paths_to_shape(paths2,accur)
end

function polyb.tri(ass_shape,accur,mode)
	local paths1=polyb.get_paths(ass_shape,accur) local mode=mode or 1 local paths2=paths1:Triangulate(mode)
	return polyb.paths_to_shape(paths2,accur)
end

function polyb.tbl_to_paths(tbl,accur)
	local paths=Clib.Paths() local accur=accur or 2
	for m=1,#tbl do
		local path=Clib.Path()
		for pt=1,#tbl[m] do
			path:Add(Clib.Point(tbl[m][pt][1]*10^accur,tbl[m][pt][2]*10^accur))
		end
		paths:Add(path)
	end
	return paths
end

function polyb.paths_to_tbl(paths,accur)
	local tbl={} local accur=accur or 2
	for m=1,paths:Size() do
		local path=paths:Get(m) tbl[m]={}
		for i=1,path:Size() do
			local pt=path:Get(i) tbl[m][i]={math_round(pt.x/10^accur,accur),math_round(pt.y/10^accur,accur)}
		end
	end
	return tbl
end

function polyb.rect_shatter(ass_shape,w_copies,h_copies,accur)
	local paths1,left,top,_,_,w,h=polyb.get_paths(ass_shape,accur) local shatter={}
	for i=1,w_copies do
		for i2=1,h_copies do
			local x,y,width,height=left+(i-1)*w/w_copies,top+(i2-1)*h/h_copies,w/w_copies,h/h_copies
			local rect={{{x,y},{x+width,y},{x+width,y+height},{x,y+height}}}
			local paths=polyb.paths_intersection(paths1,polyb.tbl_to_paths(rect,accur))
			local AND,c,m=polyb.paths_to_shape_align(paths,accur)
			if AND~='' then
				shatter[#shatter+1]={s=AND,x=c,y=m}
			end
		end
	end
	return shatter
end

function polyb.rects_shatter(ass_shape,n,w_copies,h_copies,accur)
	local rect,rects,shatter={},{},{} local paths1,left,top,_,_,w,h=polyb.get_paths(ass_shape,accur)
	for i=1,w_copies do
		for i2=1,h_copies do
			local x,y,width,height=left+(i-1)*w/w_copies,top+(i2-1)*h/h_copies,w/w_copies,h/h_copies
			rect[#rect+1]={{x,y},{x+width,y},{x+width,y+height},{x,y+height}}
		end
	end
	for i=1,n do
		rects[i]={}
	end
	for i=1,#rect do
		local idx=math.random(n)
		rects[idx][#rects[idx]+1]=rect[i]
	end
	for i=1,n do
		local paths=polyb.paths_intersection(paths1,polyb.tbl_to_paths(rects[i],accur))
		local AND,c,m=polyb.paths_to_shape_align(paths,accur)
		if AND~='' then
			shatter[#shatter+1]={s=AND,x=c,y=m}
		end
	end
	return shatter
end

function polyb.cycle_tri(ass_shape,copies,accur)
	local paths1,_,_,_,_,w,h,c,m=polyb.get_paths(ass_shape,accur) local deg,r,shatter=math.rad(360/copies),w^2+h^2,{}
	for i=1,copies do
		local tri={{{c,m},{c+math.cos(deg*(i-1))*r,m+math.sin(deg*(i-1))*r},{c+math.cos(deg*i)*r,m+math.sin(deg*i)*r}}}
		local paths=polyb.paths_intersection(paths1,polyb.tbl_to_paths(tri,accur))
		local AND,x,y=polyb.paths_to_shape_align(paths,accur)
		if AND~='' then
			shatter[#shatter+1]={s=AND,x=x,y=y}
		end
	end
	return shatter
end

function polyb.cycle_tri_rand(ass_shape,copies,accur)
	local paths1,_,_,_,_,w,h,c,m=polyb.get_paths(ass_shape,accur) local cx,cy=c+math.random(-w,w)*3/8,m+math.random(-h,h)*3/8
	local deg,r,shatter=math.rad(360/copies),w^2+h^2,{}
	for i=1,copies do
		local tri={{{cx,cy},{cx+math.cos(deg*(i-1))*r,cy+math.sin(deg*(i-1))*r},{cx+math.cos(deg*i)*r,cy+math.sin(deg*i)*r}}}
		local paths=polyb.paths_intersection(paths1,polyb.tbl_to_paths(tri,accur))
		local AND,x,y=polyb.paths_to_shape_align(paths,accur)
		if AND~='' then
			shatter[#shatter+1]={s=AND,x=x,y=y}
		end
	end
	return shatter
end

function polyb.cut(ass_shape,mode,accur)
	local paths1,x1,y1,x2,y2,w,h=polyb.get_paths(ass_shape,accur) local c,m,x,y
	local mode=mode or 1 local shatter={} if mode~=1 and mode~=2 then mode=math.random(1,2) end
	if mode==1 then
		local d=math.random(h) local s1={{{x1,y1+d},{x1,y1},{x2,y1},{x2,y2-d}}} local s2={{{x1,y1+d},{x2,y2-d},{x2,y2},{x1,y2}}}
		local AND1=polyb.paths_intersection(paths1,polyb.tbl_to_paths(s1,accur))
		local AND2=polyb.paths_intersection(paths1,polyb.tbl_to_paths(s2,accur))
		AND1,c,m=polyb.paths_to_shape_align(AND1,accur) AND2,x,y=polyb.paths_to_shape_align(AND2,accur)
		local v1={d<h/2 and x1-x2 or x2-x1,d<h/2 and y1-y2+2*d or y2-y1-2*d} local len1=(v1[1]^2+v1[2]^2)^0.5
		local u1={math_round(v1[1]/len1,1),math_round(v1[2]/len1,1)} shatter[1]={s=AND1,x=c,y=m,vx=u1[1],vy=u1[2]}
		local v2={d<h/2 and x2-x1 or x1-x2,d<h/2 and y2-y1-2*d or y1-y2+2*d} local len2=(v2[1]^2+v2[2]^2)^0.5
		local u2={math_round(v2[1]/len2,1),math_round(v2[2]/len2,1)} shatter[2]={s=AND2,x=x,y=y,vx=u2[1],vy=u2[2]}
	elseif mode==2 then
		local d=math.random(w) local s1={{{x1,y1},{x1+d,y1},{x2-d,y2},{x1,y2}}} local s2={{{x2,y2},{x2-d,y2},{x1+d,y1},{x2,y1}}}
		local AND1=polyb.paths_intersection(paths1,polyb.tbl_to_paths(s1,accur))
		local AND2=polyb.paths_intersection(paths1,polyb.tbl_to_paths(s2,accur))
		AND1,c,m=polyb.paths_to_shape_align(AND1,accur) AND2,x,y=polyb.paths_to_shape_align(AND2,accur)
		local v1={d<w/2 and x2-x1-2*d or x1-x2+2*d,d<w/2 and y2-y1 or y1-y2} local len1=(v1[1]^2+v1[2]^2)^0.5
		local u1={math_round(v1[1]/len1,1),math_round(v1[2]/len1,1)} shatter[1]={s=AND1,x=c,y=m,vx=u1[1],vy=u1[2]}
		local v2={d<w/2 and x1-x2+2*d or x2-x1-2*d,d<w/2 and y1-y2 or y2-y1} local len2=(v2[1]^2+v2[2]^2)^0.5
		local u2={math_round(v2[1]/len2,1),math_round(v2[2]/len2,1)} shatter[2]={s=AND2,x=x,y=y,vx=u2[1],vy=u2[2]}
	end
	return shatter
end

function polyb.chip(ass_shape,n,accur)
	local paths1,x1,y1,x2,y2,w,h,cx,cy=polyb.get_paths(ass_shape,accur)
	local function differences(ass_shapes,accur)
		local solution
		for i=1,#ass_shapes-1 do
			local paths1=solution or polyb.get_poly_paths(ass_shapes[i],accur) local paths2=polyb.get_poly_paths(ass_shapes[i+1],accur)
			local c=Clib.Clipper() c:AddPaths(paths1,0,false) c:AddPaths(paths2,1,false) solution=c:Execute(3,1)
		end
		return solution
	end
	local s='m %d -0.1 l %d -0.1 l %d 0.1 l %d 0.1 ' local shatter={}
	local l=w^2+h^2 s=s:format(-l,l,l,-l) local bbox_split={('m %d %d l %d %d l %d %d l %d %d '):format(x1,y1,x1,y2,x2,y2,x2,y1)}
	for i=1,n do
		bbox_split[i+1]=Xshape.shift(Xshape.roll(s,360*(i-1)/n+180/n*math.random()),cx+math.random(-w,w)*3/8,cy+math.random(-h,h)*3/8)
	end
	local paths=differences(bbox_split,accur)
	for i=1,paths:Size() do
		local m=paths:Get(i) local p=Clib.Paths() p:Add(m) local c=Clib.Clipper()
		c:AddPaths(paths1,0,false) c:AddPaths(p,1,false) local solution=c:Execute(1,1)
		local AND,x,y=polyb.paths_to_shape_align(solution,accur)
		if AND~='' then
			shatter[#shatter+1]={s=AND,x=x,y=y}
		end
	end
	return shatter
end

function polyb.hexagon(r,x,y)
	local s='m %d %d l %d %d l %d %d l %d %d l %d %d l %d %d ' local x,y=x or 0,y or 0 local coor={}
	for i=1,6 do
		coor[#coor+1]=x+r*math.cos((i-1)*math.pi/3) coor[#coor+1]=y+r*math.sin((i-1)*math.pi/3)
	end
	return s:format(coor[1],coor[2],coor[3],coor[4],coor[5],coor[6],coor[7],coor[8],coor[9],coor[10],coor[11],coor[12])
end

function polyb.beehive(ass_shape,r,accur)
	local paths1,x1,y1,_,_,w,h=polyb.get_paths(ass_shape,accur) local shatter,s={},{}
	local n1=math.ceil(w/(r*1.5))+1 local n2=math.ceil(h/(r*3^0.5))
	for i=1,n1 do
		local n=i%2==1 and n2 or n2+1 local start=i%2==1 and r*3^0.5/2 or 0
		for i2=1,n do
			local cx,cy=x1+r*(i-1)*3/2,y1+start+r*(i2-1)*3^0.5
			s[#s+1]=polyb.hexagon(r,cx,cy) local AND,x,y=polyb.paths_intersection(paths1,polyb.get_poly_paths(s[#s],accur))
			AND,x,y=polyb.paths_to_shape_align(AND,accur)
			if AND~='' then
				shatter[#shatter+1]={s=AND,x=x,y=y}
			end
		end
	end
	return shatter,s
end

function polyb.clip_blinds(width,height,n,pct,angle,x,y,round)
	local round=round or 2 local len=math_round((width^2+height^2)^0.5/2,round) local w=2*len/n local spread,s=math_round(w*pct/2,round),{}
	for i=1,n do
		local p0=-len+w/2+(i-1)*w local l1=math_round(p0-spread,round) local l2=math_round(p0+spread,round)
		local p={x+l1.." "..y-len,x+l2.." "..y-len,x+l2.." "..y+len,x+l1.." "..y+len.." "}
		s[i]="m "..table.concat(p," l ") s[i]=Xshape.roll(s[i],angle,x,y)
	end
	return table.concat(s),s
end

function polyb.blinds_shatter(ass_shape,n,pct,angle,accur)
	local paths1,_,_,_,_,w,h,x,y=polyb.get_paths(ass_shape,accur) local shatter,s={},{}
	local angle=angle or 0 local _,blinds=polyb.clip_blinds(w,h,n,pct,angle,x,y)
	for i=1,#blinds do
		local AND,c,m=polyb.paths_intersection(paths1,polyb.get_poly_paths(blinds[i],accur))
		AND,c,m=polyb.paths_to_shape_align(AND,accur)
		if AND~='' then
			local vx,vy=math_round(math.sin(math.rad(angle)),1),math_round(math.cos(math.rad(angle)),1)
			shatter[#shatter+1]={s=AND,x=c,y=m,vx=vx,vy=vy} s[#s+1]=shatter[#shatter].s
		end
	end
	return shatter,table.concat(s)
end

function polyb.con_n_shatter(ass_shape,e,d,accur)
	local paths1,_,_,_,_,w,h,x,y=polyb.get_paths(ass_shape,accur) local R=math.max(w,h)*3
	local n=math.floor(R/(2*d)) local d1=R/(n*2) local shatter,s={},{}
	for i=1,n do
		s[i]=Xshape.reg_n_at_percent(e,R-2*d1*(i-1),1,x,y)
		if i<n then
			s[i]=s[i]..Xshape.reverse(Xshape.reg_n_at_percent(e,R-2*d1*i,1,x,y))
			local AND,c,m=polyb.paths_intersection(paths1,polyb.get_poly_paths(s[i],accur))
			AND,c,m=polyb.paths_to_shape_align(AND,accur)
			if AND~='' then
				shatter[#shatter+1]={s=AND,x=c,y=m}
			end
		else
			local AND,c,m=polyb.paths_intersection(paths1,polyb.get_poly_paths(s[i],accur))
			AND,c,m=polyb.paths_to_shape_align(AND,accur)
			if AND~='' then
				shatter[#shatter+1]={s=AND,x=c,y=m}
			end
		end
	end
	return shatter,s
end

function polyb.vor_shatter(ass_shape,n,accur)
	local paths1,x0,y0,_,_,w,h=polyb.get_paths(ass_shape,accur) x0,y0,w,h=math.floor(x0),math.floor(y0),math.ceil(w),math.ceil(h)
	local pointlist,sup,shatter=polyc.rand_coor(n,x0,y0,w,h),{{-1000000,1000000},{0,-1000000},{1000000,1000000}},{}
	local final_tri,p=tin(pointlist,sup) local vor=voronoi_cell(final_tri,p,x0,y0,w,h)
	for i=1,#vor do
		local AND,c,m=polyb.paths_intersection(paths1,polyb.tbl_to_paths({vor[i]},accur))
		AND,c,m=polyb.paths_to_shape_align(AND,accur)
		if AND~='' then
			shatter[#shatter+1]={s=AND,x=c,y=m}
		end
	end
	return shatter
end

function polyb.vor_shatter2(ass_shape,n,n2,accur)
	local paths1,x0,y0,_,_,w,h=polyb.get_paths(ass_shape,accur) x0,y0,w,h=math.floor(x0),math.floor(y0),math.ceil(w),math.ceil(h)
	local pointlist,sup,shatter=polyc.rand_coor2(n,n2,x0,y0,w,h),{{-1000000,1000000},{0,-1000000},{1000000,1000000}},{}
	local final_tri,p=tin(pointlist,sup) local vor=voronoi_cell(final_tri,p,x0,y0,w,h)
	for i=1,#vor do
		local AND,c,m=polyb.paths_intersection(paths1,polyb.tbl_to_paths({vor[i]},accur))
		AND,c,m=polyb.paths_to_shape_align(AND,accur)
		if AND~='' then
			shatter[#shatter+1]={s=AND,x=c,y=m}
		end
	end
	return shatter
end

function polyb.rect_vor_shatter(ass_shape,n,n2,accur)
	local paths1,x0,y0,_,_,w,h=polyb.get_paths(ass_shape,accur) x0,y0,w,h=math.floor(x0),math.floor(y0),math.ceil(w),math.ceil(h)
	local pointlist,sup=polyc.rand_coor_rect(n,n2,x0,y0,w,h) local shatter={}
	local final_tri,p=tin(pointlist,sup) local vor=voronoi_cell(final_tri,p,x0,y0,w,h)
	for i=1,#vor do
		local AND,c,m=polyb.paths_intersection(paths1,polyb.tbl_to_paths({vor[i]},accur))
		AND,c,m=polyb.paths_to_shape_align(AND,accur)
		if AND~='' then
			shatter[#shatter+1]={s=AND,x=c,y=m}
		end
	end
	return shatter
end

function polyb.circle_vor_shatter(ass_shape,n,n2,accur)
	local paths1,x0,y0,_,_,w,h,cx,cy=polyb.get_paths(ass_shape,accur) local r=(w^2+h^2)^0.5/2
	x0,y0,w,h=math.floor(cx-r),math.floor(cy-r),math.ceil(2*r),math.ceil(2*r)
	local pointlist,sup=polyc.rand_coor_circle(n,n2,cx,cy,r) local shatter={}
	local final_tri,p=tin(pointlist,sup) local vor=voronoi_cell(final_tri,p,x0,y0,w,h)
	for i=1,#vor do
		local AND,c,m=polyb.paths_intersection(paths1,polyb.tbl_to_paths({vor[i]},accur))
		AND,c,m=polyb.paths_to_shape_align(AND,accur)
		if AND~='' then
			shatter[#shatter+1]={s=AND,x=c,y=m}
		end
	end
	return shatter
end

function polyb.tin_shatter(ass_shape,n,n2,accur)
	local paths1,x0,y0,_,_,w,h=polyb.get_paths(ass_shape,accur) x0,y0,w,h=math.floor(x0),math.floor(y0),math.ceil(w),math.ceil(h)
	local pointlist,sup,shatter=polyc.rand_coor_rect(n,n2,x0,y0,w,h),{{-1000000,1000000},{0,-1000000},{1000000,1000000}},{}
	local tris=tin(pointlist,sup)
	for i=1,#tris do
		local AND,c,m=polyb.paths_intersection(paths1,polyb.tbl_to_paths({tris[i]},accur))
		AND,c,m=polyb.paths_to_shape_align(AND,accur)
		if AND~='' then
			shatter[#shatter+1]={s=AND,x=c,y=m}
		end
	end
	return shatter
end

local function poly_area(paths,accur)
	local area,orientation=0,{} local accur=accur or 2
	for m=1,paths:Size() do
		local path=paths:Get(m) local s=0 local pt={}
		for i=1,path:Size() do
			local p=path:Get(i) pt[i]={math_round(p.x/10^accur,accur),math_round(p.y/10^accur,accur)}
		end
		for i=1,#pt do
			local idx=i==#pt and 1 or i+1
			s=s+pt[i][1]*pt[idx][2]-pt[i][2]*pt[idx][1]
		end
		if s>0 then
			orientation[#orientation+1]=1
		elseif s==0 then
			orientation[#orientation+1]=0
		else orientation[#orientation+1]=-1
		end
		area=area+s/2
	end
	return math.abs(area),orientation[1],orientation
end

function polyb.pixelate(ass_shape,n1,n2,ala,clr1,clr2)
	local paths,left,top,_,_,width,height,center,middle=polyb.get_paths(ass_shape)
	local n1,n2=n1 or 3,n2 or 10 local px={n={}}
	local clr1,clr2=clr1 or '&HFFFFFF&',clr2 or '&H000000&'
	for i=n1,n2 do
		local w,h=width/i,height/i local x0,y0=left+w/2,top+h/2 local main={}
		local x1,y1,x2,y2=math.round(-w/2,1),math.round(-h/2,1),math.round(w/2,1),math.round(-h/2,1)
		local x3,y3,x4,y4=math.round(w/2,1),math.round(h/2,1),math.round(-w/2,1),math.round(h/2,1)
		px[i-n1+1]={} local rect='m '..x1..' '..y1..' l '..x2..' '..y2..' l '..x3..' '..y3..' l '..x4..' '..y4..' '
		for xi=1,i do
			for yi=1,i do
				local cx,cy=x0+(xi-1)*w,y0+(yi-1)*h
				local u1,v1,u2,v2,u3,v3,u4,v4=x1+cx,y1+cy,x2+cx,y2+cy,x3+cx,y3+cy,x4+cx,y4+cy
				local tbl={{{u1,v1},{u2,v2},{u3,v3},{u4,v4}}}
				local s=polyb.paths_intersection(paths,polyb.tbl_to_paths(tbl))
				if s:Size()>0 then
					local area=poly_area(s) local pct=area/(w*h)
					if ala then
						local a=255*(1-pct) a=a<0 and 0 or a>255 and 255 or a
						if a==0 then
							u1,v1,u2,v2=math.round(u1-center,1),math.round(v1-middle,1),math.round(u2-center,1),math.round(v2-middle,1)
							u3,v3,u4,v4=math.round(u3-center,1),math.round(v3-middle,1),math.round(u4-center,1),math.round(v4-middle,1)
							main[#main+1]='m '..u1..' '..v1..' l '..u2..' '..v2..' l '..u3..' '..v3..' l '..u4..' '..v4..' '
						else
							px[i-n1+1][#px[i-n1+1]+1]={x=math.round(cx,1),y=math.round(cy,1),s=rect,a=("&H%02X&"):format(a)}
						end
					else
						local b,g,r=clr1:match('&H(..)(..)(..)&') local b1,g1,r1=clr2:match('&H(..)(..)(..)&')
						b,g,r=tonumber(b,16),tonumber(g,16),tonumber(r,16) b1,g1,r1=tonumber(b1,16),tonumber(g1,16),tonumber(r1,16)
						local b2,g2,r2=b*pct+b1*(1-pct),g*pct+g1*(1-pct),r*pct+r1*(1-pct)
						b2,g2,r2=b2>255 and 255 or b2<0 and 0 or b2,g2>255 and 255 or g2<0 and 0 or g2,r2>255 and 255 or r2<0 and 0 or r2
						local c=("&H%02X%02X%02X&"):format(b2,g2,r2)
						if c==clr1 then
							u1,v1,u2,v2=math.round(u1-center,1),math.round(v1-middle,1),math.round(u2-center,1),math.round(v2-middle,1)
							u3,v3,u4,v4=math.round(u3-center,1),math.round(v3-middle,1),math.round(u4-center,1),math.round(v4-middle,1)
							main[#main+1]='m '..u1..' '..v1..' l '..u2..' '..v2..' l '..u3..' '..v3..' l '..u4..' '..v4..' '
						else
							px[i-n1+1][#px[i-n1+1]+1]={x=math.round(cx,1),y=math.round(cy,1),s=rect,c=c}
						end
					end
				end
			end
		end
		if #main>0 then
			px[i-n1+1][#px[i-n1+1]+1]={x=math.round(center,1),y=math.round(middle,1),s=table.concat(main),c=clr1,a='&H00&'}
		end
		px.n[i-n1+1]=#px[i-n1+1]
	end
	return px
end




_G.polyb=polyb

return _G.polyb


