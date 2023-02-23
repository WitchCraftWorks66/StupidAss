

--自己瞎写的破烂函数库
--我b站账号：https://space.bilibili.com/346816900
--Aegisub视频整合：https://space.bilibili.com/346816900/channel/seriesdetail?sid=231885
--如果没有polyb，请注释掉第855到第865行。（以后我会发出相应的文件，可以留意一下更新）
--转载请注明作者和出处！

local polyc={}

local function math_round(n,digit)
	if digit and digit>=1 then
		digit=10^math.floor(digit)
		return math.floor(n*digit+0.5)/digit
		else return math.floor(n+0.5)
	end
end

function polyc.rand_coor(n,x0,y0,w,h)
	local coor={} local sup={{x0-w*10,y0+h*10},{x0+20*w,y0+h*10},{x0+w/2,y0-math.max(w,h)*10}}
	for i=1,n do
		coor[i]={x0+w*(i-1)/n+w/n*math.random(),y0+h*math.random()}
	end
	return coor,sup
end

function polyc.rand_coor2(n,n2,x0,y0,w,h)
	local coor={} local sup={{x0-w*10,y0+h*10},{x0+20*w,y0+h*10},{x0+w/2,y0-math.max(w,h)*10}}
	for i=1,n do
		for i1=1,n2 do
			local x=x0+w*(i-1)/n+w/n*math.random()
			local y=y0+h*(i1-1)/n2+h/n2*math.random()
			coor[#coor+1]={x,y}
		end
	end
	return coor,sup
end

function polyc.rand_coor_rect(n,n2,x0,y0,w,h)
	local coor={} local sup={{x0-w*10,y0+h*10},{x0+20*w,y0+h*10},{x0+w/2,y0-math.max(w,h)*10}}
	for i=1,n do
		for i1=1,n2 do
			local x=i==1 and x0 or (i==n and x0+w or x0+w*(i-1)/n+w/n*math.random())
			local y=i1==1 and y0 or (i1==n2 and y0+h or y0+h*(i1-1)/n2+h/n2*math.random())
			coor[#coor+1]={x,y}
		end
	end
	return coor,sup
end

function polyc.rand_coor_circle(n,n2,x0,y0,R)--园内n个点，圆上n2个点
	local coor={} local sup={{x0-R*10,y0+R*10},{x0+20*R,y0+R*10},{x0+R/2,y0-R*10}}
	local deg=math.rad(360/n2)
	for i=1,n do
		local x=x0-R/2+R*(i-1)/n+R/n*math.random() local y=((R/2)^2-(x-x0)^2)^0.5 y=y0+math.random(-y,y)
		coor[i]={x,y}
	end
	for i=1,n2 do
		local d=deg*math.random()
		coor[n+i]={x0+R/2*math.cos((i-1)*deg+d),y0+R/2*math.sin((i-1)*deg+d)}
	end
	return coor,sup
end

local function circumcircle(tri)
	local x1,y1=tri[1][1],tri[1][2] local x2,y2=tri[2][1],tri[2][2] local x3,y3=tri[3][1],tri[3][2]
	local x=((y2-y1)*(y3*y3-y1*y1+x3*x3-x1*x1)-(y3-y1)*(y2*y2-y1*y1+x2*x2-x1*x1))/(2*(x3-x1)*(y2-y1)-2*((x2-x1)*(y3-y1)))
	local y=((x2-x1)*(x3*x3-x1*x1+y3*y3-y1*y1)-(x3-x1)*(x2*x2-x1*x1+y2*y2-y1*y1))/(2*(y3-y1)*(x2-x1)-2*((y2-y1)*(x3-x1)))
	local r=((x-x1)^2+(y-y1)^2)^0.5 local cir={x=x,y=y,r=r}
	return cir
end

local function circumcircle2(tri)--只计算外接圆圆心
	local x1,y1=tri[1][1],tri[1][2] local x2,y2=tri[2][1],tri[2][2] local x3,y3=tri[3][1],tri[3][2]
	local x=((y2-y1)*(y3*y3-y1*y1+x3*x3-x1*x1)-(y3-y1)*(y2*y2-y1*y1+x2*x2-x1*x1))/(2*(x3-x1)*(y2-y1)-2*((x2-x1)*(y3-y1)))
	local y=((x2-x1)*(x3*x3-x1*x1+y3*y3-y1*y1)-(x3-x1)*(x2*x2-x1*x1+y2*y2-y1*y1))/(2*(y3-y1)*(x2-x1)-2*((y2-y1)*(x3-x1)))
	return {x=x,y=y}
end

local function check_p(tri,p)
	local cir=circumcircle(tri)
	if (p[1]-cir.x)^2+(p[2]-cir.y)^2<cir.r^2 then--判断p是否在tri的外接圆内部
		return "inside"
	elseif cir.x+cir.r<p[1] then--判断p是否在tri的外接圆右侧
		return "right"
	else return "outside"--判断p是否在tri的外接圆外侧
	end
end

local function subdi(edges,p)--将一个三角形剖分为3或4个
	local tri={}
	for i=1,#edges do
		tri[i]={edges[i][1],p,edges[i][2]}
	end
	return tri
end

local function isintbl(tbl,value)
	for _,v in pairs(tbl) do
		if value==v then
			return true
		end
	end
	return false
end

local function check(points,p)--points里有没有p这个点
	for i=1,#points do
		if points[i][1]==p[1] and points[i][2]==p[2] then
			return true
		end
	end
	return false
end

local function delete_sup(tris,p1,p2,p3)--删除超级三角形
	for i=#tris,1,-1 do
		if check(tris[i],p1) or check(tris[i],p2) or check(tris[i],p3) then
			table.remove(tris,i)
		end
	end
	return tris
end

local function edges_dedup(edges)
	local function issame(tbl1,tbl2)--两个边一样就返回true
		local cnt=0
		for i=1,#tbl1 do
			if check(tbl2,tbl1[i]) then
				cnt=cnt+1
			end
		end
		return cnt==#tbl1
	end
	local idx={} local dedup={}
	for i=#edges,1,-1 do
		for i1=1,#edges do
			if i~=i1 and issame(edges[i],edges[i1]) then
				idx[#idx+1]=i idx[#idx+1]=i1 break
			end
		end
	end
	for i=1,#edges do
		if not isintbl(idx,i) then
			dedup[#dedup+1]=edges[i]
		end
	end
	return dedup
end

function tin(p,sup)
	if #p==3 then return {p},p end
	table.sort(p,function (point,next_p) return point[1]<next_p[1] or (point[1]==next_p[1] and point[2]<next_p[2]) end)
	local final_tri,temp_tri={},{sup}
	for i=1,#p do
		local edges={}
		for i1=#temp_tri,1,-1 do--在这个循环没结束之前，不能在temp_tri里面又加新的三角形（也就是剖分出来的三角形）
			local info=check_p(temp_tri[i1],p[i])
			if info=="right" then
				final_tri[#final_tri+1]=temp_tri[i1]
				table.remove(temp_tri,i1)
			elseif info=="inside" then
				edges[#edges+1]={temp_tri[i1][1],temp_tri[i1][2]}
				edges[#edges+1]={temp_tri[i1][2],temp_tri[i1][3]}
				edges[#edges+1]={temp_tri[i1][3],temp_tri[i1][1]}
				table.remove(temp_tri,i1)
			end
		end
		edges=edges_dedup(edges) local subdivide=subdi(edges,p[i])
		for i2=1,#subdivide do
			table.insert(temp_tri,subdivide[i2])
		end
	end
	for i=1,#temp_tri do--所有点"判断"完以后，再把temp_tri和final_tri合并
		final_tri[#final_tri+1]=temp_tri[i]
	end
	final_tri=delete_sup(final_tri,sup[1],sup[2],sup[3])
	return final_tri,p
end

local function tris_include_p(tris,p)
	local cnt=0
	for i=1,#tris do
		if check(tris[i],p) then
			cnt=cnt+1 if cnt>1 then return true end
		end
	end
	return false
end

local function tri_sort(tris,p)
	local function get_coor(tri,p1,p2)
		for i=1,3 do
			if check({tri[i]},p1)==false and check({tri[i]},p2)==false then
				return tri[i]
			end
		end
	end
	local point local sort,edges={},{}
	for i=1,#tris do
		for i1=1,3 do
			if tris_include_p(tris,tris[i][i1])==false and check({tris[i][i1]},p)==false then
				sort[#sort+1]=tris[i] point=tris[i][i1] edges[#edges+1]=point
				point=get_coor(tris[i],p,point) edges[#edges+1]=point table.remove(tris,i) break
			end
		end
		if #sort>0 then break end
	end
	if #sort==0 then
		for i=1,#tris[1] do
			if tris[1][i][1]~=p[1] or tris[1][i][2]~=p[2] then
				sort[1]=tris[1] point=tris[1][i] edges[1]=point table.remove(tris,1) break
			end
		end
	end
	for _=1,#tris do
		for i=#tris,1,-1 do
			if check(tris[i],point) then
				sort[#sort+1]=tris[i] point=get_coor(tris[i],p,point) edges[#edges+1]=point
				table.remove(tris,i) break
			end
		end
	end
	return sort,edges
end

local function h_line_seg_isect(x1,y1,x2,y2,y)--水平线和线段交点
	local top,bottom=math.min(y1,y2),math.max(y1,y2)
	if y>=top and y<=bottom then
		return x1+((x2-x1)/(y2-y1))*(y-y1)
	end
end

local function h_line_isect(x1,y1,x2,y2,y)--水平线和直线交点
	return x1+((x2-x1)/(y2-y1))*(y-y1)
end

local function v_line_isect(x1,y1,x2,y2,x)--竖直线和直线交点
	return y1+((y2-y1)/(x2-x1))*(x-x1)
end

local function v_line_seg_isect(x1,y1,x2,y2,x)--竖直线和线段交点
	local left,right=math.min(x1,x2),math.max(x1,x2)
	if x>=left and x<=right then
		return y1+((y2-y1)/(x2-x1))*(x-x1)
	end
end

local function sgn(n)
	return n>0 and 1 or n<0 and -1 or 0
end

local function rect_seg_intersection(seg,x0,y0,w,h)
	local p={}
	local x1=h_line_seg_isect(seg[1][1],seg[1][2],seg[2][1],seg[2][2],y0)
	local x2=h_line_seg_isect(seg[1][1],seg[1][2],seg[2][1],seg[2][2],y0+h)
	local y1=v_line_seg_isect(seg[1][1],seg[1][2],seg[2][1],seg[2][2],x0)
	local y2=v_line_seg_isect(seg[1][1],seg[1][2],seg[2][1],seg[2][2],x0+w)
	if x1 and x1>x0 and x1<=x0+w then p[#p+1]={x1,y0} end if x2 and x2>=x0 and x2<x0+w then p[#p+1]={x2,y0+h} end--防止端点重复添加
	if y1 and y1>=y0 and y1<y0+h then p[#p+1]={x0,y1} end if y2 and y2>y0 and y2<=y0+h then p[#p+1]={x0+w,y2} end--防止端点重复添加
	if #p==2 then
		local dx,dy,dx1,dy1=seg[2][1]-seg[1][1],seg[2][2]-seg[1][2],p[2][1]-p[1][1],p[2][2]-p[1][2]
		if math.abs(dx)>1 then
			if sgn(dx)~=sgn(dx1) then p={p[2],p[1]} end
		else
			if math.abs(dy)>1 then
				if sgn(dy)~=sgn(dy1) then p={p[2],p[1]} end
			else
				if sgn(dx)~=sgn(dx1) or sgn(dy)~=sgn(dy1) then
					p={p[2],p[1]}
				end
			end
		end
	end
	return p
end

local function rect_ray_intersection(ray,x0,y0,w,h)
	local p,insect,dx,dy={},{},ray[2][1]-ray[1][1],ray[2][2]-ray[1][2]
	local x1=h_line_isect(ray[1][1],ray[1][2],ray[2][1],ray[2][2],y0)
	local x2=h_line_isect(ray[1][1],ray[1][2],ray[2][1],ray[2][2],y0+h)
	local y1=v_line_isect(ray[1][1],ray[1][2],ray[2][1],ray[2][2],x0)
	local y2=v_line_isect(ray[1][1],ray[1][2],ray[2][1],ray[2][2],x0+w)
	if x1>x0 and x1<=x0+w then p[#p+1]={x1,y0} end if x2>=x0 and x2<x0+w then p[#p+1]={x2,y0+h} end--防止端点重复添加
	if y1>=y0 and y1<y0+h then p[#p+1]={x0,y1} end if y2>y0 and y2<=y0+h then p[#p+1]={x0+w,y2} end--防止端点重复添加
	for i=1,#p do
		if math.abs(dx)>1 then
			local dx1=p[i][1]-ray[1][1]
			if sgn(dx)==sgn(dx1) then insect[#insect+1]=p[i] end
		else
			if math.abs(dy)>1 then
				local dy1=p[i][2]-ray[1][2]
				if sgn(dy)==sgn(dy1) then insect[#insect+1]=p[i] end
			else
				local dx1,dy1=p[i][1]-ray[1][1],p[i][2]-ray[1][2]
				if sgn(dx)==sgn(dx1) and sgn(dy)==sgn(dy1) then
					insect[#insect+1]=p[i]
				end
			end
		end
		if p[i][1]==ray[1][1] and p[i][2]==ray[1][2] then--如果矩形和射线的交点刚好是射线的端点
			insect[#insect+1]=p[i]
		end
	end
	if #insect==2 then
		local dx1,dy1=insect[2][1]-insect[1][1],insect[2][2]-insect[1][2]
		if math.abs(dx)>1 then
			if sgn(dx)~=sgn(dx1) then insect={insect[2],insect[1]} end
		else
			if math.abs(dy)>1 then
				if sgn(dy)~=sgn(dy1) then insect={insect[2],insect[1]} end
			else
				if sgn(dx)~=sgn(dx1) or sgn(dy)~=sgn(dy1) then
					insect={insect[2],insect[1]}
				end
			end
		end
	end
	return insect
end

local function rect_line_intersection(line,x0,y0,w,h)
	local p={}
	local x1=h_line_isect(line[1][1],line[1][2],line[2][1],line[2][2],y0)
	local x2=h_line_isect(line[1][1],line[1][2],line[2][1],line[2][2],y0+h)
	local y1=v_line_isect(line[1][1],line[1][2],line[2][1],line[2][2],x0)
	local y2=v_line_isect(line[1][1],line[1][2],line[2][1],line[2][2],x0+w)
	if x1>x0 and x1<=x0+w then p[#p+1]={x1,y0} end if x2>=x0 and x2<x0+w then p[#p+1]={x2,y0+h} end--防止端点重复添加
	if y1>=y0 and y1<y0+h then p[#p+1]={x0,y1} end if y2>y0 and y2<=y0+h then p[#p+1]={x0+w,y2} end--防止端点重复添加
	return p
end

local function pt_orientation(p_x,p_y,q_x,q_y,r_x,r_y)
	local o=(q_y-p_y)*(r_x-q_x)-(q_x-p_x)*(r_y-q_y) if math.abs(o)<1e-9 then return 0 end
	return o>0 and 1 or o<0 and -1 or 0
end

local function on_rect_edge(p,x0,y0,w,h)--p点处于矩形哪一条边上
	if p[1]==x0 then
		return {x0,y0},{x0,y0+h}
	elseif p[1]==x0+w then
		return {x0+w,y0},{x0+w,y0+h}
	elseif p[2]==y0 then
		return {x0,y0},{x0+w,y0}
	elseif p[2]==y0+h then
		return {x0,y0+h},{x0+w,y0+h}
	end
end

local function only_one_corner(p1,p2,x0,y0,w,h)
	local p1x,p1y,p2x,p2y=p1[1],p1[2],p2[1],p2[2]
	if p1x==x0 and p2x~=x0 and p2x~=x0+w then
		if p2y==y0 then
			return {x0,y0}
		end
		if p2y==y0+h then
			return {x0,y0+h}
		end
	end
	if p1x==x0+w and p2x~=x0+w and p2x~=x0 then
		if p2y==y0 then
			return {x0+w,y0}
		end
		if p2y==y0+h then
			return {x0+w,y0+h}
		end
	end
	if p2x==x0 and p1x~=x0 and p1x~=x0+w then
		if p1y==y0 then
			return {x0,y0}
		end
		if p1y==y0+h then
			return {x0,y0+h}
		end
	end
	if p2x==x0+w and p1x~=x0+w and p1x~=x0 then
		if p1y==y0 then
			return {x0+w,y0}
		end
		if p1y==y0+h then
			return {x0+w,y0+h}
		end
	end
end

local function on_the_same_edge(p1,p2,x0,y0,w,h)--p1、p2两点在矩形的同一条边上
	local p1x,p1y,p2x,p2y=p1[1],p1[2],p2[1],p2[2]
	if p1x==x0 and p2x==x0 then return true end if p1x==x0+w and p2x==x0+w then return true end
	if p1y==y0 and p2y==y0 then return true end if p1y==y0+h and p2y==y0+h then return true end
end

local function add_corner(cell,p,x0,y0,w,h)
	local first,last=cell[1],cell[#cell]
	if on_the_same_edge(first,last,x0,y0,w,h)~=true then
		local corner=only_one_corner(first,last,x0,y0,w,h)
		if corner then
			local o1=pt_orientation(first[1],first[2],cell[2][1],cell[2][2],p[1],p[2])
			local o2=pt_orientation(corner[1],corner[2],first[1],first[2],cell[2][1],cell[2][2])
			if o1==o2 then--只添加一个角落
				cell[#cell+1]=corner
			else--需要添加三个角
				local tmp1,tmp2=on_rect_edge(last,x0,y0,w,h)
				if tmp1[1]==corner[1] and tmp1[2]==corner[2] then
					cell[#cell+1]=tmp2
				else cell[#cell+1]=tmp1
				end
				if corner[1]==cell[#cell][1] then
					if cell[#cell][1]==x0 then
						cell[#cell+1]={x0+w,cell[#cell][2]}
					else cell[#cell+1]={x0,cell[#cell][2]}
					end
				else
					if cell[#cell][2]==y0 then
						cell[#cell+1]={cell[#cell][1],y0+h}
					else cell[#cell+1]={cell[#cell][1],y0}
					end
				end
				tmp1,tmp2=on_rect_edge(first,x0,y0,w,h)
				if tmp1[1]==corner[1] and tmp1[2]==corner[2] then
					cell[#cell+1]=tmp2
				else cell[#cell+1]=tmp1
				end
			end
		else--需要添加两个角
			local tmp1,tmp2=on_rect_edge(last,x0,y0,w,h)
			local o1=pt_orientation(first[1],first[2],cell[2][1],cell[2][2],p[1],p[2])
			local o2=pt_orientation(cell[#cell-1][1],cell[#cell-1][2],last[1],last[2],tmp1[1],tmp1[2])
			if o1==o2 then cell[#cell+1]=tmp1 else cell[#cell+1]=tmp2 end
			tmp1,tmp2=on_rect_edge(first,x0,y0,w,h)
			if tmp1[1]==cell[#cell][1] or tmp1[2]==cell[#cell][2] then
				cell[#cell+1]=tmp1
			else cell[#cell+1]=tmp2
			end
		end
	end
	return cell
end

local function one_tri_and_circ_outside_the_rect(edges,cp,cir,x0,y0,w,h)
	local e1,e2=edges[1],edges[2] local pts
	local o1=pt_orientation(e1[1],e1[2],cp[1],cp[2],cir.x,cir.y)
	local o2=pt_orientation(e2[1],e2[2],cp[1],cp[2],cir.x,cir.y)
	local mid1,mid2={(e1[1]+cp[1])/2,(e1[2]+cp[2])/2},{(e2[1]+cp[1])/2,(e2[2]+cp[2])/2}
	if o1==o2 then--说明外心不在两条边的同一侧，所以算射线和矩形交点时只需要其中一条边,另一条边忽略
		local o3=pt_orientation(e1[1],e1[2],e2[1],e2[2],cir.x,cir.y)
		if o1~=o3 then--那么edge[1]就是需要的边，第二条边edge[2]用不上了
			pts=rect_ray_intersection({{cir.x,cir.y},mid1},x0,y0,w,h)
		else
			pts=rect_ray_intersection({{cir.x,cir.y},mid2},x0,y0,w,h)
		end
		pts=add_corner(pts,cp,x0,y0,w,h)
	else
		pts=rect_ray_intersection({{cir.x,cir.y},mid1},x0,y0,w,h)
		local p=rect_ray_intersection({{cir.x,cir.y},mid2},x0,y0,w,h)
		if on_the_same_edge(pts[2],p[2],x0,y0,w,h)~=true then
			local corner=only_one_corner(pts[2],p[2],x0,y0,w,h)
			if corner then
				pts[#pts+1]=corner
			else--补上两个角
				local tmp1,tmp2=on_rect_edge(pts[2],x0,y0,w,h)
				o1=pt_orientation(pts[1][1],pts[1][2],pts[2][1],pts[2][2],cp[1],cp[2])
				o2=pt_orientation(pts[1][1],pts[1][2],pts[2][1],pts[2][2],tmp1[1],tmp1[2])
				if o1==o2 then pts[3]=tmp1 else pts[3]=tmp2 end
				tmp1,tmp2=on_rect_edge(p[2],x0,y0,w,h)
				if tmp1[1]==pts[3][1] or tmp1[2]==pts[3][2] then
					pts[4]=tmp1
				else pts[4]=tmp2
				end
			end
		end
		pts[#pts+1]=p[2] pts[#pts+1]=p[1] pts=add_corner(pts,cp,x0,y0,w,h)
	end
	return pts
end

local function one_tri_and_circ_inside_the_rect(edges,cp,cir,x0,y0,w,h)
	local e1,e2=edges[1],edges[2] local cell,pts
	local o1=pt_orientation(e1[1],e1[2],cp[1],cp[2],cir.x,cir.y)
	local o2=pt_orientation(e2[1],e2[2],cp[1],cp[2],cir.x,cir.y)
	local mid1,mid2={(e1[1]+cp[1])/2,(e1[2]+cp[2])/2},{(e2[1]+cp[1])/2,(e2[2]+cp[2])/2}
	if cir.x==mid1[1] and cir.y==mid1[2] then--外心在edge[1]这条边上
		local new={mid1[1]+cp[2]-e1[2],mid1[2]+e1[1]-cp[1]}
		local o3=pt_orientation(e1[1],e1[2],cp[1],cp[2],new[1],new[2])
		if o2~=o3 then new={mid1[1]+e1[2]-cp[2],mid1[2]+cp[1]-e1[1]} end
		pts=rect_ray_intersection({{cir.x,cir.y},mid2},x0,y0,w,h) pts[2]=mid1
		local p=rect_ray_intersection({{cir.x,cir.y},new},x0,y0,w,h) pts[#pts+1]=p[1]
		pts=add_corner(pts,cp,x0,y0,w,h)
		return pts
	elseif cir.x==mid2[1] and cir.y==mid2[2] then
		local new={mid2[1]+cp[2]-e2[2],mid2[2]+e2[1]-cp[1]}
		local o3=pt_orientation(e2[1],e2[2],cp[1],cp[2],new[1],new[2])
		if o1~=o3 then new={mid2[1]+e2[2]-cp[2],mid2[2]+cp[1]-e2[1]} end
		pts=rect_ray_intersection({{cir.x,cir.y},mid1},x0,y0,w,h) pts[2]=mid2
		local p=rect_ray_intersection({{cir.x,cir.y},new},x0,y0,w,h) pts[#pts+1]=p[1]
		pts=add_corner(pts,cp,x0,y0,w,h)
		return pts
	end
	if o1==o2 then--说明外心不在两条边的同一侧，所以一条射线是其边中点指向外心，另一条射线是外心指向其边中点
		local o3=pt_orientation(e1[1],e1[2],e2[1],e2[2],cir.x,cir.y)
		if o1~=o3 then--那么针对第一条边的射线是从外心指向第一边中点，针对第二条边则相反
			cell=rect_ray_intersection({{cir.x,cir.y},mid1},x0,y0,w,h)
			pts=rect_ray_intersection({mid2,{cir.x,cir.y}},x0,y0,w,h)
		else
			cell=rect_ray_intersection({{cir.x,cir.y},mid2},x0,y0,w,h)
			pts=rect_ray_intersection({mid1,{cir.x,cir.y}},x0,y0,w,h)
		end
	else
		cell=rect_ray_intersection({{cir.x,cir.y},mid1},x0,y0,w,h)
		pts=rect_ray_intersection({{cir.x,cir.y},mid2},x0,y0,w,h)
	end
	cell[2]={cir.x,cir.y} cell[3]=pts[1] cell=add_corner(cell,cp,x0,y0,w,h)
	return cell
end

local function first_tri_and_circ_inside_the_rect(edges,cp,cir,next,x0,y0,w,h)
	local e1,e2=edges[1],edges[2] local pts
	local o1=pt_orientation(e1[1],e1[2],cp[1],cp[2],cir.x,cir.y)
	local o2=pt_orientation(e2[1],e2[2],cp[1],cp[2],cir.x,cir.y)
	local mid1={(e1[1]+cp[1])/2,(e1[2]+cp[2])/2}
	if cir.x==mid1[1] and cir.y==mid1[2] then--外心在edge[1]这条边上
		local new={mid1[1]+cp[2]-e1[2],mid1[2]+e1[1]-cp[1]}
		local o3=pt_orientation(e1[1],e1[2],cp[1],cp[2],new[1],new[2])
		if o2~=o3 then new={mid1[1]+e1[2]-cp[2],mid1[2]+cp[1]-e1[1]} end
		pts=rect_ray_intersection({{cir.x,cir.y},new},x0,y0,w,h) pts[#pts+1]={cir.x,cir.y}
	end
	if o1==o2 then--此时外心在三角形外
		local o3=pt_orientation(e1[1],e1[2],e2[1],e2[2],cir.x,cir.y)
		if o1~=o3 then--则射线方向是从外心到第一条边中点
			pts=rect_ray_intersection({{cir.x,cir.y},mid1},x0,y0,w,h) pts[#pts+1]={cir.x,cir.y}
		else
			pts=rect_ray_intersection({mid1,{cir.x,cir.y}},x0,y0,w,h) pts[#pts+1]={cir.x,cir.y}
		end
	else
		pts=rect_ray_intersection({{cir.x,cir.y},mid1},x0,y0,w,h)--射线方向是外心到第一边中点
		pts[#pts+1]={cir.x,cir.y}
	end
	if next.x<x0 or next.x>x0+w or next.y<y0 or next.y>y0+h then--下一个外心又在矩形外
		local p=rect_ray_intersection({{cir.x,cir.y},{next.x,next.y}},x0,y0,w,h) pts[#pts+1]=p[1]
	else
		pts[#pts+1]={next.x,next.y}
	end
	return pts
end

local function first_tri_and_circ_outside_the_rect(edges,cp,cir,next,x0,y0,w,h)
	local e1,e2=edges[1],edges[2] local pts={}
	local o1=pt_orientation(e1[1],e1[2],cp[1],cp[2],cir.x,cir.y)
	local o2=pt_orientation(e2[1],e2[2],cp[1],cp[2],cir.x,cir.y)
	local mid1={(e1[1]+cp[1])/2,(e1[2]+cp[2])/2}
	if o1==o2 then
		local o3=pt_orientation(e1[1],e1[2],e2[1],e2[2],cir.x,cir.y)
		if o1~=o3 then--外心在第二边外侧，射线方向是从外心到第一条边中点
			local p=rect_ray_intersection({{cir.x,cir.y},mid1},x0,y0,w,h)
			pts[1]=p[2] pts[2]=p[1]
		end
	else
		local p=rect_ray_intersection({{cir.x,cir.y},mid1},x0,y0,w,h)
		pts[1]=p[2] pts[2]=p[1]
	end
	if next.x<x0 or next.x>x0+w or next.y<y0 or next.y>y0+h then--下一个外心又在矩形外
		local p=rect_seg_intersection({{cir.x,cir.y},{next.x,next.y}},x0,y0,w,h)
		if p[1] then
			pts[#pts+1]=p[1] pts[#pts+1]=p[2]
		end
	else
		local p=rect_seg_intersection({{next.x,next.y},{cir.x,cir.y}},x0,y0,w,h)
		if pts[1] and (on_the_same_edge(pts[#pts],p[1],x0,y0,w,h)~=true) then
			local corner=only_one_corner(pts[#pts],p[1],x0,y0,w,h) pts[#pts+1]=corner
		end
		pts[#pts+1]=p[1] pts[#pts+1]={next.x,next.y}
	end
	return pts
end

local function last_tri_and_circ_outside_the_rect(edges,cp,cir,x0,y0,w,h)--外心在矩形外，外心必然不在三角形边上
	local e1,e2=edges[#edges-1],edges[#edges] local pts={}
	local o1=pt_orientation(e1[1],e1[2],cp[1],cp[2],cir.x,cir.y)
	local o2=pt_orientation(e2[1],e2[2],cp[1],cp[2],cir.x,cir.y)
	local mid2={(e2[1]+cp[1])/2,(e2[2]+cp[2])/2}
	if o1==o2 then
		local o3=pt_orientation(e2[1],e2[2],e1[1],e1[2],cir.x,cir.y)
		if o1~=o3 then--外心在倒数第二边外侧，射线方向是从外心到最后一条边中点
			local p=rect_ray_intersection({{cir.x,cir.y},mid2},x0,y0,w,h)
			pts[#pts+1]=p[1] pts[#pts+1]=p[2]
		end
	else
		local p=rect_ray_intersection({{cir.x,cir.y},mid2},x0,y0,w,h)
		pts[#pts+1]=p[1] pts[#pts+1]=p[2]
	end
	return pts
end

local function last_tri_and_circ_inside_the_rect(edges,cp,cir,x0,y0,w,h)
	local e1,e2=edges[#edges-1],edges[#edges] local pts
	local o1=pt_orientation(e1[1],e1[2],cp[1],cp[2],cir.x,cir.y)
	local o2=pt_orientation(e2[1],e2[2],cp[1],cp[2],cir.x,cir.y)
	local mid2={(e2[1]+cp[1])/2,(e2[2]+cp[2])/2}
	if cir.x==mid2[1] and cir.y==mid2[2] then--外心在最后一条边上
		local new={mid2[1]+cp[2]-e2[2],mid2[2]+e2[1]-cp[1]}
		local o3=pt_orientation(e2[1],e2[2],cp[1],cp[2],new[1],new[2])
		if o1~=o3 then new={mid2[1]+e2[2]-cp[2],mid2[2]+cp[1]-e2[1]} end
		pts=rect_ray_intersection({{cir.x,cir.y},new},x0,y0,w,h)
		return pts
	end
	if o1==o2 then--外心在三角形外
		local o3=pt_orientation(e2[1],e2[2],e1[1],e1[2],cir.x,cir.y)
		if o1==o3 then--则外心在最后一条边外侧，射线方向是从最后一条边中点到外心
			pts=rect_ray_intersection({mid2,{cir.x,cir.y}},x0,y0,w,h)
		else
			pts=rect_ray_intersection({{cir.x,cir.y},mid2},x0,y0,w,h)
		end
	else
		pts=rect_ray_intersection({{cir.x,cir.y},mid2},x0,y0,w,h)
	end
	return pts
end

local function except_the_first_and_last(cir,next,x0,y0,w,h)--讨论既不是第一个也不是最后一个tri的情况（neither first nor last）
	if next.x<x0 or next.x>x0+w or next.y<y0 or next.y>y0+h then--下一个外心在矩形外
		if cir.x<x0 or cir.x>x0+w or cir.y<y0 or cir.y>y0+h then
			return rect_seg_intersection({{cir.x,cir.y},{next.x,next.y}},x0,y0,w,h)
		else return rect_ray_intersection({{cir.x,cir.y},{next.x,next.y}},x0,y0,w,h)
		end
	else
		if cir.x<x0 or cir.x>x0+w or cir.y<y0 or cir.y>y0+h then
			local pts=rect_seg_intersection({{next.x,next.y},{cir.x,cir.y}},x0,y0,w,h)
			pts[#pts+1]={next.x,next.y}
			return pts
		else return {{next.x,next.y}}
		end
	end
end

local function normal_tri(cir,next,x0,y0,w,h)--当该组三角形里没有在边缘的三角形时。每次添加一条线段，即该函数要不然返回空表，要不然返回有两个点的表
	if next.x<x0 or next.x>x0+w or next.y<y0 or next.y>y0+h then--下一个外心在矩形外
		if cir.x<x0 or cir.x>x0+w or cir.y<y0 or cir.y>y0+h then
			return rect_seg_intersection({{cir.x,cir.y},{next.x,next.y}},x0,y0,w,h)
		else
			local pts,p={{cir.x,cir.y}},rect_seg_intersection({{cir.x,cir.y},{next.x,next.y}},x0,y0,w,h)
			pts[2]=p[1] return pts
		end
	else--下一个外心在矩形内时
		if cir.x<x0 or cir.x>x0+w or cir.y<y0 or cir.y>y0+h then
			local pts=rect_seg_intersection({{next.x,next.y},{cir.x,cir.y}},x0,y0,w,h)
			pts[#pts+1]={next.x,next.y}
			return pts
		else--此时的外心和下一个外心都在矩形内
			if cir.x==next.x and cir.y==next.y then--当下一个外心和此时的外心是同一个点时，没必要再把下一个外心的坐标加进去了
				return {}
			else return {{cir.x,cir.y},{next.x,next.y}}
			end
		end
	end
end

function voronoi_cell(final_tri,p,x0,y0,w,h)
	local collect={} local x1,y1,x2,y2=x0,y0,x0+w,y0+h
	if #p==1 then
		return {{{x0,y0},{x0+w,y0},{x0+w,y0+h},{x0,y0+h}}}
	elseif #p==2 then
		local mid={(p[1][1]+p[2][1])/2,(p[1][2]+p[2][2])/2} local new={mid[1]+p[1][2]-p[2][2],mid[2]+p[2][1]-p[1][1]}
		local pts=rect_line_intersection({new,mid},x0,y0,w,h)
		local cell=add_corner({pts[1],pts[2]},p[1],x0,y0,w,h) local cell2=add_corner({pts[1],pts[2]},p[2],x0,y0,w,h)
		return {cell,cell2}
	end
	local flag={}
	for i=1,#final_tri do
		for i2=1,3 do
			local x,y=final_tri[i][i2][1],final_tri[i][i2][2]
			flag[x]=flag[x]==nil and {} or flag[x] flag[x][y]=flag[x][y]==nil and {} or flag[x][y]
			local len=#flag[x][y] flag[x][y][len+1]=final_tri[i]
		end
	end
	for i=1,#p do
		local temp,vor_cell,circum=flag[p[i][1]][p[i][2]],{},{} local edges
		temp,edges=tri_sort(temp,p[i])--temp装有以p[i]这个点为中心的一组三角形
		for i1=1,#temp do
			circum[i1]=circumcircle2(temp[i1])
		end
		for i1=1,#temp do
			local cir=circum[i1] local idx=i1==#temp and 1 or i1+1 local next=circum[idx]
			if #temp==#edges then--该组三角形数量和edge数量一样，则该组三角形不在边界、边缘上(该组三角形没有边缘三角形)
				local pts=normal_tri(cir,next,x1,y1,x2-x1,y2-y1)
				if #vor_cell==0 then
					vor_cell=pts
				else
					if #pts>0 then
						if vor_cell[#vor_cell][1]==pts[1][1] and vor_cell[#vor_cell][2]==pts[1][2] then
							vor_cell[#vor_cell+1]=pts[2]
						else
							if on_the_same_edge(vor_cell[#vor_cell],pts[1],x0,y0,w,h)~=true then
								vor_cell[#vor_cell+1]=only_one_corner(vor_cell[#vor_cell],pts[1],x0,y0,w,h)
							end
							vor_cell[#vor_cell+1]=pts[1] vor_cell[#vor_cell+1]=pts[2]
						end
					end
				end
				if i1==#temp then
					if vor_cell[1] and on_the_same_edge(vor_cell[1],vor_cell[#vor_cell],x0,y0,w,h)~=true then
						local corner=only_one_corner(vor_cell[1],vor_cell[#vor_cell],x0,y0,w,h)
						if corner then
							vor_cell[#vor_cell+1]=corner
						end
					end
				end
			elseif #temp==1 then--该组三角形里只有一个三角形时
				if cir.x<x1 or cir.x>x2 or cir.y<y1 or cir.y>y2 then
					vor_cell=one_tri_and_circ_outside_the_rect(edges,p[i],cir,x0,y0,w,h)
				else vor_cell=one_tri_and_circ_inside_the_rect(edges,p[i],cir,x0,y0,w,h)
				end
			else--该组三角形有边缘三角形、即第一个和最后一个三角形是在边缘的，与它们相邻的三角形只有一个
				if i1==1 then
					if cir.x<x1 or cir.x>x2 or cir.y<y1 or cir.y>y2 then
						vor_cell=first_tri_and_circ_outside_the_rect(edges,p[i],cir,next,x1,y1,x2-x1,y2-y1)
					else
						vor_cell=first_tri_and_circ_inside_the_rect(edges,p[i],cir,next,x1,y1,x2-x1,y2-y1)
					end
				elseif i1==#temp then
					if cir.x<x1 or cir.x>x2 or cir.y<y1 or cir.y>y2 then
						local pts=last_tri_and_circ_outside_the_rect(edges,p[i],cir,x1,y1,x2-x1,y2-y1)
						if pts[1] then
							if vor_cell[1] and (on_the_same_edge(vor_cell[#vor_cell],pts[1],x0,y0,w,h)~=true) then
								vor_cell[#vor_cell+1]=only_one_corner(vor_cell[#vor_cell],pts[1],x0,y0,w,h)
							end
							for ip=1,#pts do
								vor_cell[#vor_cell+1]=pts[ip]
							end
						end
					else
						local pts=last_tri_and_circ_inside_the_rect(edges,p[i],cir,x1,y1,x2-x1,y2-y1)
						vor_cell[#vor_cell+1]=pts[1]
					end
					vor_cell=add_corner(vor_cell,p[i],x0,y0,w,h)
				else
					local pts=except_the_first_and_last(cir,next,x1,y1,w,h)
					for ip=1,#pts do
						vor_cell[#vor_cell+1]=pts[ip]
					end
				end
			end
		end
		collect[#collect+1]=vor_cell
	end
	return collect
end

local function to_draw(points,dx,dy)
	local s={} local dx=dx or 0 local dy=dy or 0
	for i=1,#points do
		s[#s+1]="l "..math_round(points[i][1]-dx,2).." "..math_round(points[i][2]-dy,2).." "
	end
	return table.concat(s):gsub("^l","m")
end

local function tri_center(tri)
	return {x=(tri[1][1]+tri[2][1]+tri[3][1])/3,y=(tri[1][2]+tri[2][2]+tri[3][2])/3}
end

local function poly_area(pt)
	local s=0
	for i=1,#pt do
		local idx=i==#pt and 1 or i+1
		s=s+pt[i][1]*pt[idx][2]-pt[i][2]*pt[idx][1]
	end
	return math.abs(s/2)
end

local function poly_center(pt)
	local x,y=0,0
	for i=1,#pt do
		x,y=x+pt[i][1],y+pt[i][2]
	end
	return x/#pt,y/#pt
end

function polyc.reset_pts_idx(pts)
	local new={}
	for i=1,#pts do
		if pts[i].x then
			new[i]={pts[i].x,pts[i].y}
		else
			new[i]={x=pts[i][1],y=pts[i][2]}
		end
	end
	return new
end

function polyc.delaunay(p,sup)
	local sup=sup or {{-1000000,1000000},{0,-1000000},{1000000,1000000}}
	local final_tri=tin(p,sup) local s={}
	for i=1,#final_tri do
		local c=tri_center(final_tri[i])
		s[i]={s=to_draw(final_tri[i],c.x,c.y),x=math_round(c.x,1),y=math_round(c.y,1)}
	end
	return s
end

function polyc.voronoi(p,x0,y0,w,h)
	local x0,y0,w,h=math.floor(x0),math.floor(y0),math.ceil(w),math.ceil(h)
	local sup={{-1000000,1000000},{0,-1000000},{1000000,1000000}} local s={}
	local final_tri,p=tin(p,sup) local vor_cell=voronoi_cell(final_tri,p,x0,y0,w,h)
	for i=1,#vor_cell do
		s[i]={s=to_draw(vor_cell[i],p[i][1],p[i][2]),x=math_round(p[i][1],1),y=math_round(p[i][2],1)}
	end
	return s
end

function polyc.tin_and_vor(p,x0,y0,w,h)
	local x0,y0,w,h=math.floor(x0),math.floor(y0),math.ceil(w),math.ceil(h)
	local sup={{-1000000,1000000},{0,-1000000},{1000000,1000000}} local tri,vor={},{}
	local final_tri=tin(p,sup) local vor_cell=voronoi_cell(final_tri,p,x0,y0,w,h)
	for i=1,#final_tri do
		local c=tri_center(final_tri[i])
		tri[i]={s=to_draw(final_tri[i],c.x,c.y),x=math_round(c.x,1),y=math_round(c.y,1)}
	end
	for i=1,#vor_cell do
		vor[i]={s=to_draw(vor_cell[i],p[i][1],p[i][2]),x=math_round(p[i][1],1),y=math_round(p[i][2],1)}
	end
	return tri,vor
end

function polyc.vor_vortex(p,x0,y0,w,h,pct,n,area)
	local x0,y0,w,h=math.floor(x0),math.floor(y0),math.ceil(w),math.ceil(h)
	local sup={{-1000000,1000000},{0,-1000000},{1000000,1000000}} local s,poly={n={}},{}
	local final_tri,p=tin(p,sup) local vor_cell=voronoi_cell(final_tri,p,x0,y0,w,h)
	for i=1,#vor_cell do
		poly[i]={vor_cell[i]} local cnt=poly_area(vor_cell[i]) local cx,cy=poly_center(vor_cell[i])
		s[i]={{s=to_draw(vor_cell[i],cx,cy),x=math_round(cx,1),y=math_round(cy,1)}}
		if not area then
			for i2=1,n do
				poly[i][i2+1]={}
				for i3=1,#vor_cell[i] do
					local idx=i3==#vor_cell[i] and 1 or i3+1 local x=(poly[i][i2][idx][1]-poly[i][i2][i3][1])*pct+poly[i][i2][i3][1]
					local y=(poly[i][i2][idx][2]-poly[i][i2][i3][2])*pct+poly[i][i2][i3][2] poly[i][i2+1][i3]={x,y}
				end
				s[i][i2+1]={s=to_draw(poly[i][i2+1],cx,cy),x=math_round(cx,1),y=math_round(cy,1)}
			end
		else
			while cnt>area do
				local num=#poly[i] poly[i][num+1]={}
				for i2=1,#vor_cell[i] do
					local idx=i2==#vor_cell[i] and 1 or i2+1 local x=(poly[i][num][idx][1]-poly[i][num][i2][1])*pct+poly[i][num][i2][1]
					local y=(poly[i][num][idx][2]-poly[i][num][i2][2])*pct+poly[i][num][i2][2] poly[i][num+1][i2]={x,y}
				end
				cnt=poly_area(poly[i][num+1])
				s[i][num+1]={s=to_draw(poly[i][num+1],cx,cy),x=math_round(cx,1),y=math_round(cy,1)}
			end
		end
		s.n[i]=#s[i]
	end
	return s,poly
end


local polyb=require 'polyb'

polyc.vor_shatter=polyb.vor_shatter

polyc.vor_shatter2=polyb.vor_shatter2

polyc.rect_vor_shatter=polyb.rect_vor_shatter

polyc.circle_vor_shatter=polyb.circle_vor_shatter

polyc.tin_shatter=polyb.tin_shatter


function polyc.voronoi_move(frames,n,points_n,x0,y0,w,h)
	local p={} local each=math.ceil(frames/n) local s={n={{points_n,each*(n-1)}}}
	for i=1,n do
		p[i]=polyc.rand_coor(points_n,x0,y0,w,h)
	end
	for i=1,n-1 do
		for i1=1,each do
			local points={}
			for i2=1,points_n do
				points[i2]={(p[i+1][i2][1]-p[i][i2][1])*(i1-1)/(each-1)+p[i][i2][1],(p[i+1][i2][2]-p[i][i2][2])*(i1-1)/(each-1)+p[i][i2][2]}
			end
			s[#s+1]=polyc.voronoi(points,x0,y0,w,h)
		end
	end
	return s
end


_G.polyc=polyc
return _G.polyc