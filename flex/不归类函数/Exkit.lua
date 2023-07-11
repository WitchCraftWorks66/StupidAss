

local Exkit={}

function Exkit.derivative_1st(f,x,eps)
	local eps=eps or 1e-10 local delta,n=math.min(eps^0.5,1e-3),7
	local maxErr,h=math.max(50*eps,1e-3),2^n*1e-6
	local h2,r,err=2*h,{0},delta/2
	for i=1,n do
		local x1=x-h local x2,d,r1=x1+h2,1,r[1]
		r[i]=(f(x2)-f(x1))/h2 local k=i-1
		while k>0 do
			d=d*4 r[k]=r[k+1]+(r[k+1]-r[k])/(d-1) k=k-1
		end
		if i>=2 then
			err=math.abs(r[1])<=delta and math.abs(r[1]-r1) or math.abs((r[1]-r1)/r[1])
			if err<delta then break end
		end
		h2=h h=h2/2
	end
	local slope=err>maxErr and false or r[1]
	return slope
end

local function e_sinh(f,a,eps,d)
	local ev,h2=2,f(a+d/2)-f(a+d*2)*4 local i,j=1,32
	if h2<math.huge and h2>-math.huge and math.abs(h2)>1e-5 then
		local r,fl,fr,h,s=0,0,0,0,0 local lfl,lfr,lr=0,0,2
		repeat
			j=j/2 r=2^(i+j) fl=f(a+d/r) fr=f(a+d*r)*r*r ev=ev+2 h=fl-fr
		until (j<=1 or (h<math.huge and h>-math.huge))
		if j>1 and h<math.huge and h>-math.huge and Exkit.sgn(h)~=Exkit.sgn(h2) then
			lfl=fl lfr=fr
			repeat
				j=j/2 r=2^(i+j) fl=f(a+d/r) fr=f(a+d*r)*r*r ev=ev+2 h=fl-fr
				if h<math.huge and h>-math.huge then
					s=s+math.abs(h)
					if Exkit.sgn(h)==Exkit.sgn(h2) then
						i=i+j
					else lfl=fl lfr=fr lr=r
					end
				end
			until (j<=1)
			if s>eps then
				h=lfl-lfr r=lr
				if h~=0 then r=r/2 end
				if math.abs(lfl)<math.abs(lfr) then
					d=d/r
				else d=d*r
				end
			end
		end
	end
	return d
end

--超高精度数值积分，被积函数为f，积分下限为a、上限为b
function Exkit.integrator(f,a,b,eps,n)
	local eps,n=eps or 1e-14,n or 11 eps=eps<1e-15 and 1e-15 or eps
	local tol,c,d,s,sign,v,h,k,mode=10*eps,0,1,0,1,0,2,0,0
	if a>b then a,b=b,a sign=-1 end
	if a<math.huge and a>-math.huge and b<math.huge and b>-math.huge then
		c=(a+b)/2 d=(b-a)/2 v=c
	elseif a<math.huge and a>-math.huge then
		mode=1 d=e_sinh(f,a,eps,d) c=a v=a+d
	elseif b<math.huge and b>-math.huge then
		mode=1 d=e_sinh(f,b,eps,-d) sign=-sign c=b v=b+d
	else mode=2 v=0
	end
	s=f(v)
	repeat
		local q,p,fp,fm,t,eh=0,0,0,0,0,0
		h=h/2 t=math.exp(h) eh=k>0 and t*t or t
		if mode==0 then
			repeat
				local u=math.exp(1/t-t) local r=2*u/(1+u)
				local w=(t+1/t)*r/(1+u) local x=d*r
				if a+x>a then
					local y=f(a+x)
					if y<math.huge and y>-math.huge then fp=y end
				end
				if b-x<b then
					local y=f(b-x)
					if y<math.huge and y>-math.huge then fm=y end
				end
				q=w*(fp+fm) p=p+q t=t*eh
			until (math.abs(q)<=eps*math.abs(p))
		else
			t=t/2
			repeat
				local r=math.exp(t-.25/t) local x,y,w,q=0,0,r,0
				if mode==1 then
					x=c+d/r if x==c then break end y=f(x)
					if y<math.huge and y>-math.huge then q=q+y/w end
				else
					r=(r-1/r)/2 w=(w+1/w)/2 x=c-d*r y=f(x)
					if y<math.huge and y>-math.huge then q=q+y*w end
				end
				x=c+d*r y=f(x)
				if y<math.huge and y>-math.huge then q=q+y*w end
				q=q*(t+.25/t) p=p+q t=t*eh
			until (math.abs(q)<=eps*math.abs(p))
		end
		v=s-p s=s+p k=k+1
	until (math.abs(v)<=tol*math.abs(s) or k>n)
	return sign*d*s*h,math.abs(v)/(math.abs(s)+eps)
end

function Exkit.modAB(f,x1,x2,eps)
	local y1,y2,bi=f(x1),f(x2),true local cnt,eps=0,eps or 1e-14
	local x0,side,n=x1,0,-math.floor(math.log(eps,2)/2)+1
	for i=1,n do
		local x3,y3 cnt=cnt+1
		if bi then
			x3=(x1+x2)/2 y3=f(x3) local ym=(y1+y2)/2
			if math.abs(ym-y3)<0.25*(math.abs(ym)+math.abs(y3)) then
				bi=false
			end
		else x3=(x1*y2-y1*x2)/(y2-y1) y3=f(x3)
		end
		if math.abs(y3)<=eps and math.abs(x3-x0)<=eps then
			return x3,cnt
		end
		x0=x3
		if side==1 then
			local m=1-y3/y1 if m<=0 then y2=y2/2 else y2=y2*m end
		elseif side==2 then
			local m=1-y3/y2 if m<=0 then y1=y1/2 else y1=y1*m end
		end
		if Exkit.sgn(y1)==Exkit.sgn(y3) then
			if not bi then side=1 end x1=x3 y1=y3
		else
			if not bi then side=2 end x2=x3 y2=y3
		end
		if i%n==0 then bi=true side=0 end
	end
	return x0,cnt
end

function Exkit.curve_length_robust(x,y,t1,t2,eps,x1,y1)--超高精度数值积分，但是龟速，并无卵用
	local eps=eps or 1e-14
	local function func(t)
		local vx,vy=x1 and x1(t) or Exkit.derivative_1st(x,t),y1 and y1(t) or Exkit.derivative_1st(y,t)
		return (vx^2+vy^2)^0.5
	end
	return Exkit.integrator(func,t1,t2,eps)
end

function Exkit.curve_t_at_s_robust(x,y,t1,t2,s,L,x1,y1)--超高精度数值积分，但是龟速，并无卵用
	local arc_len=s*L
	local function func(u)
		return Exkit.curve_length_robust(x,y,t1,t1+u,1e-14,x1,y1)-arc_len
	end
	return Exkit.modAB(func,0,t2-t1,1e-5)
end

function Exkit.argz(x,y)
	if y==0 then
		local a=x<0 and math.pi or 0
		return math.deg(a),a
	else
		local a=math.atan(y/x)+(x<0 and 1 or 0)*Exkit.sgn(y)*math.pi
		return math.deg(a),a
	end
end

function Exkit.Mround2(num)--个人最常用的，保留两位小数
	return math.floor(num*100+.5)/100
end

function Exkit.Mround1(num) return math.floor(num*10+.5)/10 end

function Exkit.Mround3(num) return math.floor(num*1000+.5)/1000 end

function Exkit.curve_to_px_robust(x,y,t1,t2,ds,accel,x1,y1,L)--超高精度数值积分，但是龟速，并无卵用
	local L,accel=L or Exkit.curve_length_robust(x,y,t1,t2,1e-14,x1,y1),accel or 1
	local ds=ds or 1 local n,px=math.ceil(L/ds),{}
	for i=1,n do
		local a=type(accel)=='function' and accel(i,n) or accel
		local s=(i-1)/(n-1) local t=t1+Exkit.curve_t_at_s_robust(x,y,t1,t2,s^a,L,x1,y1)
		local vx,vy=x1 and x1(t) or Exkit.derivative_1st(x,t),y1 and y1(t) or Exkit.derivative_1st(y,t)
		local angle=-Exkit.argz(vx,vy) local pt_x,pt_y=x(t),y(t)
		px[i]={x=Exkit.Mround2(pt_x),y=Exkit.Mround2(pt_y),angle=Exkit.Mround2(angle)}
	end
	return px
end

Exkit.ABSCISSAS_K21={
	-0.995657163025808081,
	-0.97390652851717172,
	-0.930157491355708226,
	-0.865063366688984511,
	-0.780817726586416897,
	-0.679409568299024406,
	-0.562757134668604683,
	-0.433395394129247191,
	-0.294392862701460198,
	-0.148874338981631211,
	0,
	0.148874338981631211,
	0.294392862701460198,
	0.433395394129247191,
	0.562757134668604683,
	0.679409568299024406,
	0.780817726586416897,
	0.865063366688984511,
	0.930157491355708226,
	0.97390652851717172,
	0.995657163025808081
}
Exkit.WEIGHTS_K21={
	0.011694638867371874,
	0.032558162307964727,
	0.054755896574351996,
	0.075039674810919953,
	0.093125454583697606,
	0.109387158802297642,
	0.123491976262065851,
	0.134709217311473326,
	0.142775938577060081,
	0.14773910490133849,
	0.149445554002916906,
	0.14773910490133849,
	0.142775938577060081,
	0.134709217311473326,
	0.123491976262065851,
	0.109387158802297642,
	0.093125454583697606,
	0.075039674810919953,
	0.054755896574351996,
	0.032558162307964727,
	0.011694638867371874
}
Exkit.WEIGHTS_G10={
	0,
	0.066671344308688138,
	0,
	0.149451349150580593,
	0,
	0.219086362515982044,
	0,
	0.269266719309996355,
	0,
	0.29552422471475287,
	0,
	0.29552422471475287,
	0,
	0.269266719309996355,
	0,
	0.219086362515982044,
	0,
	0.149451349150580593,
	0,
	0.066671344308688138,
	0
}

function Exkit.gauss_kronrod(f,a,b)
	local dx,integral=(b-a)/2,0
	for i=1,#Exkit.ABSCISSAS_K21 do
		integral=integral+f(a+(Exkit.ABSCISSAS_K21[i]+1)*dx)*Exkit.WEIGHTS_K21[i]
	end
	return integral*dx
end

function Exkit.gauss_kronrod_integrate(f,a,b)
	local dx,integral,err=(b-a)/2,0,0
	for i=1,#Exkit.ABSCISSAS_K21 do
		local val=f(a+(Exkit.ABSCISSAS_K21[i]+1)*dx)
		integral=integral+val*Exkit.WEIGHTS_K21[i]
		err=err+val*Exkit.WEIGHTS_G10[i]
	end
	err=math.abs(dx)*(200*math.abs(err-integral))^1.5
	return integral*dx,err
end

function Exkit.gauss_kronrod_adaptive(f,a,b,n,tol)--自适应Gauss-Kronrod积分
	local quad,err=Exkit.gauss_kronrod_integrate(f,a,b)
	local n,tol,parts=n or 10,tol or 1e-12,{{q=quad,err=err,l=0,r=b}}
	for len=1,n do
		local err2,total,err_max,MAX=0,0,0,1
		for i=1,#parts do
			total=total+parts[i].q
			if parts[i].err>err_max then
				err_max=parts[i].err MAX=i
			end
			err2=err2+parts[i].err*parts[i].err
		end
		if math.abs(err2^0.5/total)<tol or len==n then
			return total,err2,len
		end
		local l,r=parts[MAX].l,parts[MAX].r local m=l+(r-l)/2
		local q_l,err_l=Exkit.gauss_kronrod_integrate(f,l,m) parts[MAX].r=m
		local q_r,err_r=Exkit.gauss_kronrod_integrate(f,m,r) parts[MAX].q=q_l
		parts[MAX].err=err_l parts[#parts+1]={} parts[#parts].l=m
		parts[#parts].r=r parts[#parts].q=q_r parts[#parts].err=err_r
	end
end

function Exkit.curve_length(x,y,t1,t2,x1,y1)
	local function func(t)
		local vx,vy=x1 and x1(t) or Exkit.derivative_1st(x,t),y1 and y1(t) or Exkit.derivative_1st(y,t)
		return (vx^2+vy^2)^0.5
	end
	return Exkit.gauss_kronrod(func,t1,t2)
end

function Exkit.curve_t_at_s(x,y,t1,t2,s,L,x1,y1)
	local arc_len=s*L
	local function func(u)
		return Exkit.curve_length(x,y,t1,t1+u,x1,y1)-arc_len
	end
	return Exkit.modAB(func,0,t2-t1,1e-5)
end

function Exkit.curve_to_px(x,y,t1,t2,ds,n,accel,x1,y1)--匀速化一般参数曲线(精准快速)
	local n,px,accel,ds=n or 16,{},accel or 1,ds or 1
	local d,cnt,len=(t2-t1)/n,{},0
	for i=1,n do
		local L=Exkit.curve_length(x,y,t1+(i-1)*d,t1+i*d,x1,y1) cnt[i]=L len=len+L
	end
	local N=math.ceil(len/ds)
	for i=1,N do
		local a=type(accel)=='function' and accel(i,N) or accel local at_len,L=len*((i-1)/(N-1))^a,0
		for k=1,n do
			local L0=L L=L+cnt[k]
			if at_len>L then
			else
				local s=(at_len-L0)/cnt[k]
				local t=t1+(k-1)*d+Exkit.curve_t_at_s(x,y,t1+(k-1)*d,t1+k*d,s,cnt[k],x1,y1)
				local vx,vy=x1 and x1(t) or Exkit.derivative_1st(x,t),y1 and y1(t) or Exkit.derivative_1st(y,t)
				local angle=-Exkit.argz(vx,vy) local pt_x,pt_y=x(t),y(t)
				px[#px+1]={x=Exkit.Mround2(pt_x),y=Exkit.Mround2(pt_y),angle=Exkit.Mround2(angle)} break
			end
		end
	end
	return px
end

function Exkit.curve_to_px_raw(x,y,t1,t2,N,x1,y1)--原速的参数曲线
	local px={}
	for i=1,N do
		local t=t1+(i-1)/(N-1)*(t2-t1)
		local vx,vy=x1 and x1(t) or Exkit.derivative_1st(x,t),y1 and y1(t) or Exkit.derivative_1st(y,t)
		local angle=-Exkit.argz(vx,vy) local pt_x,pt_y=x(t),y(t)
		px[#px+1]={x=Exkit.Mround2(pt_x),y=Exkit.Mround2(pt_y),angle=Exkit.Mround2(angle)}
	end
	return px
end

function Exkit.Bezier(x,y,t)
	local pos_x,pos_y,n=0,0,#x
	for i=1,n do
		pos_x=pos_x+x[i]*t^(i-1)*((1-t)^(n-i))*Exkit.combination(n-1,i-1)
		pos_y=pos_y+y[i]*t^(i-1)*((1-t)^(n-i))*Exkit.combination(n-1,i-1)
	end
	return pos_x,pos_y
end

function Exkit.Bezier_derivative_1st(x,y,t)
	local vx,vy,n=0,0,#x
	for i=1,n-1 do
		vx=vx+(x[i+1]-x[i])*t^(i-1)*((1-t)^(n-i-1))*Exkit.combination(n-2,i-1)
		vy=vy+(y[i+1]-y[i])*t^(i-1)*((1-t)^(n-i-1))*Exkit.combination(n-2,i-1)
	end
	return (n-1)*vx,(n-1)*vy
end

function Exkit.sgn(n)
	return n<0 and -1 or n>0 and 1 or 0
end

function Exkit.permutation(n,m)
	local p=1 for i=n,n-m+1,-1 do p=p*i end
	return p
end

function Exkit.combination(a,b)
	return Exkit.permutation(a,b)/Exkit.permutation(b,b)
end

function Exkit.Bezier_length(x,y,t1,t2)
	local function f(u)
		local vx,vy=Exkit.Bezier_derivative_1st(x,y,u)
		return (vx^2+vy^2)^0.5
	end
	return Exkit.gauss_kronrod(f,t1,t2)
end

function Exkit.Bezier_t_at_s(x,y,t1,t2,s,L)
	local arc_len=s*L
	local function f(u)
		return Exkit.Bezier_length(x,y,t1,t1+u)-arc_len
	end
	return Exkit.modAB(f,0,t2-t1,1e-5)
end

function Exkit.Bezier_to_px(x,y,ds,accel,n)--匀速n阶贝塞尔曲线(精准快速)
	local n,px,accel,ds=n or 8,{},accel or 1,ds or 1
	local d,cnt,len=1/n,{},0
	for i=1,n do
		local L=Exkit.Bezier_length(x,y,(i-1)*d,i*d) cnt[i]=L len=len+L
	end
	local N=math.ceil(len/ds)
	for i=1,N do
		local a=type(accel)=='function' and accel(i,N) or accel local at_len,L=len*((i-1)/(N-1))^a,0
		for k=1,n do
			local L0=L L=L+cnt[k]
			if at_len>L then
			else local s=(at_len-L0)/cnt[k]
				local t=(k-1)*d+Exkit.Bezier_t_at_s(x,y,(k-1)*d,k*d,s,cnt[k])
				local vx,vy=Exkit.Bezier_derivative_1st(x,y,t)
				local angle=-Exkit.argz(vx,vy) local pt_x,pt_y=Exkit.Bezier(x,y,t)
				px[#px+1]={x=Exkit.Mround2(pt_x),y=Exkit.Mround2(pt_y),angle=Exkit.Mround2(angle)} break
			end
		end
	end
	return px
end

function Exkit.Bezier_to_px_raw(x,y,N)--原始速度的n阶贝塞尔曲线
	local px={}
	for i=1,N do
		local t=(i-1)/(N-1) local vx,vy=Exkit.Bezier_derivative_1st(x,y,t)
		local angle=-Exkit.argz(vx,vy) local pt_x,pt_y=Exkit.Bezier(x,y,t)
		px[#px+1]={x=Exkit.Mround2(pt_x),y=Exkit.Mround2(pt_y),angle=Exkit.Mround2(angle)}
	end
	return px
end

function Exkit.Bezier_length_at_t(x,y,t)--n阶贝塞尔曲线在t处的长度
	local function f(u)
		local vx,vy=Exkit.Bezier_derivative_1st(x,y,u)
		return (vx^2+vy^2)^0.5
	end
	local quad=0
	for i=1,16 do
		quad=quad+Exkit.gauss_kronrod(f,(i-1)*t*0.0625,i*t*0.0625)
	end
	return quad
end

function Exkit.smoothstep(t,s,e)
	if s==nil or e==nil then
		return 3*t^2-2*t^3
	else
		t=(t-s)/(e-s) t=t>1 and 1 or t<0 and 0 or t
		return 3*t^2-2*t^3
	end
end






_G.Exkit=Exkit

return _G.Exkit

