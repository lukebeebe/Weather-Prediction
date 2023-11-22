maketable.pck <-
c("maketable.pck", "make.table.ad1.ext", "make.table.fdr.ext", 
"format.table.natsnp.byseas.ad1", "format.table.natsnp.byseas.a.alt", 
"lik.mix.ratcorr1e.byseas.ad1", "pinttrans.check", "pinttrans", 
"lik.mix.ratcorr1e.byseas.a.alt", "fdr", "llik.check.perm", "my.paper.figure1", 
"my.paper.figure2", "pvalplotpred2", "pvalplotpred1", "makefig2tot", 
"makefig2", "format.table.natsnp.byseas.a", "lik.mix.ratcorr1e.byseas.a"
)
make.table.ad1.ext <-
function(Temp=T,Hawaii=T){
xnatsnp.12.llik.t.TT<-format.table.natsnp.byseas.ad1(my.natsnpalt.1.2.clean,1,2,Temp,Hawaii,.2)
xnatsnp.36.llik.t.TT<-format.table.natsnp.byseas.ad1(my.natsnpalt.3.6.clean,3,6,Temp,Hawaii,.2)
xnatsnp.7.llik.t.TT<-format.table.natsnp.byseas.ad1(my.natsnpalt.7.7.clean,7,7,Temp,Hawaii,.2)
xnatsnp.8.llik.t.TT<-format.table.natsnp.byseas.ad1(my.natsnpalt.8.8.clean,8,8,Temp,Hawaii,.2)
xnatsnp.910.llik.t.TT<-format.table.natsnp.byseas.ad1(my.natsnpalt.9.10.clean,9,10,Temp,Hawaii,.2)
xnatsnp.11.llik.t.TT<-format.table.natsnp.byseas.ad1(my.natsnpalt.11.11.clean,11,11,Temp,Hawaii,.2)
xnatsnp.t12.llik.t.TT<-format.table.natsnp.byseas.ad1(my.natsnpalt.12.12.clean,12,12,Temp,Hawaii,.2)


rbind(xnatsnp.12.llik.t.TT,xnatsnp.36.llik.t.TT,xnatsnp.7.llik.t.TT,xnatsnp.8.llik.t.TT,xnatsnp.910.llik.t.TT,xnatsnp.11.llik.t.TT,xnatsnp.t12.llik.t.TT)
}
make.table.fdr.ext <-
function(Temp=T,Hawaii=T,NP=F,altlik=F){
if(altlik){
xnatsnp.12.llik.t.TT<-format.table.natsnp.byseas.a.alt(my.natsnpalt.1.2,1,2,Temp,Hawaii,.2,NP)
xnatsnp.36.llik.t.TT<-format.table.natsnp.byseas.a.alt(my.natsnpalt.3.6,3,6,Temp,Hawaii,.2,NP)
xnatsnp.79.llik.t.TT<-format.table.natsnp.byseas.a.alt(my.natsnpalt.7.9,7,9,Temp,Hawaii,.2,NP)
xnatsnp.10.llik.t.TT<-format.table.natsnp.byseas.a.alt(my.natsnpalt.10,10,10,Temp,Hawaii,.2,NP)
xnatsnp.11.llik.t.TT<-format.table.natsnp.byseas.a.alt(my.natsnpalt.11,11,11,Temp,Hawaii,.2,NP)
xnatsnp.t12.llik.t.TT<-format.table.natsnp.byseas.a.alt(my.natsnpalt.12,12,12,Temp,Hawaii,.2,NP)
}else{
xnatsnp.12.llik.t.TT<-format.table.natsnp.byseas.a(my.natsnpalt.1.2.clean,1,2,Temp,Hawaii,.2)
xnatsnp.36.llik.t.TT<-format.table.natsnp.byseas.a(my.natsnpalt.3.6.clean,3,6,Temp,Hawaii,.2)
xnatsnp.7.llik.t.TT<-format.table.natsnp.byseas.a(my.natsnpalt.7.7.clean,7,7,Temp,Hawaii,.2)
xnatsnp.8.llik.t.TT<-format.table.natsnp.byseas.a(my.natsnpalt.8.8.clean,8,8,Temp,Hawaii,.2)
xnatsnp.910.llik.t.TT<-format.table.natsnp.byseas.a(my.natsnpalt.9.10.clean,9,10,Temp,Hawaii,.2)
xnatsnp.11.llik.t.TT<-format.table.natsnp.byseas.a(my.natsnpalt.11.11.clean,11,11,Temp,Hawaii,.2)
xnatsnp.t12.llik.t.TT<-format.table.natsnp.byseas.a(my.natsnpalt.12.12.clean,12,12,Temp,Hawaii,.2)
}

rbind(xnatsnp.12.llik.t.TT,xnatsnp.36.llik.t.TT,xnatsnp.7.llik.t.TT,xnatsnp.8.llik.t.TT,xnatsnp.910.llik.t.TT,xnatsnp.11.llik.t.TT,xnatsnp.t12.llik.t.TT)
}
format.table.natsnp.byseas.ad1 <-
function(strA=my.natsnpalt.12,i1=12,k1=12,Temp=F,Hawaii=T,Q=.1,NP=F,normlik=T){
mat<-NULL
#format.table.natsnp.byseas.ad1(my.natsnpalt.3.6,3,6,T,T,.2,T,T)
# attributes(meipdosnpnat1200.1.2[[1]]$Temp[[1]][[2]][[1]][[2]][[1]]$dum$Fresnofull)
#$names
#[1] "nsnsp" "snp"
par(mfrow=c(3,3))
altlik<-normlik
for(i in c(i1:k1)){
	Prowvectemp<-NULL
	Likrowvectemp<-NULL	
	#
if(Temp){
	str1<-strA[[i]]$Temp
}else{
	str1<-strA[[i]]$Precip
}
for(j in 1:4){
print(j)
		if(j==1){
			mat12<-NULL
			mat34<-NULL
			mat56<-NULL
			mat0<-NULL
		}
		doh<-lik.mix.ratcorr1e.byseas.ad1(str1,Hawaii,j,np=NP,altlik=altlik)
		#doh<-lik.mix.ratcorr1e.byseas.y(str1,Hawaii,j)
		rowvec<-c(i,j,doh$out)
#print(apply(doh$seaslik,1,sum))
		mat12<-rbind(mat12,doh$seaslik)
		#mat34<-rbind(mat34,doh$seaslik[2,])
		#mat56<-rbind(mat56,doh$seaslik[3,])
		mat<-rbind(mat,rowvec)
		mat0<-rbind(mat0,rowvec)
}
#FIX STARTING HERE
j<-5
n1<-length(c(mat12))
#print(n1)
nnnn1<-n1/4
v000<-c(0:(nnnn1-1))*4
vec12<-rep(0,n1)
#vec34<-rep(0,n1)
#vec56<-rep(0,n1)
for(j1 in 1:4){
	vec12[v000+(j1)]<-mat12[j1,]
	#vec34[v000+(j1)]<-mat34[j1,]
	#vec56[v000+(j1)]<-mat56[j1,]

}
#print(sum(mat12))
#print(c(sum(vec12),sum(mat0[,3]-mat0[,4])))

#v12<-vec12
#v12acf<-acf(v12,type="covariance",plot=F)$acf
#naa<-ceiling(sqrt(length(v12)))
#vv120<-var((v12))*(length(v12)-1)*c(1,2*cumsum(v12acf[-1][1:naa]))
#vv12<-max(vv120,na.rm=T)
#vv12<-vv120[1]
#tv12<-sum(v12)/sqrt(vv12)
#pv12<-pt(tv12,length(v12)-1)
#rowvec<-c(i,j,0,sum(vec12),pv12)
#mat<-rbind(mat,rowvec)
# do rest later, distributing calculating
}
#print(dim(mat))
#print(mat)
#I1<-mat[,2]!=5
#mat<-mat[I1,]
#print(mat)
#m1<-mat[,c(5,8,11)]
#par(mfrow=c(2,2))
dum<-fdr(c(mat[,c(5)]),Q,ind=F)
dum<-fdr(c(mat[,c(6,7)]),Q,ind=F)
mat
}
format.table.natsnp.byseas.a.alt <-
function(strA,i1,k1,Temp=T,Hawaii=T,Q=.1,NP=F){
mat<-NULL
# attributes(meipdosnpnat1200.1.2[[1]]$Temp[[1]][[2]][[1]][[2]][[1]]$dum$Fresnofull)
#$names
#[1] "nsnsp" "snp"
par(mfrow=c(3,3))
for(i in c(i1:k1)){
	Prowvectemp<-NULL
	Likrowvectemp<-NULL	
	#
if(Temp){
	str1<-strA[[i]]$Temp
}else{
	str1<-strA[[i]]$Precip
}
for(j in 1:4){
print(j)
		if(j==1){
			mat12<-NULL
			mat34<-NULL
			mat56<-NULL
			mat0<-NULL
		}
		doh<-lik.mix.ratcorr1e.byseas.a.alt(str1,Hawaii,j,np=NP)
		#doh<-lik.mix.ratcorr1e.byseas.y(str1,Hawaii,j)
		rowvec<-c(i,j,doh$out)
print(apply(doh$seaslik,1,sum))
		mat12<-rbind(mat12,doh$seaslik)
		#mat34<-rbind(mat34,doh$seaslik[2,])
		#mat56<-rbind(mat56,doh$seaslik[3,])
		mat<-rbind(mat,rowvec)
		mat0<-rbind(mat0,rowvec)
}
#FIX STARTING HERE
j<-5
n1<-length(c(mat12))
print(n1)
nnnn1<-n1/4
v000<-c(0:(nnnn1-1))*4
vec12<-rep(0,n1)
#vec34<-rep(0,n1)
#vec56<-rep(0,n1)
for(j1 in 1:4){
	vec12[v000+(j1)]<-mat12[j1,]
	#vec34[v000+(j1)]<-mat34[j1,]
	#vec56[v000+(j1)]<-mat56[j1,]

}
#print(sum(mat12))
#print(c(sum(vec12),sum(mat0[,3]-mat0[,4])))

v12<-vec12
v12acf<-acf(v12,type="covariance",plot=F)$acf
naa<-ceiling(sqrt(length(v12)))
vv120<-var((v12))*(length(v12)-1)*c(1,2*cumsum(v12acf[-1][1:naa]))
vv12<-max(vv120,na.rm=T)
#vv12<-vv120[1]
tv12<-sum(v12)/sqrt(vv12)
pv12<-pt(tv12,length(v12)-1)
rowvec<-c(i,j,0,sum(vec12),pv12)
mat<-rbind(mat,rowvec)
# do rest later, distributing calculating
}
#print(dim(mat))
#print(mat)
I1<-mat[,2]!=5
mat<-mat[I1,]
#print(mat)
#m1<-mat[,c(5,8,11)]
#par(mfrow=c(2,2))
dum<-fdr(c(mat[,c(5)]),Q,ind=F)
dum<-fdr(c(mat[,c(6,7)]),Q,ind=F)
mat
}
lik.mix.ratcorr1e.byseas.ad1 <-
function(str1,Hawaii=T,j,np=F,altlik=F){
#[[1]][[2]][[1]][[2]][[4]]$dum
#FINISH FIXING comb in this and do in llik.check.perm1
comb<-F
if(Hawaii){
		if(comb){
			v1aa<-str1[[1]][[2]][[1]][[2]][[j]]$dum$Hawaiifull$nsnsp$llika
			v1ab<-str1[[1]][[2]][[1]][[2]][[j]]$dum$Hawaiifull$nsnsp$llik
			#print(v1aa)
			#print(v1ab)
			v1a<-log((exp(v1aa)+exp(v1ab))/2)
		}else{
			if(altlik){
				v1a<-str1[[1]][[2]][[1]][[2]][[j]]$dum$Hawaiifull$nsnsp$llika
			}else{
				v1a<-str1[[1]][[2]][[1]][[2]][[j]]$dum$Hawaiifull$nsnsp$llik
			}
		}
		d1a<-str1[[1]][[2]][[1]][[2]][[j]]$dum$Hawaiifull$nsnsp$dens
		#v1a<-v1a-listllikcorrect1(d1a,altlik)
		n1<-length(v1a)
		if(comb){
			v2aa<-str1[[1]][[2]][[1]][[2]][[j]]$dum$Hawaiifull$snp$llika
			v2ab<-str1[[1]][[2]][[1]][[2]][[j]]$dum$Hawaiifull$snp$llik
			#print(v2aa)
			#print(v2ab)
			v2a<-log((exp(v2aa)+exp(v2ab))/2)
		}else{
			if(altlik){
				v2a<-str1[[1]][[2]][[1]][[2]][[j]]$dum$Hawaiifull$snp$llika
			}else{
				v2a<-str1[[1]][[2]][[1]][[2]][[j]]$dum$Hawaiifull$snp$llik
			}
		}
		
		d2a<-str1[[1]][[2]][[1]][[2]][[j]]$dum$Hawaiifull$snp$dens
		y<-str1[[1]][[2]][[1]][[2]][[j]]$dum$Hawaiifull$snp$y
		#v2a<-v2a-listllikcorrect1(d2a,altlik)
		n2<-length(v2a)
		
}else{
		if(comb){
			v1aa<-str1[[1]][[2]][[1]][[2]][[j]]$dum$Fresnofull$nsnsp$llika
			v1ab<-str1[[1]][[2]][[1]][[2]][[j]]$dum$Fresnofull$nsnsp$llik
			#print(v1aa)
			#print(v1ab)
			v1a<-log((exp(v1aa)+exp(v1ab))/2)
		}else{
			if(altlik){
				v1a<-str1[[1]][[2]][[1]][[2]][[j]]$dum$Fresnofull$nsnsp$llika
			}else{
				v1a<-str1[[1]][[2]][[1]][[2]][[j]]$dum$Fresnofull$nsnsp$llik
			}
		}
		d1a<-str1[[1]][[2]][[1]][[2]][[j]]$dum$Fresnofull$nsnsp$dens
		#v1a<-v1a-listllikcorrect1(d1a,altlik)
		n1<-length(v1a)
		if(comb){
			v2aa<-str1[[1]][[2]][[1]][[2]][[j]]$dum$Fresnofull$snp$llika
			v2ab<-str1[[1]][[2]][[1]][[2]][[j]]$dum$Fresnofull$snp$llik
			#print(v2aa)
			#print(v2ab)
			v2a<-log((exp(v2aa)+exp(v2ab))/2)
		}else{
			if(altlik){
				v2a<-str1[[1]][[2]][[1]][[2]][[j]]$dum$Fresnofull$snp$llika
			}else{
				v2a<-str1[[1]][[2]][[1]][[2]][[j]]$dum$Fresnofull$snp$llik
			}
		}
		
		d2a<-str1[[1]][[2]][[1]][[2]][[j]]$dum$Hawaiifull$snp$dens
		y<-str1[[1]][[2]][[1]][[2]][[j]]$dum$Hawaiifull$snp$y
		#v2a<-v2a-listllikcorrect1(d2a,altlik)
		n2<-length(v2a)

            
		
}
#print(v1a)
#print(v2a)
#print(d1a)
#plot(y1a,y2a)
#plist<-llik.check.perm1(y,d1a,d2a,altlik,comb)
plist<-pinttrans.check(y,d1a,d2a,altlik)

#print(plist)
		n0<-min(n1,n2)
print(c(n1,n2))
		n1s<-n1-n0
		n2s<-n2-n0
		v1<-v1a[(n1s+1):(n1-4)]
		v2<-v2a[(n2s+1):(n2-4)]
		v1b<-v1a[c((n1-3):n1)]
		v2b<-v2a[c((n2-3):n2)]

		
v1<-c(v1)
v2<-c(v2)
#NOTE SECOND PVALUE SMALL IF ALTERNATIVE MUCH BETTER THAN NATURAL direct comparison
v12<-v1-v2
v12acf<-acf(v2,type="covariance",plot=F)$acf
naa<-ceiling(sqrt(length(v12)))
vv120<-var((v12))*(length(v12)-1)*c(1,2*cumsum(v12acf[-1][1:naa]))
vv12<-max(vv120,na.rm=T)
tv12<-sum(v12)/sqrt(vv12)
if(np){
pv12<-wilcox.test(clean.w(v12),alt="less")$p.value
}else{
pv12<-pt(tv12,n0-1)
}
#par(mfrow=c(2,2))
#dum<-acf(v12)
#dum<-acf(v34)
#dum<-acf(v56)

#print(dum)
#print(length(v32))
#dum<-acf(v32)
#print(dum)
#add names below
fullnsp<-sum(v1)
fullsp<-sum(v2)
fullP<-pv12
vA<-log((exp(v1)+exp(v2))/2)
fullav<-sum(vA)
vB<-log((exp(v1b)+exp(v2b))/2)
fullb<-sum(vB)
list(out=c(fullnsp,fullsp,fullP,plist$p1,plist$p2,fullav,sum(v1b),sum(v2b),fullb),seaslik=rbind(v12))
}
pinttrans.check <-
function(y,d1a,d2a,altlik){
nperm<-100
#y<-str1[[1]][[2]][[1]][[2]][[j]]$dum$Hawaiifull$nsnsp$y
d01llik<-NULL
d02llik<-NULL
#d1a<-str1[[1]][[2]][[1]][[2]][[j]]$dum$Hawaiifull$nsnsp$dens
#d2a<-str1[[1]][[2]][[1]][[2]][[j]]$dum$Hawaiifull$snp$dens


if(altlik){
for(i in 1:length(y)){
	d01llik<-c(d01llik,pinttrans(y[i],d1a[[i]]$altdens))
	d02llik<-c(d02llik,pinttrans(y[i],d2a[[i]]$altdens))
}
}else{
for(i in 1:length(y)){
	d01llik<-c(d01llik,pinttrans(y[i],d1a[[i]]$dens))
	d02llik<-c(d02llik,pinttrans(y[i],d2a[[i]]$dens))

}
}

p1perm<-ks.test(d01llik,punif,0,1)$p
p2perm<-ks.test(d02llik,punif,0,1)$p

list(p1=p1perm,p2=p2perm)
}
pinttrans <-
function(y0,dens0){
integrand<-function(x){approx(dens0$x,dens0$y,x,yleft=1e-50,yright=1e-50,rule=2)$y}
#log(integrate(integrand,-10,10,subdivisions=250L)$val)
#print("HEY")
#plot(dens0$x,dens0$y)
#log(integrate(integrand,min(dens0$x)-.1,max(dens0$x)+.1,subdivisions=100L)$val)
dum1<-(integrate(integrand,-Inf,y0,subdivisions=500L,stop.on.error=F)$val)
dum2<-(integrate(integrand,-Inf,Inf,subdivisions=500L,stop.on.error=F)$val)

#print(dum)
dum1/dum2

}
lik.mix.ratcorr1e.byseas.a.alt <-
function(str1,Hawaii=T,j,np=F){
#[[1]][[2]][[1]][[2]][[4]]$dum
if(Hawaii){
		v1a<-str1[[1]][[2]][[1]][[2]][[j]]$dum$Hawaiifull$nsnsp$llika
		d1a<-str1[[1]][[2]][[1]][[2]][[j]]$dum$Hawaiifull$nsnsp$dens
		v1a<-v1a-listllikcorrect(d1a)
		n1<-length(v1a)
		v2a<-str1[[1]][[2]][[1]][[2]][[j]]$dum$Hawaiifull$snp$llika
		d2a<-str1[[1]][[2]][[1]][[2]][[j]]$dum$Hawaiifull$snp$dens
		y<-str1[[1]][[2]][[1]][[2]][[j]]$dum$Hawaiifull$snp$y
		v2a<-v2a-listllikcorrect(d2a)
		n2<-length(v2a)
		
}else{
		v1a<-str1[[1]][[2]][[1]][[2]][[j]]$dum$Fresnofull$nsnsp$llika
		d1a<-str1[[1]][[2]][[1]][[2]][[j]]$dum$Fresnofull$nsnsp$dens
		v1a<-v1a-listllikcorrect(d1a)
		n1<-length(v1a)
		v2a<-str1[[1]][[2]][[1]][[2]][[j]]$dum$Fresnofull$snp$llika
		d2a<-str1[[1]][[2]][[1]][[2]][[j]]$dum$Fresnofull$snp$dens
		y<-str1[[1]][[2]][[1]][[2]][[j]]$dum$Fresnofull$snp$y
		v2a<-v2a-listllikcorrect(d2a)
		n2<-length(v2a)
		
}
#plot(y1a,y2a)
plist<-llik.check.perm(y,d1a,d2a)
		n0<-min(n1,n2)
print(c(n1,n2))
		n1s<-n1-n0
		n2s<-n2-n0
		v1<-v1a[(n1s+1):n1]
		v2<-v2a[(n2s+1):n2]
		
v1<-c(v1)
v2<-c(v2)
#NOTE SECOND PVALUE SMALL IF ALTERNATIVE MUCH BETTER THAN NATURAL direct comparison
v12<-v1-v2
v12acf<-acf(v2,type="covariance",plot=F)$acf
naa<-ceiling(sqrt(length(v12)))
vv120<-var((v12))*(length(v12)-1)*c(1,2*cumsum(v12acf[-1][1:naa]))
vv12<-max(vv120,na.rm=T)
tv12<-sum(v12)/sqrt(vv12)
if(np){
pv12<-wilcox.test(v12,alt="less")$p.value
}else{
pv12<-pt(tv12,n0-1)
}
#par(mfrow=c(2,2))
#dum<-acf(v12)
#dum<-acf(v34)
#dum<-acf(v56)

#print(dum)
#print(length(v32))
#dum<-acf(v32)
#print(dum)
#add names below
fullnsp<-sum(v1)
fullsp<-sum(v2)
fullP<-pv12
vA<-log((exp(v1)+exp(v2))/2)
fullav<-sum(vA)
list(out=c(fullnsp,fullsp,fullP,plist$p1,plist$p2,fullav),seaslik=rbind(v12))
}
fdr <-
function(v0,Q,ind=F,do.plot=T){
	n1<-length(v0)
	nvec<-c(1:n1)
	nvec1<-nvec[!is.na(v0)]
		v1<-v0[!is.na(v0)]
        o1<-order(v1)
        pvec<-v1[o1]
        m<-length(v1)
        qline<-Q*c(1:m)/m
	if(!ind){
		qline<-qline/sum(1/c(1:m))
	}
pun<-ks.test(v0,"punif",0,1)$p
if(do.plot){
        plot(c(c(1:m),c(1:m)),c(qline,pvec),type="n",xlab="ordering",ylab="pvalue",main=paste("ks=",pun))
        lines(c(1:m),qline)
        points(c(1:m),pvec)
}
        dv<-pvec-qline
        I1<-(dv<0)
        pmax<-max(pvec[I1])
        I2<-pvec<=pmax
if(do.plot){
        points(c(1:m)[I2],pvec[I2],col="red")
}	  
        list(Interesting=nvec1[o1[I2]],Pstar=pmax)

}
llik.check.perm <-
function(y,d1a,d2a){
nperm<-100
#y<-str1[[1]][[2]][[1]][[2]][[j]]$dum$Hawaiifull$nsnsp$y
d01llik<-0
d02llik<-0
#d1a<-str1[[1]][[2]][[1]][[2]][[j]]$dum$Hawaiifull$nsnsp$dens
#d2a<-str1[[1]][[2]][[1]][[2]][[j]]$dum$Hawaiifull$snp$dens
for(i in 1:length(y)){
	d01llik<-d01llik+analog.llik(y[i],d1a[[i]]$dens)
	d02llik<-d02llik+analog.llik(y[i],d2a[[i]]$dens)
}
dperm1llik<-NULL
dperm2llik<-NULL
for(j in 1:nperm){
	ny<-length(y)
     ystar<-y[sample(ny)]
	duhh1<-0
	duhh2<-0
for(i in 1:length(y)){
	duhh1<-duhh1+analog.llik(ystar[i],d1a[[i]]$dens)
	duhh2<-duhh2+analog.llik(ystar[i],d2a[[i]]$dens)
}
dperm1llik<-c(dperm1llik,duhh1)
dperm2llik<-c(dperm2llik,duhh2)
}
p1perm<-sum(dperm1llik>d01llik)/nperm
p2perm<-sum(dperm2llik>d02llik)/nperm
list(p1=p1perm,p2=p2perm)
}
my.paper.figure1 <-
function(){
par(mfrow=c(2,2))
duh1<-pvalplotpred1.clean(dum1.12.FT.clean,1,2,5,.2,0,xmain="Hawaii rain")
duh2<-pvalplotpred1.clean(dum1.12.TT.clean,1,2,5,.2,0,xmain="Hawaii Temperature")
duh3<-pvalplotpred1.clean(dum1.12.FF.clean,1,2,5,.2,0,xmain="Fresno rain")
duh4<-pvalplotpred1.clean(dum1.12.TF.clean,1,2,5,.2,0,xmain="Fresno Temperature")
n1<-duh1$agree+duh2$agree+duh3$agree+duh4$agree
n2<-4*48
1-pbinom(n1,n2,.5)
}
my.paper.figure2 <-
function(zed1,zed2,zed3,zed4,Q=.05){
par(mfrow=c(2,2))
pvalplotpred2(zed1,1,2,5,Q,xmain="Hawaii rain BH")
pvalplotpred2(zed2,1,2,5,Q,xmain="Hawaii Temperature BH")
pvalplotpred2(zed3,1,2,5,Q,xmain="Fresno rain BH")
pvalplotpred2(zed4,1,2,5,Q,xmain="Fresno Temperature BH")

}
pvalplotpred2 <-
function(mat1,lagcol,seascol,pcol,Qval=.2,xmain=""){
library(scatterplot3d)

m1<-mat1[,c(seascol,lagcol,pcol)]
#par(mfrow=c(1,1))
duh<-fdr(m1[,3],Qval,do.plot=F)
v1<-duh$Int
p1<-duh$Pstar
if(is.inf(-1*p1)){
p1<-NA
}
else{
p1<-ceiling(1e6*duh$Pstar)/1e6
}
icolvec<-rep(1,length(m1[,3]))
icolvec[v1]<-2
m1<-mat1[,c(seascol,lagcol,pcol)]
m1[,3]<-1-m1[,3]
duh<-fdr(m1[,3],Qval,do.plot=F)
v2<-duh$Int
p2<-duh$Pstar
if(is.inf(-1*p2)){
p2<-NA
}
else{
p2<-ceiling(1e6*duh$Pstar)/1e6
}
icolvec[v2]<-4
m0<-cbind(mat1[,c(seascol,lagcol)],1*(icolvec>1.5))
sctstr<-scatterplot3d(m0,type="n",main=paste(xmain,"\nP1*=",p1,"P2*=",p2))
sctstr$points3d(m0,col=icolvec,type="h")

}
pvalplotpred1 <-
function(mat,lagcol,seascol,pcol,Qval=.2,reg=0,xmain=""){
library(scatterplot3d)

m1<-mat[,c(seascol,lagcol,pcol)]
#par(mfrow=c(1,1))
v1<-fdr(m1[,3],Qval,do.plot=F)$Int
m1[,3]<--1*log(m1[,3]+1e-10)
icolvec<-rep(1,length(m1[,3]))
icolvec[mat[,pcol]<.05]<-3
icolvec[mat[,pcol]<.005]<-4
icolvec[v1]<-2
#sctstr<-scatterplot3d(m1,type="n")
#sctstr$points3d(m1,col=icolvec,type="h")
#print(c(i,nz,ztime[i],ztime[nz]))
m1<-mat[,c(seascol,lagcol,pcol)]
m1[,3]<-1-m1[,3]
#par(mfrow=c(2,2))
v2<-fdr(m1[,3],Qval,do.plot=F)$Int
m1[,3]<--1*log(m1[,3]+1e-10)
icolvec<-rep(1,length(m1[,3]))
icolvec[(1-mat[,pcol])<.05]<-3
icolvec[(1-mat[,pcol])<.005]<-4
icolvec[v2]<-2
#sctstr<-scatterplot3d(m1,type="n")
#sctstr$points3d(m1,col=icolvec,type="h")
#print(c(i,nz,ztime[i],ztime[nz]))
I1<-rep(0,length(mat[,1]))
I1[v1]<-1
I1[v2]<-(-1)
dum2<-cbind(mat,I1)
I1<-dum2[,8]>(apply(dum2[,c(3,4)],1,max)-reg)
I2<-dum2[,11]>(apply(dum2[,c(9,10)],1,max)-reg)
I3<-as.logical(1-I1)
c2<-sum(((dum2[I3,9]-dum2[I3,10])*dum2[I3,12])>0)
I4<-abs(dum2[,12])>0
c4<-sum(((dum2[I4,9]-dum2[I4,10])*dum2[I4,12])>0)
print(((dum2[I4,9]-dum2[I4,10])*dum2[I4,12])>0)
d4<-sum(I4)
c1<-sum(I1*I2)
d1<-sum(I1)
d2<-sum(abs(dum2[I3,12]))
z1<-sign(dum2[,3]-dum2[,4])*sign(dum2[,9]-dum2[,10])
agree<-sum(z1>0)
pval.pred=1-pbinom(agree-1,length(z1),.5)
m2<-cbind(mat[,c(seascol,lagcol)],z1)
icolvec2<-rep(1,length(m2[,3]))
icolvec2[(z1>0)&(dum2[,3]>dum2[,4])]<-4
icolvec2[(z1>0)&(dum2[,3]<dum2[,4])]<-2
n1<-sum(icolvec2==4)
n2<-sum(icolvec2==2)
pval.pred=pbinom(n1,n1+n2,.5)
sctstr<-scatterplot3d(m2,type="n",main=paste(xmain,"\n",pval.pred))
sctstr$points3d(m2,col=icolvec2,type="h")

if(is.na(c2)){c2<-c4}
list(mat=dum2,ntot=length(I1),ndec=c(d2,d4),ncorrect=c(c2,c4),agree=agree,pval.pred=1-pbinom(agree-1,length(z1),.5))
}
makefig2tot <-
function(seas.ah,seas){
par(mfrow=c(3,3))
 makefig2(seas.ah,seas,1,my.natsnpalt.3.6.clean)
 makefig2(seas.ah,seas,2,my.natsnpalt.3.6.clean)
 makefig2(seas.ah,seas,3,my.natsnpalt.3.6.clean)
 makefig2(seas.ah,seas,4,my.natsnpalt.3.6.clean)
 makefig2(seas.ah,seas,5,my.natsnpalt.3.6.clean)
 makefig2(seas.ah,seas,6,my.natsnpalt.3.6.clean)
 makefig2(seas.ah,seas,7,my.natsnpalt.3.6.clean)
 makefig2(seas.ah,seas,8,my.natsnpalt.3.6.clean)
 makefig2(seas.ah,seas,9,my.natsnpalt.3.6.clean)
}
makefig2 <-
function(seas.ahead=3,seas=2,year=1,data.str=my.natsnpalt.1.4){
main.str=paste(seas.ahead,"Seas ahead","year",year,"Summer")
plot(c(-5,5),c(0,1),main=main.str,type="n",xlab="Standardized Precipitation",ylab="Probability density")
y0<-data.str[[seas.ahead]]$Precip[[1]][[2]][[1]][[2]][[seas]]$dum$Fresnofull$nsnsp$y[year]
lines(data.str[[seas.ahead]]$Precip[[1]][[2]][[1]][[2]][[seas]]$dum$Fresnofull$nsnsp$dens[[year]]$dens)
z1<-analog.llik(y0,data.str[[seas.ahead]]$Precip[[1]][[2]][[1]][[2]][[seas]]$dum$Fresnofull$nsnsp$dens[[year]]$dens)
lines(data.str[[seas.ahead]]$Precip[[1]][[2]][[1]][[2]][[seas]]$dum$Fresnofull$snp$dens[[year]]$dens,col=2)
z2<-analog.llik(y0,data.str[[seas.ahead]]$Precip[[1]][[2]][[1]][[2]][[seas]]$dum$Fresnofull$snp$dens[[year]]$dens)
print(z1-z2)
lines(c(y0,y0),c(0,1),col=3)
}
format.table.natsnp.byseas.a <-
function(strA,i1,k1,Temp=T,Hawaii=T,Q=.1,NP=F){
mat<-NULL
# attributes(meipdosnpnat1200.1.2[[1]]$Temp[[1]][[2]][[1]][[2]][[1]]$dum$Fresnofull)
#$names
#[1] "nsnsp" "snp"
par(mfrow=c(3,3))
for(i in c(i1:k1)){
	Prowvectemp<-NULL
	Likrowvectemp<-NULL	
	#
if(Temp){
	str1<-strA[[i]]$Temp
}else{
	str1<-strA[[i]]$Precip
}
for(j in 1:4){
print(j)
		if(j==1){
			mat12<-NULL
			mat34<-NULL
			mat56<-NULL
			mat0<-NULL
		}
		doh<-lik.mix.ratcorr1e.byseas.a(str1,Hawaii,j,np=NP)
		#doh<-lik.mix.ratcorr1e.byseas.y(str1,Hawaii,j)
		rowvec<-c(i,j,doh$out)
print(apply(doh$seaslik,1,sum))
		mat12<-rbind(mat12,doh$seaslik)
		#mat34<-rbind(mat34,doh$seaslik[2,])
		#mat56<-rbind(mat56,doh$seaslik[3,])
		mat<-rbind(mat,rowvec)
		mat0<-rbind(mat0,rowvec)
}
#FIX STARTING HERE
j<-5
n1<-length(c(mat12))
print(n1)
nnnn1<-n1/4
v000<-c(0:(nnnn1-1))*4
vec12<-rep(0,n1)
#vec34<-rep(0,n1)
#vec56<-rep(0,n1)
for(j1 in 1:4){
	vec12[v000+(j1)]<-mat12[j1,]
	#vec34[v000+(j1)]<-mat34[j1,]
	#vec56[v000+(j1)]<-mat56[j1,]

}
#print(sum(mat12))
#print(c(sum(vec12),sum(mat0[,3]-mat0[,4])))

v12<-vec12
v12acf<-acf(v12,type="covariance",plot=F)$acf
naa<-ceiling(sqrt(length(v12)))
vv120<-var((v12))*(length(v12)-1)*c(1,2*cumsum(v12acf[-1][1:naa]))
vv12<-max(vv120,na.rm=T)
#vv12<-vv120[1]
tv12<-sum(v12)/sqrt(vv12)
pv12<-pt(tv12,length(v12)-1)
rowvec<-c(i,j,0,sum(vec12),pv12)
mat<-rbind(mat,rowvec)
# do rest later, distributing calculating
}
#print(dim(mat))
#print(mat)
I1<-mat[,2]!=5
mat<-mat[I1,]
#print(mat)
#m1<-mat[,c(5,8,11)]
#par(mfrow=c(2,2))
dum<-fdr(c(mat[,c(5)]),Q,ind=F)
dum<-fdr(c(mat[,c(6,7)]),Q,ind=F)
mat
}
lik.mix.ratcorr1e.byseas.a <-
function(str1,Hawaii=T,j,np=F){
#[[1]][[2]][[1]][[2]][[4]]$dum
if(Hawaii){
		v1a<-str1[[1]][[2]][[1]][[2]][[j]]$dum$Hawaiifull$nsnsp$llik
		
		n1<-length(v1a)
		v2a<-str1[[1]][[2]][[1]][[2]][[j]]$dum$Hawaiifull$snp$llik
		y<-str1[[1]][[2]][[1]][[2]][[j]]$dum$Hawaiifull$snp$y
		n2<-length(v2a)
		
}else{
		v1a<-str1[[1]][[2]][[1]][[2]][[j]]$dum$Fresnofull$nsnsp$llik
		n1<-length(v1a)
		v2a<-str1[[1]][[2]][[1]][[2]][[j]]$dum$Fresnofull$snp$llik
		y<-str1[[1]][[2]][[1]][[2]][[j]]$dum$Fresnofull$snp$y
		n2<-length(v2a)
		
}
#plot(y1a,y2a)
		n0<-min(n1,n2)
print(c(n1,n2))
		n1s<-n1-n0
		n2s<-n2-n0
		v1<-v1a[(n1s+1):n1]
		v2<-v2a[(n2s+1):n2]
		
v1<-c(v1)
v2<-c(v2)
#NOTE SECOND PVALUE SMALL IF ALTERNATIVE MUCH BETTER THAN NATURAL direct comparison
v12<-v1-v2
v12acf<-acf(v2,type="covariance",plot=F)$acf
naa<-ceiling(sqrt(length(v12)))
vv120<-var((v12))*(length(v12)-1)*c(1,2*cumsum(v12acf[-1][1:naa]))
vv12<-max(vv120,na.rm=T)
tv12<-sum(v12)/sqrt(vv12)
if(np){
pv12<-wilcox.test(v12,alt="less")$p.value
}else{
pv12<-pt(tv12,n0-1)
}
#par(mfrow=c(2,2))
#dum<-acf(v12)
#dum<-acf(v34)
#dum<-acf(v56)

#print(dum)
#print(length(v32))
#dum<-acf(v32)
#print(dum)
#add names below
fullnsp<-sum(v1)
fullsp<-sum(v2)
fullP<-pv12
vA<-log((exp(v1)+exp(v2))/2)
fullav<-sum(vA)
list(out=c(fullnsp,fullsp,fullP,.5,.5,fullav),seaslik=rbind(v12))
}
