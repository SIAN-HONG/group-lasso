#=======================p=30 ,group size =3
n_edge=11
set.seed(5454)
#=======================p=60,group size =3
#n_edge=20
#set.seed(2604)
#=======================p=90,group size =3
#n_edge=34
#set.seed(5083)
p=90
ng=3
for(g in 9:12){
  for(f in 1:10000){
    A1=matrix(0,p,p)
    n_edge=g
    set.seed(f)
    seed=sample(1:(p*p/ng/ng),n_edge)
    i=floor(seed/(p/ng))
    j=seed-i*(p/ng)
    for(k in 1:n_edge){
      if(j[k]==0){
        i[k]=i[k]-1
        j[k]=(p/ng)
        #A1[((j[k]-1)*ng+1):(j[k]*ng),(i[k]*ng+1):((i[k]+1)*ng)]=matrix(rnorm(ng*ng),ng,ng)
        A1[((j[k]-1)*ng+1):(j[k]*ng),(i[k]*ng+1):((i[k]+1)*ng)]=matrix(((rbinom(ng*ng,1,0.5)*2-1)*runif(ng*ng,0.6,1)),ng,ng)
      }else{
        #A1[((j[k]-1)*ng+1):(j[k]*ng),(i[k]*ng+1):((i[k]+1)*ng)]=matrix(rnorm(ng*ng),ng,ng)
        A1[((j[k]-1)*ng+1):(j[k]*ng),(i[k]*ng+1):((i[k]+1)*ng)]=matrix(((rbinom(ng*ng,1,0.5)*2-1)*runif(ng*ng,0.6,1)),ng,ng)
        }
    }
    eigen_A1=(abs(eigen(A1)$values)<0.9999999999)
    
    #rowsum=apply(A1,1,sum)>0
    if(sum(eigen_A1)==p && sum(eigen(A1)$values!=0)>13){
      print(c("rnorm",g,f,sum(eigen(A1)$values!=0)))
    }
  }
}
A1=matrix(0,p,p)
n_edge=34
set.seed(5083)
seed=sample(1:(p*p/ng/ng),n_edge)
i=floor(seed/(p/ng))
j=seed-i*(p/ng)
for(k in 1:n_edge){
  if(j[k]==0){
    i[k]=i[k]-1
    j[k]=(p/ng)
    #A1[((j[k]-1)*ng+1):(j[k]*ng),(i[k]*ng+1):((i[k]+1)*ng)]=matrix(rnorm(ng*ng),ng,ng)
    A1[((j[k]-1)*ng+1):(j[k]*ng),(i[k]*ng+1):((i[k]+1)*ng)]=matrix(((rbinom(ng*ng,1,0.5)*2-1)*runif(ng*ng,0.6,1)),ng,ng)
  }else{
    #A1[((j[k]-1)*ng+1):(j[k]*ng),(i[k]*ng+1):((i[k]+1)*ng)]=matrix(rnorm(ng*ng),ng,ng)
    A1[((j[k]-1)*ng+1):(j[k]*ng),(i[k]*ng+1):((i[k]+1)*ng)]=matrix(((rbinom(ng*ng,1,0.5)*2-1)*runif(ng*ng,0.6,1)),ng,ng)
  }
}
abs(eigen(A1)$values)
abs(A1[A1!=0])
library(gplots)
dim(A1)
sum(A1!=0)
sum(rowSums(A1)==0)/p
heatmap.2(matrix(as.numeric(abs(A1)>0),p,p),main = 'True',scale = "none",Rowv = F,Colv = F,trace='none',dendrogram="none",col=colorpanel(2*N,"white","darkgrey","black"))
