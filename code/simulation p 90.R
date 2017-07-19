ptm <- proc.time()

library(timeDate)
library(grpreg)
library(tsDyn)
library(gplots)
library(parallel)
library(glmnet)
simulate_a=function(g){
  E=matrix(runif(g*g),g,g)
  B=matrix(0,g,g)
  for(i in 1:g){
    B[i,i]=runif(1)
  }
  fin=E%*%B%*%solve(E)
  fin
  return(fin)
}
simulate_A=function(p,ng,G,l_th,h_th){
  A=matrix(0,p,p)
  for(j in 1:(p/ng)){
    for(k in 1:(p/ng)){
      b=matrix(runif(ng*ng),ng,ng)
      #b=simulate_a(ng)
      if(sum(abs(b)>h_th)>0){
        b[abs(b)<l_th]=0
      }else{
        b=matrix(0,ng,ng)
      }
      A[((j-1)*ng+1):(j*ng),((k-1)*ng+1):(k*ng)]=b
    }
  }
  #for(i in 1:(p/ng)){
  #  a=simulate_a(ng)
  #  a[abs(a)<l_th]=0
  #  A[((i-1)*ng+1):(i*ng),((i-1)*ng+1):(i*ng)]=a
  #  
  #}
  return(A)
}
Table=function(pre,true,p){
  table=table(pre!=0,true!=0)
  table[,"TRUE"]=as.numeric(table[,"TRUE"])
  table[,"FALSE"]=as.numeric(table[,"FALSE"])
  Precision=table["TRUE","TRUE"]/(table["TRUE","FALSE"]+table["TRUE","TRUE"])
  Recall=table["TRUE","TRUE"]/(table["FALSE","TRUE"]+table["TRUE","TRUE"])
  MCC=(table["TRUE","TRUE"]*table["FALSE","FALSE"]-table["TRUE","FALSE"]*table["FALSE","TRUE"])/
    sqrt(sum(table["TRUE",])*sum(table[,"TRUE"])*sum(table[,"FALSE"])*sum(table["FALSE",]))
  ERR=sum((pre!=0)[,1:(dim(pre)[2]-p) ])/sum(pre!=0)
  A=c(Precision,Recall,MCC,ERR)
  names(A)=c("Precision","Recall","MCC","ERR")
  return(A)
}
f_lasso=function(X,Y,k_fold,penal=T,lamda=NULL){
  if(penal==T){
    m1_old_lass=cv.grpreg(X=X,y=Y,nfolds=k_fold,penalty="grLasso",lambda=lamda)
  }else{
    m1_old_lass=cv.grpreg(X=X,y=Y,nfolds=k_fold,penalty="grLasso")
  }
  b=coef(m1_old_lass)
  
  return(list(beta=b,lamda=m1_old_lass$lambda.min))
}
gln_lasso=function(X,Y,k_fold,lamda=NULL){
  m=cv.glmnet(x=X,y=Y,nfolds=k_fold,lambda=lamda,family = 'gaussian')
  coefm=as.vector(coef(m))
  return(list(beta=coefm,lamda=m$lambda.min))
}
g_lasso=function(X,Y,k_fold,penal=T,group,lamda=NULL){
  if(penal==T){
    m=cv.grpreg(X=X,y=Y,nfolds=k_fold,group=group,penalty="grLasso",lambda=lamda)
  }else{
    m=cv.grpreg(X=X,y=Y,nfolds=k_fold,group=group,penalty="grLasso")
  }
  beta_Alass=coef(m)
  return(list(beta=beta_Alass,lamda=m$lambda.min))
}
Ag_lasso_penal=function(X,Y,k_fold,group,lamda=NULL){
  m=cv.grpreg(X=X,y=Y,nfolds=k_fold,group=group,penalty="grLasso",lambda=lamda)
  beta_Aglass=coef(m)[-1]
  w=c()
  for(k in 1:max(group)){
    w[k]=1/norm(beta_Aglass[which(group==k)],'2')
  }
  if(length(which(w!=Inf))==0){
    beta_Aglass=c(coef(m)[1] ,rep(0,dim(X)[2]))
  }else{
    sub_n_group=as.matrix(table(group))[which(w!=Inf)]
    m=cv.grpreg(X=X[,group%in%which(w!=Inf)],
                y=Y,nfolds=k_fold
                ,lambda=lamda
                ,group.multiplier=w[which(w!=Inf)]
                ,group=rep(1:(length(sub_n_group)),time=sub_n_group)
                ,penalty="grLasso")
    beta_Aglass=rep(0,dim(X)[2]+1)
    group_seat=which(group%in%which(w!=Inf))+1
    beta_Aglass[c(1,group_seat)]=coef(m)
  }
  return(list(beta=beta_Aglass,lamda=m$lambda.min))
}
Ag_lasso_nopenal=function(X,Y,k_fold,group,lamda=NULL){
  m=cv.grpreg(X=X,y=Y,nfolds=k_fold,group=group,penalty="grLasso")
  beta_Aglass=coef(m)[-1]
  w=c()
  for(k in 1:max(group)){
    w[k]=1/norm(beta_Aglass[which(group==k)],'2')
  }
  if(length(which(w!=Inf))==0){
    beta_Aglass= rep(0,dim(X)[2]+1)
  }else{
    sub_n_group=as.matrix(table(group))[which(w!=Inf)]
    m1_old_Aglass_w=cv.grpreg(X=X[,group%in%which(w!=Inf)],
                              y=Y,nfolds=k_fold
                              ,group.multiplier=w[which(w!=Inf)]
                              ,group=rep(1:(length(sub_n_group)),time=sub_n_group)
                              ,penalty="grLasso")
    beta_Aglass=rep(0,dim(X)[2]+1)
    group_seat=which(group%in%which(w!=Inf))+1
    beta_Aglass[c(1,group_seat)]=coef(m1_old_Aglass_w)
  }
  return(beta_Aglass)
}
result_plot=function(hat_A,p,TT,name,main){
  library(gplots)
  library(igraph)
  edge=matrix(0,p,p)
  for(i in 1:(TT-1)){
    E=t(hat_A[,(p*(i-1)+1):(p*i)])
    E[which(E!=0)]=1
    edge=edge+E
  }
  colnames(edge)=name
  row.names(edge)=name
  heatmap.2(edge,scale = "none",main=main,Rowv = F,Colv = F,trace='none',dendrogram="none",col=colorpanel(20,"white","darkgrey","black"))
  net =graph.incidence(edge,directed =T,mode ="out")
  mm=cbind(rep(c(1,2),each=p),rep(p:1,time=2))
  plot.igraph(net,layout=mm,vertex.label.degree=0
              ,vertex.label.dist=rep(c(-1,1),each=21)
              , vertex.size=0,edge.arrow.size=0.3
              ,vertex.label.color=rep(rep(1:4,c(4,7,6,4)),time=2)
              ,main= main)
}
cv_thgrp=function(X,Y,group,fold,k_fold,lambda,th_grp,th_miss){
  fold_list=Map(function(x) which(fold!=x),1:k_fold)
  beta_matrix=sapply(1:k_fold,function(x) grpreg(X=X[fold_list[[x]],],y=Y[fold_list[[x]]]
                                                 ,group=group,penalty="grLasso"
                                                 ,lambda = lambda)$beta)
  beta_matrix_inter=beta_matrix[1,]
  beta_matrix=beta_matrix[-1,]
  beta_matrix_new=sapply(1:k_fold,function(i_fold) {
    beta_sq=beta_matrix[,i_fold]^2
    sapply(1:max(group),function(k){
      beta_gsq=sqrt(sum(beta_sq[which(group==k)]))
      beta_matrix[which(group==k),i_fold]=(beta_gsq>=th_grp)*beta_matrix[which(group==k),i_fold]
      beta_matrix[which(group==k),i_fold]=(abs(beta_matrix[which(group==k),i_fold])>=beta_gsq*th_miss)*beta_matrix[which(group==k),i_fold]
    })
  })
  mean(sapply(1:k_fold,function(i_fold) {
    cvpre=c(cbind(rep(1,nrow(X[-1*fold_list[[i_fold]],])),X[-1*fold_list[[i_fold]],])%*%c(beta_matrix_inter[i_fold],beta_matrix_new[,i_fold]))
    cvy=c(Y[-1*fold_list[[i_fold]]])
    sum((cvpre-cvy)^2)
  })
  )
  
}
thgrp_lasso=function(X,Y,group,fold,k_fold,lambda,nth_grp,nth_miss){
  m1_old_thgrp=cv.grpreg(X=X,y=Y,lambda=lambda
                         ,group=group,penalty="grLasso")
  th_miss=n^(-(1:nth_miss)/10)
  th_grp=(1:nth_grp)/10 *m1_old_thgrp$lambda.min 
  #row=nth_miss , col=nth_grp
  cv_error=sapply(th_grp,function(th_grp){
    sapply(th_miss,function(x) {
      cv_thgrp(X,Y,group,fold,k_fold,lambda=m1_old_thgrp$lambda.min,th_grp,th_miss=x)
    })
  })
  min_local=which(cv_error==min(cv_error),arr.ind = T)[1,]
  th_grp_min=th_grp[min_local[2]]
  th_miss_min=th_miss[min_local[1]]
  beta=coef(m1_old_thgrp)[-1]
  beta_sq=beta^2
  beta_new=c(sapply(1:max(group),function(k){
    beta_gsq=sqrt(sum(beta_sq[which(group==k)]));
    beta[which(group==k)]=(beta_gsq>=th_grp_min)*beta[which(group==k)];
    beta[which(group==k)]=(abs(beta[which(group==k)])>=beta_gsq*th_miss_min)*beta[which(group==k)];
  }))
  beta_thgrp=c(coef(m1_old_thgrp)[1],beta_new)
  return(list(beta=beta_thgrp
              ,lambda=m1_old_thgrp$lambda.min
              ,th.grp=th_grp_min
              ,th.miss=th_miss_min)) 
}


n=45 #芠代计
t=5 #琵家览丁キ铆翴 
p=90 #跑计计
TT=t #丁祏(︳璸程d)
N=30#Ω计
ng=3 #group柑计
nth_miss=10 #th_miss计 
nth_grp=10  #th_grp计 
A_sigma=0.1
A1=matrix(0,p,p)
#=======================p=30 ,group size =3
#n_edge=11
#set.seed(5454)
#=======================p=60,group size =3
#n_edge=20
#set.seed(5993)
#=======================p=90,group size =3
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
    A1[((j[k]-1)*ng+1):(j[k]*ng),(i[k]*ng+1):((i[k]+1)*ng)]=matrix(((rbinom(ng*ng,1,0.5)*2-1)*runif(ng*ng,0.5,1)),ng,ng)
  }else{
    #A1[((j[k]-1)*ng+1):(j[k]*ng),(i[k]*ng+1):((i[k]+1)*ng)]=matrix(rnorm(ng*ng),ng,ng)
    A1[((j[k]-1)*ng+1):(j[k]*ng),(i[k]*ng+1):((i[k]+1)*ng)]=matrix(((rbinom(ng*ng,1,0.5)*2-1)*runif(ng*ng,0.5,1)),ng,ng)
  }
}
eigen_A1=(abs(eigen(A1)$values)<0.9)
abs(eigen(A1)$values)
library(gplots)
dim(A1)
heatmap.2(matrix(as.numeric(abs(A1)>0),p,p),main = 'True',scale = "none",Rowv = F,Colv = F,trace='none',dendrogram="none",col=colorpanel(2*N,"white","darkgrey","black"))
TA1=cbind(matrix(0,p,(TT-2)*p),A1)
sum(rowSums(A1)==0)
k_fold=20   #cv fold 
set.seed(66) #cv fold
if((n%%k_fold)==0){
  fold=sample(rep(1:k_fold,floor(n/k_fold)),n)
}else{
  fold=sample(c(rep(1:k_fold,floor(n/k_fold)),1:(n%%k_fold)),n)
}
group=rep(1:((p*(TT-1))/ng),rep(ng,((p*(TT-1))/ng)))  

c_interval=c(0.01,(1:9)*0.1)
for(ccc in 1:10){   
  MSE=matrix(0,N,5)
  sum_A_lass=matrix(0,p,(TT-1)*p)
  sum_A_glass=matrix(0,p,(TT-1)*p)
  sum_A_thgrp=matrix(0,p,(TT-1)*p)
  sum_A_Aglass=matrix(0,p,(TT-1)*p)
  sum_A_gln_lass=matrix(0,p,(TT-1)*p)
  table_lass=matrix(0,N,4)
  table_glass=matrix(0,N,4)
  table_thgrp=matrix(0,N,4)
  table_Aglass=matrix(0,N,4)
  table_gln_lass=matrix(0,N,4)
  summary_lamda=list()
  summary_th=list()
  c1=0.01
  c2=2
  nc=100
  lamda_seq = seq(from = c1*sqrt((2*log(p))/n), to = c2*sqrt((2*log(p))/n),
                  by=(c2*sqrt((2*log(p))/n)-c1*sqrt((2*log(p))/n))/(nc-1))
  c1_g=c1
  c2_g=c2
  nc_g=100
  group_from=c1_g*sqrt((2*log(max(group)/(TT-1)))/n)
  group_to=c2_g*sqrt((2*log(max(group)/(TT-1)))/n)
  lamda_g_seq=seq(from = group_from, to = group_to,
                  by=(group_to- group_from)/(nc_g-1))
  c1_cut=c_interval[ccc]
  c1_g_cut=c_interval[ccc]
  lamda_seq=lamda_seq[which(lamda_seq>=c1_cut*sqrt((2*log(p))/n))]
  lamda_g_seq=lamda_g_seq[which(lamda_g_seq>=c1_g_cut*sqrt((2*log(max(group)/(TT-1)))/n))]
  for(j in 1:N){
    x=matrix(0,n,p*t)
    for (i in 1:n){#A1T-1玒计:::::A2T-2玒计cbind(A1,A2)
      var1<-VAR.sim(B=A1,lag=1,n=500,include="none",varcov=diag(A_sigma,p,p))
      x[i,]=c(t(tail(var1,n = t)))
    }
    
    X=x[,-1*(dim(x)[2]-p+1):(dim(x)[2])]
    Y=x[,(dim(x)[2]-p+1):(dim(x)[2])]
    #sapply(1:(p/ng),function(i) mean(x[,(0:(t-1))*p+i]))
    #sapply(1:(p/ng),function(i) sd(x[,(0:(t-1))*p+i]))
    #cl = makeCluster(detectCores() -1)
    cl = makeCluster(detectCores() -6)
    clusterExport(cl, c("X",'Y','k_fold','f_lasso','gln_lasso','g_lasso','nth_miss','nth_grp'
                        ,'Ag_lasso_penal','simulate_A','Table','cv_thgrp','n'
                        ,'lamda_g_seq','group','lamda_seq','cv.grpreg','cv.glmnet','fold'
                        ,'thgrp_lasso','grpreg'))
    m_lass=t(parSapply(cl,1:p,function(x) f_lasso(X=X,Y=Y[,x],penal=T
                                                  ,k_fold=k_fold,lamda=lamda_seq)))
    hat_A_lass=Reduce(rbind,m_lass[,1])
    
    glmnet_lass=t(parSapply(cl,1:p,function(x) gln_lasso(X=X,Y=Y[,x],k_fold=k_fold,lamda=lamda_seq)))
    hat_A_gln_lass=Reduce(rbind,glmnet_lass[,1])
    
    m_glass=t(parSapply(cl,1:p,function(x) g_lasso(X=X,Y=Y[,x],penal=T,group=group
                                                   ,k_fold=k_fold,lamda=lamda_g_seq)))
    hat_A_glass=Reduce(rbind,m_glass[,1])
    
    
    m_Aglass=t(parSapply(cl,1:p,function(x) Ag_lasso_penal(X=X,Y=Y[,x],group=group
                                                           ,k_fold=k_fold,lamda=lamda_g_seq)))
    hat_A_Aglass=Reduce(rbind,m_Aglass[,1])
    
    m_thgrp=t(parSapply(cl,1:p,function(x) thgrp_lasso(X=X,Y=Y[,x],group=group
                                                       ,fold,k_fold,lambda=lamda_g_seq,nth_grp,nth_miss)))
    hat_A_thgrp=Reduce(rbind,m_thgrp[,1])
    
    stopCluster(cl)# Stop all worker instances
    
    #heatmap.2(matrix(as.numeric(abs(hat_A_gln_lass[,-1])>0),p,(TT-1)*p),main = 'True',scale = "none",Rowv = F,Colv = F,trace='none',dendrogram="none",col=colorpanel(2*N,"white","darkgrey","black"))
    
    summary_lamda[[j]]=cbind(Reduce(rbind,m_lass[,2]),Reduce(rbind,glmnet_lass[,2]),Reduce(rbind,m_glass[,2]),Reduce(rbind,m_Aglass[,2]),Reduce(rbind,m_thgrp[,2]))
    summary_th[[j]]=cbind(Reduce(rbind,m_thgrp[,3]),Reduce(rbind,m_thgrp[,4]))
    
    table_lass[j,]=Table(hat_A_lass[,-1],TA1,p)
    sum_A_lass=sum_A_lass+matrix(as.numeric(abs(hat_A_lass[,-1])>0),p,(TT-1)*p)
    
    table_gln_lass[j,]=Table(hat_A_gln_lass[,-1],TA1,p)
    sum_A_gln_lass=sum_A_gln_lass+matrix(as.numeric(abs(hat_A_gln_lass[,-1])>0),p,(TT-1)*p)
    
    table_glass[j,]=Table(hat_A_glass[,-1],TA1,p)
    sum_A_glass=sum_A_glass+matrix(as.numeric(abs(hat_A_glass[,-1])>0),p,(TT-1)*p)
    
    table_Aglass[j,]=Table(hat_A_Aglass[,-1],TA1,p)
    sum_A_Aglass=sum_A_Aglass+matrix(as.numeric(abs(hat_A_Aglass[,-1])>0),p,(TT-1)*p)
    
    table_thgrp[j,]=Table(hat_A_thgrp[,-1],TA1,p)
    sum_A_thgrp=sum_A_thgrp+matrix(as.numeric(abs(hat_A_thgrp[,-1])>0),p,(TT-1)*p)
    
    pre_l =cbind(rep(1,n),X)%*%t(hat_A_lass)
    pre_gln_l=cbind(rep(1,n),X)%*%t(hat_A_gln_lass)
    pre_g =cbind(rep(1,n),X)%*%t(hat_A_glass)
    pre_thgrp=cbind(rep(1,n),X)%*%t(hat_A_thgrp)
    pre_Ag=cbind(rep(1,n),X)%*%t(hat_A_Aglass)
    MSE[j,]=c(mean((Y-pre_l)^2),mean((Y-pre_gln_l)^2),mean((Y-pre_g)^2),mean((Y-pre_thgrp)^2),mean((Y-pre_Ag)^2))
    print(noquote(sprintf( "c1_cut=%.0f j=%.0f times=%s",c1_cut,j,Sys.time())))
  }
  apply(MSE,2,mean)
  apply(MSE,2,sd)
  mse_table=sprintf( "%.3f(%.2f)",apply(MSE,2,mean),apply(MSE,2,sd))
  summary=rbind(sprintf( "%.1f(%.1f)",apply(table_lass,2,mean)*100,apply(table_lass,2,sd)*100)
                ,sprintf( "%.1f(%.1f)",apply(table_gln_lass,2,mean)*100,apply(table_gln_lass,2,sd)*100)
                ,sprintf( "%.1f(%.1f)",apply(table_glass,2,mean)*100,apply(table_glass,2,sd)*100)
                ,sprintf( "%.1f(%.1f)",apply(table_thgrp,2,mean)*100,apply(table_thgrp,2,sd)*100)
                ,sprintf( "%.1f(%.1f)",apply(table_Aglass,2,mean)*100,apply(table_Aglass,2,sd)*100))
  summary=cbind(summary,noquote(mse_table))
  colnames(summary)=c("Precision","Recall","MCC","ERR",'MSE')
  row.names(summary)=c("Lasso",'gln_lass',"Grp","Thgrp","Agrp")
  summary=noquote(summary)
  
  #heatmap.2(matrix(as.numeric(abs(TA1)>0),p,(TT-1)*p),main = 'True',scale = "none",Rowv = F,Colv = F,trace='none',dendrogram="none",col=colorpanel(2*N,"white","darkgrey","black"))
  #heatmap.2(sum_A_lass,main =paste('Lasso',c1_cut),scale = "none",Rowv = F,Colv = F,trace='none',dendrogram="none",col=colorpanel(2*N,"white","darkgrey","black"))
  #heatmap.2(sum_A_gln_lass,main =paste('gln_Lasso',c1_cut),scale = "none",Rowv = F,Colv = F,trace='none',dendrogram="none",col=colorpanel(2*N,"white","darkgrey","black"))
  #heatmap.2(sum_A_glass,main =paste('Grp',c1_cut),scale = "none",Rowv = F,Colv = F,trace='none',dendrogram="none",col=colorpanel(2*N,"white","darkgrey","black"))
  #heatmap.2(sum_A_thgrp,main =paste('Thgrp',c1_cut),scale = "none",Rowv = F,Colv = F,trace='none',dendrogram="none",col=colorpanel(2*N,"white","darkgrey","black"))
  #heatmap.2(sum_A_Aglass,main =paste('Agrp',c1_cut),scale = "none",Rowv = F,Colv = F,trace='none',dendrogram="none",col=colorpanel(2*N,"white","darkgrey","black"))
  sum_mix_matrix=rbind(sum_A_lass,sum_A_glass,sum_A_thgrp,sum_A_Aglass)
  all = list(lamda_seq=lamda_seq,A1=A1,TA1=TA1,A_sigma=A_sigma,c1_cut=c1_cut,summary_lamda=summary_lamda,summary_th=summary_th,
             table_lass=table_lass,table_glass=table_glass,table_Aglass=table_Aglass,
             table_thgrp=table_thgrp,MSE=MSE,summary = summary, sum_mix_matrix = sum_mix_matrix,n=n,p=p,N=N
             ,sum_A_lass=sum_A_lass,sum_A_gln_lass=sum_A_gln_lass,sum_A_glass=sum_A_glass,sum_A_Aglass=sum_A_Aglass,sum_A_thgrp=sum_A_thgrp)
  #setwd("D:/hong")
  save(all,list=c('lamda_seq','A1','TA1','c1_cut','summary_th','summary_lamda','A_sigma','sum_A_lass','sum_A_gln_lass','sum_A_glass','sum_A_Aglass','sum_A_thgrp'
                  ,'MSE','table_lass','table_glass','table_gln_lass','table_Aglass','table_thgrp',
                  'summary','sum_mix_matrix','n','p','N'), file =sprintf("2c MSE,sum_matrix n %.0f p %.0f ng %.0f sigma %.1f  c %.1f.RData",n,p,ng,A_sigma,c1_cut))
  #heatmap.2(matrix(as.numeric(abs(hat_A_thgrp)>0),p,(TT-1)*p),main = 'True',scale = "none",Rowv = F,Colv = F,trace='none',dendrogram="none",col=colorpanel(2*N,"white","darkgrey","black"))
  #setwd("D:/阶ゅ磕戈/だ猂挡狦")
  #write.csv(noquote(summary),file = sprintf("MSE n %.0f c %.1f.csv",n,c1))
  #save(sum_mix_matrix,file = sprintf("sum_mix_matrix n %.0f c %.1f.RData",n,c1),row.names = F)
  print(list(c1_cut,summary))
}
proc.time() - ptm
#par(mfrow=c(2,3))
#heatmap.2(matrix(as.numeric(abs(TA1)>0),p,(TT-1)*p),main = 'True',scale = "none",Rowv = F,Colv = F,trace='none',dendrogram="none",col=colorpanel(2*N,"white","darkgrey","black"))
#heatmap.2(sum_A_lass,main =paste('Lasso',c1_cut),scale = "none",Rowv = F,Colv = F,trace='none',dendrogram="none",col=colorpanel(2*N,"white","darkgrey","black"))
#heatmap.2(sum_A_gln_lass,main =paste('gln_Lasso',c1_cut),scale = "none",Rowv = F,Colv = F,trace='none',dendrogram="none",col=colorpanel(2*N,"white","darkgrey","black"))
#heatmap.2(sum_A_glass,main =paste('Grp',c1_cut),scale = "none",Rowv = F,Colv = F,trace='none',dendrogram="none",col=colorpanel(2*N,"white","darkgrey","black"))
#heatmap.2(sum_A_thgrp,main =paste('Thgrp',c1_cut),scale = "none",Rowv = F,Colv = F,trace='none',dendrogram="none",col=colorpanel(2*N,"white","darkgrey","black"))
#heatmap.2(sum_A_Aglass,main =paste('Agrp',c1_cut),scale = "none",Rowv = F,Colv = F,trace='none',dendrogram="none",col=colorpanel(2*N,"white","darkgrey","black"))

