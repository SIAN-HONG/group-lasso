library(timeDate)
library(grpreg)
library(tsDyn)
library(gplots)
library(parallel)
library(glmnet)
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
##########################################################################
#Lasso grpreg
##########################################################################
f_lasso=function(X,Y,k_fold,penal=T,lamda=NULL){
  if(penal==T){
    m1_old_lass=cv.grpreg(X=X,y=Y,nfolds=k_fold,penalty="grLasso",lambda=lamda)
  }else{
    m1_old_lass=cv.grpreg(X=X,y=Y,nfolds=k_fold,penalty="grLasso")
  }
  b=coef(m1_old_lass)
  
  return(list(beta=b,lamda=m1_old_lass$lambda.min))
}
##########################################################################
#Lasso glmnet
##########################################################################
gln_lasso=function(X,Y,k_fold,lamda=NULL){
  m=cv.glmnet(x=X,y=Y,nfolds=k_fold,lambda=lamda,family = 'gaussian')
  coefm=as.vector(coef(m))
  return(list(beta=coefm,lamda=m$lambda.min))
}
##########################################################################
#Group lasso
##########################################################################
g_lasso=function(X,Y,k_fold,penal=T,group,lamda=NULL){
  if(penal==T){
    m=cv.grpreg(X=X,y=Y,nfolds=k_fold,group=group,penalty="grLasso",lambda=lamda)
  }else{
    m=cv.grpreg(X=X,y=Y,nfolds=k_fold,group=group,penalty="grLasso")
  }
  beta_Alass=coef(m)
  return(list(beta=beta_Alass,lamda=m$lambda.min))
}
##########################################################################
#Adaptive group lasso
##########################################################################
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
##########################################################################
#Threshold group lasso
##########################################################################
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