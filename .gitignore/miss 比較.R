library(timeDate)
library(grpreg)
library(tsDyn)
library(gplots)
library(parallel)
library(glmnet)
col=c(1,3,4,6,7)
setwd("D:/論文金融資料/分析結果")
c_interval=c(0.01,0.1*(1:9))

n=90
p=60
ng=3
lasso_time_matrix=matrix(0,length(c_interval),5)
gln_lasso_time_matrix=matrix(0,length(c_interval),5)
glasso_time_matrix=matrix(0,length(c_interval),5)
Aglasso_time_matrix=matrix(0,length(c_interval),5)
thlasso_time_matrix=matrix(0,length(c_interval),5)
colnames(lasso_time_matrix)=c("Precision","Recall","MCC","ERR",'MSE')
colnames(gln_lasso_time_matrix)=c("Precision","Recall","MCC","ERR",'MSE')
colnames(glasso_time_matrix)=c("Precision","Recall","MCC","ERR",'MSE')
colnames(Aglasso_time_matrix)=c("Precision","Recall","MCC","ERR",'MSE')
colnames(thlasso_time_matrix)=c("Precision","Recall","MCC","ERR",'MSE')
for(i in 1:length(c_interval)){
  temp=sprintf("D:/論文金融資料/分析結果/2c MSE,sum_matrix n %.0f p %.0f ng %.0f sigma 0.1  c %.1f.RData",n,p,ng,c_interval[i])
  load(temp)
  lasso_time_matrix[i,]=c(apply(table_lass,2,mean)*100,apply(MSE,2,mean)[1])
  gln_lasso_time_matrix[i,]=c(apply(table_gln_lass,2,mean)*100,apply(MSE,2,mean)[2])
  glasso_time_matrix[i,]=c(apply(table_glass,2,mean)*100,apply(MSE,2,mean)[3])
  thlasso_time_matrix[i,]=c(apply(table_thgrp,2,mean)*100,apply(MSE,2,mean)[4])
  Aglasso_time_matrix[i,]=c(apply(table_Aglass,2,mean)*100,apply(MSE,2,mean)[5])
  print(list(c_interval[i],summary))
}
result_table= rbind(
  c(apply(lasso_time_matrix[,1:3],2,max),apply(lasso_time_matrix[,4:5],2,min))
  ,c(apply(gln_lasso_time_matrix[,1:3],2,max),apply(gln_lasso_time_matrix[,4:5],2,min))
  ,c(apply(glasso_time_matrix[,1:3],2,max),apply(glasso_time_matrix[,4:5],2,min))
  ,c(apply(thlasso_time_matrix[,1:3],2,max),apply(thlasso_time_matrix[,4:5],2,min))
  ,c(apply(Aglasso_time_matrix[,1:3],2,max),apply(Aglasso_time_matrix[,4:5],2,min)))
colnames(result_table)=c("Precision","Recall","MCC","ERR",'MSE')
row.names(result_table)=c("Lasso",'gln_lass',"Grp","Thgrp","Agrp")
result_table
result=list()
result[[1]]=list(parameter=sprintf("n %.0f p %.0f ng %.0f",n,p,ng),table=result_table)
result
#===================miss
lasso.miss_time_matrix=matrix(0,length(c_interval),5)
gln_lasso.miss_time_matrix=matrix(0,length(c_interval),5)
glasso.miss_time_matrix=matrix(0,length(c_interval),5)
Aglasso.miss_time_matrix=matrix(0,length(c_interval),5)
thlasso.miss_time_matrix=matrix(0,length(c_interval),5)
colnames(lasso.miss_time_matrix)=c("Precision","Recall","MCC","ERR",'MSE')
colnames(gln.miss_lasso_time_matrix)=c("Precision","Recall","MCC","ERR",'MSE')
colnames(glasso.miss_time_matrix)=c("Precision","Recall","MCC","ERR",'MSE')
colnames(Aglasso.miss_time_matrix)=c("Precision","Recall","MCC","ERR",'MSE')
colnames(thlasso.miss_time_matrix)=c("Precision","Recall","MCC","ERR",'MSE')
ng=3
for(i in 1:length(c_interval)){
  temp=sprintf("D:/論文金融資料/分析結果/miss MSE,sum_matrix n %.0f p %.0f ng %.0f sigma 0.1  c %.1f.RData",n,p,ng,c_interval[i])
  load(temp)
  lasso.miss_time_matrix[i,]=c(apply(table_lass,2,mean)*100,apply(MSE,2,mean)[1])
  gln_lasso.miss_time_matrix[i,]=c(apply(table_gln_lass,2,mean)*100,apply(MSE,2,mean)[2])
  glasso.miss_time_matrix[i,]=c(apply(table_glass,2,mean)*100,apply(MSE,2,mean)[3])
  thlasso.miss_time_matrix[i,]=c(apply(table_thgrp,2,mean)*100,apply(MSE,2,mean)[4])
  Aglasso.miss_time_matrix[i,]=c(apply(table_Aglass,2,mean)*100,apply(MSE,2,mean)[5])
  print(list(c_interval[i],summary))
}
result.miss_table= rbind(
  c(apply(lasso.miss_time_matrix[,1:3],2,max),apply(lasso.miss_time_matrix[,4:5],2,min))
  ,c(apply(gln_lasso.miss_time_matrix[,1:3],2,max),apply(gln_lasso.miss_time_matrix[,4:5],2,min))
  ,c(apply(glasso.miss_time_matrix[,1:3],2,max),apply(glasso.miss_time_matrix[,4:5],2,min))
  ,c(apply(thlasso.miss_time_matrix[,1:3],2,max),apply(thlasso.miss_time_matrix[,4:5],2,min))
  ,c(apply(Aglasso.miss_time_matrix[,1:3],2,max),apply(Aglasso.miss_time_matrix[,4:5],2,min)))
colnames(result.miss_table)=c("Precision","Recall","MCC","ERR",'MSE')
row.names(result.miss_table)=c("Lasso",'gln_lass',"Grp","Thgrp","Agrp")
result[[2]]=list(parameter=sprintf("n %.0f p %.0f ng %.0f.miss",n,p,ng),table=result.miss_table)
result

{
  plot_main=sprintf("n %.0f p %.0f",n,p)
  windows()
  par(mfrow=c(1,2))
  #precision
  plot(c_interval,lasso_time_matrix[,1],ylim=c(1,100),col=col[1],type='b',main=plot_main,ylab='Precision*100',xlab='c1')
  lines(c_interval,gln_lasso_time_matrix[,1],type='b',col=col[2])
  lines(c_interval,glasso_time_matrix[,1],type='b',col=col[3])
  lines(c_interval,thlasso_time_matrix[,1],type='b',col=col[4])
  lines(c_interval,Aglasso_time_matrix[,1],type='b',col=col[5])
  legend(0.57,35,legend = c('Lasso','gln_L','Group', 'Threshold','Adaptive Group' ),col=col, pch = 1)
  #precision.miss
  plot(c_interval,lasso.miss_time_matrix[,1],ylim=c(1,100),col=col[1],type='b',main=paste(sprintf("n %.0f p %.0f",n,p),'.miss'),ylab='Precision*100.miss',xlab='c1')
  lines(c_interval,gln_lasso.miss_time_matrix[,1],type='b',col=col[2])
  lines(c_interval,glasso.miss_time_matrix[,1],type='b',col=col[3])
  lines(c_interval,thlasso.miss_time_matrix[,1],type='b',col=col[4])
  lines(c_interval,Aglasso.miss_time_matrix[,1],type='b',col=col[5])
  legend(0.57,35,legend = c('Lasso','gln_L','Group', 'Threshold','Adaptive Group' ),col=col, pch = 1)
  
  windows()
  par(mfrow=c(1,2))
  #ERR
  plot(c_interval,lasso_time_matrix[,4],ylim=c(1,100),col=col[1],type='b',main=plot_main,ylab='ERR*100',xlab='c1')
  lines(c_interval,gln_lasso_time_matrix[,4],type='b',col=col[2])
  lines(c_interval,glasso_time_matrix[,4],type='b',col=col[3])
  lines(c_interval,thlasso_time_matrix[,4],type='b',col=col[4])
  lines(c_interval,Aglasso_time_matrix[,4],type='b',col=col[5])
  legend(0.57,100,legend = c('Lasso','gln_L','Group', 'Threshold','Adaptive Group' ),col=col, pch = 1)
  #ERR.miss
  plot(c_interval,lasso.miss_time_matrix[,4],ylim=c(1,100),col=col[1],type='b',main=paste(sprintf("n %.0f p %.0f",n,p),'.miss'),ylab='ERR*100.miss',xlab='c1')
  lines(c_interval,gln_lasso.miss_time_matrix[,4],type='b',col=col[2])
  lines(c_interval,glasso.miss_time_matrix[,4],type='b',col=col[3])
  lines(c_interval,thlasso.miss_time_matrix[,4],type='b',col=col[4])
  lines(c_interval,Aglasso.miss_time_matrix[,4],type='b',col=col[5])
  legend(0.57,100,legend = c('Lasso','gln_L','Group', 'Threshold','Adaptive Group' ),col=col, pch = 1)
  
  #Recall
  windows()
  par(mfrow=c(1,2))
  plot(c_interval,lasso_time_matrix[,2],ylim=c(1,100),col=col[1],type='b',main=plot_main,ylab='Recall*100',xlab='c1')
  lines(c_interval,gln_lasso_time_matrix[,2],type='b',col=col[2])
  lines(c_interval,glasso_time_matrix[,2],type='b',col=col[3])
  lines(c_interval,thlasso_time_matrix[,2],type='b',col=col[4])
  lines(c_interval,Aglasso_time_matrix[,2],type='b',col=col[5])
  legend(0,57,legend = c('Lasso','gln_L','Group', 'Threshold','Adaptive Group' ),col=col, pch = 1)
  #Recall.miss
  plot(c_interval,lasso.miss_time_matrix[,2],ylim=c(1,100),col=col[1],type='b',main=paste(sprintf("n %.0f p %.0f",n,p),'.miss'),ylab='Recall*100.miss',xlab='c1')
  lines(c_interval,gln_lasso.miss_time_matrix[,2],type='b',col=col[2])
  lines(c_interval,glasso.miss_time_matrix[,2],type='b',col=col[3])
  lines(c_interval,thlasso.miss_time_matrix[,2],type='b',col=col[4])
  lines(c_interval,Aglasso.miss_time_matrix[,2],type='b',col=col[5])
  legend(0,57,legend = c('Lasso','gln_L','Group', 'Threshold','Adaptive Group' ),col=col, pch = 1)
  
  #MCC
  windows()
  par(mfrow=c(1,2))
  plot(c_interval,lasso_time_matrix[,3],ylim=c(1,100),col=col[1],type='b',main=plot_main,ylab='MCC*100',xlab='c1')
  lines(c_interval,gln_lasso_time_matrix[,3],type='b',col=col[2])
  lines(c_interval,glasso_time_matrix[,3],type='b',col=col[3])
  lines(c_interval,thlasso_time_matrix[,3],type='b',col=col[4])
  lines(c_interval,Aglasso_time_matrix[,3],type='b',col=col[5])
  legend(0.57,35,legend = c('Lasso','gln_L','Group', 'Threshold','Adaptive Group' ),col=col, pch = 1)
  #MCC.miss
  plot(c_interval,lasso.miss_time_matrix[,3],ylim=c(1,100),col=col[1],type='b',main=paste(sprintf("n %.0f p %.0f",n,p),'.miss'),ylab='MCC*100.miss',xlab='c1')
  lines(c_interval,gln_lasso.miss_time_matrix[,3],type='b',col=col[2])
  lines(c_interval,glasso.miss_time_matrix[,3],type='b',col=col[3])
  lines(c_interval,thlasso.miss_time_matrix[,3],type='b',col=col[4])
  lines(c_interval,Aglasso.miss_time_matrix[,3],type='b',col=col[5])
  legend(0.57,35,legend = c('Lasso','gln_L','Group', 'Threshold','Adaptive Group' ),col=col, pch = 1)
  
  #MSE
  windows()
  par(mfrow=c(1,2))
  plot(c_interval,lasso_time_matrix[,5],ylim=c(0,0.4),col=col[1],type='b',main=plot_main,ylab='MSE*100',xlab='c1')
  lines(c_interval,gln_lasso_time_matrix[,5],type='b',col=col[2])
  lines(c_interval,glasso_time_matrix[,5],type='b',col=col[3])
  lines(c_interval,thlasso_time_matrix[,5],type='b',col=col[4])
  lines(c_interval,Aglasso_time_matrix[,5],type='b',col=col[5])
  legend(0.57,0.15,legend = c('Lasso','gln_L','Group', 'Threshold','Adaptive Group' ),col=col, pch = 1)
  #MSE.miss
  plot(c_interval,lasso.miss_time_matrix[,5],ylim=c(0,0.4),col=col[1],type='b',main=paste(sprintf("n %.0f p %.0f",n,p),'.miss'),ylab='MSE*100.miss',xlab='c1')
  lines(c_interval,gln_lasso.miss_time_matrix[,5],type='b',col=col[2])
  lines(c_interval,glasso.miss_time_matrix[,5],type='b',col=col[3])
  lines(c_interval,thlasso.miss_time_matrix[,5],type='b',col=col[4])
  lines(c_interval,Aglasso.miss_time_matrix[,5],type='b',col=col[5])
  legend(0.57,0.15,legend = c('Lasso','gln_L','Group', 'Threshold','Adaptive Group' ),col=col, pch = 1)
  
  #ROC
  windows()
  par(mfrow=c(1,2))
  location=c(which.max((lasso_time_matrix[,2]/100)*(lasso_time_matrix[,1]/100)),
             which.max((gln_lasso_time_matrix[,2]/100)*(gln_lasso_time_matrix[,1]/100)),
             which.max((glasso_time_matrix[,2]/100)*(glasso_time_matrix[,1]/100)),
             which.max((thlasso_time_matrix[,2]/100)*(thlasso_time_matrix[,1]/100)),
             which.max((Aglasso_time_matrix[,2]/100)*(Aglasso_time_matrix[,1]/100)))
  plot(lasso_time_matrix[,2]/100,lasso_time_matrix[,1]/100,col=col[1],main=plot_main,xlab='Recall',ylab='Precision',type='b',xlim=c(0.35,1),,ylim=c(0.15,1))
  lines(gln_lasso_time_matrix[,2]/100,gln_lasso_time_matrix[,1]/100,type='b',col=col[2])
  lines(glasso_time_matrix[,2]/100,glasso_time_matrix[,1]/100,type='b',col=col[3])
  lines(thlasso_time_matrix[,2]/100,thlasso_time_matrix[,1]/100,type='b',col=col[4])
  lines(Aglasso_time_matrix[,2]/100,Aglasso_time_matrix[,1]/100,type='b',col=col[5])
  
  point_lamda=matrix(c((lasso_time_matrix[,2]/100)[location[1]],(lasso_time_matrix[,1]/100)[location[1]]
                       ,(gln_lasso_time_matrix[,2]/100)[location[2]],(gln_lasso_time_matrix[,1]/100)[location[2]]
                       ,(glasso_time_matrix[,2]/100)[location[3]],(glasso_time_matrix[,1]/100)[location[3]]
                       ,(thlasso_time_matrix[,2]/100)[location[4]],(thlasso_time_matrix[,1]/100)[location[4]]
                       ,(Aglasso_time_matrix[,2]/100)[location[5]],(Aglasso_time_matrix[,1]/100)[location[5]]),5,2,byrow = T)
  for(i in 1:5){
    points(point_lamda[i,1],point_lamda[i,2],col=1,pch=19)
    text(point_lamda[i,1],point_lamda[i,2],labels=c_interval[location[i]],pos=1)
  }
  legend(0.35,0.4,legend = c('Lasso','gln_L','Group', 'Threshold','Adaptive Group' ),col=col, pch = 1)
  #ROC.miss
  location=c(which.max((lasso.miss_time_matrix[,2]/100)*(lasso.miss_time_matrix[,1]/100)),
             which.max((gln_lasso.miss_time_matrix[,2]/100)*(gln_lasso.miss_time_matrix[,1]/100)),
             which.max((glasso.miss_time_matrix[,2]/100)*(glasso.miss_time_matrix[,1]/100)),
             which.max((thlasso.miss_time_matrix[,2]/100)*(thlasso.miss_time_matrix[,1]/100)),
             which.max((Aglasso.miss_time_matrix[,2]/100)*(Aglasso.miss_time_matrix[,1]/100)))
  plot(lasso.miss_time_matrix[,2]/100,lasso.miss_time_matrix[,1]/100,col=col[1],main=paste(sprintf("n %.0f p %.0f",n,p),'.miss'),xlab='Recall.miss',ylab='Precision.miss',type='b',xlim=c(0.35,1),,ylim=c(0.15,1))
  lines(gln_lasso.miss_time_matrix[,2]/100,gln_lasso.miss_time_matrix[,1]/100,type='b',col=col[2])
  lines(glasso.miss_time_matrix[,2]/100,glasso.miss_time_matrix[,1]/100,type='b',col=col[3])
  lines(thlasso.miss_time_matrix[,2]/100,thlasso.miss_time_matrix[,1]/100,type='b',col=col[4])
  lines(Aglasso.miss_time_matrix[,2]/100,Aglasso.miss_time_matrix[,1]/100,type='b',col=col[5])
  
  point_lamda=matrix(c((lasso.miss_time_matrix[,2]/100)[location[1]],(lasso.miss_time_matrix[,1]/100)[location[1]]
                       ,(gln_lasso.miss_time_matrix[,2]/100)[location[2]],(gln_lasso.miss_time_matrix[,1]/100)[location[2]]
                       ,(glasso.miss_time_matrix[,2]/100)[location[3]],(glasso.miss_time_matrix[,1]/100)[location[3]]
                       ,(thlasso.miss_time_matrix[,2]/100)[location[4]],(thlasso.miss_time_matrix[,1]/100)[location[4]]
                       ,(Aglasso.miss_time_matrix[,2]/100)[location[5]],(Aglasso.miss_time_matrix[,1]/100)[location[5]]),5,2,byrow = T)
  for(i in 1:5){
    points(point_lamda[i,1],point_lamda[i,2],col=1,pch=19)
    text(point_lamda[i,1],point_lamda[i,2],labels=c_interval[location[i]],pos=1)
  }
  legend(0.35,0.4,legend = c('Lasso','gln_L','Group', 'Threshold','Adaptive Group' ),col=col, pch = 1)
  
  
}


{
  library(gplots)
  load("D:/論文金融資料/分析結果/miss MSE,sum_matrix n 60 p 60 sigma 0.1  c 0.4.RData")
  TT=5
  setwd('C:/Users/user_01/Desktop/新增資料夾')
  png("f1.png")
  heatmap.2(matrix(as.numeric(abs(TA1)>0),p,(TT-1)*p), key=FALSE,main = 'True',scale = "none",Rowv = F,Colv = F,trace='none',dendrogram="none",col=colorpanel(2*N,"white","darkgrey","black"))
  dev.off()
  png("f2.png")
  heatmap.2(sum_A_lass,main =paste('Lasso',c1_cut), key=FALSE,scale = "none",Rowv = F,Colv = F,trace='none',dendrogram="none",col=colorpanel(2*N,"white","darkgrey","black"))
  dev.off()
  png("f3.png")
  heatmap.2(sum_A_gln_lass,main =paste('gln_Lasso',c1_cut), key=FALSE,scale = "none",Rowv = F,Colv = F,trace='none',dendrogram="none",col=colorpanel(2*N,"white","darkgrey","black"))
  dev.off()
  png("f4.png")
  heatmap.2(sum_A_glass,main =paste('Grp',c1_cut), key=FALSE,scale = "none",Rowv = F,Colv = F,trace='none',dendrogram="none",col=colorpanel(2*N,"white","darkgrey","black"))
  dev.off()
  png("f5.png")
  heatmap.2(sum_A_thgrp,main =paste('Thgrp',c1_cut), key=FALSE,scale = "none",Rowv = F,Colv = F,trace='none',dendrogram="none",col=colorpanel(2*N,"white","darkgrey","black"))
  dev.off()
  png("f6.png")
  heatmap.2(sum_A_Aglass,main =paste('Agrp',c1_cut), key=FALSE,scale = "none",Rowv = F,Colv = F,trace='none',dendrogram="none",col=colorpanel(2*N,"white","darkgrey","black"))
  dev.off()
}
