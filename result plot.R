library(SGL)
library(timeDate)
library(grpreg)
library(tsDyn)
library(gplots)
library(parallel)
library(glmnet)
col=c(1,3,4,6,7,8)
pch=c(0,1,2,5,7,8)
setwd("D:/論文金融資料/分析結果")
c_interval=c(0.01,0.1*(1:9))
ng=3
pp=c(30,60,90)

rate=c(0.5,0.8,1,1.2,1.5)

result=list()
for(h in 1:3){
  NN=pp[h]*rate
  for(kk in 1:5){
    lasso_time_matrix=matrix(0,length(c_interval),5)
    gln_lasso_time_matrix=matrix(0,length(c_interval),5)
    glasso_time_matrix=matrix(0,length(c_interval),5)
    Aglasso_time_matrix=matrix(0,length(c_interval),5)
    thlasso_time_matrix=matrix(0,length(c_interval),5)
    sglasso_time_matrix=matrix(0,length(c_interval),5)
    colnames(lasso_time_matrix)=c("Precision","Recall","MCC","ERR",'MSE')
    colnames(gln_lasso_time_matrix)=c("Precision","Recall","MCC","ERR",'MSE')
    colnames(glasso_time_matrix)=c("Precision","Recall","MCC","ERR",'MSE')
    colnames(Aglasso_time_matrix)=c("Precision","Recall","MCC","ERR",'MSE')
    colnames(thlasso_time_matrix)=c("Precision","Recall","MCC","ERR",'MSE')
    colnames(sglasso_time_matrix)=c("Precision","Recall","MCC","ERR",'MSE')
    for(i in 1:length(c_interval)){
      temp=sprintf("D:/論文金融資料/分析結果/2c MSE,sum_matrix n %.0f p %.0f ng %.0f sigma 0.1  c %.1f.RData",NN[kk],pp[h],ng,c_interval[i])
      load(temp)
      lasso_time_matrix[i,]=c(apply(table_lass,2,mean)*100,apply(MSE,2,mean)[1])
      gln_lasso_time_matrix[i,]=c(apply(table_gln_lass,2,mean)*100,apply(MSE,2,mean)[2])
      glasso_time_matrix[i,]=c(apply(table_glass,2,mean)*100,apply(MSE,2,mean)[3])
      thlasso_time_matrix[i,]=c(apply(table_thgrp,2,mean)*100,apply(MSE,2,mean)[4])
      Aglasso_time_matrix[i,]=c(apply(table_Aglass,2,mean)*100,apply(MSE,2,mean)[5])
      #print(list(c_interval[i],summary))
    }
    for(ii in 1:length(c_interval)){
      temp=sprintf("D:/論文金融資料/分析結果/SGL n=%.0f p=%.0f c1=%.1f.RData",NN[kk],pp[h],c_interval[ii])
      load(temp)
      sglasso_time_matrix[ii,]=c(apply(table_sglass,2,mean)*100,mean(mse))
    }
    result_table= rbind(
      c(apply(lasso_time_matrix[,1:3],2,max),apply(lasso_time_matrix[,4:5],2,min))
      ,c(apply(gln_lasso_time_matrix[,1:3],2,max),apply(gln_lasso_time_matrix[,4:5],2,min))
      ,c(apply(glasso_time_matrix[,1:3],2,max),apply(glasso_time_matrix[,4:5],2,min))
      ,c(apply(thlasso_time_matrix[,1:3],2,max),apply(thlasso_time_matrix[,4:5],2,min))
      ,c(apply(Aglasso_time_matrix[,1:3],2,max),apply(Aglasso_time_matrix[,4:5],2,min))
      ,c(apply(sglasso_time_matrix[,1:3],2,max),apply(sglasso_time_matrix[,4:5],2,min)))
    colnames(result_table)=c("Precision","Recall","MCC","ERR",'MSE')
    row.names(result_table)=c("Lasso",'gln_lass',"Grp","Thgrp","Agrp","SGL")
    lamda_table=rbind(
      c_interval[c(sapply(1:3,function(x) order(lasso_time_matrix[,x],decreasing =T)[1]),sapply(4:5,function(x) order(lasso_time_matrix[,x])[1]))],
      c_interval[c(sapply(1:3,function(x) order(gln_lasso_time_matrix[,x],decreasing =T)[1]),sapply(4:5,function(x) order(gln_lasso_time_matrix[,x])[1]))],
      c_interval[c(sapply(1:3,function(x) order(glasso_time_matrix[,x],decreasing =T)[1]),sapply(4:5,function(x) order(glasso_time_matrix[,x])[1]))],
      c_interval[c(sapply(1:3,function(x) order(thlasso_time_matrix[,x],decreasing =T)[1]),sapply(4:5,function(x) order(thlasso_time_matrix[,x])[1]))],
      c_interval[c(sapply(1:3,function(x) order(Aglasso_time_matrix[,x],decreasing =T)[1]),sapply(4:5,function(x) order(Aglasso_time_matrix[,x])[1]))],
      c_interval[c(sapply(1:3,function(x) order(sglasso_time_matrix[,x],decreasing =T)[1]),sapply(4:5,function(x) order(sglasso_time_matrix[,x])[1]))]
    )
    colnames(lamda_table)=c("Precision","Recall","MCC","ERR",'MSE')
    row.names(lamda_table)=c("Lasso",'gln_lass',"Grp","Thgrp","Agrp","SGL")
    summary_table=matrix(0,6,5)
    for(i in 1:6){
      for(j in 1:5){
        summary_table[i,j]=sprintf("%.2f(%.2f)",result_table[i,j],lamda_table[i,j])
      }
    }
    colnames(summary_table)=c("Precision","Recall","MCC","ERR",'MSE')
    row.names(summary_table)=c("Lasso",'gln_lass',"Grp","Thgrp","Agrp","SGL")
    
    result[[kk+(h-1)*5]]=noquote(list(parameter=sprintf("n %.0f p %.0f ng %.0f",NN[kk],pp[h],ng),cbind.table=summary_table
                                    ,result.table=result_table,lamda.table=lamda_table))
  } 
}
result$cbind.table
AAA=Reduce(rbind,Reduce(rbind,result)[,3])
BBB=Reduce(rbind,Reduce(rbind,result)[,4])
row.names(AAA)
AAA[which(row.names(AAA)=="Lasso")[1:5],]
BBB[which(row.names(BBB)=="Lasso")[1:5],]
AAA[which(row.names(AAA)=="SGL")[1:5],]
BBB[which(row.names(BBB)=="SGL")[1:5],]
AAA[which(row.names(AAA)=="SGL")[1:5],]
AAA[which(row.names(AAA)=="SGL")[6:10],]
AAA[which(row.names(AAA)=="SGL")[11:15],]
AAA[which(row.names(AAA)=="SGL"),]
{
  plot_main=sprintf("n %.0f p %.0f",n,p)
  windows()
  par(mfrow=c(1,3))
  #precision
  plot(rate,AAA[which(row.names(AAA)=="Lasso")[1:5],1],ylim=c(40,100),cex.main=2,cex.lab=1.7,col=col[1],pch=pch[1],type='b',main="p=30 ng=3 |E|=99",ylab='Precision*100',xlab='n/p')
  lines(rate,AAA[which(row.names(AAA)=="gln_lass")[1:5],1],type='b',pch=pch[2],col=col[2])
  lines(rate,AAA[which(row.names(AAA)=="Grp")[1:5],1],type='b',pch=pch[3],col=col[3])
  lines(rate,AAA[which(row.names(AAA)=="Thgrp")[1:5],1],type='b',pch=pch[4],col=col[4])
  lines(rate,AAA[which(row.names(AAA)=="Agrp")[1:5],1],type='b',pch=pch[5],col=col[5])
  lines(rate,AAA[which(row.names(AAA)=="SGL")[1:5],1],type='b',pch=pch[6],col=col[6])
  legend(1,53,legend = c('Lasso','gln_L','Group', 'Threshold','Adaptive Group','SGL' ),cex=1.5,col=col, pch = pch)
  for(i in 1:5){
    points(rate[i],AAA[which(row.names(AAA)=="Lasso")[i],1],col=col[1],pch=pch[1])
    text(rate[i],AAA[which(row.names(AAA)=="Lasso")[i],1],BBB[which(row.names(BBB)=="Lasso")[i],1],cex=1.5,pos=1)
    points(rate[i],AAA[which(row.names(AAA)=="gln_lass")[i],1],col=col[2],pch=pch[2])
    text(rate[i],AAA[which(row.names(AAA)=="gln_lass")[i],1],BBB[which(row.names(BBB)=="gln_lass")[i],1],cex=1.5,pos=1)
    points(rate[i],AAA[which(row.names(AAA)=="Grp")[i],1],col=col[3],pch=pch[3])
    text(rate[i],AAA[which(row.names(AAA)=="Grp")[i],1],BBB[which(row.names(BBB)=="Grp")[i],1],cex=1.5,pos=1)
    points(rate[i],AAA[which(row.names(AAA)=="Thgrp")[i],1],col=col[4],pch=pch[4])
    text(rate[i],AAA[which(row.names(AAA)=="Thgrp")[i],1],BBB[which(row.names(BBB)=="Thgrp")[i],1],cex=1.5,pos=1)
    points(rate[i],AAA[which(row.names(AAA)=="Agrp")[i],1],col=col[5],pch=pch[5])
    text(rate[i],AAA[which(row.names(AAA)=="Agrp")[i],1],BBB[which(row.names(BBB)=="Agrp")[i],1],cex=1.5,pos=1)
    points(rate[i],AAA[which(row.names(AAA)=="SGL")[i],1],col=col[6],pch=pch[6])
    text(rate[i],AAA[which(row.names(AAA)=="SGL")[i],1],BBB[which(row.names(BBB)=="SGL")[i],1],cex=1.5,pos=1)
    }
  #precision2
  plot(rate,AAA[which(row.names(AAA)=="Lasso")[6:10],1],ylim=c(40,100),cex.main=2,cex.lab=1.7,pch=pch[1],col=col[1],type='b',main="p=60 ng=3 |E|=180",ylab='Precision*100',xlab='n/p')
  lines(rate,AAA[which(row.names(AAA)=="gln_lass")[6:10],1],type='b',pch=pch[2],col=col[2])
  lines(rate,AAA[which(row.names(AAA)=="Grp")[6:10],1],type='b',pch=pch[3],col=col[3])
  lines(rate,AAA[which(row.names(AAA)=="Thgrp")[6:10],1],type='b',pch=pch[4],col=col[4])
  lines(rate,AAA[which(row.names(AAA)=="Agrp")[6:10],1],type='b',pch=pch[5],col=col[5])
  lines(rate,AAA[which(row.names(AAA)=="SGL")[6:10],1],type='b',pch=pch[6],col=col[6])
  legend(1,53,legend = c('Lasso','gln_L','Group', 'Threshold','Adaptive Group','SGL' ),cex=1.5,col=col, pch = pch)
  for(i in 1:5){
    points(rate[i],AAA[which(row.names(AAA)=="Lasso")[i+5],1],col=col[1],pch=pch[1])
    text(rate[i],AAA[which(row.names(AAA)=="Lasso")[i+5],1],BBB[which(row.names(BBB)=="Lasso")[i+5],1],cex=1.5,pos=1)
    points(rate[i],AAA[which(row.names(AAA)=="gln_lass")[i+5],1],col=col[2],pch=pch[2])
    text(rate[i],AAA[which(row.names(AAA)=="gln_lass")[i+5],1],BBB[which(row.names(BBB)=="gln_lass")[i+5],1],cex=1.5,pos=1)
    points(rate[i],AAA[which(row.names(AAA)=="Grp")[i+5],1],col=col[3],pch=pch[3])
    text(rate[i],AAA[which(row.names(AAA)=="Grp")[i+5],1],BBB[which(row.names(BBB)=="Grp")[i+5],1],cex=1.5,pos=1)
    points(rate[i],AAA[which(row.names(AAA)=="Thgrp")[i+5],1],col=col[4],pch=pch[4])
    text(rate[i],AAA[which(row.names(AAA)=="Thgrp")[i+5],1],BBB[which(row.names(BBB)=="Thgrp")[i+5],1],cex=1.5,pos=1)
    points(rate[i],AAA[which(row.names(AAA)=="Agrp")[i+5],1],col=col[5],pch=pch[5])
    text(rate[i],AAA[which(row.names(AAA)=="Agrp")[i+5],1],BBB[which(row.names(BBB)=="Agrp")[i+5],1],cex=1.5,pos=1)
    points(rate[i],AAA[which(row.names(AAA)=="SGL")[i+5],1],col=col[6],pch=pch[6])
    text(rate[i],AAA[which(row.names(AAA)=="SGL")[i+5],1],BBB[which(row.names(BBB)=="SGL")[i+5],1],cex=1.5,pos=1)
  }
  #precision3
  plot(rate,AAA[which(row.names(AAA)=="Lasso")[11:15],1],ylim=c(40,100),cex.main=2,cex.lab=1.7,pch=pch[1],col=col[1],type='b',main="p=90 ng=3 |E|=306",ylab='Precision*100',xlab='n/p')
  lines(rate,AAA[which(row.names(AAA)=="gln_lass")[11:15],1],type='b',pch=pch[2],col=col[2])
  lines(rate,AAA[which(row.names(AAA)=="Grp")[11:15],1],type='b',pch=pch[3],col=col[3])
  lines(rate,AAA[which(row.names(AAA)=="Thgrp")[11:15],1],type='b',pch=pch[4],col=col[4])
  lines(rate,AAA[which(row.names(AAA)=="Agrp")[11:15],1],type='b',pch=pch[5],col=col[5])
  lines(rate,AAA[which(row.names(AAA)=="SGL")[11:15],1],type='b',pch=pch[6],col=col[6])
  legend(1,53,legend = c('Lasso','gln_L','Group', 'Threshold','Adaptive Group','SGL' ),cex=1.5,col=col, pch = pch)
  for(i in 1:5){
    points(rate[i],AAA[which(row.names(AAA)=="Lasso")[i+10],1],col=col[1],pch=pch[1])
    text(rate[i],AAA[which(row.names(AAA)=="Lasso")[i+10],1],BBB[which(row.names(BBB)=="Lasso")[i+10],1],cex=1.5,pos=1)
    points(rate[i],AAA[which(row.names(AAA)=="gln_lass")[i+10],1],col=col[2],pch=pch[2])
    text(rate[i],AAA[which(row.names(AAA)=="gln_lass")[i+10],1],BBB[which(row.names(BBB)=="gln_lass")[i+10],1],cex=1.5,pos=1)
    points(rate[i],AAA[which(row.names(AAA)=="Grp")[i+10],1],col=col[3],pch=pch[3])
    text(rate[i],AAA[which(row.names(AAA)=="Grp")[i+10],1],BBB[which(row.names(BBB)=="Grp")[i+10],1],cex=1.5,pos=1)
    points(rate[i],AAA[which(row.names(AAA)=="Thgrp")[i+10],1],col=col[4],pch=pch[4])
    text(rate[i],AAA[which(row.names(AAA)=="Thgrp")[i+10],1],BBB[which(row.names(BBB)=="Thgrp")[i+10],1],cex=1.5,pos=1)
    points(rate[i],AAA[which(row.names(AAA)=="Agrp")[i+10],1],col=col[5],pch=pch[5])
    text(rate[i],AAA[which(row.names(AAA)=="Agrp")[i+10],1],BBB[which(row.names(BBB)=="Agrp")[i+10],1],cex=1.5,pos=1)
    points(rate[i],AAA[which(row.names(AAA)=="SGL")[i+10],1],col=col[6],pch=pch[6])
    text(rate[i],AAA[which(row.names(AAA)=="SGL")[i+10],1],BBB[which(row.names(BBB)=="SGL")[i+10],1],cex=1.5,pos=1)
  }
  windows()
  par(mfrow=c(1,3))
  #ERR
  plot(rate,AAA[which(row.names(AAA)=="Lasso")[1:5],4],ylim=c(0,50),cex.main=2,cex.lab=1.7,pch=pch[1],col=col[1],type='b',main="p=30 ng=3 |E|=99",ylab='ERR*100',xlab='n/p')
  lines(rate,AAA[which(row.names(AAA)=="gln_lass")[1:5],4],type='b',pch=pch[2],col=col[2])
  lines(rate,AAA[which(row.names(AAA)=="Grp")[1:5],4],type='b',pch=pch[3],col=col[3])
  lines(rate,AAA[which(row.names(AAA)=="Thgrp")[1:5],4],type='b',pch=pch[4],col=col[4])
  lines(rate,AAA[which(row.names(AAA)=="Agrp")[1:5],4],type='b',pch=pch[5],col=col[5])
  lines(rate,AAA[which(row.names(AAA)=="SGL")[1:5],4],type='b',pch=pch[6],col=col[6])
  legend(1.1,53,legend = c('Lasso','gln_L','Group', 'Threshold','Adaptive ','SGL' ),cex=1.5,col=col, pch = pch)
  for(i in 1:5){
    points(rate[i],AAA[which(row.names(AAA)=="Lasso")[i],4],col=col[1],pch=pch[1])
    text(rate[i],AAA[which(row.names(AAA)=="Lasso")[i],4],BBB[which(row.names(BBB)=="Lasso")[i],4],cex=1.5,pos=1)
    points(rate[i],AAA[which(row.names(AAA)=="gln_lass")[i],4],col=col[2],pch=pch[2])
    text(rate[i],AAA[which(row.names(AAA)=="gln_lass")[i],4],BBB[which(row.names(BBB)=="gln_lass")[i],4],cex=1.5,pos=1)
    points(rate[i],AAA[which(row.names(AAA)=="Grp")[i],4],col=col[3],pch=pch[3])
    text(rate[i],AAA[which(row.names(AAA)=="Grp")[i],4],BBB[which(row.names(BBB)=="Grp")[i],4],cex=1.5,pos=1)
    points(rate[i],AAA[which(row.names(AAA)=="Thgrp")[i],4],col=col[4],pch=pch[4])
    text(rate[i],AAA[which(row.names(AAA)=="Thgrp")[i],4],BBB[which(row.names(BBB)=="Thgrp")[i],4],cex=1.5,pos=1)
    points(rate[i],AAA[which(row.names(AAA)=="Agrp")[i],4],col=col[5],pch=pch[5])
    text(rate[i],AAA[which(row.names(AAA)=="Agrp")[i],4],BBB[which(row.names(BBB)=="Agrp")[i],4],cex=1.5,pos=1)
    points(rate[i],AAA[which(row.names(AAA)=="SGL")[i],4],col=col[6],pch=pch[6])
    text(rate[i],AAA[which(row.names(AAA)=="SGL")[i],4],BBB[which(row.names(BBB)=="SGL")[i],4],cex=1.5,pos=1)
  }
  #ERR2
  plot(rate,AAA[which(row.names(AAA)=="Lasso")[6:10],4],ylim=c(0,50),cex.main=2,cex.lab=1.7,pch=pch[1],col=col[1],type='b',main="p=60 ng=3 |E|=180",ylab='ERR*100',xlab='n/p')
  lines(rate,AAA[which(row.names(AAA)=="gln_lass")[6:10],4],type='b',pch=pch[2],col=col[2])
  lines(rate,AAA[which(row.names(AAA)=="Grp")[6:10],4],type='b',pch=pch[3],col=col[3])
  lines(rate,AAA[which(row.names(AAA)=="Thgrp")[6:10],4],type='b',pch=pch[4],col=col[4])
  lines(rate,AAA[which(row.names(AAA)=="Agrp")[6:10],4],type='b',pch=pch[5],col=col[5])
  lines(rate,AAA[which(row.names(AAA)=="SGL")[6:10],4],type='b',pch=pch[6],col=col[6])
  legend(1.1,53,legend = c('Lasso','gln_L','Group', 'Threshold','Adaptive ','SGL' ),cex=1.5,col=col, pch = pch)
  for(i in 1:5){
    points(rate[i],AAA[which(row.names(AAA)=="Lasso")[i+5],4],col=col[1],pch=pch[1])
    text(rate[i],AAA[which(row.names(AAA)=="Lasso")[i+5],4],BBB[which(row.names(BBB)=="Lasso")[i+5],4],cex=1.5,pos=1)
    points(rate[i],AAA[which(row.names(AAA)=="gln_lass")[i+5],4],col=col[2],pch=pch[2])
    text(rate[i],AAA[which(row.names(AAA)=="gln_lass")[i+5],4],BBB[which(row.names(BBB)=="gln_lass")[i+5],4],cex=1.5,pos=1)
    points(rate[i],AAA[which(row.names(AAA)=="Grp")[i+5],4],col=col[3],pch=pch[3])
    text(rate[i],AAA[which(row.names(AAA)=="Grp")[i+5],4],BBB[which(row.names(BBB)=="Grp")[i+5],4],cex=1.5,pos=1)
    points(rate[i],AAA[which(row.names(AAA)=="Thgrp")[i+5],4],col=col[4],pch=pch[4])
    text(rate[i],AAA[which(row.names(AAA)=="Thgrp")[i+5],4],BBB[which(row.names(BBB)=="Thgrp")[i+5],4],cex=1.5,pos=1)
    points(rate[i],AAA[which(row.names(AAA)=="Agrp")[i+5],4],col=col[5],pch=pch[5])
    text(rate[i],AAA[which(row.names(AAA)=="Agrp")[i+5],4],BBB[which(row.names(BBB)=="Agrp")[i+5],4],cex=1.5,pos=1)
    points(rate[i],AAA[which(row.names(AAA)=="SGL")[i+5],4],col=col[6],pch=pch[6])
    text(rate[i],AAA[which(row.names(AAA)=="SGL")[i+5],4],BBB[which(row.names(BBB)=="SGL")[i+5],4],cex=1.5,pos=1)
  }
  #ERR3
  plot(rate,AAA[which(row.names(AAA)=="Lasso")[11:15],4],ylim=c(0,50),cex.main=2,cex.lab=1.7,pch=pch[1],col=col[1],type='b',main="p=90 ng=3 |E|=306",ylab='ERR*100',xlab='n/p')
  lines(rate,AAA[which(row.names(AAA)=="gln_lass")[11:15],4],type='b',pch=pch[2],col=col[2])
  lines(rate,AAA[which(row.names(AAA)=="Grp")[11:15],4],type='b',pch=pch[3],col=col[3])
  lines(rate,AAA[which(row.names(AAA)=="Thgrp")[11:15],4],type='b',pch=pch[4],col=col[4])
  lines(rate,AAA[which(row.names(AAA)=="Agrp")[11:15],4],type='b',pch=pch[5],col=col[5])
  lines(rate,AAA[which(row.names(AAA)=="SGL")[11:15],4],type='b',pch=pch[6],col=col[6])
  legend(1.1,53,legend = c('Lasso','gln_L','Group', 'Threshold','Adaptive ','SGL' ),cex=1.5,col=col, pch = pch)
  for(i in 1:5){
    points(rate[i],AAA[which(row.names(AAA)=="Lasso")[i+10],4],col=col[1],pch=pch[1])
    text(rate[i],AAA[which(row.names(AAA)=="Lasso")[i+10],4],BBB[which(row.names(BBB)=="Lasso")[i+10],4],cex=1.5,pos=1)
    points(rate[i],AAA[which(row.names(AAA)=="gln_lass")[i+10],4],col=col[2],pch=pch[2])
    text(rate[i],AAA[which(row.names(AAA)=="gln_lass")[i+10],4],BBB[which(row.names(BBB)=="gln_lass")[i+10],4],cex=1.5,pos=1)
    points(rate[i],AAA[which(row.names(AAA)=="Grp")[i+10],4],col=col[3],pch=pch[3])
    text(rate[i],AAA[which(row.names(AAA)=="Grp")[i+10],4],BBB[which(row.names(BBB)=="Grp")[i+10],4],cex=1.5,pos=1)
    points(rate[i],AAA[which(row.names(AAA)=="Thgrp")[i+10],4],col=col[4],pch=pch[4])
    text(rate[i],AAA[which(row.names(AAA)=="Thgrp")[i+10],4],BBB[which(row.names(BBB)=="Thgrp")[i+10],4],cex=1.5,pos=1)
    points(rate[i],AAA[which(row.names(AAA)=="Agrp")[i+10],4],col=col[5],pch=pch[5])
    text(rate[i],AAA[which(row.names(AAA)=="Agrp")[i+10],4],BBB[which(row.names(BBB)=="Agrp")[i+10],4],cex=1.5,pos=1)
    points(rate[i],AAA[which(row.names(AAA)=="SGL")[i+10],4],col=col[6],pch=pch[6])
    text(rate[i],AAA[which(row.names(AAA)=="SGL")[i+10],4],BBB[which(row.names(BBB)=="SGL")[i+10],4],cex=1.5,pos=1)
  }
  windows()
  par(mfrow=c(1,3))
  #Recall
  plot(rate,AAA[which(row.names(AAA)=="Lasso")[1:5],2],ylim=c(30,100),cex.main=2,cex.lab=1.7,pch=pch[1],col=col[1],type='b',main="p=30 ng=3 |E|=99",ylab='Recall*100',xlab='n/p')
  lines(rate,AAA[which(row.names(AAA)=="gln_lass")[1:5],2],type='b',pch=pch[2],col=col[2])
  lines(rate,AAA[which(row.names(AAA)=="Grp")[1:5],2],type='b',pch=pch[3],col=col[3])
  lines(rate,AAA[which(row.names(AAA)=="Thgrp")[1:5],2],type='b',pch=pch[4],col=col[4])
  lines(rate,AAA[which(row.names(AAA)=="Agrp")[1:5],2],type='b',pch=pch[5],col=col[5])
  lines(rate,AAA[which(row.names(AAA)=="SGL")[1:5],2],type='b',pch=pch[6],col=col[6])
  legend(1,45,legend = c('Lasso','gln_L','Group', 'Threshold','Adaptive Group','SGL' ),cex=1.5,col=col, pch = pch)
  for(i in 1:5){
    points(rate[i],AAA[which(row.names(AAA)=="Lasso")[i],2],col=col[1],pch=pch[1])
    text(rate[i],AAA[which(row.names(AAA)=="Lasso")[i],2],BBB[which(row.names(BBB)=="Lasso")[i],2],cex=1.5,pos=1)
    points(rate[i],AAA[which(row.names(AAA)=="gln_lass")[i],2],col=col[2],pch=pch[2])
    text(rate[i],AAA[which(row.names(AAA)=="gln_lass")[i],2],BBB[which(row.names(BBB)=="gln_lass")[i],2],cex=1.5,pos=1)
    points(rate[i],AAA[which(row.names(AAA)=="Grp")[i],2],col=col[3],pch=pch[3])
    text(rate[i],AAA[which(row.names(AAA)=="Grp")[i],2],BBB[which(row.names(BBB)=="Grp")[i],2],cex=1.5,pos=1)
    points(rate[i],AAA[which(row.names(AAA)=="Thgrp")[i],2],col=col[4],pch=pch[4])
    text(rate[i],AAA[which(row.names(AAA)=="Thgrp")[i],2],BBB[which(row.names(BBB)=="Thgrp")[i],2],cex=1.5,pos=1)
    points(rate[i],AAA[which(row.names(AAA)=="Agrp")[i],2],col=col[5],pch=pch[5])
    text(rate[i],AAA[which(row.names(AAA)=="Agrp")[i],2],BBB[which(row.names(BBB)=="Agrp")[i],2],cex=1.5,pos=1)
    points(rate[i],AAA[which(row.names(AAA)=="SGL")[i],2],col=col[6],pch=pch[6])
    text(rate[i],AAA[which(row.names(AAA)=="SGL")[i],2],BBB[which(row.names(BBB)=="SGL")[i],2],cex=1.5,pos=1)
  }
  #Recall2
  plot(rate,AAA[which(row.names(AAA)=="Lasso")[6:10],2],ylim=c(43,100),cex.main=2,cex.lab=1.7,pch=pch[1],col=col[1],type='b',main="p=60 ng=3 |E|=180",ylab='Recall*100',xlab='n/p')
  lines(rate,AAA[which(row.names(AAA)=="gln_lass")[6:10],2],type='b',pch=pch[2],col=col[2])
  lines(rate,AAA[which(row.names(AAA)=="Grp")[6:10],2],type='b',pch=pch[3],col=col[3])
  lines(rate,AAA[which(row.names(AAA)=="Thgrp")[6:10],2],type='b',pch=pch[4],col=col[4])
  lines(rate,AAA[which(row.names(AAA)=="Agrp")[6:10],2],type='b',pch=pch[5],col=col[5])
  lines(rate,AAA[which(row.names(AAA)=="SGL")[6:10],2],type='b',pch=pch[6],col=col[6])
  legend(1,56,legend = c('Lasso','gln_L','Group', 'Threshold','Adaptive Group','SGL' ),cex=1.5,col=col, pch = pch)
  for(i in 1:5){
    points(rate[i],AAA[which(row.names(AAA)=="Lasso")[i+5],2],col=col[1],pch=pch[1])
    text(rate[i],AAA[which(row.names(AAA)=="Lasso")[i+5],2],BBB[which(row.names(BBB)=="Lasso")[i+5],2],cex=1.5,pos=1)
    points(rate[i],AAA[which(row.names(AAA)=="gln_lass")[i+5],2],col=col[2],pch=pch[2])
    text(rate[i],AAA[which(row.names(AAA)=="gln_lass")[i+5],2],BBB[which(row.names(BBB)=="gln_lass")[i+5],2],cex=1.5,pos=1)
    points(rate[i],AAA[which(row.names(AAA)=="Grp")[i+5],2],col=col[3],pch=pch[3])
    text(rate[i],AAA[which(row.names(AAA)=="Grp")[i+5],2],BBB[which(row.names(BBB)=="Grp")[i+5],2],cex=1.5,pos=1)
    points(rate[i],AAA[which(row.names(AAA)=="Thgrp")[i+5],2],col=col[4],pch=pch[4])
    text(rate[i],AAA[which(row.names(AAA)=="Thgrp")[i+5],2],BBB[which(row.names(BBB)=="Thgrp")[i+5],2],cex=1.5,pos=1)
    points(rate[i],AAA[which(row.names(AAA)=="Agrp")[i+5],2],col=col[5],pch=pch[5])
    text(rate[i],AAA[which(row.names(AAA)=="Agrp")[i+5],2],BBB[which(row.names(BBB)=="Agrp")[i+5],2],cex=1.5,pos=1)
    points(rate[i],AAA[which(row.names(AAA)=="SGL")[i+5],2],col=col[6],pch=pch[6])
    text(rate[i],AAA[which(row.names(AAA)=="SGL")[i+5],2],BBB[which(row.names(BBB)=="SGL")[i+5],2],cex=1.5,pos=1)
  }
  #Recall3
  plot(rate,AAA[which(row.names(AAA)=="Lasso")[11:15],2],ylim=c(43,100),cex.main=2,cex.lab=1.7,pch=pch[1],col=col[1],type='b',main="p=90 ng=3 |E|=306",ylab='Recall*100',xlab='n/p')
  lines(rate,AAA[which(row.names(AAA)=="gln_lass")[11:15],2],type='b',pch=pch[2],col=col[2])
  lines(rate,AAA[which(row.names(AAA)=="Grp")[11:15],2],type='b',pch=pch[3],col=col[3])
  lines(rate,AAA[which(row.names(AAA)=="Thgrp")[11:15],2],type='b',pch=pch[4],col=col[4])
  lines(rate,AAA[which(row.names(AAA)=="Agrp")[11:15],2],type='b',pch=pch[5],col=col[5])
  lines(rate,AAA[which(row.names(AAA)=="SGL")[11:15],2],type='b',pch=pch[6],col=col[6])
  legend(1,56,legend = c('Lasso','gln_L','Group', 'Threshold','Adaptive Group',"SGL" ),cex=1.5,col=col, pch = pch)
  for(i in 1:5){
    points(rate[i],AAA[which(row.names(AAA)=="Lasso")[i+10],2],col=col[1],pch=pch[1])
    text(rate[i],AAA[which(row.names(AAA)=="Lasso")[i+10],2],BBB[which(row.names(BBB)=="Lasso")[i+10],2],cex=1.5,pos=1)
    points(rate[i],AAA[which(row.names(AAA)=="gln_lass")[i+10],2],col=col[2],pch=pch[2])
    text(rate[i],AAA[which(row.names(AAA)=="gln_lass")[i+10],2],BBB[which(row.names(BBB)=="gln_lass")[i+10],2],cex=1.5,pos=1)
    points(rate[i],AAA[which(row.names(AAA)=="Grp")[i+10],2],col=col[3],pch=pch[3])
    text(rate[i],AAA[which(row.names(AAA)=="Grp")[i+10],2],BBB[which(row.names(BBB)=="Grp")[i+10],2],cex=1.5,pos=1)
    points(rate[i],AAA[which(row.names(AAA)=="Thgrp")[i+10],2],col=col[4],pch=pch[4])
    text(rate[i],AAA[which(row.names(AAA)=="Thgrp")[i+10],2],BBB[which(row.names(BBB)=="Thgrp")[i+10],2],cex=1.5,pos=1)
    points(rate[i],AAA[which(row.names(AAA)=="Agrp")[i+10],2],col=col[5],pch=pch[5])
    text(rate[i],AAA[which(row.names(AAA)=="Agrp")[i+10],2],BBB[which(row.names(BBB)=="Agrp")[i+10],2],cex=1.5,pos=1)
    points(rate[i],AAA[which(row.names(AAA)=="SGL")[i+10],2],col=col[6],pch=pch[6])
    text(rate[i],AAA[which(row.names(AAA)=="SGL")[i+10],2],BBB[which(row.names(BBB)=="SGL")[i+10],2],cex=1.5,pos=1)
  }
  windows()
  par(mfrow=c(1,3))
  #MCC
  plot(rate,AAA[which(row.names(AAA)=="Lasso")[1:5],3],ylim=c(30,100),cex.main=2,cex.lab=1.7,pch=pch[1],col=col[1],type='b',main="p=30 ng=3 |E|=99",ylab='MCC*100',xlab='n/p')
  lines(rate,AAA[which(row.names(AAA)=="gln_lass")[1:5],3],type='b',pch=pch[2],col=col[2])
  lines(rate,AAA[which(row.names(AAA)=="Grp")[1:5],3],type='b',pch=pch[3],col=col[3])
  lines(rate,AAA[which(row.names(AAA)=="Thgrp")[1:5],3],type='b',pch=pch[4],col=col[4])
  lines(rate,AAA[which(row.names(AAA)=="Agrp")[1:5],3],type='b',pch=pch[5],col=col[5])
  lines(rate,AAA[which(row.names(AAA)=="SGL")[1:5],3],type='b',pch=pch[6],col=col[6])
  legend(1.1,45,legend = c('Lasso','gln_L','Group', 'Threshold','Adaptive Group','SGL' ),cex=1.5,col=col, pch = pch)
  for(i in 1:5){
    points(rate[i],AAA[which(row.names(AAA)=="Lasso")[i],3],col=col[1],pch=pch[1])
    text(rate[i],AAA[which(row.names(AAA)=="Lasso")[i],3],BBB[which(row.names(BBB)=="Lasso")[i],3],cex=1.5,pos=1)
    points(rate[i],AAA[which(row.names(AAA)=="gln_lass")[i],3],col=col[2],pch=pch[2])
    text(rate[i],AAA[which(row.names(AAA)=="gln_lass")[i],3],BBB[which(row.names(BBB)=="gln_lass")[i],3],cex=1.5,pos=1)
    points(rate[i],AAA[which(row.names(AAA)=="Grp")[i],3],col=col[3],pch=pch[3])
    text(rate[i],AAA[which(row.names(AAA)=="Grp")[i],3],BBB[which(row.names(BBB)=="Grp")[i],3],cex=1.5,pos=1)
    points(rate[i],AAA[which(row.names(AAA)=="Thgrp")[i],3],col=col[4],pch=pch[4])
    text(rate[i],AAA[which(row.names(AAA)=="Thgrp")[i],3],BBB[which(row.names(BBB)=="Thgrp")[i],3],cex=1.5,pos=1)
    points(rate[i],AAA[which(row.names(AAA)=="Agrp")[i],3],col=col[5],pch=pch[5])
    text(rate[i],AAA[which(row.names(AAA)=="Agrp")[i],3],BBB[which(row.names(BBB)=="Agrp")[i],3],cex=1.5,pos=1)
    points(rate[i],AAA[which(row.names(AAA)=="SGL")[i],3],col=col[6],pch=pch[6])
    text(rate[i],AAA[which(row.names(AAA)=="SGL")[i],3],BBB[which(row.names(BBB)=="SGL")[i],3],cex=1.5,pos=1)
  }
  #MCC2
  plot(rate,AAA[which(row.names(AAA)=="Lasso")[6:10],3],ylim=c(40,100),cex.main=2,cex.lab=1.7,pch=pch[1],col=col[1],type='b',main="p=60 ng=3 |E|=180",ylab='MCC*100',xlab='n/p')
  lines(rate,AAA[which(row.names(AAA)=="gln_lass")[6:10],3],type='b',pch=pch[2],col=col[2])
  lines(rate,AAA[which(row.names(AAA)=="Grp")[6:10],3],type='b',pch=pch[3],col=col[3])
  lines(rate,AAA[which(row.names(AAA)=="Thgrp")[6:10],3],type='b',pch=pch[4],col=col[4])
  lines(rate,AAA[which(row.names(AAA)=="Agrp")[6:10],3],type='b',pch=pch[5],col=col[5])
  lines(rate,AAA[which(row.names(AAA)=="SGL")[6:10],3],type='b',pch=pch[6],col=col[6])
  legend(1,53,legend = c('Lasso','gln_L','Group', 'Threshold','Adaptive Group','SGL' ),cex=1.5,col=col, pch = pch)
  for(i in 1:5){
    points(rate[i],AAA[which(row.names(AAA)=="Lasso")[i+5],3],col=col[1],pch=pch[1])
    text(rate[i],AAA[which(row.names(AAA)=="Lasso")[i+5],3],BBB[which(row.names(BBB)=="Lasso")[i+5],3],cex=1.5,pos=1)
    points(rate[i],AAA[which(row.names(AAA)=="gln_lass")[i+5],3],col=col[2],pch=pch[2])
    text(rate[i],AAA[which(row.names(AAA)=="gln_lass")[i+5],3],BBB[which(row.names(BBB)=="gln_lass")[i+5],3],cex=1.5,pos=1)
    points(rate[i],AAA[which(row.names(AAA)=="Grp")[i+5],3],col=col[3],pch=pch[3])
    text(rate[i],AAA[which(row.names(AAA)=="Grp")[i+5],3],BBB[which(row.names(BBB)=="Grp")[i+5],3],cex=1.5,pos=1)
    points(rate[i],AAA[which(row.names(AAA)=="Thgrp")[i+5],3],col=col[4],pch=pch[4])
    text(rate[i],AAA[which(row.names(AAA)=="Thgrp")[i+5],3],BBB[which(row.names(BBB)=="Thgrp")[i+5],3],cex=1.5,pos=1)
    points(rate[i],AAA[which(row.names(AAA)=="Agrp")[i+5],3],col=col[5],pch=pch[5])
    text(rate[i],AAA[which(row.names(AAA)=="Agrp")[i+5],3],BBB[which(row.names(BBB)=="Agrp")[i+5],3],cex=1.5,pos=1)
    points(rate[i],AAA[which(row.names(AAA)=="SGL")[i+5],3],col=col[6],pch=pch[6])
    text(rate[i],AAA[which(row.names(AAA)=="SGL")[i+5],3],BBB[which(row.names(BBB)=="SGL")[i+5],3],cex=1.5,pos=1)
  }
  #MCC3
  plot(rate,AAA[which(row.names(AAA)=="Lasso")[11:15],3],ylim=c(40,100),cex.main=2,cex.lab=1.7,pch=pch[1],col=col[1],type='b',main="p=90 ng=3 |E|=306",ylab='MCC*100',xlab='n/p')
  lines(rate,AAA[which(row.names(AAA)=="gln_lass")[11:15],3],type='b',pch=pch[2],col=col[2])
  lines(rate,AAA[which(row.names(AAA)=="Grp")[11:15],3],type='b',pch=pch[3],col=col[3])
  lines(rate,AAA[which(row.names(AAA)=="Thgrp")[11:15],3],type='b',pch=pch[4],col=col[4])
  lines(rate,AAA[which(row.names(AAA)=="Agrp")[11:15],3],type='b',pch=pch[5],col=col[5])
  lines(rate,AAA[which(row.names(AAA)=="SGL")[11:15],3],type='b',pch=pch[6],col=col[6])
  legend(1,53,legend = c('Lasso','gln_L','Group', 'Threshold','Adaptive Group','SGL' ),cex=1.5,col=col, pch = pch)
  for(i in 1:5){
    points(rate[i],AAA[which(row.names(AAA)=="Lasso")[i+10],3],col=col[1],pch=pch[1])
    text(rate[i],AAA[which(row.names(AAA)=="Lasso")[i+10],3],BBB[which(row.names(BBB)=="Lasso")[i+10],3],cex=1.5,pos=1)
    points(rate[i],AAA[which(row.names(AAA)=="gln_lass")[i+10],3],col=col[2],pch=pch[2])
    text(rate[i],AAA[which(row.names(AAA)=="gln_lass")[i+10],3],BBB[which(row.names(BBB)=="gln_lass")[i+10],3],cex=1.5,pos=1)
    points(rate[i],AAA[which(row.names(AAA)=="Grp")[i+10],3],col=col[3],pch=pch[3])
    text(rate[i],AAA[which(row.names(AAA)=="Grp")[i+10],3],BBB[which(row.names(BBB)=="Grp")[i+10],3],cex=1.5,pos=1)
    points(rate[i],AAA[which(row.names(AAA)=="Thgrp")[i+10],3],col=col[4],pch=pch[4])
    text(rate[i],AAA[which(row.names(AAA)=="Thgrp")[i+10],3],BBB[which(row.names(BBB)=="Thgrp")[i+10],3],cex=1.5,pos=1)
    points(rate[i],AAA[which(row.names(AAA)=="Agrp")[i+10],3],col=col[5],pch=pch[5])
    text(rate[i],AAA[which(row.names(AAA)=="Agrp")[i+10],3],BBB[which(row.names(BBB)=="Agrp")[i+10],3],cex=1.5,pos=1)
    points(rate[i],AAA[which(row.names(AAA)=="SGL")[i+10],3],col=col[6],pch=pch[6])
    text(rate[i],AAA[which(row.names(AAA)=="SGL")[i+10],3],BBB[which(row.names(BBB)=="SGL")[i+10],3],cex=1.5,pos=1)
  }
  windows()
  par(mfrow=c(1,3))
  #MSE
  plot(rate,AAA[which(row.names(AAA)=="Lasso")[1:5],5],ylim=c(0,0.8),cex.main=2,cex.lab=1.7,pch=pch[1],col=col[1],type='b',main="p=30 ng=3 |E|=99",ylab='MSE',xlab='n/p')
  lines(rate,AAA[which(row.names(AAA)=="gln_lass")[1:5],5],type='b',pch=pch[2],col=col[2])
  lines(rate,AAA[which(row.names(AAA)=="Grp")[1:5],5],type='b',pch=pch[3],col=col[3])
  lines(rate,AAA[which(row.names(AAA)=="Thgrp")[1:5],5],type='b',pch=pch[4],col=col[4])
  lines(rate,AAA[which(row.names(AAA)=="Agrp")[1:5],5],type='b',pch=pch[5],col=col[5])
  legend(1,0.8,legend = c('Lasso','gln_L','Group', 'Threshold','Adaptive Group' ),cex=1.5,col=col, pch = pch)
  for(i in 1:5){
    points(rate[i],AAA[which(row.names(AAA)=="Lasso")[i],5],col=col[1],pch=pch[1])
    text(rate[i],AAA[which(row.names(AAA)=="Lasso")[i],5],BBB[which(row.names(BBB)=="Lasso")[i],5],cex=1.5,pos=1)
    points(rate[i],AAA[which(row.names(AAA)=="gln_lass")[i],5],col=col[2],pch=pch[2])
    text(rate[i],AAA[which(row.names(AAA)=="gln_lass")[i],5],BBB[which(row.names(BBB)=="gln_lass")[i],5],cex=1.5,pos=1)
    points(rate[i],AAA[which(row.names(AAA)=="Grp")[i],5],col=col[3],pch=pch[3])
    text(rate[i],AAA[which(row.names(AAA)=="Grp")[i],5],BBB[which(row.names(BBB)=="Grp")[i],5],cex=1.5,pos=1)
    points(rate[i],AAA[which(row.names(AAA)=="Thgrp")[i],5],col=col[4],pch=pch[4])
    text(rate[i],AAA[which(row.names(AAA)=="Thgrp")[i],5],BBB[which(row.names(BBB)=="Thgrp")[i],5],cex=1.5,pos=1)
    points(rate[i],AAA[which(row.names(AAA)=="Agrp")[i],5],col=col[5],pch=pch[5])
    text(rate[i],AAA[which(row.names(AAA)=="Agrp")[i],5],BBB[which(row.names(BBB)=="Agrp")[i],5],cex=1.5,pos=1)
  }
  #MSE2
  plot(rate,AAA[which(row.names(AAA)=="Lasso")[6:10],5],ylim=c(0,0.8),cex.main=2,cex.lab=1.7,pch=pch[1],col=col[1],type='b',main="p=60 ng=3 |E|=180",ylab='MSE',xlab='n/p')
  lines(rate,AAA[which(row.names(AAA)=="gln_lass")[6:10],5],type='b',pch=pch[2],col=col[2])
  lines(rate,AAA[which(row.names(AAA)=="Grp")[6:10],5],type='b',pch=pch[3],col=col[3])
  lines(rate,AAA[which(row.names(AAA)=="Thgrp")[6:10],5],type='b',pch=pch[4],col=col[4])
  lines(rate,AAA[which(row.names(AAA)=="Agrp")[6:10],5],type='b',pch=pch[5],col=col[5])
  legend(1,0.8,legend = c('Lasso','gln_L','Group', 'Threshold','Adaptive Group' ),cex=1.5,col=col, pch = pch)
  for(i in 1:5){
    points(rate[i],AAA[which(row.names(AAA)=="Lasso")[i+5],5],col=col[1],pch=pch[1])
    text(rate[i],AAA[which(row.names(AAA)=="Lasso")[i+5],5],BBB[which(row.names(BBB)=="Lasso")[i+5],5],cex=1.5,pos=1)
    points(rate[i],AAA[which(row.names(AAA)=="gln_lass")[i+5],5],col=col[2],pch=pch[2])
    text(rate[i],AAA[which(row.names(AAA)=="gln_lass")[i+5],5],BBB[which(row.names(BBB)=="gln_lass")[i+5],5],cex=1.5,pos=1)
    points(rate[i],AAA[which(row.names(AAA)=="Grp")[i+5],5],col=col[3],pch=pch[3])
    text(rate[i],AAA[which(row.names(AAA)=="Grp")[i+5],5],BBB[which(row.names(BBB)=="Grp")[i+5],5],cex=1.5,pos=1)
    points(rate[i],AAA[which(row.names(AAA)=="Thgrp")[i+5],5],col=col[4],pch=pch[4])
    text(rate[i],AAA[which(row.names(AAA)=="Thgrp")[i+5],5],BBB[which(row.names(BBB)=="Thgrp")[i+5],5],cex=1.5,pos=1)
    points(rate[i],AAA[which(row.names(AAA)=="Agrp")[i+5],5],col=col[5],pch=pch[5])
    text(rate[i],AAA[which(row.names(AAA)=="Agrp")[i+5],5],BBB[which(row.names(BBB)=="Agrp")[i+5],5],cex=1.5,pos=1)
  }
  #MSE3
  plot(rate,AAA[which(row.names(AAA)=="Lasso")[11:15],5],ylim=c(0,0.8),cex.main=2,cex.lab=1.7,pch=pch[1],col=col[1],type='b',main="p=90 ng=3 |E|=306",ylab='MSE',xlab='n/p')
  lines(rate,AAA[which(row.names(AAA)=="gln_lass")[11:15],5],type='b',pch=pch[2],col=col[2])
  lines(rate,AAA[which(row.names(AAA)=="Grp")[11:15],5],type='b',pch=pch[3],col=col[3])
  lines(rate,AAA[which(row.names(AAA)=="Thgrp")[11:15],5],type='b',pch=pch[4],col=col[4])
  lines(rate,AAA[which(row.names(AAA)=="Agrp")[11:15],5],type='b',pch=pch[5],col=col[5])
  legend(1,0.8,legend = c('Lasso','gln_L','Group', 'Threshold','Adaptive Group' ),cex=1.5,col=col, pch = pch)
  for(i in 1:5){
    points(rate[i],AAA[which(row.names(AAA)=="Lasso")[i+10],5],col=col[1],pch=pch[1])
    text(rate[i],AAA[which(row.names(AAA)=="Lasso")[i+10],5],BBB[which(row.names(BBB)=="Lasso")[i+10],5],cex=1.5,pos=1)
    points(rate[i],AAA[which(row.names(AAA)=="gln_lass")[i+10],5],col=col[2],pch=pch[2])
    text(rate[i],AAA[which(row.names(AAA)=="gln_lass")[i+10],5],BBB[which(row.names(BBB)=="gln_lass")[i+10],5],cex=1.5,pos=1)
    points(rate[i],AAA[which(row.names(AAA)=="Grp")[i+10],5],col=col[3],pch=pch[3])
    text(rate[i],AAA[which(row.names(AAA)=="Grp")[i+10],5],BBB[which(row.names(BBB)=="Grp")[i+10],5],cex=1.5,pos=1)
    points(rate[i],AAA[which(row.names(AAA)=="Thgrp")[i+10],5],col=col[4],pch=pch[4])
    text(rate[i],AAA[which(row.names(AAA)=="Thgrp")[i+10],5],BBB[which(row.names(BBB)=="Thgrp")[i+10],5],cex=1.5,pos=1)
    points(rate[i],AAA[which(row.names(AAA)=="Agrp")[i+10],5],col=col[5],pch=pch[5])
    text(rate[i],AAA[which(row.names(AAA)=="Agrp")[i+10],5],BBB[which(row.names(BBB)=="Agrp")[i+10],5],cex=1.5,pos=1)
  }
}









#===================
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
  temp=sprintf("D:/論文金融資料/分析結果/2c MSE,sum_matrix n %.0f p %.0f ng %.0f sigma 0.1  c %.1f.RData",n,p,ng,c_interval[i])
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
result[[2]]=list(parameter=sprintf("n %.0f p %.0f ng %.0f",n,p,ng),table=result.miss_table)

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
  #ERR
  plot(c_interval,lasso_time_matrix[,4],ylim=c(1,100),col=col[1],type='b',main=plot_main,ylab='ERR*100',xlab='c1')
  lines(c_interval,gln_lasso_time_matrix[,4],type='b',col=col[2])
  lines(c_interval,glasso_time_matrix[,4],type='b',col=col[3])
  lines(c_interval,thlasso_time_matrix[,4],type='b',col=col[4])
  lines(c_interval,Aglasso_time_matrix[,4],type='b',col=col[5])
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
  #MCC
  plot(c_interval,lasso_time_matrix[,3],ylim=c(1,100),col=col[1],type='b',main=plot_main,ylab='MCC*100',xlab='c1')
  lines(c_interval,gln_lasso_time_matrix[,3],type='b',col=col[2])
  lines(c_interval,glasso_time_matrix[,3],type='b',col=col[3])
  lines(c_interval,thlasso_time_matrix[,3],type='b',col=col[4])
  lines(c_interval,Aglasso_time_matrix[,3],type='b',col=col[5])
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
  
  #ROC
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
    text(point_lamda[i,1],point_lamda[i,2],labels=c_interval[location[i]],cex=1.5,pos=1)
  }
  legend(0.35,0.4,legend = c('Lasso','gln_L','Group', 'Threshold','Adaptive Group' ),col=col, pch = 1)

}


{
  library(gplots)
  load("D:/論文金融資料/分析結果/miss MSE,sum_matrix n 60 p 30 sigma 0.1  c 0.5.RData")
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



{
  par(mfrow=c(3,2))
  plot(0,0,ylim=c(-1.5,1.5))
  legend('center',cex=0.9,legend = c('Lasso','gln_L','Group', 'Threshold','Adaptive Group' ),col=col, pch = 1)
  
  #precision
  plot(c_interval,lasso_time_matrix[,1],ylim=c(1,100),col=col[1],type='b',main='n 60 p 30',ylab='Precision*100',xlab='c1')
  lines(c_interval,gln_lasso_time_matrix[,1],type='b',col=col[2])
  lines(c_interval,glasso_time_matrix[,1],type='b',col=col[3])
  lines(c_interval,thlasso_time_matrix[,1],type='b',col=col[4])
  lines(c_interval,Aglasso_time_matrix[,1],type='b',col=col[5])
  #legend(0.57,35,legend = c('Lasso','gln_L','Group', 'Threshold','Adaptive Group' ),col=c(1,3,4,7,6), pch = 1)
  #Recall
  plot(c_interval,lasso_time_matrix[,2],ylim=c(1,100),col=col[1],type='b',main='n 60 p 30',ylab='Recall*100',xlab='c1')
  lines(c_interval,gln_lasso_time_matrix[,2],type='b',col=col[2])
  lines(c_interval,glasso_time_matrix[,2],type='b',col=col[3])
  lines(c_interval,thlasso_time_matrix[,2],type='b',col=col[4])
  lines(c_interval,Aglasso_time_matrix[,2],type='b',col=col[5])
  #legend(0,35,legend = c('Lasso','gln_L','Group', 'Threshold','Adaptive Group' ),col=c(1,3,4,7,6), pch = 1)
  #MCC
  plot(c_interval,lasso_time_matrix[,3],ylim=c(1,100),col=col[1],type='b',main='n 60 p 30',ylab='MCC*100',xlab='c1')
  lines(c_interval,gln_lasso_time_matrix[,3],type='b',col=col[2])
  lines(c_interval,glasso_time_matrix[,3],type='b',col=col[3])
  lines(c_interval,thlasso_time_matrix[,3],type='b',col=col[4])
  lines(c_interval,Aglasso_time_matrix[,3],type='b',col=col[5])
  #legend(0.57,35,legend = c('Lasso','gln_L','Group', 'Threshold','Adaptive Group' ),col=c(1,3,4,7,6), pch = 1)
  #ERR
  plot(c_interval,lasso_time_matrix[,4],ylim=c(1,100),col=col[1],type='b',main='n 60 p 30',ylab='ERR*100',xlab='c1')
  lines(c_interval,gln_lasso_time_matrix[,4],type='b',col=col[2])
  lines(c_interval,glasso_time_matrix[,4],type='b',col=col[3])
  lines(c_interval,thlasso_time_matrix[,4],type='b',col=col[4])
  lines(c_interval,Aglasso_time_matrix[,4],type='b',col=col[5])
  #legend(0.57,100,legend = c('Lasso','gln_L','Group', 'Threshold','Adaptive Group' ),col=c(1,3,4,7,6), pch = 1)
  #MSE
  plot(c_interval,lasso_time_matrix[,5],ylim=c(0,0.4),col=col[1],type='b',main='n 60 p 30',ylab='MSE*100',xlab='c1')
  lines(c_interval,gln_lasso_time_matrix[,5],type='b',col=col[2])
  lines(c_interval,glasso_time_matrix[,5],type='b',col=col[3])
  lines(c_interval,thlasso_time_matrix[,5],type='b',col=col[4])
  lines(c_interval,Aglasso_time_matrix[,5],type='b',col=col[5])
  #legend(0.57,0.15,legend = c('Lasso','gln_L','Group', 'Threshold','Adaptive Group' ),col=c(1,3,4,7,6), pch = 1)
  
}




for(i in 1:length(c_interval)){
  temp=sprintf("D:/論文金融資料/分析結果/miss MSE,sum_matrix n %.0f p %.0f ng %.0f sigma 0.1  c %.1f.RData",n,p,ng,c_interval[i])
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

