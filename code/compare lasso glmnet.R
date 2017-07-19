library(gplots)
ng=3
pp=c(30,60,90)
rate=c(0.5,0.8,1,1.2,1.5)
for(h in 1:1){
  NN=pp[h]*rate
  for(k in 1:5){
    temp=sprintf("D:/論文金融資料/分析結果/2c MSE,sum_matrix n %.0f p %.0f ng %.0f sigma 0.1  c 0.0.RData",NN[k],pp[h],ng)
    load(temp)
    rowsum_0_A1=which(rowSums(matrix(as.numeric(abs(A1)>0),p,p))==0)
    #rowsum_0_A1
    lasso_lamda=t(matrix(Reduce(rbind,summary_lamda)[,1],p,N))
    glnlasso_lamda=t(matrix(Reduce(rbind,summary_lamda)[,2],p,N))
    glasso_lamda=t(matrix(Reduce(rbind,summary_lamda)[,3],p,N))
    Aglasso_lamda=t(matrix(Reduce(rbind,summary_lamda)[,4],p,N))
    thlasso_lamda=t(matrix(Reduce(rbind,summary_lamda)[,5],p,N))
    diff_lasso_gln_lamda=lasso_lamda-glnlasso_lamda
    col_row=rep(0,p)
    col_row[rowSums(matrix(as.numeric(abs(A1)>0),p,p))==0]='blue'
    setwd("D:/郭老咪聽/論文/thesis_format/論文書面")
    #postscript(paste("diff",k,".eps",sep=""))
    windows()
    par(mfrow=c(1,3))
    boxplot(lasso_lamda,col = col_row,main=paste('greLasso n=',n,"p=",p),ylab="lambda",xlab="variable",cex.main=2.5,cex.lab=2)
    boxplot(glnlasso_lamda,col = col_row,main=paste('glmLasso n=',n,"p=",p),ylab="lambda",xlab="variable",cex.main=2.5,cex.lab=2)
    #dev.off()
    boxplot(diff_lasso_gln_lamda,col = col_row,main=paste('greLasso - glmLasso n=',n,"p=",p),ylab="lambda",xlab="variable",cex.main=2.5,cex.lab=2)
  }
}
windows()
heatmap.2(matrix(as.numeric(abs(A1)>0),p,p),main = 'True',scale = "none",Rowv = F,Colv = F,key=FALSE,trace='none',dendrogram="none",col=colorpanel(2*N,"white","darkgrey","black"))
TA1=cbind(matrix(0,p,(TT-2)*p),A1)
windows()
heatmap.2(matrix(as.numeric(abs(TA1)>0),p,(TT-1)*p),main = 'True',scale = "none",Rowv = F,Colv = F,key=FALSE,trace='none',dendrogram="none",col=colorpanel(2*N,"white","darkgrey","black"))
sum(A1!=0)
sum(rowSums(A1)==0)
#boxplot(glasso_lamda,col = col_row,main=paste('gLasso',c1_cut))
#boxplot(Aglasso_lamda,col = col_row,main=paste('AgLasso',c1_cut))
#boxplot(thlasso_lamda,col = col_row,main=paste('thLasso',c1_cut))
for(h in 1:1){
  NN=pp[h]*rate
  for(k in 1:5){
temp=sprintf("D:/論文金融資料/分析結果/2c MSE,sum_matrix n %.0f p %.0f ng %.0f sigma 0.1  c 0.0.RData",NN[k],pp[h],ng)
load(temp)
print(summary)
  }
}

