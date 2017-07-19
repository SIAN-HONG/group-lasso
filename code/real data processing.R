setwd("D:/論文金融資料/DATA")
time=c(20141231,
       20150331,20150630,20150930,20151231,
       20160331,20160630,20160930,20161231,20170331)
obj=c("_Assets and Liabilities.csv",
      "_Cash and Balances Due.csv",
      "_Securities.csv",
      "_Net Loans and Leases.csv",
      "_Total Interest Income.csv",
      "_Deposits Based on the $250,000 Reporting Threshold.csv"
)
i=1
j=1
for(i in 1:10){
  setwd("D:/論文金融資料/DATA")
  for(j in 1:6){
    pas=paste('All_Reports_', time[i], sep='',obj[j])
    if(j==1){
      A=read.csv(pas,header = T)[,c("name","asset")]
    }else if(j==6){
      B=read.csv(pas,header = T)[,c("IDdepsam","DEPSMRA",
                                    "IDdeplam","DEPLGRA")]
      A=cbind(A,B)
    }else if(j==4){
      B=read.csv(pas,header = T)[,c("lnre",	"lnag",	"lnci",	"lncon")]
      A=cbind(A,B)
    }else if(j==5){
      B=read.csv(pas,header = T)[,c("itrade",	"ifrepo")]
      A=cbind(A,B)
    }else if(j==3){
      B=read.csv(pas,header = T)[,c("scus","scust","scmuni",
                                    "SCDOMO","idscod","scford","sceq")]
      A=cbind(A,B)
    }else if(j==2){
      B=read.csv(pas,header = T)[,c("chbal","chbalni")]
      A=cbind(A,B)
    }
  }
  setwd("D:/論文金融資料/整理後data")
  write.csv(A,paste(time[i],"remove.csv",sep=''))
}

setwd("D:/論文金融資料/整理後data")
#命名A1~A10
for(i in 1:10) { 
  nam <- paste("A",i, sep="")
  assign(nam,read.csv(paste(time[i],"remove.csv",sep=''),header = T)[,-1])
}

#拉
summary(A1)
c=intersect(A1[,1],A2[,1])
c=intersect(c,A3[,1])
c=intersect(c,A4[,1])
c=intersect(c,A5[,1])
c=intersect(c,A6[,1])
c=intersect(c,A7[,1])
c=intersect(c,A8[,1])
c=intersect(c,A9[,1])
c=intersect(c,A10[,1])
c[1042]
C=NULL
for(i in 1:length(c)){
  C[i]=( sum(as.numeric(A1[which(A1[,1]%in%c[i]),2]))
         +sum(as.numeric(A2[which(A2[,1]%in%c[i]),2]))
         +sum(as.numeric(A3[which(A3[,1]%in%c[i]),2]))
         +sum(as.numeric(A4[which(A4[,1]%in%c[i]),2]))
         +sum(as.numeric(A5[which(A5[,1]%in%c[i]),2]))
         +sum(as.numeric(A6[which(A6[,1]%in%c[i]),2]))
         +sum(as.numeric(A7[which(A7[,1]%in%c[i]),2]))
         +sum(as.numeric(A8[which(A8[,1]%in%c[i]),2]))
         +sum(as.numeric(A9[which(A9[,1]%in%c[i]),2]))
         +sum(as.numeric(A10[which(A10[,1]%in%c[i]),2])))
}
library(VIM)
C_50=cbind(c[order(C,decreasing = T)]
           , C[order(C,decreasing = T)])[1:50,]

NA_knn=function(A1,k,A9){
  for(i in 1:dim(A1)[2]){
    n=length(which(is.na(A1[,i])))
    A9_new=A9[!is.na(A9[,i]),]
    na_location=which(is.na(A1[,i]))
    if(n!=0){
      for(j in 1:n){
        asset=A1[na_location[j],2]
        orde=order(abs(A9_new[,2]-asset))[1:k]
        A1[na_location[j],i]=median(A9_new[orde,i])
      }
    }
    
  }
  return(A1)
}
A1=NA_knn(A1,k=5,A1)
A2=NA_knn(A2,k=5,A2)
A3=NA_knn(A3,k=5,A3)
A4=NA_knn(A4,k=5,A4)
A5=NA_knn(A5,k=5,A5)
A6=NA_knn(A6,k=5,A6)
A7=NA_knn(A7,k=5,A7)
A8=NA_knn(A8,k=5,A8)
A9=NA_knn(A9,k=5,A9)
A10=NA_knn(A10,k=5,A10)

a=matrix(0,50,20*10)
for(i in 1:dim(C_50)[1]){
  x=cbind(apply(A1[which(A1[,1]==C_50[i,1]),c(-1,-2)],2,sum),
          apply(A2[which(A2[,1]==C_50[i,1]),c(-1,-2)],2,sum),
          apply(A3[which(A3[,1]==C_50[i,1]),c(-1,-2)],2,sum),
          apply(A4[which(A4[,1]==C_50[i,1]),c(-1,-2)],2,sum),
          apply(A5[which(A5[,1]==C_50[i,1]),c(-1,-2)],2,sum),
          apply(A6[which(A6[,1]==C_50[i,1]),c(-1,-2)],2,sum),
          apply(A7[which(A7[,1]==C_50[i,1]),c(-1,-2)],2,sum),
          apply(A8[which(A8[,1]==C_50[i,1]),c(-1,-2)],2,sum),
          apply(A9[which(A9[,1]==C_50[i,1]),c(-1,-2)],2,sum),
          apply(A10[which(A10[,1]==C_50[i,1]),c(-1,-2)],2,sum))
  a[i,]=c(x)
}
row.names(a)=C_50[,1]
colnames(a)=c(paste(colnames(A1)[c(-1,-2)],sep = '_',1),
              paste(colnames(A1)[c(-1,-2)],sep = '_',2),
              paste(colnames(A1)[c(-1,-2)],sep = '_',3),
              paste(colnames(A1)[c(-1,-2)],sep = '_',4),
              paste(colnames(A1)[c(-1,-2)],sep = '_',5),
              paste(colnames(A1)[c(-1,-2)],sep = '_',6),
              paste(colnames(A1)[c(-1,-2)],sep = '_',7),
              paste(colnames(A1)[c(-1,-2)],sep = '_',8),
              paste(colnames(A1)[c(-1,-2)],sep = '_',9),
              paste(colnames(A1)[c(-1,-2)],sep = '_',10))
dim(a)
write.csv(a,"金融資料my_paper remove.csv")
#====================================================================================