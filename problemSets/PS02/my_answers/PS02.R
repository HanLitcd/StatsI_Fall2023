class_bribery_matrix <- matrix(
  c(14,6,7,7,7,1),
  nrow = 2,
  ncol = 3,
  byrow = TRUE
)
#Observerd Matrix
colnames(class_bribery_matrix) <- c("NotStop", "Bribery", "Stopped")
rownames(class_bribery_matrix) <- c("UpperClass", "LowerClass") 
Total<- cbind(class_bribery_matrix, RowTotal=rowSums(class_bribery_matrix))
Total<- rbind(Total,ColTotal= colSums(Total))

#Exp Matrix
Exp_UC<-Total['ColTotal',]*Total[1,'RowTotal']/Total['ColTotal','RowTotal']
Exp_LC <-Total['ColTotal',]*Total[2,'RowTotal']/Total['ColTotal','RowTotal']
Exp_Matrix <- rbind(Exp_UC,Exp_LC)[,c(1,2,3)]
#chi squared
chi2<-sum((class_bribery_matrix-Exp_Matrix)^2/Exp_Matrix)
sanity_check <- chisq.test(class_bribery_matrix)
#pvalue (note we know there are 2 rows and 3 columns hence df =2)
pvalue<- pchisq(chi2,df=(2-1)*(3-1),lower.tail = FALSE)
#std. residual
ColP <-Total["ColTotal",]/Total["ColTotal","RowTotal"]
RowP <-Total[,"RowTotal"]/Total["ColTotal","RowTotal"]
SE_UC <- sqrt(Exp_Matrix["Exp_UC",]*(1-RowP[1])*(1-ColP[c(1,2,3)]))
SE_LC <- sqrt(Exp_Matrix["Exp_LC",]*(1-RowP[2])*(1-ColP[c(1,2,3)]))
SE_Matrix <- rbind(SE_UC,SE_LC)
z_matrix <- (class_bribery_matrix-Exp_Matrix)/ SE_Matrix

#q2
model<- lm(policy$water~policy$reserved)
summary(model)