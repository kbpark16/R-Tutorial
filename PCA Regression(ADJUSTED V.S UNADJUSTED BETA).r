
# 데이터 불러들이기 
skull = read.table("F:/고대 다변량 분석(송주원교수님)/5주차(Cluster analysis)/skull.txt")
# 데이터 특성 파악(특히 n이 몇인지, 변수가 몇개인지:p , n by p matrix: multivariate data)
str(skull)

y=rnorm(32) # 정규분포 따르는 Y(REPONSE 변수) 표준정규분포 에서 추출
skull2<-cbind(skull,y) # 기존 V1-V5만 있는 데이터에 붙이기 (linear regression 하려고)

#beta1 for v1 when unadjusted , =-0.006391
lm(y~skull2$V1,data=skull2)
#beta1 for v1 when adjusted for v2, =-0.003914
lm(y~skull2$V1+skull2$V2,data=skull2)

# CORRELATION BETWEEN V1 AND V2=0.1089425 , NOT ZERO!!
# SO ADJUSTED BETA1 AND UNADJUSTED BETA1 ARE "DIFFRENT". 
cor(skull2$V1,skull2$V2)


#PCA using correlation matrix(NOT VARIANCE-COVARIANCE MATRIX)
# -> PCA자체가 UNCORRELATED 된 PC1,PC2,...PCp를 만드는것 (linear combination of orginal variables)
skull.pca=princomp(skull2,cor=T)

#beta1 for pc1 :0.1027
lm(y~skull.pca$scores[,1])
#beta1 for pc1 when adjusted for pc2 : 0.1027
lm(y~skull.pca$scores[,1]+skull.pca$scores[,2],data=skull2)


# It's almost zero(-3.673812e-16): R에서 이 정도 값이면 zero 임.
# So, ADJSTED BETA1 AND UNADJSTED BETA1 ARE THE "SAME".
cor(skull.pca$scores[,1],skull.pca$scores[,2])


# 위에서 PCA의 PCA SCORE FOR PC1,PCA SCORE FOR  PC2처럼 UNCORRELATED 되게끔 만든 변수가 아닌
# 현실에서의 확률변수 들은 절대 CORRELATIN=0일 수가 없다. 그러므로 ADJUST, UNADJUSTED BETA가 다를 수밖에 없다.
