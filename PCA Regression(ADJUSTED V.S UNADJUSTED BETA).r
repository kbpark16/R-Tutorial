
# ������ �ҷ����̱� 
skull = read.table("F:/��� �ٺ��� �м�(���ֿ�������)/5����(Cluster analysis)/skull.txt")
# ������ Ư�� �ľ�(Ư�� n�� ������, ������ �����:p , n by p matrix: multivariate data)
str(skull)

y=rnorm(32) # ���Ժ��� ������ Y(REPONSE ����) ǥ�����Ժ��� ���� ����
skull2<-cbind(skull,y) # ���� V1-V5�� �ִ� �����Ϳ� ���̱� (linear regression �Ϸ���)

#beta1 for v1 when unadjusted , =-0.006391
lm(y~skull2$V1,data=skull2)
#beta1 for v1 when adjusted for v2, =-0.003914
lm(y~skull2$V1+skull2$V2,data=skull2)

# CORRELATION BETWEEN V1 AND V2=0.1089425 , NOT ZERO!!
# SO ADJUSTED BETA1 AND UNADJUSTED BETA1 ARE "DIFFRENT". 
cor(skull2$V1,skull2$V2)


#PCA using correlation matrix(NOT VARIANCE-COVARIANCE MATRIX)
# -> PCA��ü�� UNCORRELATED �� PC1,PC2,...PCp�� ����°� (linear combination of orginal variables)
skull.pca=princomp(skull2,cor=T)

#beta1 for pc1 :0.1027
lm(y~skull.pca$scores[,1])
#beta1 for pc1 when adjusted for pc2 : 0.1027
lm(y~skull.pca$scores[,1]+skull.pca$scores[,2],data=skull2)


# It's almost zero(-3.673812e-16): R���� �� ���� ���̸� zero ��.
# So, ADJSTED BETA1 AND UNADJSTED BETA1 ARE THE "SAME".
cor(skull.pca$scores[,1],skull.pca$scores[,2])


# ������ PCA�� PCA SCORE FOR PC1,PCA SCORE FOR  PC2ó�� UNCORRELATED �ǰԲ� ���� ������ �ƴ�
# ���ǿ����� Ȯ������ ���� ���� CORRELATIN=0�� ���� ����. �׷��Ƿ� ADJUST, UNADJUSTED BETA�� �ٸ� ���ۿ� ����.
