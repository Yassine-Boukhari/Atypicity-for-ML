library(caret)
library(klaR)
rm(list=ls()) 
#datas<-iris
split=0.70
trainIndex <- createDataPartition(iris$Species, p=split, list=FALSE)
data_train <- iris[ trainIndex,]
data_test <- iris[-trainIndex,]
#apprenstissage avec classifieur rad
#vecteur des classes 
vcl<-c('setosa','versicolor','virginica')
#dimension de l'échantillon d'apprentissage
k=dim.data.frame(data_train)
#dimension de l'echantillon de test
kt=dim.data.frame(data_test)
#nombre de variable explicatives
kexp<-k[2]-1
#nombre de classes
kcl<-length(vcl)
#variable d'iteration au sein de la boucle
h<-1
d<-1
#tableau de trois dimensions pour le stockage des covariances des classes
Vk<-array(dim=c(kexp,kexp,kcl))
#matrice pour stocker les centres de gravités des classes
gk<-matrix(nrow=kcl,ncol=kexp)
#matrice pour stocker l'effectif de chaque classe
nbi<-matrix(nrow=kcl,ncol=2)
nbt<-matrix(nrow=kcl,ncol=2)
for(i in vcl){
  print(i)
  data_testk<-data_test[data_test$Species==i,1:(kexp)]
  data_testk<-data.matrix(data_testk)
  nbt[d,]=dim(data_testk)
  d<-d+1
}
for(i in vcl)
{print(i)
  data_traink<-data_train[data_train$Species==i,1:(kexp)]
  data_traink<-data.matrix(data_traink)
  nbi[h,]=dim(data_traink)
  Vk[,,h]=var(data_traink)
  gk[h,]=colMeans(data_traink)
  h<-h+1
}
#calcul de la matrice de covariance intraclasse
W<-matrix(0,ncol=kexp,nrow=kexp)
for(i in 1:kcl)
{print(W)
  W=W+nbi[i,1]*Vk[,,i]/sum(nbi[,1])
}
#indice d'atypicité en supposant la normalité a l'intérieur des groupes
#nombre d'individu a classer
ktest<-kt[1]
#matrice d'atypicité
atypicity<-matrix(0,nrow=ktest,ncol=kcl)
rad<-matrix(0,nrow=ktest,ncol=kcl)
#boucle sur les classes
{for(j in 1:kcl)
{print(j)
  data_testk<-data_test[data_test$Species==vcl[j],1:kexp]
  data_testk<-data.matrix(data_testk)
  for(i in nbt[j,1]){it<-t(as.matrix(data_testk[i,1:kexp]))
  #print(it)
  density<-(1/(2*pi)**(kexp/2)*det(Vk[,,j]))*exp((it-gk[j,])%*%solve(Vk[,,j])%*%t((it-gk[j,])))**1/2
  data_train_k<-data.matrix(data_train[data_train$Species==j,1:kexp])
  for(n in 1:nbi[j,1])
  { ib<-t(as.matrix(data_traink[n,1:kexp]))
  #print(ib)
  densityn<-((1/(2*pi)**(kexp/2))*det(Vk[,,j]))*(exp((ib-gk[j,])%*%solve(Vk[,,j])%*%t((ib-gk[j,])))**1/2)
  #print(densityn)
  if(densityn>density)
  {atypicity[i,j]<-atypicity[i,j]+densityn}
  
  }
  rad[i,j]<-atypicity[i,j]/density
  }}}
predictionrad<-matrix(nrow=ktest,ncol=1)
for(i in 1:ktest)
{ci<-which.min(rad[i,])
predictionrad[i,1]<-vcl[ci]
}
predictionrad<-as.factor(predictionrad)
labelog<-as.factor(data_test[,kt[2]])
confusionMatrix(predictionrad,labelog)
