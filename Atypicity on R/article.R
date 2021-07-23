rm(list=ls()) 
 datas<-iris
  vcl<-c('setosa','versicolor','virginica')
 k=dim.data.frame(datas)
 #nombre de variable explicatives
 kexp<-k[2]-1
 #nombre de classes
 kcl<-length(vcl)
   h<-1
   Vk<-array(dim=c(kexp,kexp,kcl))
   gk<-matrix(nrow=kcl,ncol=kexp)
   nbi<-matrix(nrow=kcl,ncol=2)
  for(i in vcl)
    { print(i)
       datass<-datas[datas$Species==i,1:(kexp)]
       datass<-data.matrix(datass)
       nbi[h,]=dim(datass)
       Vk[,,h]=var(datass)
       gk[h,]=colMeans(datass)
       h<-h+1
       }
V<-var(data.matrix(datas[,1:kexp]))
g<-colMeans(data.matrix(datas[,1:kexp]))
W<-matrix(0,ncol=kexp,nrow=kexp)
for(i in 1:kcl)
{print(W)
  W=W+nbi[i,1]*Vk[,,i]
}
#echantillon
it<-data.matrix(datas[50,1:4])
#règle quadratique bayesienne
distance<-matrix(nrow=1,ncol=kcl)
for (i in 1:kcl)
{distance[1,i]<-(v-gk[i,])%*%solve(W)%*%t((v-gk[i,]))}
#indice d'atypicité en supposant la normalité a l'intérieur des groupes
#nombre d'individu a classer
ktest<-1
atypicity<-matrix(0,nrow=ktest,ncol=kcl)
for(i in 1:ktest)
{for(j in 1:kcl)
{print(j)
   density<-(1/(2*pi)**(kexp/2)*det(Vk[,,j]))*exp((it-gk[j,])%*%solve(Vk[,,j])%*%t((it-gk[j,])))**1/2
  datak<-datas[datas$Species==vcl[j],1:kexp]
   for(n in 1:nbi[j,1])
     { ib<-data.matrix(datak[n,1:kexp])
      densityn<-(1/(2*pi)**(kexp/2)*det(Vk[,,j]))*exp((ib-gk[j,])%*%solve(Vk[,,j])%*%t((ib-gk[j,])))**1/2
      if(densityn>density)
      {atypicity[i,j]<-atypicity[i,j]+densityn}
   
}
   
}}
predictionrad<-matrix(0,nrow=ktest,ncol=1)
for(i in 1:ktest)
{ci<-which.min(atypicity[i,])
predictionrad<-vcl[ci]
}

