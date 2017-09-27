require(plyr)
require(dplyr)
setwd("c:/Users/afons/Dropbox/Artigos-Google/MaxEnt/Codes")
source(file="g.functions1.R")

load("../Data/candles.data.RData")
load("../Data/candles.samples.RData")

quantification.prior.shift=function(x.train,y.train,x.target,g)
{
  
  n.methods=length(g)
  p=vector(length = n.methods)
  mu.g=vector(length = n.methods)
  mu.g0=vector(length = n.methods)
  mu.g1=vector(length = n.methods)
  index.0=which(y.train==0)
  index.1=which(y.train==1)
  
  for(i in 1:n.methods){
    
    gx=g[[i]]$func(as.matrix(y.train),x.train,x.target,g[[i]]$extra)
    
    g.train=gx[1:nrow(x.train)]
    g.target=gx[-c(1:nrow(x.train))]
    
    mu.g[i]=mean(g.target)
    mu.g0[i]=mean(g.train[y.train==0])
    mu.g1[i]=mean(g.train[y.train==1])
    
  }
  p=(mu.g-mu.g0)/(mu.g1-mu.g0)
  out=data.frame(method=names(g),estimative=p,mu_g=mu.g,mu_g0=mu.g0,mu_g1=mu.g1)
  return(out)
}


##Iniciando o data frame
x.train=data[samples[201,-c(1,2)]==1,-c(which(names(data)=="response"))]
y.train=data[samples[201,-c(1,2)]==1,"response"]

x.target=data[samples[201,-c(1,2)]==0,-which(names(data)=="response")]
y.target=data[samples[201,-c(1,2)]==0,"response"]

forman.random=list(func=g.forman.random,extra=list(n.tree=100))

forman.knn=list(func=g.forman.knn,
                extra=list(k=k.choosen(y.train,x.train,grid=NULL,prop=0.3)))

forman.logistc=list(func=g.forman.logistic,extra=NULL)

kernel=list(func=g.kernel,extra=list(lambda=0.001,theta.hat=quantification.prior.shift(x.train,y.train,x.target,g=list(forman.random=forman.random))$estimative,kernel=gaussian.kernel,
                                     bandwidth=NULL))

g=list(forman.random=forman.random,forman.knn=forman.knn,
       forman.logistc=forman.logistc,kernel=kernel)


experiment.estimative=cbind(samples[1,1:2],quantification.prior.shift(x.train,y.train,x.target,g))
experiment.real=cbind(samples[1,1:2],theta.real=mean(y.target))

for(i in 2:nrow(samples)){
  print(paste(i/nrow(samples)))
  x.train=data[samples[i,-c(1,2)]==1,-c(which(names(data)=="response"))]
  y.train=data[samples[i,-c(1,2)]==1,"response"]
  
  x.target=data[samples[i,-c(1,2)]==0,-which(names(data)=="response")]
  y.target=data[samples[i,-c(1,2)]==0,"response"]
  
  forman.random=list(func=g.forman.random,extra=list(n.tree=100))
  
  forman.knn=list(func=g.forman.knn,
                  extra=list(k=k.choosen(y.train,x.train,grid=NULL,prop=0.3)))
  
  forman.logistc=list(func=g.forman.logistic,extra=NULL)
  
  kernel=list(func=g.kernel,extra=list(lambda=0.001,theta.hat=quantification.prior.shift(x.train,y.train,x.target,g=list(forman.random=forman.random))$estimative,kernel=gaussian.kernel,
                                       bandwidth=NULL))
  
  g=list(forman.random=forman.random,forman.knn=forman.knn,
         forman.logistc=forman.logistc,kernel=kernel)
  
  
  experiment.estimative=rbind(experiment.estimative,cbind(samples[i,1:2],
                                                          quantification.prior.shift(x.train,y.train,x.target,g)))
  experiment.real=rbind(experiment.real,cbind(samples[i,1:2],theta.real=mean(y.target)))
  save(experiment.estimative,file="../Outputs/Candles/experiment.estimative_all.RData")
  save(experiment.real,file="../Outputs/Candles/experiment.real_all.RData")
}
