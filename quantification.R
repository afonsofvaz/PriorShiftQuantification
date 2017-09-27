require(plyr)
require(dplyr)
source(file="g.functions.R")

load("..\\Data\\spam.data.RData")
load("..\\Data\\spam.samples.RData")

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
    
    gx=g[[i]]$func(y.train,x.train,x.target,g[[i]]$extra)
    
    mu.g[i]=mean(gx)
    mu.g0[i]=mean(gx[index.0])
    mu.g1[i]=mean(gx[index.1])
    
    
  }
  p=(mu.g-mu.g0)/(mu.g1-mu.g0)
  out=data.frame(method=names(g),estimative=p,mu_g=mu.g,mu_g0=mu.g0,mu_g1=mu.g1)
  return(out)
}


##Iniciando o data frame
x.train=data[samples[1,-c(1,2)]==1,-c(which(names(data)=="response"))]
y.train=data[samples[1,-c(1,2)]==1,"response"]

x.target=data[samples[1,-c(1,2)]==0,-which(names(data)=="response")]
y.target=data[samples[1,-c(1,2)]==0,"response"]

forman.random=list(func=g.forman.random,extra=list(n.tree=100))

forman.knn=list(func=g.forman.knn,
                extra=list(k=k.choosen(y.train,x.train,grid=NULL,prop=0.3)))

forman.logistc=list(func=g.forman.logistic,extra=NULL)

#kernel=list(func=g.kernel,extra=list(lambda=0.001,kernel=gaussian.kernel,
                                    # bandwidth=NULL))

g=list(forman.random=forman.random,forman.knn=forman.knn,
       forman.logistc=forman.logistc)

experiment.estimative=cbind(samples[1,1:2],quantification.prior.shift(x.train,y.train,x.target,g))
experiment.real=cbind(samples[1,1:2],theta.real=mean(y.target))

for(i in 2:3){

x.train=data[samples[i,-c(1,2)]==1,-c(which(names(data)=="response"))]
y.train=data[samples[i,-c(1,2)]==1,"response"]

x.target=data[samples[i,-c(1,2)]==0,-which(names(data)=="response")]
y.target=data[samples[i,-c(1,2)]==0,"response"]

forman.random=list(func=g.forman.random,extra=list(n.tree=100))

forman.knn=list(func=g.forman.knn,
                extra=list(k=k.choosen(y.train,x.train,grid=NULL,prop=0.3)))

forman.logistc=list(func=g.forman.logistic,extra=NULL)

# kernel=list(func=g.kernel,extra=list(lambda=0.001,kernel=gaussian.kernel,
#                                      bandwidth=NULL))

g=list(forman.random=forman.random,forman.knn=forman.knn,
       forman.logistc=forman.logistc)



experiment.estimative=rbind(experiment.estimative,cbind(samples[i,1:2],
                quantification.prior.shift(x.train,y.train,x.target,g)))
experiment.real=rbind(experiment.real,cbind(samples[1,1:2],theta.real=mean(y.target)))
save(experiment.real,file="..\\Outputs\\experiment.estimative.RData")
save(experiment.estimative,file="..\\Outputs\\experiment.real.RData")
}
