#install.packages("class")
library(class)
#install.packages("randomForest")
library(randomForest)
library(fields)
library(geigen)

## Forman using random forest classifier
## "extra" parameter is a list cotaining only the n.tree parameter 
g.forman.random=function(y.train,x.train,x.target,extra){
  
  n.tree=forman.random$extra$n.tree
  
  covariates=names(x.train)
  formula=paste(covariates,collapse="+")
  formula=paste("as.factor(response) ~ ",formula)
  fit=randomForest(as.formula(formula),method="class",data=cbind(response=y.train,x.train),
                   importance=TRUE, ntree=n.tree)
  
  
  g.x=c(as.numeric(fit$predicted)-1,as.numeric(predict(fit,x.target))-1)
  return(g.x)
  
}


## Ratio estimator using RKHS
## "extra" parameter is a list cotaining "lambda", "kernel" wich represent a kernel function and
## "bandwidth" wich represent the kernel function bandwidth
gaussian.kernel=function(x.data.1,x.data.2,extra)
{
  return(exp(-(fields::rdist(x.data.1,x.data.2))^2/extra$bandwidth))
}

# g.kernel=function(y.train,x.train,x.target,extra=list(lambda=0.001,theta.hat=1/2,kernel=gaussian.kernel,bandwidth=NULL)){
#   
#   # define bandwdth of gaussian kernel if not defined
#   if(identical(extra$kernel,gaussian.kernel))
#     if(is.null(extra$bandwidth))
#     {
#       sample.size=min(c(nrow(x.train),400))
#       sample=sample(1:nrow(x.train),sample.size)
#       extra$bandwidth=quantile(c(rdist(x.train[sample,],x.train[sample,])^2),probs=0.025)
#     }
#   
#   kernel.matrix=extra$kernel(x.train,x.train,extra)
#   
#   mu.0=colMeans(kernel.matrix[y.train==0,])
#   mu.1=colMeans(kernel.matrix[y.train==1,])
#   Sigma.0=cov(kernel.matrix[y.train==0,])
#   Sigma.1=cov(kernel.matrix[y.train==1,])
#   
#   #!!!theta.hat=(completar com chute inicial do forman)!!!
#   #theta.hat=1/2
#   
#   M=(mu.1-mu.0)%*%t(mu.1-mu.0)
#   N=(extra$theta.hat*Sigma.1+(1-extra$theta.hat)*Sigma.0)#+extra$lambda*kernel.matrix
#   
#   # try to decompose matrix; if not invertible increase lambda
#   while(1)
#   {
#     print(extra$lambda)
#     #decomposition=try(geigen(M,N),silent = TRUE)
#     decomposition=try(eigen(solve(N)%*%M),silent = TRUE)
#     if(!identical(class(decomposition),"try-error"))
#       break;
#     extra$lambda=2*extra$lambda
#     N=(extra$theta.hat*Sigma.1+(1-extra$theta.hat)*Sigma.0)+extra$lambda*diag(nrow(Sigma.0))
#     #N=(theta.hat*Sigma.1+(1-theta.hat)*Sigma.0)+extra$lambda*kernel.matrix
#   } 
#   
#   weights=Re(decomposition$vectors[,1])
#   
#   
#   return(extra$kernel(rbind(x.train,x.target),x.train,extra)%*%weights)
#   
# }


normalize <- function(data) {
  data=as.data.frame(data)
  index.col=sapply(data,is.numeric)
  data[,index.col]=(data[,index.col] - min(data[,index.col])) /(max(data[,index.col]) - min(data[,index.col]))
  return (data)
}

g.kernel=function(y.train,x.train,x.target,extra=list(lambda=0.001,kernel=gaussian.kernel,bandwidth=NULL,theta.hat=1/2)){
  #x.train=normalize(x.train); x.target=normalize(x.target)
  aux.scale=scale(rbind(x.train,x.target))
  x.train=aux.scale[1:nrow(x.train),]
  x.target=aux.scale[-c(1:nrow(x.train)),]
  which.estimate=sample(1:nrow(x.train),round(0.3*nrow(x.train)))
  sample.size=min(c(nrow(x.train),400))
  sample=sample(1:nrow(x.train),sample.size)
  bandwidth.grid=quantile(c(rdist(x.train[sample,],x.train[sample,])^2),probs=c(0.01,0.025,0.1,0.25,0.5))
  lambda.grid=seq(0.00001,0.1,length.out=10)
  
  #bandwidth.grid=quantile(c(rdist(x.train[sample,],x.train[sample,])^2),probs=c(0.01,0.025,0.1))
  #lambda.grid=seq(0.00001,0.1,length.out=3)
  mse=matrix(NA,length(bandwidth.grid),length(lambda.grid))
  for(band in 1:length(bandwidth.grid))
  {
    print(band/length(bandwidth.grid))
    kernel.matrix=extra$kernel(x.train[-which.estimate,],
                               x.train[-which.estimate,],
                               extra=list(bandwidth=bandwidth.grid[band]))
    
    mu.0=colMeans(kernel.matrix[y.train[-which.estimate]==0,])
    mu.1=colMeans(kernel.matrix[y.train[-which.estimate]==1,])
    Sigma.0=cov(kernel.matrix[y.train[-which.estimate]==0,])
    Sigma.1=cov(kernel.matrix[y.train[-which.estimate]==1,])
    
    #!!!theta.hat=(completar com chute inicial do forman)!!!
    theta.hat=extra$theta.hat
    
    M=(mu.1-mu.0)%*%t(mu.1-mu.0)
    N=(theta.hat*Sigma.1+(1-theta.hat)*Sigma.0)#+extra$lambda*kernel.matrix
    
    espectral=try(eigen(N),silent = TRUE)
    if(identical(class(espectral),"try-error"))
      next;
    U.inverse=try(solve(espectral$vectors),silent = TRUE)
    if(identical(class(U.inverse),"try-error"))
      next;
    for(lambda in 1:length(lambda.grid))
    {
      print(".")
      N.inverse=t(U.inverse)%*%diag(1/(espectral$values+lambda.grid[lambda]))%*%U.inverse
      #decomposition=try(geigen(M,N),silent = TRUE)
      decomposition=try(eigen(N.inverse%*%M),silent = TRUE)
      if(identical(class(decomposition),"try-error"))
        next;
      
      weights=Re(decomposition$vectors[,1])      
      
      g.estimate=extra$kernel(x.train[which.estimate,],x.train[-which.estimate,],
                              extra=list(bandwidth=bandwidth.grid[band]))%*%weights
      mean.0=mean(g.estimate[y.train[which.estimate]==0])
      mean.1=mean(g.estimate[y.train[which.estimate]==1])
      var.0=var(g.estimate[y.train[which.estimate]==0])
      var.1=var(g.estimate[y.train[which.estimate]==1])
      mse[band,lambda]=1/(mean.1-mean.0)^2*(theta.hat*var.1+(1-theta.hat)*var.0)
    }
  }  
  
  index=which(mse==min(mse),arr.ind=TRUE)
  best.band=bandwidth.grid[index[1]]
  best.lambda=lambda.grid[index[2]]
  
  kernel.matrix=extra$kernel(x.train,
                             x.train,
                             extra=list(bandwidth=best.band))
  mu.0=colMeans(kernel.matrix[y.train==0,])
  mu.1=colMeans(kernel.matrix[y.train==1,])
  Sigma.0=cov(kernel.matrix[y.train==0,])
  Sigma.1=cov(kernel.matrix[y.train==1,])
  
  M=(mu.1-mu.0)%*%t(mu.1-mu.0)
  N=(theta.hat*Sigma.1+(1-theta.hat)*Sigma.0)#+extra$lambda*kernel.matrix
  espectral=eigen(N)
  U.inverse=solve(espectral$vectors)
  N.inverse=t(U.inverse)%*%diag(1/(espectral$values+best.lambda))%*%U.inverse
  decomposition=eigen(N.inverse%*%M)
  weights=Re(decomposition$vectors[,1])      
  
  return(extra$kernel(rbind(x.train,x.target),x.train,extra=list(bandwidth=best.band))%*%weights)
}

## Forman using KNN classifier
normalize <- function(data) {
  data=as.data.frame(data)
  index.col=sapply(data,is.numeric)
  data[,index.col]=(data[,index.col] - min(data[,index.col])) /(max(data[,index.col]) - min(data[,index.col]))
  return (data)
  }

prop=0.3
grid=NULL

k.risk=function(y.train,x.train,grid=NULL,prop){
          
        size.test=round(prop*nrow(x.train))
  
          if(is.null(grid)){ 
          center=round(sqrt(size.test))
          grid=c(center-(1:5),center,(1:5)+center)
          }

        test.index=sample(1:nrow(x.train),size.test)
          
        risk=vector(length = length(grid))
        
        for(i in 1:length(grid)){
        risk[i]=mean(y.train[test.index]!=knn(train = x.train[-test.index,],test = x.train[test.index,],
                cl=y.train[-test.index],k=grid[i]))
        }
        return(data.frame(k=grid,risk=risk))
}

k.choosen=function(y.train,x.train,grid=NULL,prop){
  
  risk.table=k.risk(y.train,x.train,grid=NULL,prop)
  k=risk.table$k[which.min(risk.table$risk)]
  return(k)
  }
  
  
g.forman.knn=function(y.train,x.train,x.target,extra){
  
  aux.scale=scale(rbind(x.train,x.target))
  x.train=aux.scale[1:nrow(x.train),]
  x.target=aux.scale[-c(1:nrow(x.train)),]

return(as.numeric(knn(train=x.train,test=rbind(x.train,x.target),
                      cl=y.train,k=extra$k))-1)
}


## Forman using Logistic Regression
g.forman.logistic=function(y.train,x.train,x.target,extra){
data=cbind(y.train,x.train)
model=glm(y.train ~ . ,family = "binomial",data=data)  
return(ifelse(predict(model,newdata=rbind(x.train,x.target),type = "response")>0.5,1,0))
}

##nonparametric conditional density estimation

g.entropy=function(y.train,x.train,x.target,extra){
  bws=npcdensbw(y=x.train,x=as.factor(y.train),bwmethod = "normal-reference")
  density=npcdens(bws=bw,bwmeth)
  ?npcdensbw
  
}
