samplesGenerate=function(data,d,n){
  
  minority.class=as.numeric(names(which.min(table(data$response))))
  minority.prob=mean(data$response==minority.class)
  majority.class=as.numeric(!minority.class)
  majority.prob=mean(data$response==majority.class)
  
  alpha=(d+minority.prob+sqrt( (d+ minority.prob)^2 - 2*d*minority.prob))/2
  p=(1/2)*minority.prob*((1-alpha)/alpha)/(1-minority.prob)
  
  type=vector(length = nrow(data))
  index=which(data$response==minority.class)
  samples=do.call(data.frame, replicate(nrow(data)+2, rep(FALSE,n), simplify=FALSE))
  names(samples)=c("S","d",paste(1:nrow(data)))

  for(i in 1:n){
    type[index]=rbinom(length(index),1,0.5)
    type[-index]=rbinom(nrow(data)-length(index),1,p)
    samples[i,]=c(i,d,type)
  }
  

  return(samples)
  
}


d.information=function(data,samples,n,d){
  index=seq(3,nrow(data))
  dobs=vector(length = n)
  for(i in 1:n){
    dobs[i]=mean(data[which(samples[i,index]==0),]$response)-
            mean(data[which(samples[i,index]==1),]$response)
    dobs[i]=abs(dobs[i])
  }

  d.information=data.frame(S=seq(1,n),d=rep(as.factor(d),n),dobs=dobs)
  return(d.information)
  
}

