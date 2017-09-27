source("sampling.functions.R")
data=read.table("..\\Data\\Spambase\\spambase.data",sep=",",header=T)
head(data)

names=NULL

#remame covariates
for(i in 1:(ncol(data)-1)){ 
  names=cbind(names,paste("V",i,sep=""))
}
names(data)=c(names,"response")

save(data,file = "..\\Data\\spam.data.RData")

n=100
d=c(0,0.2,0.4)

samples=NULL

samples=samplesGenerate(data,d[1],n)
d.inf=d.information(data,samples,n,d[1])

for(i in 2:length(d)){
  samples=rbind(samples,samplesGenerate(data,d[i],n))
  d.inf=rbind(d.inf,d.information(data,samples[seq(1+n*(i-1),n*i),],n,d[i]))
}


save(samples,file="..\\Data\\spam.samples.RData")
save(d.inf,file="..\\Data\\spam.d.information.RData")

load("..\\Data\\spam.samples.RData")

load("..\\Data\\spam.d.information.RData")

require(ggplot2)
ggplot(d.inf,aes(x=d,y=dobs,fill=d))+geom_boxplot()
