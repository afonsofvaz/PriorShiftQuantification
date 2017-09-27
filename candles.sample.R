source("sampling.functions.R")

data=read.table("../Data/Candles/CANDLES.txt",sep=" ",header=T)
head(data)
data$response=ifelse(data$nonRegular>0.5,1,0)
data=data[,12:20]
head(data)
n=100
d=c(0,0.2,0.4)

samples=NULL

samples=samplesGenerate(data,d[1],n)
d.inf=d.information(data,samples,n,d[1])

for(i in 2:length(d)){
  samples=rbind(samples,samplesGenerate(data,d[i],n))
  d.inf=rbind(d.inf,d.information(data,samples[seq(1+n*(i-1),n*i),],n,d[i]))
}

save(samples,file="../Data/candles.samples.RData")
save(d.inf,file="../Data/candles.d.information.RData")
save(data,file="../Data/candles.data.RData")

boxplot(d.inf$dobs~d.inf$d)
