source("sampling.functions.R")

data=read.csv("..\\Data\\Bank\\bank-full.csv",sep=";",header=T)
names(data)[names(data)=="y"]="response"
data$response=ifelse(data$response=="no",0,1)
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

save(samples,file="..\\Data\\bank.samples.RData")
save(d.inf,file="..\\Data\\bank.d.information.RData")
save(data,file="..\\Data\\bank.data.RData")
