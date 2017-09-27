source("sampling.functions.R")
load("..\\Data\\BreastCancer\\Data")
data=read.table("..\\Data\\BreastCancer\\Data",sep=",")

data=data[,-1]
head(data)
names(data)[10]="response"
data$response=ifelse(data$response==4,1,0)
data=na.omit(data)
data$V7=as.numeric(data$V7)-1

n=100
d=c(0,0.2,0.4)

samples=NULL

samples=samplesGenerate(data,d[1],n)
d.inf=d.information(data,samples,n,d[1])

for(i in 2:length(d)){
  samples=rbind(samples,samplesGenerate(data,d[i],n))
  d.inf=rbind(d.inf,d.information(data,samples[seq(1+n*(i-1),n*i),],n,d[i]))
}

save(samples,file="..\\Data\\cancer.samples.RData")
save(d.inf,file="..\\Data\\cancer.d.information.RData")
save(data,file="..\\Data\\cancer.data.RData")

boxplot(d.inf$dobs~d.inf$d)
