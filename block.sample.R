source("sampling.functions.R")
load("..\\Data\\PageBlock\\page-blocks.data")
data=read.table("..\\Data\\PageBlock\\page-blocks.data")
head(data)
names(data)
data$V11=ifelse(data$V11==1,data$V11,0)
table(data$V11)
mean(data$V11)
names(data)[11]="response"
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

save(samples,file="..\\Data\\block.samples.RData")
save(d.inf,file="..\\Data\\block.d.information.RData")
save(data,file="..\\Data\\block.data.RData")

boxplot(d.inf$dobs~d.inf$d)
