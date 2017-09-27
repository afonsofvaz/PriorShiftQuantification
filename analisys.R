require(dplyr)
require(plyr)
require(ggplot2)

load("/Data/spam.samples.RData")
load("/Data/spam.d.information.RData")
spam.d.information=d.inf
spam.samples=samples

load("/Data/candles.samples.RData")
load("/Data/candles.d.information.RData")
candles.d.information=d.inf
candles.samples=samples

load("/Data/block.samples.RData")
load("/Data/block.d.information.RData")
block.d.information=d.inf
block.samples=samples

load("/Data/cancer.samples.RData")
load("/Data/cancer.d.information.RData")
cancer.d.information=d.inf
cancer.samples=samples

d.information=rbind(spam.d.information,block.d.information,candles.d.information,
                    cancer.d.information)
d.information$Dataset=c(rep("Spam",nrow(spam.d.information)),
                        rep("BlockPage",nrow(block.d.information)),
                        rep("Candles",nrow(candles.d.information)),
                        rep("Cancer",nrow(cancer.d.information)))


theme = theme_set(theme_minimal(base_size = 40))
theme = theme_update(legend.position="top", legend.title=element_blank(), panel.grid.major.x=element_blank(),
                     panel.grid.major.y=element_blank(),
                     panel.grid.minor.y=element_blank(),
                     panel.border = element_rect(colour = "black", fill=NA, size=0.5),
                     axis.text.y = element_text(colour="black",size=35), 
                     axis.text.x = element_text(size=35),axis.ticks.y= element_line(colour="black"))+ theme_update(axis.ticks.length=unit(.15, "cm"),panel.spacing.y = unit(1.5, "lines"))

g1=ggplot(d.information,aes(x=d,y=dobs,fill=Dataset))+geom_boxplot()+
          xlab("Theoretical d")+ylab("Observed d")
g1

###############Restults

theme = theme_set(theme_minimal(base_size = 22))
theme = theme_update(legend.position="top", legend.title=element_blank(), panel.grid.major.x=element_blank(),
                     panel.grid.major.y=element_blank(),
                     panel.grid.minor.y=element_blank(),
                     panel.border = element_rect(colour = "black", fill=NA, size=0.5),
                     axis.text.y = element_text(colour="black",size=15), 
                     axis.text.x = element_text(size=15),
                     axis.ticks.y= element_line(colour="black"))+ theme_update(axis.ticks.length=unit(.15, "cm"),panel.spacing.y = unit(1.5, "lines"))

load("/Outputs/Candles/experiment.estimative_all.RData")
load("./Outputs/Candles/experiment.real_all.RData")
candles.estimative=experiment.estimative
candles.real=experiment.real
candles.real$d=as.factor(candles.real$d)
candles.estimative$d=as.factor(candles.estimative$d)

load("/Outputs/Block/experiment.estimative.all.RData")
load("/Outputs/Block/experiment.real.all.RData")
block.estimative=experiment.estimative
block.real=experiment.real
block.real$d=as.factor(block.real$d)
block.estimative$d=as.factor(block.estimative$d)

load("/Outputs/Spam/experiment.real_all.RData")
load("/Outputs/Spam/experiment.estimative_all.RData")
spam.estimative=experiment.estimative
spam.real=experiment.real
spam.real$d=as.factor(spam.real$d)
spam.estimative$d=as.factor(spam.estimative$d)

load("/Outputs/Cancer/experiment.real_all.RData")
load("/Outputs/Cancer/experiment.estimative_all.RData")
cancer.estimative=experiment.estimative
cancer.real=experiment.real
cancer.estimative$d=as.factor(cancer.estimative$d)
cancer.real$d=as.factor(cancer.real$d)

results=rbind(candles.estimative,cancer.estimative,block.estimative,spam.estimative)
dataset=c(rep("Candles",nrow(candles.estimative)),
          rep("Cancer",nrow(cancer.estimative)),
          rep("Block",nrow(block.estimative)),
          rep("Spam",nrow(spam.estimative)))
results$Dataset=dataset

real=rbind(candles.real,cancer.real,block.real,spam.real)
dataset=c(rep("Candles",nrow(candles.real)),rep("Cancer",nrow(cancer.real)),
          rep("Block",nrow(block.real)),
          rep("Spam",nrow(spam.real)))
real$Dataset=dataset

results.complete=join(results,real)
head(results.complete)


results.complete=results.complete[which(is.finite(results.complete$estimative)),]
head(results.complete)

##CC

cc=subset(results.complete,method!= "kernel")
cc$estimative=cc$mu_g
cc$method=paste0("cc.",substring(cc$method, first=8,last = 100L))
cc$method

results.complete=rbind(results.complete,cc)
results.complete$error=(results.complete$estimative-results.complete$theta.real)^2

mean_2se=function (x, mult = 1)
{
  x <- stats::na.omit(x)
  se <- mult * sqrt(stats::var(x)/length(x))
  mean <- mean(x)
  data.frame(y = mean, ymin = mean - 2*se, ymax = mean + 2*se)
}

mean_0se=function (x, mult = 1)
{
  x <- stats::na.omit(x)
  se <-rep(0,length(x))
  mean <- mean(x)
  data.frame(y = mean, ymin = mean - 2*se, ymax = mean + 2*se)
}
levels(results.complete$method)=c("Ratio-KNN","Ratio-LR","Ratio-RF",
                                  "Ratio-RKHS","CC-RF","CC-KNN","CC-LR")

d=ggplot(results.complete, aes(y=error,x=method)) + #geom_point() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
d=d + 
  facet_wrap(d ~ Dataset, scales="free_y")+xlab("Method")+ylab("Squared Error")+
  stat_summary(fun.data = "mean_2se", colour = "blue", size = 0.4) +
  stat_summary(fun.data = "mean_0se", colour = "red", size = 0.2)
d



