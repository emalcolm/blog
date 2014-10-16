#Histogram of Poisson Distribution with Lambda=100
x<-c(60:150)
p<-vector(length=length(x))
for(i in 1:length(x)){
  p[i]<-dpois(x[i],100)
}
barplot(p, ylim=c(0,max(p)+0.01), cex.names=0.6, ylab=("f(x)=P(X=x)"), cex.lab=0.7, names.arg=c(60:150), cex.axis=0.7, col="Light Blue")
mtext(side = 1, "X = number of customers in a day", line = 2, cex=0.7)

##Take 1000 Samples of Size 40 from the Poisson Distribution
set.seed(1)
sample<-matrix(rpois(40000,lambda=100),nrow=1000,ncol=40)
samplemean<-rowMeans(sample)

#Histogram of 1 Sample Mean
hist(samplemean[1], breaks=c(((1:21)/2)+94.5),xlim=c(94,106), ylim=c(0,25), xlab=NA, ylab=("Frequency"), main=c("Distribution of 1 Sample Mean, n=40"), cex.lab=0.7, cex.axis=0.7, cex.main=0.7, axes=F, col="Red")
axis(side=1, cex.axis=0.7)
mtext(side = 1, "Sample Mean (X Bar)", line = 2, cex=0.7)

#Histogram of 10 Sample Means
hist(samplemean[1:10], breaks=c(((1:21)/2)+94.5),xlim=c(94,106), ylim=c(0,25), xlab=NA, ylab=("Frequency"), main=c("Distribution of 10 Sample Means, n=40"), cex.lab=0.7, cex.axis=0.7, cex.main=0.7, axes=F, col="red")
axis(side=1, cex.axis=0.7)
mtext(side = 1, "Sample Mean (X Bar)", line = 2, cex=0.7)

#Histogram of 100 Sample Means
hist(samplemean[1:100], breaks=c(((1:21)/2)+94.5),xlim=c(94,106), ylim=c(0,25), xlab=NA, ylab=("Frequency"), main=c("Distribution of 100 Sample Means, n=40"), cex.lab=0.7, cex.axis=0.7, cex.main=0.7, axes=F, col="red")
axis(side=1, cex.axis=0.7)
mtext(side = 1, "Sample Mean (X Bar)", line = 2, cex=0.7)

#Histogram of 1000 Sample Means
hist(samplemean[1:1000], breaks=c(((1:21)/2)+94.5), freq=FALSE, xlim=c(94,106), ylab=("Frequency"), xlab=NA,main=c("Distribution of 1000 Sample Means, n=40"),cex.lab=0.7, cex.axis=0.7, cex.main=0.7, axes=F, col="red")
axis(side=1, cex.axis=0.7)
mtext(side = 1, "Sample Mean (X Bar)", line = 2, cex=0.7)

##Take 1000 Samples of Size 100 from the Poisson Distribution
sample<-matrix(rpois(100000,lambda=100),nrow=1000,ncol=100)
samplemean<-rowMeans(sample)

#Histogram of 1000 Sample Means
hist(samplemean[1:1000], breaks=c(((1:21)/2)+94.5), freq=FALSE, xlim=c(94,106), ylab=("Frequency"), xlab=NA,main=c("Distribution of 1000 Sample Means, n = 100"),cex.lab=0.7, cex.axis=0.7, cex.main=0.7, axes=F, col="red")
axis(side=1, cex.axis=0.7)
mtext(side = 1, "Sample Mean (X Bar)", line = 2, cex=0.7)