rtmPlot <-
function(rtmOutput){
range=rtmOutput$details[,1]
falsePositive=rtmOutput$details[,2]
truePositive=rtmOutput$details[,4]
opt.k=rtmOutput$Optimum.k
alpha.num=rtmOutput$details[,5]
beta.num=rtmOutput$details[,6]
alpha=falsePositive
beta=1-truePositive
par(mfrow=c(1,2))
# ROC Curve
plot(falsePositive,truePositive,type="c",col="forestgreen",lwd=2,xlim=c(0,1),ylim=c(0,1),ylab="True Positive Rate",xlab="False Positive Rate", main="(a) ROC Curve")
text(falsePositive,truePositive,labels=range,col="forestgreen")
abline(0,1,col="orange",lty=3,lwd=2)
points(falsePositive[opt.k+1],truePositive[opt.k+1],pch=1,cex=5,col="indianred",lwd=2)
# Error Types for varying threshold
plot(range,alpha,type="c",col="orange2",lty=1,lwd=2,xlab="k",ylab="Probability",main="(b) Type I and Type II Errors")
text(range,alpha,labels=alpha.num,col="forestgreen")
points(range,beta,type="c",col="steelblue",lty=1,lwd=1)
text(range,beta,labels=beta.num,col="forestgreen")
abline(v=opt.k,col="indianred",lty=3,lwd=2)
legend("right",c("Type 1","Type 2"),col = c("orange2","steelblue"),lty=c(1,1),lwd=c(2,2),cex=0.8)
text(opt.k+0.5,1,"k=6",col="indianred")
}
