rtm <-
function(d,sampled){
alpha=array()
beta=array()
alpha.num=array()
alpha.denom=array()
beta.num=array()
beta.denom=array()
n=dim(d)[3]
range=0:n
# Find h by looking at slice densities------------------- 
# Compute Average of Slice Densities
densities=array(dim=n)
for (j in 1:n){
densities[j]=gden(d[,,j])
}
h=1/mean(densities)
for (k in range){
if (k==0)
{
alpha[k+1]=1
beta[k+1]=0
}else
{
e=ftm(d,sampled,k)
#if(e[[2]]<0.001){k;break}
alpha[k+1]=e[[2]]
beta[k+1]=e[[3]]
alpha.num[k+1]=e[[4]]
alpha.denom[k+1]=e[[5]]
beta.num[k+1]=e[[6]]
beta.denom[k+1]=e[[7]]
}
}
truePositive=1-beta
falsePositive=alpha
alpha.num[1]=alpha.denom[2]
alpha.denom[1]=alpha.denom[2]
beta.num[1]=0
beta.denom[1]=beta.denom[2]
dist=sqrt((1-truePositive)^2+(h*falsePositive)^2) 
opt.k=range[which.min(dist)]
# Estimated Network
estimated=ftm(d,sampled,opt.k)
details=round(cbind(range,falsePositive,1-truePositive,truePositive,alpha.num,beta.num,dist),3)
colnames(details)=c("k","type1Error(FPR)","type2Error","TPR(1-type2Error)","type1count","type2count","distance")
output=list(estimated[[1]],estimated[[2]],estimated[[3]],opt.k,details)
names(output)=c("estimatedNetwork","type1Error","type2Error","threshold","details")
return(output)
}
