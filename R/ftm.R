ftm <-
function(d,sampled,k){

# Preparations -------------------------------------------------
size=dim(d)[1]
n=dim(d)[3]
# Assign Zero to all diagonals of the sampled slices (in case 1's were coded)
for (i in 1:n){diag(d[,,i])<-0}
# Preparations -----------------------------------------------
# Define the sampled and unsampled regions
unsampled=(1:size)[-sampled]
sampled.matrix=matrix(1,size,size)
sampled.matrix[,unsampled]<-0
sampled.matrix[unsampled,]<-0
unsampled.matrix=(sampled.matrix-1)*(-1)

# Seperate the knowledge and perception parts of each sampled individual
sampled.knowledge=d
for (i in 1:n)
{
sampled.knowledge[,,i][(1:size)[-sampled[i]],(1:size)[-sampled[i]]]<-0
}
sampled.perception=d-sampled.knowledge

# Step 1 -------------------
# EXACT (Estimate the knowledge region, step 1 in paper)
# combine sampled knowledge
combined.knowledge=matrix(0,size,size)
for (i in 1:n){
combined.knowledge=combined.knowledge+sampled.knowledge[,,i]
}
exact=combined.knowledge*sampled.matrix
exact[exact<2]<-0 #an entry becomes zero only if both claim, that is entry=2 (LAS intersection)
exact[exact>=1]<-1

# Contribution to perception
contribution2perception=combined.knowledge*unsampled.matrix

# Step 2 -------------------
# combine sampled perception
combined.perception=matrix(0,size,size)
for (i in 1:n){
combined.perception=combined.perception+sampled.perception[,,i]
}
active.perception=combined.perception*unsampled.matrix

# Find alpha
inactive.perception=combined.perception*sampled.matrix
inactive.perception[inactive.perception<k]<-0
inactive.perception[inactive.perception>=k]<-1
# alpha.num is the type 1 errors committed, so we count 1 in inactive perc and zero in knowledge region
alpha.num=sum((inactive.perception-exact)==1)
# alpha.denom is the number of zeros in the knowledge region
alpha.denom=sum(sampled.matrix-exact)
alpha=alpha.num/alpha.denom

# Find beta
beta.num=sum(inactive.perception<exact)
beta.denom=(sum(exact)+0.000001)
beta=beta.num/beta.denom

updated.perception=active.perception+contribution2perception
# chop the updated perception to get the final perception
final.perception=updated.perception
final.perception[final.perception<k]<-0 #choped here
# Step 3: Combine Steps 1 and 2 to get the estimated network------------------
estimated=exact+final.perception
estimated[estimated>=1]<-1
output=list(estimated,alpha,beta,alpha.num,alpha.denom,beta.num,beta.denom)
names(output)=c("estimatedNetwork","type1Error","type2Error","type1Count","type1Instances","type2Count","type2Instances")
return(output)
}
