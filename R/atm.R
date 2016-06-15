atm <-
function(d,sampled,alpha){
k=1
for (t in 1:100){
k=k+1
c=ftm(d,sampled,k)
if (c[[2]]<alpha){estimated=c[[1]]}&{t;break}
}
output=list(estimated,k)
names(output)=c("estimatedNetwork","threshold")
return(output)
}
