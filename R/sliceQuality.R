sliceQuality <-
function(d){
true=consensus(cssTools2sna(d),method="LAS.intersection") 
size=dim(d)[1]
Z=array(dim=c(size,7))
for(i in 1:size){
# make sure that the diagonals for true and slices are same, say all 1's
diag(d[,,i])<-1
diag(true)<-1
t=table(d[,,i],true) 
A=t[1,1] # Matching 0's
D=t[2,2] # Matching 1's minus the diagonal
C=t[2,1] # 1 in perception, 0 in true (TYPE 1 ERROR)
B=t[1,2] # 0 in perception, 1 in true (TYPE 2 ERROR)
r1=A/(A+C)
r2=B/(B+D)
r3=A/(A+B)
r4=C/(C+D)
S=sqrt((r1-r2)*(r3-r4)) #This is the alternative computational form for S14. Different than the formula in Siciliano et. al. 2012, but produces the same result! 
# This definition of S14 was given in Krachardt (1990) (Assessing the political landscape, Administrative Science Quarterly)
Er=(B+C)/((size-1)*size)
cor=gcor(d[,,i],true)
Z[i,]=c(A,B,C,D,S,Er,cor)
}
colnames(Z)=c("A","B","C","D","s14","errorProp","correlation")
output=list(true,Z)
names(output)=c("trueNetwork","sliceQuality")
return(output)
}
