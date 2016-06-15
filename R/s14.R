s14 <-
function(d1,d2){
size=dim(d1)[1]
# make sure that the diagonals for both martices are same, say all 1's
diag(d1)<-1
diag(d2)<-1
t=table(d1,d2)
A=t[1,1] # Matching 0's
D=t[2,2] # Matching 1's minus the diagonal
C=t[2,1] # 1 in perception, 0 in true (TYPE 1 ERROR)
B=t[1,2] # 0 in perception, 1 in true (TYPE 2 ERROR)
r1=A/(A+C)
r2=B/(B+D)
r3=A/(A+B)
r4=C/(C+D)
S=sqrt((r1-r2)*(r3-r4)) #This is the alternative computational form for S14. Different than the formula in Siciliano et. al. 2012, but produces the same result!
S
}
