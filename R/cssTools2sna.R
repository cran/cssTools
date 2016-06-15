cssTools2sna <-
function(d){
size=dim(d)[3]
y=array(dim=c(size,dim(d)[1],dim(d)[2]))
for (i in 1:size) {y[i,,]=d[,,i]}
y
}
