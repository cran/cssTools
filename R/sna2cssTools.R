sna2cssTools <-
function(d){
size=dim(d)[1]
y=array(dim=c(dim(d)[2],dim(d)[3],size))
for (i in 1:size) {y[,,i]=d[i,,]}
y
}
