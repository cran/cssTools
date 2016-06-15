\name{sna2cssTools}
\alias{sna2cssTools}
\title{Convert a CSS in \code{sna}  Format to a CSS in \code{cssTools} Format}
\description{
Converts a CSS in \code{sna} package format to a CSS in \code{cssTools} package format.
}
\usage{
sna2cssTools(d)
}
\arguments{
  \item{d}{A CSS in \code{sna} package format.}
}
\details{
In \code{sna} package, a CSS \code{d} is coded in a three dimensional array such that 
\code{d[i,,]} is the \emph{i}-th slice. In \code{cssTools} package, the same object is coded in a 
three dimensional array such that \code{d[,,i]} is the \emph{i}-th slice. The \code{sna2cssTools} 
function transforms \code{sna} format to \code{cssTools} format.
}
\value{
The same CSS coded in \code{cssTools} format.
}
\author{
Deniz Yenigun, Gunes Ertan, Michael Siciliano
}
\seealso{
\code{\link{cssTools2sna}}
}
\examples{
# Consider the example in Siciliano et. al. (2012),
# a network with five actors A, B, C, D, E
sA=matrix(c(0,0,1,0,1,0,0,1,0,0,1,1,0,0,0,0,0,0,0,0,1,0,0,0,0),5,5)
sB=matrix(c(0,1,0,0,0,1,0,1,0,0,0,1,0,0,0,0,0,0,0,0,0,1,1,0,0),5,5)
sC=matrix(c(0,1,1,0,0,0,0,1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0),5,5)
sD=matrix(c(0,0,1,0,1,0,0,1,1,0,1,1,0,0,0,0,1,0,0,1,1,0,0,1,0),5,5)
sE=matrix(c(0,0,0,1,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,1,0,1,0,1,0),5,5)
d=array(dim=c(5,5,5))
d[,,1]=sA
d[,,2]=sB
d[,,3]=sC
d[,,4]=sD
d[,,5]=sE

# Here d is coded in cssTools package format
# Switching between sna and cssTools formats
e=cssTools2sna(d)
f=sna2cssTools(e)
}

