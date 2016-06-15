\name{atm}
\alias{atm}
\title{Estimate a Network Using the Adaptive Threshold Method}
\description{
Estimate a network of interest by aggregating the sampled CSS slices using the adaptive threshold method. 
This requires setting a tolerable level of type 1 error.
}
\usage{atm(d, sampled, alpha)}
\arguments{
  \item{d}{Sampled CSS slices in \code{cssTools} package format.}
  \item{sampled}{A vector indicating which network individuals are sampled.}
  \item{alpha}{Tolerable type 1 error.}
}
\details{
Given a random sample of observed CSS slices and a tolerable type 1 error, 
the \code{atm} function uses the adaptive threshold method (ATM) of Siciliano et. al. (2012) to aggregate 
the observed slices and provides an estimate for the network of interest. 
}
\value{
\item{estimatedNetwork}{An estimate of the network of interest.}
\item{threshold}{The threshold value required to reach the given type 1 error rate.}
}
\references{
M.D. Siciliano, D. Yenigun, G. Ertan (2012). Estimating network structure via random sampling: 
Cognitive social structures and adaptive threshold method. 
Social Networks, Vol. 34, No. 4, 585-600. \url{http://dx.doi.org/10.1016/j.socnet.2012.06.004}
}
\author{
Deniz Yenigun, Gunes Ertan, Michael Siciliano
}
\seealso{
\code{\link{ftm}}, \code{\link{rtm}}
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

# Suppose you randomly sampled A, D, and E
sampled=c(1,4,5)

# Then all you have is the following three sampled slices of A, D and E
dSampled=d[,,sampled]

# For a given alpha value, say 0.2, we can combine these slices as follows,
# which gives an estimate of the complete network 
atm(dSampled,sampled,0.2)
}

