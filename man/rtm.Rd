\name{rtm}
\alias{rtm}
\title{Estimate a Network Using the ROC Based Threshold Method}
\description{
Estimate a network of interest by aggregating the sampled CSS slices using the ROC based threshold method. 
}
\usage{
rtm(d, sampled)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{d}{Sampled CSS slices in \code{cssTools} package format.}
  \item{sampled}{A vector indicating which network individuals are sampled.}
}
\details{
Given a random sample of observed CSS slices, the \code{rtm} function uses the density weighted 
ROC based threshold method (RTM) of Yenigun  et. al. (2016) to aggregate the observed slices, 
and provides an estimate for the network of interest. Slice densities are computed by the 
\code{gden} function in the \code{sna} package.
}
\value{
\item{estimatedNetwork}{An estimate of the network of interest.}
\item{type1Error}{Estimated type 1 error rate at the optimum threshold returned by the density weighted ROC method.}
\item{type2Error}{Estimated type 2 error rate at the optimum threshold returned by the density weighted ROC method.}
\item{threshold}{The optimum threshold value.}
\item{details}{A table giving the details of the density weighted ROC method.Columns indicate the threshold, 
type 1 error (false positive rate), type 2 error, true positive rate (1 - type 2 error), type 1 error count, 
type 2 error count, and distance.}
}
\references{
D. Yenigun, G. Ertan, M.D. Siciliano (2016). Omission and commission errors in 
network cognition and estimation using ROC curve. arXiv:1606.03245 [stat.CO] \url{https://arxiv.org/abs/1606.03245}
}
\author{
Deniz Yenigun, Gunes Ertan, Michael Siciliano
}
\seealso{
\code{\link{atm}}, \code{\link{ftm}}
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

# We can combine these slices as follows, 
# which gives an estimate of the complete network
rtm(dSampled,sampled)
}


