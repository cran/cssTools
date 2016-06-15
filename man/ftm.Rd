\name{ftm}
\alias{ftm}
\title{Aggregate CSS Slices for a Fixed Threshod}
\description{
Estimate a network of interest by aggregating the sampled CSS slices for a fixed threshold.
}
\usage{
ftm(d, sampled, k)
}
\arguments{
  \item{d}{Sampled CSS slices in \code{cssTools} package format.}
  \item{sampled}{A vector indicating which network individuals are sampled.}
  \item{k}{A threshold for aggregating the CSS slices.}
}
\details{
Given a random sample of observed CSS slices and a fixed threshold 
value \emph{k} for aggregation, the \code{ftm} function aggregates the observed 
slices and provides an estimate for the network of interest by using 
the fixed threshold method (FTM) given in Yenigun et. al. (2016). 
The function also returns the estimated type 1 and type 2 errors. 
}
\value{
\item{estimatedNetwork}{An estimate of the network of interest.}
\item{type1Error}{Estimated type 1 error rate.}
\item{type2Error}{Estimated type 2 error rate.}
\item{type1Count}{Total number of type 1 errors committed.}
\item{type1Instances}{Number of instances for a potential type 1 error. 
In other words, number of zeros in the knowledge region of the true network. 
Here by knowledge region we mean the ties in the network such that both
actors are sampled, and the tie is estimated by the intersection of 
the self reports from both actors.
Note that \code{type1Error} equals \code{type1Count} divided by \code{type1Instances}.}
\item{type2Count}{Total number of type 2 errors committed.}
\item{type2Instances}{Number of instances for a potential type 2 error. 
In other words, number of ones in the knowledge region of the true network. 
Note that \code{type2Error} equals \code{type2Count} divided by \code{type2Instances}.}
}
\references{
D. Yenigun, G. Ertan, M.D. Siciliano (2016). Omission and commission errors in 
network cognition and estimation using ROC curve. arXiv:1606.03245 [stat.CO] \url{https://arxiv.org/abs/1606.03245}
}
\author{
Deniz Yenigun, Gunes Ertan, Michael Siciliano
}
\seealso{
\code{\link{atm}}, \code{\link{rtm}}
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

# For a given threshold, say 2, we can combine these slices as follows,
# which gives an estimate of the complete network 
ftm(dSampled,sampled,2)
}