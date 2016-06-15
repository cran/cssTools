\name{sliceQuality}
\alias{sliceQuality}
\title{Evaluate Several Characteristics of Slices from a CSS}
\description{
Given a fully observed CSS, this function evaluates the quality of each slice 
by comparing them with the true network obtained by LAS intersection. 
}
\usage{
sliceQuality(d)
}
\arguments{
  \item{d}{A CSS in \code{cssTools} package format.}
}
\details{
A common way of defining a true network for a given CSS is the LAS intersection 
(see, for example, Siciliano et. al. 2012, or Krackhardt, 1987). For a given CSS, the function \code{sliceQuality} first computes 
the true network by LAS intersection, and then compares each slice with the true network. 
The considered quantities are matching zeros, matching ones, type 1 errors, type 2 errors, 
\eqn{S_{14}} similarity index, error proportion and correlation.
}
\value{
\item{trueNetwork}{The true network obtained by LAS intersection method.}
\item{sliceQuality}{A table summarizing the quality of each CSS slice in rows. Columns indicate 
A (matching zeros), B (0 in CSS slice, 1 in true matrix, i.e., type 2 error), 
C (1 in CSS slice, 0 in true network, i.e., type 1 error) D (matching ones), 
s14 (\eqn{S_{14}} similarity index between the CSS slice and the true nework), 
errorProp (proportion of unmatching cells), and correlation (correlation between the CSS slice 
and the true network computed by the \code{gcor} function in the \code{sna} package).}
}
\references{
Krackhardt, D. (1987). Cognitive social structures. Social Networks 9, 109-134. \url{http://dx.doi.org/10.1016/0378-8733(87)90009-8}

M.D. Siciliano, D. Yenigun, G. Ertan (2012). Estimating network structure via random sampling: 
Cognitive social structures and adaptive threshold method. 
Social Networks, Vol. 34, No. 4, 585-600. \url{http://dx.doi.org/10.1016/j.socnet.2012.06.004}
}
\author{
Deniz Yenigun, Gunes Ertan, Michael Siciliano
}
\seealso{
\code{\link{s14}}
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

# Compute the quality of CSS slices
sliceQuality(d)
}
