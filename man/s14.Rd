\name{s14}
\alias{s14}
\title{Calculate s14 Similarity Index}
\description{
Computes the \eqn{S_{14}} similarity index between two network matrices.
}
\usage{
s14(d1, d2)
}

\arguments{
  \item{d1}{An \eqn{n} by \eqn{n} matrix representing a network.}
  \item{d2}{An \eqn{n} by \eqn{n} matrix representing a network.}
}
\details{
Given two networks of interest, a common measure of similarity is the \eqn{S_{14}} 
index introduced by Gower and Lagendre (1986). The function \code{s14} computes this similarity 
measure for two networks having the same dimensions.
}
\value{
The \eqn{S_{14}} similarity index.
}
\references{
Gower, J.C., Legendre, P. (1986). Metric and Euclidean properties of dissimilarity coefficients. 
Journal of Classification, 3, 5-48. \url{http://dx.doi.org/10.1007/BF01896809}
}
\author{
Deniz Yenigun, Gunes Ertan, Michael Siciliano
}
\seealso{
\code{\link{sliceQuality}}
}
\examples{
# Consider two matrices representing networks, d1 and d2
d1=matrix(c(0,0,1,0,1,0,0,1,0,0,1,1,0,0,0,0,0,0,0,0,1,0,0,0,0),5,5)
d2=matrix(c(0,1,0,0,0,1,0,1,0,0,0,1,0,0,0,0,0,0,0,0,0,1,1,0,0),5,5)

# The similarity index between d1 and d2
s14(d1,d2)
}

