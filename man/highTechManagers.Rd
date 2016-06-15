\name{highTechManagers}
\alias{highTechManagers}
\docType{data}
\title{High Tech Managers Data Set}
\description{
Krackhardt (1987) reports the CSS data collected from 21 managers in a high tech machinery firm. 
Perceptions of all individuals on the whole network is provided.
}
\usage{data(highTechManagers)}
\format{
A 21 by 21 by 21 array of zeroes (nonexistence of tie) and ones (existence of tie), where the 
perception slice of the \emph{i}-th individual correspons to \code{highTechManagers[,,i]}. 
}
\details{
In a CSS data set, each actor not only reports his or her self-ties, but also answers questions on all 
possible ties in the network. Then a CSS for a network involving \emph{N} individuals may be represented 
by a three dimensional array \eqn{R_{i,j,m}} (\eqn{i,j,m=1,...,N}), where \emph{i} is the sender, \emph{j} 
is the receiver, and \emph{m} is the perceiver of the relationship. 
This data set contains the CSS given in Krackhardt (1987), which reports the perceptions of 
all individuals in a network of 21 managers in a high tech machinery firm. In the original data 17th slice is 
problematic since row 17 in this slice consists of ones only. To overcome this, we replaced row 17 with column 17.
}

\references{
Krackhardt, D. (1987). Cognitive social structures. Social Networks 9, 109-134. \url{http://dx.doi.org/10.1016/0378-8733(87)90009-8}
}
\examples{
data(highTechManagers)
sliceQuality(highTechManagers)
}
\keyword{datasets}
