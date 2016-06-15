\name{rtmPlot}
\alias{rtmPlot}
\title{Plots for the ROC Based Threshold Method for Estimating Networks}
\description{
Visualisation of the ROC based threshold method for estimating networks, 
implemented by the \code{rtm} function. 
}
\usage{
rtmPlot(rtmOutput)
}
\arguments{
  \item{rtmOutput}{Output from the function \code{rtm}.}
}
\details{
The function \code{rtm} uses the density weighted ROC based threshold method (RTM) of Yenigun et. al. (2016) 
for estimating networks from a random sample of CSS slices. The output from \code{rtm} is 
visualized by the function \code{rtmPlot}, which displays the ROC curve, as well as the 
type 1 and type 2 error counts for each threshold value.
}
\references{
D. Yenigun, G. Ertan, M.D. Siciliano (2016). Omission and commission errors in 
network cognition and estimation using ROC curve. arXiv:1606.03245 [stat.CO] \url{https://arxiv.org/abs/1606.03245}
}
\author{
Deniz Yenigun, Gunes Ertan, Michael Siciliano
}
\seealso{
\code{\link{rtm}}
}
\examples{
# Load the highTechManagers data given in cssTools package
data(highTechManagers)

# There are 21 CSS slices in the complete data 
# Suppose we only observed the 10 slices with the following indexes
sampled=c(2,4,5,8,9,10,11,14,18,19)

# Then the observed data is the following
dSampled=highTechManagers[,,sampled]

# Apply the ROC based threshold method to estimate the network
y=rtm(dSampled,sampled)

# Now plot the ROC curve and the error types for various threshold values
rtmPlot(y)
}

