\name{mysummary}
\alias{mysummary}
\title{get measures of central tendency}
\description{
  get measures of central tendency
}

\usage{
mysummary(x, npar=TRUE, print=TRUE)
}

\arguments{
  \item{x}{A data vector}
  \item{npar}{ if true then sd else mad }
  \item{print}{ print results }
}

\value{
  \code{mysummary} returns an object of class
}

\author{Jan Verbesselt}

\examples{

# invoking the function 
set.seed(1234)
x <- rpois(500, 4) 
y <- mysummary(x)
Median= 4
MAD= 1.4826 
# y$center is the median (4) 
# y$spread is the median absolute deviation (1.4826)

y <- mysummary(x, npar=FALSE, print=FALSE)
# no output 
# y$center is the mean (4.052)
# y$spread is the standard deviation (2.01927)

}


