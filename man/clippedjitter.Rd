\name{clippedjitter}
\alias{clippedjitter}
\title{
Jitters a vector while preserving the minimum and maximum
}
\description{
Provides a small jitter to the data so that it is non-overlapping, but preserves
the minimum and maximum so the plotted points do not appear out of bounds
on an axis showing the range.
}
\usage{
clippedjitter(x, ...)
}

\arguments{
  \item{x}{
numeric vector to have jitter applied
}
  \item{...}{
other arguments passed to the normal jitter function
}
}

\value{
data which has jitter applied
}

\author{
Steven J. Murdoch <http://www.cl.cam.ac.uk/users/sjm217/>
}

\seealso{
\code{\link{jitter}}
}

\examples{
  data(faithful)
  jitter(faithful$waiting)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
