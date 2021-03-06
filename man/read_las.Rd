% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_las.R
\name{read_las}
\alias{read_las}
\title{read an las file}
\usage{
read_las(path, ...)
}
\arguments{
\item{path}{single path}

\item{paths}{vector of paths for 'read_header()'}
}
\value{
<Delete and Replace>
}
\description{
read an las file
}
\details{
read an las file

\cr
Revision History
\tabular{ll}{
1.0 \tab 2017 March 08 Created\cr
}
}
\examples{
   files=list.files("C:\\\\temp\\\\lidar_test\\\\",full.names=T)
   print(read_header("C:\\\\temp\\\\lidar_test\\\\183_302.las"))
   print(read_header(files))

}
\seealso{
\code{\link{read_dtm}}\cr \code{\link{gridmetrics}}\cr
}
\author{
Jacob Strunk <Jstrunk@fs.fed.us>
}
