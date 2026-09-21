\name{l_length}
\alias{l_length}
\alias{l_clamp}
\title{Logical Lengths and Responsive Constraints}
\description{Construct device-independent lengths that are resolved in a layout context.}
\usage{
l_length(value = "auto", unit = "px")
l_clamp(minimum, preferred, maximum)
}
\arguments{
\item{value}{A finite number, length string, or existing logical length.}
\item{unit}{Unit for numeric values: px, percent (\code{"\%"}), pt, mm, cm, in, vw, vh, vmin or vmax.}
\item{minimum, preferred, maximum}{Logical lengths defining a clamp expression.}
}
\details{One logical pixel is 1/96 inch; one point is 1/72 inch. Percentages refer
to the parent content box. Viewport units refer to the root. Strings support
\code{auto}, \code{min()}, \code{max()} and \code{clamp()}, including nested expressions.
Parsing never evaluates R code. Device DPI does not change logical pixel size.}
\value{An immutable S3 \code{l_length} object.}
\examples{
l_length("50\%")
l_length(2, "cm")
l_clamp("12pt", "3vmin", "24pt")
}
