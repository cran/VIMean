\name{VIM}
\alias{VIM}
\title{variability independent of mean (VIM)}
\usage{
VIM(data,id,repeat.vars)
}
\arguments{
  \item{data}{a width type data frame}
  \item{id}{the name of variable indicating the subjects, e.g., "ID"}
  \item{repeat.vars}{the name of variables indicating the repeated measurement data, e.g., c("x1","x2","x3")}
}
\value{
  \item{vim}{the VIM of each subject}
  \item{coef}{the coefficients fitted from the regression of "sntd ~ k*avg^p"}
  \item{r.squared,adj.r.squared}{the r-squared and adjusted r-squared of the regression}
}
\description{
Please feel free to contact us, if you have any advice and find any bug!

Version History:

Version 0.1.0: The first version.
}
\examples{
set.seed(123)
df <- data.frame( "ID" = paste("ID", seq(1,100,1), sep = ""),
                  "x1" = sample(90:220, size = 100, replace = TRUE),
                  "x2" = sample(90:220, size = 100, replace = TRUE),
                  "x3" = sample(90:220, size = 100, replace = TRUE),
                  "x4" = sample(90:220, size = 100, replace = TRUE),
                  "x5" = sample(90:220, size = 100, replace = TRUE))
rst <- VIM(data=df,id="ID",repeat.vars=paste0("x",1:5))
}
