\name{oesch}
\alias{oesch}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Oesch class schema}
\description{This function codes the Oesch class schema based on respondent's occupation, employment status, and number of employees}
\usage{
oesch(isco08=data$isco08, self.employed=data$selfem, n.employees=data$n_employees, n.classes=5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
\item{isco08}{The four digit ISCO 08 code}
\item{self.employed}{Numeric vector with 0=not self-employed and 1=self-employed}
\item{n.employees}{Numeric vector with the number of employees. Will be recoded to 0/<=10/>10}
\item{n.classes}{Numeric value indicating desired degree of differentiation of class schema}
}
\references{
https://people.unil.ch/danieloesch/scripts/
}
\author{
Simon Bienstman
}
\note{

# prep status indicator variables
# for function input like occupar, we need to properly code NA in n.employees before calling function
# Those who are working for family business are coded as self-employed without employees in Oesch schema
# -> need to fix that as well before function call
# those working for family business have no empl. anyways -> ess[emplrel==3, table(emplno)]

freq(ess$emplrel)
ess[, self.employed:=0]
ess[emplrel==2 | emplrel==3, self.employed:=1]
tail(freq(ess$emplno))
ess[, n.employees:=ifelse(is.na(emplno) | emplno>66665,0, emplno)]

freq(ess$self.employed)
summary(ess$n.employees)

# Function depends on package "sjlabelled"
# Function based on isco08. If isco88, convert to isco08 using occupar::isco88to08()
# For input variables, see above.
# possible classes: 16,8,5
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x)
{
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
