\name{nutrient_requirements}

\alias{nutrient_requirements}

\title{
Compute requirements for a population 
}

\description{
Compute nutrient requirements for a population based on the age and sex distribution
}

\usage{
nutrient_requirements(pop, reqs)
}

\arguments{
  \item{pop}{data.frame. Columns must be ordered. Order : gender, age, values}
  \item{reqs}{data.frame. Columns must be ordered. Order : gender, physiological status (one of "lactating", "pregnant", ""), age group start, age group end, micronutrient, values}
}

\details{
Pregnant women are assumed to be 3/4 of the 1 year old population.
Lactating women are assumed to be the 1 year old population.

Partition of the population groups into the 3 age groups are assumed to be 1/4 for 14-19 years, 31-51 years and 1/2 for 19-31 years. 
Micronutrient tagname are available at "http://www.fao.org/infoods/infoods/standards-guidelines/food-component-identifiers-tagnames/en/"
}

\value{
data.frame
}

\author{
Eric Massiot, Ty Beal, Robert Hijmans
}


\seealso{
\code{\link{help}}
}

\examples{
\dontrun{
req_age <- readRDS(file.path(system.file(package = "diets"), "external/ReqAge.rds"))
pop <- base_6_getPopulation(ISO3='RWA', start=2005, end=2009)
requirements <- base_7_Requirements(pop, req_age, zn=TRUE)
}
}

