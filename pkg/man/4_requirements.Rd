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
The number of pregnant women is assumed to be equal to 3/4 of the 1 year old population.
The number of lactating women is assumed to be equal to the 1 year old population.

Micronutrient tagname are available at "http://www.fao.org/infoods/infoods/standards-guidelines/food-component-identifiers-tagnames/en/"
}

\value{
data.frame
}

\examples{
# standard daily requirements in g 
reqrda <- readRDS(system.file("ex/reqs_rda.rds", package = "diets"))

pop <- readRDS(system.file("ex/rwa_pop.rds", package = "diets"))

popreq <- nutrient_requirements(pop, reqrda)
head(popreq)

}


