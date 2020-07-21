\name{nutrientIntake}

\alias{nutrientIntake}

\title{
Compute nutrient intake from consumption and nutrient content data
}

\description{
This function computes micronutrient intake from food quantity (consumption) and food micronutrient composition (content).
The tables are merged by the food group. By default, content and consumption are merged usung by the energy consumption and content of the food groups. It can also be done by weight, in which case a data.frame with parameters for the food groups needs to be supplied.
}


\usage{
nutrientIntake(consumption, content, weight=NULL, verbose=TRUE)
}

\arguments{
  \item{consumption}{data.frame. Must contain variables "group" and "value". "group" is used to join the \code{consumption} and \code{content} data.frames}
  \item{content}{data.frame. Must contain variables "group", "tag", "value", and "unit"}
  \item{weight}{data.frame for merging by weight. Columns must include "group" "edible" and "yield"}
  \item{verbose}{logical. If \code{TRUE} groups that are in consumption but not in conent are printed}
}

\details{
Micronutrient tag names are available at http://www.fao.org/infoods/infoods/standards-guidelines/food-component-identifiers-tagnames/en/
}

\value{
data.frame that includes for each group the mass consumed and the amount of micronutrients conmsumed with that , both in (g/capita)/day
}


\examples{
consumption <- readRDS(system.file("ex/rwa_FBS.rds", package = "diets"))
head(consumption)
consumption <- consumption[consumption$Element == "Food supply (kcal/capita/day)", c("Item", "Value")]
colnames(consumption) <- c("group", "value")
# consumption by food group in (kcal/capita) / day
head(consumption) 

content <- nutrientContent(continent="Africa", redpalmoil=0.5, orangesweetpot=0.2)
head(content)

intake  <- nutrientIntake(consumption, content)

## You need Wg only if you choose use = "weight"
Wg <- readRDS(system.file("ex/groupWeight.rds", package = "diets"))
intake2  <- nutrientIntake(consumption, content, weight=Wg)
}


