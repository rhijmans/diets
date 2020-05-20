\name{nutrientIntake}

\alias{nutrientIntake}

\title{
Compute nutrient intake from consumption and nutrient content data
}

\description{
This function computes micronutrient intake from food quantity (consumption) and food micronutrient composition (content).
The tables are merged by the food group. 
}


\usage{
nutrientIntake(consumption, content, byEnergy=TRUE, Miller=TRUE, weight, verbose=TRUE)
}

\arguments{
  \item{consumption}{data.frame. Must contain variables "group" and "value". "group" is used to join the \code{consumption} and \code{content} data.frames}
  \item{content}{data.frame. Must contain variables "group", "tag", "value", and "unit"}
  \item{byEnergy}{logical. If \code{TRUE} merge content and consumption by "energy", else by "weight"}
  \item{Miller}{logical. if \code{TRUE}, apply Miller equation for phytic acid and Zn to compute physiological Zn}
  \item{weight}{data.frame. Only useful if \code{byEnergy=FALSE}. Columns must include "group", "yield" and "edible"}
}

\details{
Micronutrient tagnames are available at http://www.fao.org/infoods/infoods/standards-guidelines/food-component-identifiers-tagnames/en/
}

\value{
matrix
}

\references{
Hotz C and Brown KH, (Eds.) (2004) International Zinc Nutrition Consultative Group (IZiNCG) Assessment of the risk of zinc deficiency in populations and options for its control. Food and Nutrition Bulletin 25: S91-S204, 2004.
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
Wg <- readRDS(system.file("ex/groupWeights.rds", package = "diets"))
}


