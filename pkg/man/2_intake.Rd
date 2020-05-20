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
nutrientIntake(consumption, content, byEnergy=TRUE, Miller=TRUE, Wg)
}

\arguments{
  \item{consumption}{data.frame. Columns must be ordered. Order : Food group code, food group value}
  \item{content}{data.frame. Columns must include "code", "group", "tag", "value", "unit"}
  \item{byEnergy}{logical. If \code{TRUE} merge content and consumption by "energy", else by "weight"}
  \item{Miller}{logical. if \code{TRUE}, apply Miller equation for phytic acid and Zn to compute physiological Zn}
  \item{Wg}{data.frame. Only useful if \code{byENERGY = FALSE}. Columns must be ordered. Order : Food group Code, Yield , Edible}
}

\details{
Micronutrient tagname are avaliable at http://www.fao.org/infoods/infoods/standards-guidelines/food-component-identifiers-tagnames/en/
}

\value{
matrix
}

\references{
Hotz C and Brown KH, (Eds.) (2004) International Zinc Nutrition Consultative Group (IZiNCG) Assessment of the risk of zinc deficiency in populations and options for its control. Food and Nutrition Bulletin 25: S91-S204, 2004.
}

\author{
Eric Massiot, Ty Beal, Robert Hijmans
}


\examples{
content <- nutrientContent(continent="Africa", redpalmoil=0.5, orangesweetpot=0.2)
consumption <- readRDS(system.file("ex/rwa_FBS.rds", package = "diets"))
intake  <- nutrientIntake(consumption, content)


## You need Wg only if you choose use = "weight"
Wg <- readRDS(system.file("ex/Wg.rds", package = "diets")
}


