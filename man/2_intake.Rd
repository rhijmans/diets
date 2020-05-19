\name{nutrientIntake}

\alias{nutrientIntake}

\title{
Compute nutrient intake from consumption and nutrient content data
}

\description{
This function return micronutrient intake by combining food quantity (consumption) and food micronutrient composition (content).
The tables are merged by the food group.
}


\usage{
nutrientIntake(Consumption, Content, byEnergy=TRUE, Miller=TRUE, Wg)
}

\arguments{
  \item{Consumption}{data.frame. Columns must be ordered. Order : Food group code, food group value}
  \item{Content}{data.frame. Columns must be ordered. Order : Food group Code, Micronutrient Tagname, micronutrient value}
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
\dontrun{
content  <-  1_nutrientContent(ISO3="RWA", conti=TRUE, palmoil=TRUE, sweetpot=TRUE)
consumption <- base_2_getConsumption(ISO3="RWA", start=2005, end=2009, use="ENERGY")

## You need Wg only if you choose use = "weight"
#Wg <- file.path(system.file(package = "diets"), "external/Wg.rds")

intake  <- base_3_Intake(consumption, content, use="ENERGY", zn=TRUE)
}
}

