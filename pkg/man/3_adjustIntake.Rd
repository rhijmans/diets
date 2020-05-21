\encoding{UTF-8}

\name{adjustIntake}

\alias{add_Ca}
\alias{adjust_Ca}
\alias{adjust_Zn}
\alias{adjust_Fe}

\title{
Adjusted micronutrient intake for Ca, Zn, Fe, and by fortification

add_Ca adds Ca from water consumption 

adjust_Ca adjust Ca consumption based on protein consumption 

adjust_Zn uses the Miller equation for phytic acid and Zn to compute physiological Zn

adjust_Fe ...
}

\description{
Adjust micronutrient intake.
}

\usage{
add_Ca(intake, consumption=1.7, concentration=0.042)
adjust_Ca(intake)
adjust_Zn(intake)
adjust_Fe(intake, heme)
}

\arguments{
  \item{intake}{\code{data.frame} created by \code{\link{nutrientIntake}}. The columns must contain "tag" and "intake"}
  \item{consumption}{numeric}
  \item{concentration}{numeric}
  \item{heme}{\code{data.frame}}
}

\value{
data.frame
}

\references{
Hotz C and Brown KH, (eds.), 2004. International Zinc Nutrition Consultative Group (IZiNCG) Assessment of the risk of zinc deficiency in populations and options for its control. Food and Nutrition Bulletin 25: S91-S204.

Pourkhalili A, Mirlohi M, Rahimi E , 2013. Heme iron content in lamb meat is differentially altered upon boiling, grilling, or frying as assessed by four distinct analytical methods. The Scientific World Journal

Hallberg L, Björn-Rasmussen E, Howard L, Rossander L, 1979. Dietary heme iron absorption: a discussion of possible mechanisms for the absorption-promoting effect of meat and for the regulation of iron absorption. Scandinavian journal of gastroenterology 14: 769-779

Hallberg L, Hultén L, Gramatkovski E, 1997. Iron absorption from the whole diet in men: how effective is the regulation of iron absorption? The American journal of clinical nutrition 66: 347-356
}


\examples{
consumption <- readRDS(system.file("ex/rwa_FBS.rds", package = "diets"))
consumption <- consumption[consumption$Element == "Food supply (kcal/capita/day)", c("Item", "Value")]
colnames(consumption) <- c("group", "value")
content <- nutrientContent(continent="Africa", redpalmoil=0.5, orangesweetpot=0.2)
intake  <- nutrientIntake(consumption, content)

intake <- add_Ca(intake)
intake <- adjust_Ca(intake)

intake <- adjust_Zn(intake)

iron <- readRDS(system.file("ex/NHemeIron.rds", package="diets"))
intake <- adjust_Fe(intake, iron)
}
