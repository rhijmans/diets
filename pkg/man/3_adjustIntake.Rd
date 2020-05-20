\encoding{UTF-8}

\name{adjustIntake}

\alias{adjustIntake}

\title{
Adjusted micronutrient intake
}

\description{
Adjust micronutrient intake.
}

\usage{
adjust_ca(intake)
adjust_zn(intake)
adjust_fe(intake, heme)
adjust_fortify(intake, fortif)
}

\arguments{
  \item{intake}{\code{data.frame} created by \code{\link{nutrientIntake}}. The columns must contain "tag", "intake"}
  \item{fortif}{\code{data.frame} with fortification data}
}

\value{
data.frame
}

\references{
Pourkhalili A, Mirlohi M, Rahimi E (2013) Heme iron content in lamb meat is differentially altered upon boiling, grilling, or frying as assessed by four distinct analytical methods. The Scientific World Journal, 2013

Hallberg L, Björn-Rasmussen E, Howard L, Rossander L (1979) Dietary heme iron absorption: a discussion of possible mechanisms for the absorption-promoting effect of meat and for the regulation of iron absorption. Scandinavian journal of gastroenterology, 14(7), 769-779

Hallberg L, Hultén L, Gramatkovski E (1997) Iron absorption from the whole diet in men: how effective is the regulation of iron absorption? The American journal of clinical nutrition, 66(2), 347-356
}


\examples{

#iron <- readRDS(system.file("ex/NHemeIron.rds", package="diets"))
#adjust_fe(intake, iron)

\dontrun{

#content <- nutrientContent(conti=TRUE, palmoil=TRUE, sweetpot=TRUE)
#consumption <- getConsumption(start=2005, end=2009, use="ENERGY")
#intake  <- Intake(consumption, content, use="ENERGY", zn=TRUE)
#adj_intake <- adjustIntake(intake, ca=TRUE, fe=TRUE, fortif=FALSE)
}
}
