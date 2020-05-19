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
adjust_intake_ca(intake)
adjust_intake_fe(intake)
adjust_intake_fortificaiton(intake, fortif)
}

\arguments{
  \item{intake}{data.frame. The column must be ordered as "Tagname", "Value Capita day", "Code", "Value", "Mass"}
  \item{fortif}{data.frame with fortification data}
}

\value{
data.frame
}

\references{
Pourkhalili A, Mirlohi M, Rahimi E (2013) Heme iron content in lamb meat is differentially altered upon boiling, grilling, or frying as assessed by four distinct analytical methods. The Scientific World Journal, 2013

Hallberg L, Björn-Rasmussen E, Howard L, Rossander L (1979) Dietary heme iron absorption: a discussion of possible mechanisms for the absorption-promoting effect of meat and for the regulation of iron absorption. Scandinavian journal of gastroenterology, 14(7), 769-779

Hallberg L, Hultén L, Gramatkovski E (1997) Iron absorption from the whole diet in men: how effective is the regulation of iron absorption? The American journal of clinical nutrition, 66(2), 347-356
}

\author{
Eric Massiot, Ty Beal, Robert Hijmans
}

\examples{
\dontrun{
#content <- 1_nutrientContent(ISO3="RWA", conti=TRUE, palmoil=TRUE, sweetpot=TRUE)
#consumption <- base_2_getConsumption(ISO3="RWA", start=2005, end=2009, use="ENERGY")
#intake  <- base_3_Intake(consumption, content, use="ENERGY", zn=TRUE)
#adj_intake <- base_4_AdjustIntake(ISO3="RWA", intake, ca=TRUE, fe=TRUE, fortif=FALSE)
}
}
