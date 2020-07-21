
library(gtdq)
library(diets)
#path <- file.path(system.file(package = "gtdq"), "external")
#old_content <- base::readRDS(file.path(path, "example/Content.RDS"))
#old_consump <- base::readRDS(file.path(path, "example/Consumption.RDS"))
#old_intake  <- gtdq::Intake(old_consump, old_content, "ENERGY", Wgpath=NULL, zn=TRUE)
#old_adj_intake <- gtdq::AdjustIntake("RWA", old_intake, ca=TRUE, fe=TRUE, fortif=FALSE)

new_content <- nutrientContent(continent="Africa", redpalmoil=0.5, orangesweetpot=0)
old_content <- new_content[, c(1,3,4)]
colnames(old_content) <- c("Code_FdGp1", "Tagname", "MNutr_Val")

consumption <- readRDS(system.file("ex/rwa_FBS.rds", package = "diets"))
u <- unique(new_content[,1:2])
m <- merge(consumption, u, by.x="Item", by.y="group", all.x=TRUE)
m[is.na(m$code), ]
consumption <- m[!is.na(m$code), ]

consumption <- consumption[consumption$Element == "Food supply (kcal/capita/day)", c("Item", "Value", "code")]
new_consumption <- consumption[,1:2]
colnames(new_consumption) <- c("group", "value")
old_consumption <- consumption[,3:2]
colnames(old_consumption) <- c("Code_FdGp1", "FdGp1_Val")

old_intake  <- gtdq::Intake(old_consumption, old_content, "ENERGY", Wgpath=NULL, zn=FALSE)
new_intake  <- nutrientIntake(new_consumption, new_content, verbose=FALSE)

old <- aggregate(old_intake[,"MNutr_PersonDay",drop=FALSE],  old_intake[,"Tagname",drop=FALSE], sum, na.rm=TRUE)
old[old[,1] =="FE", ]
new <- aggregate(new_intake[,"intake",drop=FALSE], new_intake[ ,"tag",drop=FALSE], sum)
new[new[,1] =="FE", ]

# intake is good
x <- merge(old, new, by=1)
plot(x[,2:3])
abline(0,1)


