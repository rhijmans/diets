

.onAttach <- function(lib, pkg)  {
	pkg.info <- utils::packageDescription("diets") 
	packageStartupMessage(paste("diets ", pkg.info[["Version"]], " (", pkg.info["Date"], ")", sep=""))
}

