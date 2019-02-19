filenames<-list.files(getwd(),full.names = TRUE,
                      recursive = TRUE)
for( f in filenames ){
    x <- readLines(f)
    y <- gsub( "TicagrelorVsClopidogrel", "TicagrelorVsClopidogrel_narrow", x )
    cat(y, file=f, sep="\n")
}


#Define the analyses settings
source("extras/CreateStudyAnalysisDetails.R")
createAnalysesDetails("inst/settings/")

#Positive control synthesis
source("extras/CreateStudyAnalysisDetails.R")
createPositiveControlSynthesisArgs("inst/settings/")

#insert Environment
OhdsiRTools::insertEnvironmentSnapshotInPackage("TicagrelorVsClopidogrel")
