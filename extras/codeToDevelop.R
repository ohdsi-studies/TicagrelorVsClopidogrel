#Define the analyses settings
source("extras/CreateStudyAnalysisDetails.R")
createAnalysesDetails("inst/settings/")

#Positive control synthesis
source("extras/CreateStudyAnalysisDetails.R")
createPositiveControlSynthesisArgs("inst/settings/")

#insert Environment
OhdsiRTools::insertEnvironmentSnapshotInPackage("TicagrelorVsClopidogrel")
