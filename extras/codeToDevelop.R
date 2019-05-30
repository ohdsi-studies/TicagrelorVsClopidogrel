#Define the analyses settings
source("extras/CreateStudyAnalysisDetails.R")
createAnalysesDetails("inst/settings/")
#Define the analyses settings for limited analysis (without on-treatment with blanking period)
source("extras/CreateStudyAnalysisDetailsWithoutOntreatmentWithBlankingPeriod.R")
createAnalysesDetails("inst/settings/")

#Positive control synthesis
source("extras/CreateStudyAnalysisDetails.R")
createPositiveControlSynthesisArgs("inst/settings/")

#insert Environment
OhdsiRTools::insertEnvironmentSnapshotInPackage("TicagrelorVsClopidogrel")

#Build Docker Image
system("docker build -t chandryou/ticagrelorvsclopidogrel .")
