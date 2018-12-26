# Copyright 2018 Observational Health Data Sciences and Informatics
#
# This file is part of TicagrelorVsClopidogrel
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#' Create the analyses details
#'
#' @details
#' This function creates files specifying the analyses that will be performed.
#'
#' @param workFolder        Name of local folder to place results; make sure to use forward slashes
#'                            (/)
#'
#' @export
createAnalysesDetails <- function(workFolder) {
  defaultPrior <- Cyclops::createPrior("laplace", 
                                       exclude = c(0),
                                       useCrossValidation = TRUE)
  
  defaultControl <- Cyclops::createControl(cvType = "auto",
                                           startingVariance = 0.01,
                                           noiseLevel = "quiet",
                                           tolerance  = 1e-06,
                                           maxIterations = 2500,
                                           cvRepetitions = 10,
                                           seed = 1234)

  defaultCovariateSettings <- FeatureExtraction::createCovariateSettings(useDemographicsGender = TRUE,
                                                                         useDemographicsAge = FALSE, 
                                                                         useDemographicsAgeGroup = TRUE,
                                                                         useDemographicsRace = TRUE, 
                                                                         useDemographicsEthnicity = FALSE,
                                                                         useDemographicsIndexYear = TRUE, 
                                                                         useDemographicsIndexMonth = TRUE,
                                                                         useDemographicsPriorObservationTime = FALSE,
                                                                         useDemographicsPostObservationTime = FALSE,
                                                                         useDemographicsTimeInCohort = FALSE,
                                                                         useDemographicsIndexYearMonth = FALSE,
                                                                         useConditionOccurrenceAnyTimePrior = FALSE,
                                                                         useConditionOccurrenceLongTerm = FALSE,
                                                                         useConditionOccurrenceMediumTerm = FALSE,
                                                                         useConditionOccurrenceShortTerm = TRUE,
                                                                         useConditionOccurrencePrimaryInpatientAnyTimePrior = FALSE,
                                                                         useConditionOccurrencePrimaryInpatientLongTerm = FALSE,
                                                                         useConditionOccurrencePrimaryInpatientMediumTerm = FALSE,
                                                                         useConditionOccurrencePrimaryInpatientShortTerm = FALSE,
                                                                         useConditionEraAnyTimePrior = FALSE, 
                                                                         useConditionEraLongTerm = FALSE,
                                                                         useConditionEraMediumTerm = FALSE, 
                                                                         useConditionEraShortTerm = FALSE,
                                                                         useConditionEraOverlapping = FALSE,
                                                                         useConditionEraStartLongTerm = FALSE,
                                                                         useConditionEraStartMediumTerm = FALSE,
                                                                         useConditionEraStartShortTerm = FALSE,
                                                                         useConditionGroupEraAnyTimePrior = FALSE,
                                                                         useConditionGroupEraLongTerm = TRUE,
                                                                         useConditionGroupEraMediumTerm = FALSE,
                                                                         useConditionGroupEraShortTerm = TRUE,
                                                                         useConditionGroupEraOverlapping = FALSE,
                                                                         useConditionGroupEraStartLongTerm = FALSE,
                                                                         useConditionGroupEraStartMediumTerm = FALSE,
                                                                         useConditionGroupEraStartShortTerm = FALSE,
                                                                         useDrugExposureAnyTimePrior = FALSE, 
                                                                         useDrugExposureLongTerm = FALSE,
                                                                         useDrugExposureMediumTerm = FALSE, 
                                                                         useDrugExposureShortTerm = FALSE,
                                                                         useDrugEraAnyTimePrior = FALSE, 
                                                                         useDrugEraLongTerm = FALSE,
                                                                         useDrugEraMediumTerm = FALSE, 
                                                                         useDrugEraShortTerm = FALSE,
                                                                         useDrugEraOverlapping = FALSE, 
                                                                         useDrugEraStartLongTerm = FALSE,
                                                                         useDrugEraStartMediumTerm = FALSE, 
                                                                         useDrugEraStartShortTerm = FALSE,
                                                                         useDrugGroupEraAnyTimePrior = FALSE, 
                                                                         useDrugGroupEraLongTerm = TRUE,
                                                                         useDrugGroupEraMediumTerm = FALSE, 
                                                                         useDrugGroupEraShortTerm = TRUE,
                                                                         useDrugGroupEraOverlapping = FALSE,
                                                                         useDrugGroupEraStartLongTerm = FALSE,
                                                                         useDrugGroupEraStartMediumTerm = FALSE,
                                                                         useDrugGroupEraStartShortTerm = FALSE,
                                                                         useProcedureOccurrenceAnyTimePrior = FALSE,
                                                                         useProcedureOccurrenceLongTerm = TRUE,
                                                                         useProcedureOccurrenceMediumTerm = FALSE,
                                                                         useProcedureOccurrenceShortTerm = TRUE,
                                                                         useDeviceExposureAnyTimePrior = FALSE,
                                                                         useDeviceExposureLongTerm = FALSE,
                                                                         useDeviceExposureMediumTerm = FALSE,
                                                                         useDeviceExposureShortTerm = TRUE,
                                                                         useMeasurementAnyTimePrior = FALSE, 
                                                                         useMeasurementLongTerm = FALSE,
                                                                         useMeasurementMediumTerm = FALSE, 
                                                                         useMeasurementShortTerm = FALSE,
                                                                         useMeasurementValueAnyTimePrior = FALSE,
                                                                         useMeasurementValueLongTerm = FALSE,
                                                                         useMeasurementValueMediumTerm = FALSE,
                                                                         useMeasurementValueShortTerm = FALSE,
                                                                         useMeasurementRangeGroupAnyTimePrior = FALSE,
                                                                         useMeasurementRangeGroupLongTerm = FALSE,
                                                                         useMeasurementRangeGroupMediumTerm = FALSE,
                                                                         useMeasurementRangeGroupShortTerm = FALSE,
                                                                         useObservationAnyTimePrior = FALSE, 
                                                                         useObservationLongTerm = FALSE,
                                                                         useObservationMediumTerm = FALSE, 
                                                                         useObservationShortTerm = FALSE,
                                                                         useCharlsonIndex = TRUE, 
                                                                         useDcsi = FALSE, 
                                                                         useChads2 = FALSE,
                                                                         useChads2Vasc = FALSE, 
                                                                         useDistinctConditionCountLongTerm = FALSE,
                                                                         useDistinctConditionCountMediumTerm = FALSE,
                                                                         useDistinctConditionCountShortTerm = FALSE,
                                                                         useDistinctIngredientCountLongTerm = FALSE,
                                                                         useDistinctIngredientCountMediumTerm = FALSE,
                                                                         useDistinctIngredientCountShortTerm = FALSE,
                                                                         useDistinctProcedureCountLongTerm = FALSE,
                                                                         useDistinctProcedureCountMediumTerm = FALSE,
                                                                         useDistinctProcedureCountShortTerm = FALSE,
                                                                         useDistinctMeasurementCountLongTerm = FALSE,
                                                                         useDistinctMeasurementCountMediumTerm = FALSE,
                                                                         useDistinctMeasurementCountShortTerm = FALSE,
                                                                         useDistinctObservationCountLongTerm = FALSE,
                                                                         useDistinctObservationCountMediumTerm = FALSE,
                                                                         useDistinctObservationCountShortTerm = FALSE,
                                                                         useVisitCountLongTerm = TRUE, 
                                                                         useVisitCountMediumTerm = FALSE,
                                                                         useVisitCountShortTerm = TRUE, 
                                                                         useVisitConceptCountLongTerm = FALSE,
                                                                         useVisitConceptCountMediumTerm = FALSE,
                                                                         useVisitConceptCountShortTerm = FALSE, 
                                                                         longTermStartDays = -365,
                                                                         mediumTermStartDays = -180, 
                                                                         shortTermStartDays = -7, 
                                                                         endDays = 0,
                                                                         includedCovariateConceptIds = c(), 
                                                                         addDescendantsToInclude = FALSE,
                                                                         excludedCovariateConceptIds = c(), 
                                                                         addDescendantsToExclude = FALSE,
                                                                         includedCovariateIds = c())
 		
  subGroupCovariateSettings <- function(windowStart = -365, windowEnd = -1, 
                                        shortTermWindowStart = -7, MaintenanceWindowEnd = 365,
                                        analysisId = 998)
  
  covariateSettings <- list(subGroupCovariateSettings, defaultCovariateSettings)
  
  getDbCmDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(washoutPeriod = 0,
                                                                   studyStartDate='20130301',
                                                                   firstExposureOnly = FALSE,
                                                                   removeDuplicateSubjects = 'keep first',
                                                                   restrictToCommonPeriod = TRUE,
                                                                   maxCohortSize = 0, 
                                                                   excludeDrugsFromCovariates = FALSE,
                                                                   covariateSettings = covariateSettings)

  timeToFirstPostIndexEvent1Year <- CohortMethod::createCreateStudyPopulationArgs(removeSubjectsWithPriorOutcome = FALSE,
                                                                                  firstExposureOnly = FALSE,
                                                                                  washoutPeriod = 0,
                                                                                  removeDuplicateSubjects = FALSE,
                                                                                  minDaysAtRisk = 364,
                                                                                  riskWindowStart = 1,
                                                                                  addExposureDaysToStart = FALSE,
                                                                                  riskWindowEnd = 365,
                                                                                  addExposureDaysToEnd = FALSE,
                                                                                  censorAtNewRiskWindow = FALSE)
  
  timeToFirstPostIndexEventOnTreatment <- CohortMethod::createCreateStudyPopulationArgs(removeSubjectsWithPriorOutcome = FALSE,
                                                                                        firstExposureOnly = FALSE,
                                                                                        washoutPeriod = 0,
                                                                                        removeDuplicateSubjects = FALSE,
                                                                                        minDaysAtRisk = 1,
                                                                                        riskWindowStart = 1,
                                                                                        addExposureDaysToStart = FALSE,
                                                                                        riskWindowEnd = 0,
                                                                                        addExposureDaysToEnd = TRUE,
                                                                                        censorAtNewRiskWindow = FALSE)

  timeToFirstPostIndexEvent7DaysFromTreatment <- CohortMethod::createCreateStudyPopulationArgs(removeSubjectsWithPriorOutcome = FALSE,
                                                                                               firstExposureOnly = FALSE,
                                                                                               washoutPeriod = 0,
                                                                                               removeDuplicateSubjects = FALSE,
                                                                                               minDaysAtRisk = 1,
                                                                                               riskWindowStart = 1,
                                                                                               addExposureDaysToStart = FALSE,
                                                                                               riskWindowEnd = 7,
                                                                                               addExposureDaysToEnd = TRUE,
                                                                                               censorAtNewRiskWindow = FALSE)
  
  timeToFirstPostIndexEventITT <- CohortMethod::createCreateStudyPopulationArgs(removeSubjectsWithPriorOutcome = FALSE,
                                                                                firstExposureOnly = FALSE,
                                                                                washoutPeriod = 0,
                                                                                removeDuplicateSubjects = FALSE,
                                                                                minDaysAtRisk = 1,
                                                                                riskWindowStart = 1,
                                                                                addExposureDaysToStart = FALSE,
                                                                                riskWindowEnd = 9999,
                                                                                addExposureDaysToEnd = FALSE,
                                                                                censorAtNewRiskWindow = FALSE)
  
  # timeToFirstPostIndexEventModifiedITT <- CohortMethod::createCreateStudyPopulationArgs(removeSubjectsWithPriorOutcome = FALSE,
  #                                                                                       firstExposureOnly = FALSE,
  #                                                                                       washoutPeriod = 0,
  #                                                                                       removeDuplicateSubjects = FALSE,
  #                                                                                       minDaysAtRisk = 1,
  #                                                                                       riskWindowStart = 1,
  #                                                                                       addExposureDaysToStart = FALSE,
  #                                                                                       riskWindowEnd = 9999,
  #                                                                                       addExposureDaysToEnd = FALSE,
  #                                                                                       censorAtNewRiskWindow = TRUE)

  createPsArgs1 <- CohortMethod::createCreatePsArgs(control = defaultControl, 
                                                    errorOnHighCorrelation = FALSE,
                                                    stopOnError = FALSE) 
  
  oneToOneMatchOnPsArgs <- CohortMethod::createMatchOnPsArgs(maxRatio = 1)

  variableRatioMatchOnPsArgs <- CohortMethod::createMatchOnPsArgs(maxRatio = 100)
  
  stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(numberOfStrata = 10) 
  
  fitOutcomeModelArgs1 <- CohortMethod::createFitOutcomeModelArgs(useCovariates = FALSE,
                                                                  modelType = "cox",
                                                                  stratified = TRUE,
                                                                  prior = defaultPrior, 
                                                                  control = defaultControl)
  
  a1 <- CohortMethod::createCmAnalysis(analysisId = 1,
                                       description = "Time to First Post Index Event within 1 year, Without Matching",
                                       getDbCohortMethodDataArgs = getDbCmDataArgs,
                                       createStudyPopArgs = timeToFirstPostIndexEvent1Year,
                                       fitOutcomeModel = TRUE,
                                       fitOutcomeModelArgs = fitOutcomeModelArgs1)

  a2 <- CohortMethod::createCmAnalysis(analysisId = 2,
                                       description = "Time to First Post Index Event within 1 year, With 1 to 1 Matching",
                                       getDbCohortMethodDataArgs = getDbCmDataArgs,
                                       createStudyPopArgs = timeToFirstPostIndexEvent1Year,
                                       createPs = TRUE,
                                       createPsArgs = createPsArgs1,
                                       matchOnPs = TRUE,
                                       matchOnPsArgs = oneToOneMatchOnPsArgs,
                                       fitOutcomeModel = TRUE,
                                       fitOutcomeModelArgs = fitOutcomeModelArgs1)

  a3 <- CohortMethod::createCmAnalysis(analysisId = 3,
                                       description = "Time to First Post Index Event within 1 year, With Variable Ratio Matching",
                                       getDbCohortMethodDataArgs = getDbCmDataArgs,
                                       createStudyPopArgs = timeToFirstPostIndexEvent1Year,
                                       createPs = TRUE,
                                       createPsArgs = createPsArgs1,
                                       matchOnPs = TRUE,
                                       matchOnPsArgs = variableRatioMatchOnPsArgs,
                                       fitOutcomeModel = TRUE,
                                       fitOutcomeModelArgs = fitOutcomeModelArgs1)

  a4 <- CohortMethod::createCmAnalysis(analysisId = 4,
                                       description = "Time to First Post Index Event within 1 year, With Stratification",
                                       getDbCohortMethodDataArgs = getDbCmDataArgs,
                                       createStudyPopArgs = timeToFirstPostIndexEvent1Year,
                                       createPs = TRUE,
                                       createPsArgs = createPsArgs1,
                                       stratifyByPs = TRUE,
                                       stratifyByPs = stratifyByPsArgs,
                                       fitOutcomeModel = TRUE,
                                       fitOutcomeModelArgs = fitOutcomeModelArgs1)

  a5 <- CohortMethod::createCmAnalysis(analysisId = 5,
                                       description = "Time To First Post Index Event On Treatment, Without Matching",
                                       getDbCohortMethodDataArgs = getDbCmDataArgs,
                                       createStudyPopArgs = timeToFirstPostIndexEventOnTreatment,
                                       fitOutcomeModel = TRUE,
                                       fitOutcomeModelArgs = fitOutcomeModelArgs1)

  a6 <- CohortMethod::createCmAnalysis(analysisId = 6,
                                       description = "Time To First Post Index Event On Treatment, With 1 to 1 Matching",
                                       getDbCohortMethodDataArgs = getDbCmDataArgs,
                                       createStudyPopArgs = timeToFirstPostIndexEventOnTreatment,
                                       createPs = TRUE,
                                       createPsArgs = createPsArgs1,
                                       matchOnPs = TRUE,
                                       matchOnPsArgs = oneToOneMatchOnPsArgs,
                                       fitOutcomeModel = TRUE,
                                       fitOutcomeModelArgs = fitOutcomeModelArgs1)

  a7 <- CohortMethod::createCmAnalysis(analysisId = 7,
                                       description = "Time To First Post Index Event On Treatment, With Variable Ratio Matching",
                                       getDbCohortMethodDataArgs = getDbCmDataArgs,
                                       createStudyPopArgs = timeToFirstPostIndexEventOnTreatment,
                                       createPs = TRUE,
                                       createPsArgs = createPsArgs1,
                                       matchOnPs = TRUE,
                                       matchOnPsArgs = variableRatioMatchOnPsArgs,
                                       fitOutcomeModel = TRUE,
                                       fitOutcomeModelArgs = fitOutcomeModelArgs1)

  a8 <- CohortMethod::createCmAnalysis(analysisId = 8,
                                       description = "Time To First Post Index Event On Treatment, With Stratification",
                                       getDbCohortMethodDataArgs = getDbCmDataArgs,
                                       createStudyPopArgs = timeToFirstPostIndexEventOnTreatment,
                                       createPs = TRUE,
                                       createPsArgs = createPsArgs1,
                                       stratifyByPs = TRUE,
                                       stratifyByPs = stratifyByPsArgs,
                                       fitOutcomeModel = TRUE,
                                       fitOutcomeModelArgs = fitOutcomeModelArgs1)

  a9 <- CohortMethod::createCmAnalysis(analysisId = 9,
                                       description = "Time To First Post Index Event 7Days From Treatment, Without Matching",
                                       getDbCohortMethodDataArgs = getDbCmDataArgs,
                                       createStudyPopArgs = timeToFirstPostIndexEvent7DaysFromTreatment,
                                       fitOutcomeModel = TRUE,
                                       fitOutcomeModelArgs = fitOutcomeModelArgs1)

  a10 <- CohortMethod::createCmAnalysis(analysisId = 10,
                                       description = "Time To First Post Index Event 7Days From Treatment, With 1 to 1 Matching",
                                       getDbCohortMethodDataArgs = getDbCmDataArgs,
                                       createStudyPopArgs = timeToFirstPostIndexEvent7DaysFromTreatment,
                                       createPs = TRUE,
                                       createPsArgs = createPsArgs1,
                                       matchOnPs = TRUE,
                                       matchOnPsArgs = oneToOneMatchOnPsArgs,
                                       fitOutcomeModel = TRUE,
                                       fitOutcomeModelArgs = fitOutcomeModelArgs1)

  a11 <- CohortMethod::createCmAnalysis(analysisId = 11,
                                       description = "Time To First Post Index Event 7Days From Treatment, With Variable Ratio Matching",
                                       getDbCohortMethodDataArgs = getDbCmDataArgs,
                                       createStudyPopArgs = timeToFirstPostIndexEvent7DaysFromTreatment,
                                       createPs = TRUE,
                                       createPsArgs = createPsArgs1,
                                       matchOnPs = TRUE,
                                       matchOnPsArgs = variableRatioMatchOnPsArgs,
                                       fitOutcomeModel = TRUE,
                                       fitOutcomeModelArgs = fitOutcomeModelArgs1)

  a12 <- CohortMethod::createCmAnalysis(analysisId = 12,
                                       description = "Time To First Post Index Event 7Days From Treatment, With Stratification",
                                       getDbCohortMethodDataArgs = getDbCmDataArgs,
                                       createStudyPopArgs = timeToFirstPostIndexEvent7DaysFromTreatment,
                                       createPs = TRUE,
                                       createPsArgs = createPsArgs1,
                                       stratifyByPs = TRUE,
                                       stratifyByPs = stratifyByPsArgs,
                                       fitOutcomeModel = TRUE,
                                       fitOutcomeModelArgs = fitOutcomeModelArgs1)

  a13 <- CohortMethod::createCmAnalysis(analysisId = 13,
                                       description = "Time To First Post Index Event ITT, Without Matching",
                                       getDbCohortMethodDataArgs = getDbCmDataArgs,
                                       createStudyPopArgs = timeToFirstPostIndexEventITT,
                                       fitOutcomeModel = TRUE,
                                       fitOutcomeModelArgs = fitOutcomeModelArgs1)

  a14 <- CohortMethod::createCmAnalysis(analysisId = 14,
                                       description = "Time To First Post Index Event ITT, With 1 to 1 Matching",
                                       getDbCohortMethodDataArgs = getDbCmDataArgs,
                                       createStudyPopArgs = timeToFirstPostIndexEventITT,
                                       createPs = TRUE,
                                       createPsArgs = createPsArgs1,
                                       matchOnPs = TRUE,
                                       matchOnPsArgs = oneToOneMatchOnPsArgs,
                                       fitOutcomeModel = TRUE,
                                       fitOutcomeModelArgs = fitOutcomeModelArgs1)

  a15 <- CohortMethod::createCmAnalysis(analysisId = 15,
                                       description = "Time To First Post Index Event ITT, With Variable Ratio Matching",
                                       getDbCohortMethodDataArgs = getDbCmDataArgs,
                                       createStudyPopArgs = timeToFirstPostIndexEventITT,
                                       createPs = TRUE,
                                       createPsArgs = createPsArgs1,
                                       matchOnPs = TRUE,
                                       matchOnPsArgs = variableRatioMatchOnPsArgs,
                                       fitOutcomeModel = TRUE,
                                       fitOutcomeModelArgs = fitOutcomeModelArgs1)

  a16 <- CohortMethod::createCmAnalysis(analysisId = 16,
                                       description = "Time To First Post Index Event ITT, With Stratification",
                                       getDbCohortMethodDataArgs = getDbCmDataArgs,
                                       createStudyPopArgs = timeToFirstPostIndexEventITT,
                                       createPs = TRUE,
                                       createPsArgs = createPsArgs1,
                                       stratifyByPs = TRUE,
                                       stratifyByPs = stratifyByPsArgs,
                                       fitOutcomeModel = TRUE,
                                       fitOutcomeModelArgs = fitOutcomeModelArgs1)


  cmAnalysisList <- list(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16)
  #cmAnalysisList <- list(a1)
  
  CohortMethod::saveCmAnalysisList(cmAnalysisList, file.path(workFolder, "cmAnalysisList.json"))
}

createTcos <- function(outputFolder) {
  pathToCsv <- system.file("settings", "TcosOfInterest.csv", package = "TicagrelorVsClopidogrel")
  tcosOfInterest <- read.csv(pathToCsv, stringsAsFactors = FALSE)
  pathToCsv <- system.file("settings", "NegativeControls.csv", package = "TicagrelorVsClopidogrel")
  negativeControls <- read.csv(pathToCsv)
  negativeControlOutcomes <- negativeControls[negativeControls$type == "Outcome", ]
  dcosList <- list()
  tcs <- unique(tcosOfInterest[, c("targetId", "comparatorId")])
  for (i in 1:nrow(tcs)) {
    targetId <- tcs$targetId[i]
    comparatorId <- tcs$comparatorId[i]
    outcomeIds <- as.character(tcosOfInterest$outcomeIds[tcosOfInterest$targetId == targetId & tcosOfInterest$comparatorId == comparatorId])
    outcomeIds <- as.numeric(strsplit(outcomeIds, split = ";")[[1]])
    outcomeIds <- c(outcomeIds, negativeControlOutcomes$outcomeId)
    excludeConceptIds <- tcosOfInterest$excludedCovariateConceptIds[tcosOfInterest$targetId == targetId & tcosOfInterest$comparatorId == comparatorId]
    excludeConceptIds <- as.numeric(strsplit(excludeConceptIds, split = ";")[[1]])
    dcos <- CohortMethod::createDrugComparatorOutcomes(targetId = targetId,
                                                       comparatorId = comparatorId,
                                                       outcomeIds = outcomeIds,
                                                       excludedCovariateConceptIds =  excludeConceptIds)
    dcosList[[length(dcosList) + 1]] <- dcos
  }
  return(dcosList)
}
