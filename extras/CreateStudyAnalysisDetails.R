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
                                             tolerance  = 2e-07,
                                             maxIterations = 2500,
                                             cvRepetitions = 10,
                                             seed = 1234)
    
    defaultCovariateSettings <-  FeatureExtraction::createCovariateSettings(useDemographicsGender = TRUE,
                                                                            #useDemographicsAge = FALSE,
                                                                            useDemographicsAgeGroup = TRUE,
                                                                            useDemographicsRace = TRUE,
                                                                            #useDemographicsEthnicity = FALSE,
                                                                            useDemographicsIndexYear = TRUE,
                                                                            useDemographicsIndexMonth = TRUE,
                                                                            #useDemographicsPriorObservationTime = FALSE,
                                                                            #useDemographicsPostObservationTime = FALSE,
                                                                            #useDemographicsTimeInCohort = FALSE,
                                                                            #useDemographicsIndexYearMonth = FALSE,
                                                                            #useConditionOccurrenceAnyTimePrior = FALSE,
                                                                            useConditionOccurrenceLongTerm = TRUE,
                                                                            #useConditionOccurrenceMediumTerm = TRUE,
                                                                            useConditionOccurrenceShortTerm = TRUE,
                                                                            #useConditionOccurrencePrimaryInpatientAnyTimePrior = FALSE,
                                                                            #useConditionOccurrencePrimaryInpatientLongTerm = FALSE,
                                                                            #useConditionOccurrencePrimaryInpatientMediumTerm = TRUE,
                                                                            #useConditionOccurrencePrimaryInpatientShortTerm = FALSE,
                                                                            #useConditionEraAnyTimePrior = FALSE,
                                                                            #useConditionEraLongTerm = FALSE,
                                                                            #useConditionEraMediumTerm = FALSE,
                                                                            #useConditionEraShortTerm = FALSE,
                                                                            #useConditionEraOverlapping = FALSE,
                                                                            #useConditionEraStartLongTerm = FALSE,
                                                                            #useConditionEraStartMediumTerm = FALSE,
                                                                            #useConditionEraStartShortTerm = FALSE,
                                                                            useConditionGroupEraAnyTimePrior = TRUE,
                                                                            #useConditionGroupEraLongTerm = TRUE,
                                                                            #useConditionGroupEraMediumTerm = FALSE,
                                                                            #useConditionGroupEraShortTerm = TRUE,
                                                                            #useConditionGroupEraOverlapping = FALSE,
                                                                            #useConditionGroupEraStartLongTerm = FALSE,
                                                                            #useConditionGroupEraStartMediumTerm = FALSE,
                                                                            #useConditionGroupEraStartShortTerm = FALSE,
                                                                            #useDrugExposureAnyTimePrior = FALSE,
                                                                            #useDrugExposureLongTerm = FALSE,
                                                                            #useDrugExposureMediumTerm = TRUE,
                                                                            useDrugExposureShortTerm = TRUE,
                                                                            #useDrugEraAnyTimePrior = FALSE,
                                                                            #useDrugEraLongTerm = FALSE,
                                                                            #useDrugEraMediumTerm = FALSE,
                                                                            #useDrugEraShortTerm = FALSE,
                                                                            #useDrugEraOverlapping = FALSE,
                                                                            #useDrugEraStartLongTerm = FALSE,
                                                                            #useDrugEraStartMediumTerm = FALSE,
                                                                            #useDrugEraStartShortTerm = FALSE,
                                                                            useDrugGroupEraAnyTimePrior = TRUE,
                                                                            useDrugGroupEraLongTerm = TRUE,
                                                                            #useDrugGroupEraMediumTerm = FALSE,
                                                                            useDrugGroupEraShortTerm = TRUE,
                                                                            useDrugGroupEraOverlapping = TRUE,
                                                                            #useDrugGroupEraStartLongTerm = FALSE,
                                                                            #useDrugGroupEraStartMediumTerm = FALSE,
                                                                            #useDrugGroupEraStartShortTerm = FALSE,
                                                                            #useProcedureOccurrenceAnyTimePrior = FALSE,
                                                                            useProcedureOccurrenceLongTerm = TRUE,
                                                                            #useProcedureOccurrenceMediumTerm = FALSE,
                                                                            useProcedureOccurrenceShortTerm = TRUE,
                                                                            #useDeviceExposureAnyTimePrior = FALSE,
                                                                            #useDeviceExposureLongTerm = TRUE,
                                                                            #useDeviceExposureMediumTerm = FALSE,
                                                                            useDeviceExposureShortTerm = TRUE,
                                                                            #useMeasurementAnyTimePrior = FALSE,
                                                                            #useMeasurementLongTerm = FALSE,
                                                                            #useMeasurementMediumTerm = TRUE,
                                                                            useMeasurementShortTerm = TRUE,
                                                                            #useMeasurementValueAnyTimePrior = FALSE,
                                                                            #useMeasurementValueLongTerm = FALSE,
                                                                            #useMeasurementValueMediumTerm = TRUE,
                                                                            useMeasurementValueShortTerm = TRUE,
                                                                            #useMeasurementRangeGroupAnyTimePrior = FALSE,
                                                                            #useMeasurementRangeGroupLongTerm = FALSE,
                                                                            #useMeasurementRangeGroupMediumTerm = FALSE,
                                                                            #useMeasurementRangeGroupShortTerm = FALSE,
                                                                            #useObservationAnyTimePrior = FALSE,
                                                                            #useObservationLongTerm = FALSE,
                                                                            #useObservationMediumTerm = FALSE,
                                                                            #useObservationShortTerm = FALSE,
                                                                            #useCharlsonIndex = FALSE,
                                                                            #useDcsi = FALSE,
                                                                            #useChads2 = FALSE,
                                                                            #useChads2Vasc = FALSE,
                                                                            #useDistinctConditionCountLongTerm = FALSE,
                                                                            #useDistinctConditionCountMediumTerm = FALSE,
                                                                            #useDistinctConditionCountShortTerm = FALSE,
                                                                            #useDistinctIngredientCountLongTerm = FALSE,
                                                                            #useDistinctIngredientCountMediumTerm = FALSE,
                                                                            #useDistinctIngredientCountShortTerm = FALSE,
                                                                            #useDistinctProcedureCountLongTerm = FALSE,
                                                                            #useDistinctProcedureCountMediumTerm = FALSE,
                                                                            #useDistinctProcedureCountShortTerm = FALSE,
                                                                            #useDistinctMeasurementCountLongTerm = FALSE,
                                                                            #useDistinctMeasurementCountMediumTerm = FALSE,
                                                                            #useDistinctMeasurementCountShortTerm = FALSE,
                                                                            #useDistinctObservationCountLongTerm = FALSE,
                                                                            #useDistinctObservationCountMediumTerm = FALSE,
                                                                            #useDistinctObservationCountShortTerm = FALSE,
                                                                            useVisitCountLongTerm = TRUE,
                                                                            #useVisitCountMediumTerm = FALSE,
                                                                            #useVisitCountShortTerm = FALSE,
                                                                            #useVisitConceptCountLongTerm = FALSE,
                                                                            #useVisitConceptCountMediumTerm = FALSE,
                                                                            #useVisitConceptCountShortTerm = FALSE,
                                                                            longTermStartDays = -365,
                                                                            mediumTermStartDays = -30,
                                                                            shortTermStartDays = -7,
                                                                            endDays = 0,
                                                                            includedCovariateConceptIds = c(),
                                                                            addDescendantsToInclude = FALSE,
                                                                            excludedCovariateConceptIds = c(1322184,
                                                                                                            40241186),
                                                                            addDescendantsToExclude = TRUE,
                                                                            includedCovariateIds = c())
    
    # defaultCovariateSettings <-  FeatureExtraction::createCovariateSettings(useDemographicsGender = TRUE,
    #                                                                        useDemographicsAge = FALSE,
    #                                                                        useDemographicsAgeGroup = TRUE,
    #                                                                        useConditionOccurrenceLongTerm = FALSE,
    #                                                                        useConditionOccurrenceMediumTerm = FALSE,
    #                                                                        useConditionOccurrenceShortTerm = TRUE,
    #                                                                        addDescendantsToInclude = FALSE,
    #                                                                        excludedCovariateConceptIds = c(1322184,
    #                                                                                                        40241186),
    #                                                                        addDescendantsToExclude = TRUE,
    #                                                                        includedCovariateIds = c())
    
    
    subGroupCovariateSettings <- createSubgroupCovariateSettings(windowStart = -365, windowEnd = -1,
                                                                 shortTermWindowStart = -7,
                                                                 MaintenanceWindowEnd = 365,
                                                                 analysisId = 998)
    
    covariateSettings <- list(defaultCovariateSettings ,subGroupCovariateSettings
    )
    getDbCmDataArgs <- CohortMethod::createGetDbCohortMethodDataArgs(washoutPeriod = 0,
                                                                     firstExposureOnly = FALSE,
                                                                     removeDuplicateSubjects = 'keep first',
                                                                     restrictToCommonPeriod = TRUE,
                                                                     maxCohortSize = 0,
                                                                     excludeDrugsFromCovariates = FALSE,
                                                                     covariateSettings = covariateSettings)
    
    OneYearOutcome <- CohortMethod::createCreateStudyPopulationArgs(removeSubjectsWithPriorOutcome = FALSE,
                                                                    firstExposureOnly = FALSE,
                                                                    washoutPeriod = 0,
                                                                    removeDuplicateSubjects = 'keep first',
                                                                    minDaysAtRisk = 1,
                                                                    riskWindowStart = 1,
                                                                    addExposureDaysToStart = FALSE,
                                                                    riskWindowEnd = 365,
                                                                    addExposureDaysToEnd = FALSE,
                                                                    censorAtNewRiskWindow = FALSE)
    
    OneYearOutcomeWithBlanking <- CohortMethod::createCreateStudyPopulationArgs(removeSubjectsWithPriorOutcome = FALSE,
                                                                                firstExposureOnly = FALSE,
                                                                                washoutPeriod = 0,
                                                                                removeDuplicateSubjects = 'keep first',
                                                                                minDaysAtRisk = 28,
                                                                                riskWindowStart = 29,
                                                                                addExposureDaysToStart = FALSE,
                                                                                riskWindowEnd = 365,
                                                                                addExposureDaysToEnd = FALSE,
                                                                                censorAtNewRiskWindow = FALSE)
    
    # OneYearOutcomeWithMinTAR <- CohortMethod::createCreateStudyPopulationArgs(removeSubjectsWithPriorOutcome = FALSE,
    #                                                                           firstExposureOnly = FALSE,
    #                                                                           washoutPeriod = 0,
    #                                                                           removeDuplicateSubjects = 'keep first',
    #                                                                           minDaysAtRisk = 364,
    #                                                                           riskWindowStart = 1,
    #                                                                           addExposureDaysToStart = FALSE,
    #                                                                           riskWindowEnd = 365,
    #                                                                           addExposureDaysToEnd = FALSE,
    #                                                                           censorAtNewRiskWindow = FALSE)
    
    OnTreatment <- CohortMethod::createCreateStudyPopulationArgs(removeSubjectsWithPriorOutcome = FALSE,
                                                                 firstExposureOnly = FALSE,
                                                                 washoutPeriod = 0,
                                                                 removeDuplicateSubjects = 'keep first',
                                                                 minDaysAtRisk = 1,
                                                                 riskWindowStart = 1,
                                                                 addExposureDaysToStart = FALSE,
                                                                 riskWindowEnd = 0,
                                                                 addExposureDaysToEnd = TRUE,
                                                                 censorAtNewRiskWindow = FALSE)
    
    OnTreatmentWithBlanking <- CohortMethod::createCreateStudyPopulationArgs(removeSubjectsWithPriorOutcome = FALSE,
                                                                             firstExposureOnly = FALSE,
                                                                             washoutPeriod = 0,
                                                                             removeDuplicateSubjects = 'keep first',
                                                                             minDaysAtRisk = 28,
                                                                             riskWindowStart = 29,
                                                                             addExposureDaysToStart = FALSE,
                                                                             riskWindowEnd = 0,
                                                                             addExposureDaysToEnd = TRUE,
                                                                             censorAtNewRiskWindow = FALSE)
    
    ITT <- CohortMethod::createCreateStudyPopulationArgs(removeSubjectsWithPriorOutcome = FALSE,
                                                         firstExposureOnly = FALSE,
                                                         washoutPeriod = 0,
                                                         removeDuplicateSubjects = 'keep first',
                                                         minDaysAtRisk = 1,
                                                         riskWindowStart = 1,
                                                         addExposureDaysToStart = FALSE,
                                                         riskWindowEnd = 1825,
                                                         addExposureDaysToEnd = FALSE,
                                                         censorAtNewRiskWindow = FALSE)
    
    
    ITTwithBlanking <- CohortMethod::createCreateStudyPopulationArgs(removeSubjectsWithPriorOutcome = FALSE,
                                                                     firstExposureOnly = FALSE,
                                                                     washoutPeriod = 0,
                                                                     removeDuplicateSubjects = 'keep first',
                                                                     minDaysAtRisk = 28,
                                                                     riskWindowStart = 29,
                                                                     addExposureDaysToStart = FALSE,
                                                                     riskWindowEnd = 1825,
                                                                     addExposureDaysToEnd = FALSE,
                                                                     censorAtNewRiskWindow = FALSE)
    
    # timeToFirstPostIndexEvent7DaysFromTreatment <- CohortMethod::createCreateStudyPopulationArgs(removeSubjectsWithPriorOutcome = FALSE,
    #                                                                                              firstExposureOnly = FALSE,
    #                                                                                              washoutPeriod = 0,
    #                                                                                              removeDuplicateSubjects = 'keep first',
    #                                                                                              minDaysAtRisk = 1,
    #                                                                                              riskWindowStart = 1,
    #                                                                                              addExposureDaysToStart = FALSE,
    #                                                                                              riskWindowEnd = 7,
    #                                                                                              addExposureDaysToEnd = TRUE,
    #                                                                                              censorAtNewRiskWindow = FALSE)
    
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
    
    subgroupCovariateIds <- c(1000, 2000, 3000, 4000, 5000, 6000)+subGroupCovariateSettings$analysisId #c(1998, 2998, 3998, 4998, 5998, 6998)
    
    createPsArgs1 <- CohortMethod::createCreatePsArgs(control = defaultControl,
                                                      errorOnHighCorrelation = FALSE,
                                                      excludeCovariateIds = subgroupCovariateIds,
                                                      stopOnError = FALSE)
    
    #trimByPsArgs<- CohortMethod::createTrimByPsArgs(trimFraction = 0.05)
    
    MatchOnPsArgs <- CohortMethod::createMatchOnPsArgs(maxRatio = 10,
                                                       caliper = 0.2,
                                                       caliperScale = "standardized logit")
    
    # variableRatioMatchOnPsArgs <- CohortMethod::createMatchOnPsArgs(maxRatio = 100,
    #                                                                 caliper = 0.2,
    #                                                                 caliperScale = "standardized logit")
    
    stratifyByPsArgs <- CohortMethod::createStratifyByPsArgs(numberOfStrata = 10)
    
    #without matching arg
    fitOutcomeModelArgs0 <- CohortMethod::createFitOutcomeModelArgs(useCovariates = FALSE,
                                                                    modelType = "cox",
                                                                    stratified = FALSE,
                                                                    prior = defaultPrior,
                                                                    control = defaultControl)
    
    fitOutcomeModelArgs1 <- CohortMethod::createFitOutcomeModelArgs(useCovariates = FALSE,
                                                                    modelType = "cox",
                                                                    stratified = TRUE,
                                                                    prior = defaultPrior,
                                                                    control = defaultControl)
    
    
    a1 <- CohortMethod::createCmAnalysis(analysisId = 1,
                                         description = "One-year outcome, matching",
                                         getDbCohortMethodDataArgs = getDbCmDataArgs,
                                         createStudyPopArgs = OneYearOutcome,
                                         createPs = TRUE,
                                         createPsArgs = createPsArgs1,
                                         matchOnPs = TRUE,
                                         matchOnPsArgs = MatchOnPsArgs,
                                         fitOutcomeModel = TRUE,
                                         fitOutcomeModelArgs = fitOutcomeModelArgs1)
    
    a2 <- CohortMethod::createCmAnalysis(analysisId = 2,
                                         description = "One-year outcome, stratification",
                                         getDbCohortMethodDataArgs = getDbCmDataArgs,
                                         createStudyPopArgs = OneYearOutcome,
                                         createPs = TRUE,
                                         createPsArgs = createPsArgs1,
                                         #trimByPs = TRUE,
                                         #trimByPsArgs = trimByPsArgs,
                                         stratifyByPs = TRUE,
                                         stratifyByPsArgs = stratifyByPsArgs,
                                         fitOutcomeModel = TRUE,
                                         fitOutcomeModelArgs = fitOutcomeModelArgs1)
    
    a3 <- CohortMethod::createCmAnalysis(analysisId = 3,
                                         description = "One-year outcome, without matching",
                                         getDbCohortMethodDataArgs = getDbCmDataArgs,
                                         createStudyPopArgs = OneYearOutcome,
                                         createPs = FALSE,
                                         createPsArgs = NULL,
                                         #trimByPs = TRUE,
                                         #trimByPsArgs = trimByPsArgs,
                                         stratifyByPs = FALSE,
                                         stratifyByPsArgs = NULL,
                                         fitOutcomeModel = TRUE,
                                         fitOutcomeModelArgs = fitOutcomeModelArgs0)
    
    a4 <- CohortMethod::createCmAnalysis(analysisId = 4,
                                         description = "One-year outcome, matching with blanking period",
                                         getDbCohortMethodDataArgs = getDbCmDataArgs,
                                         createStudyPopArgs = OneYearOutcomeWithBlanking,
                                         createPs = TRUE,
                                         createPsArgs = createPsArgs1,
                                         matchOnPs = TRUE,
                                         matchOnPsArgs = MatchOnPsArgs,
                                         fitOutcomeModel = TRUE,
                                         fitOutcomeModelArgs = fitOutcomeModelArgs1)
    
    # a5 <- CohortMethod::createCmAnalysis(analysisId = 5,
    #                                      description = "One-year outcome only for observed, stratification",
    #                                      getDbCohortMethodDataArgs = getDbCmDataArgs,
    #                                      createStudyPopArgs = OneYearOutcomeWithMinTAR,
    #                                      createPs = TRUE,
    #                                      createPsArgs = createPsArgs1,
    #                                      #trimByPs = TRUE,
    #                                      #trimByPsArgs = trimByPsArgs,
    #                                      stratifyByPs = TRUE,
    #                                      stratifyByPsArgs = stratifyByPsArgs,
    #                                      fitOutcomeModel = TRUE,
    #                                      fitOutcomeModelArgs = fitOutcomeModelArgs1)
    #
    # a6 <- CohortMethod::createCmAnalysis(analysisId = 6,
    #                                      description = "One-year outcome only for observed, without matching",
    #                                      getDbCohortMethodDataArgs = getDbCmDataArgs,
    #                                      createStudyPopArgs = OneYearOutcomeWithMinTAR,
    #                                      createPs = FALSE,
    #                                      createPsArgs = NULL,
    #                                      #trimByPs = TRUE,
    #                                      #trimByPsArgs = trimByPsArgs,
    #                                      stratifyByPs = FALSE,
    #                                      stratifyByPsArgs = NULL,
    #                                      fitOutcomeModel = TRUE,
    #                                      fitOutcomeModelArgs = fitOutcomeModelArgs0)
    
    a7 <- CohortMethod::createCmAnalysis(analysisId = 7,
                                         description = "On-treatment, matching",
                                         getDbCohortMethodDataArgs = getDbCmDataArgs,
                                         createStudyPopArgs = OnTreatment,
                                         createPs = TRUE,
                                         createPsArgs = createPsArgs1,
                                         matchOnPs = TRUE,
                                         matchOnPsArgs = MatchOnPsArgs,
                                         fitOutcomeModel = TRUE,
                                         fitOutcomeModelArgs = fitOutcomeModelArgs1)
    
    a8 <- CohortMethod::createCmAnalysis(analysisId = 8,
                                         description = "On-treatment, stratification",
                                         getDbCohortMethodDataArgs = getDbCmDataArgs,
                                         createStudyPopArgs = OnTreatment,
                                         createPs = TRUE,
                                         createPsArgs = createPsArgs1,
                                         #trimByPs = TRUE,
                                         #trimByPsArgs = trimByPsArgs,
                                         stratifyByPs = TRUE,
                                         stratifyByPsArgs = stratifyByPsArgs,
                                         fitOutcomeModel = TRUE,
                                         fitOutcomeModelArgs = fitOutcomeModelArgs1)
    
    a9 <- CohortMethod::createCmAnalysis(analysisId = 9,
                                         description = "On-treatment, without matching",
                                         getDbCohortMethodDataArgs = getDbCmDataArgs,
                                         createStudyPopArgs = OnTreatment,
                                         createPs = FALSE,
                                         createPsArgs = NULL,
                                         #trimByPs = TRUE,
                                         #trimByPsArgs = trimByPsArgs,
                                         stratifyByPs = FALSE,
                                         stratifyByPsArgs = NULL,
                                         fitOutcomeModel = TRUE,
                                         fitOutcomeModelArgs = fitOutcomeModelArgs0)
    
    a10 <- CohortMethod::createCmAnalysis(analysisId = 10,
                                          description = "Five-year, matching",
                                          getDbCohortMethodDataArgs = getDbCmDataArgs,
                                          createStudyPopArgs = ITT,
                                          createPs = TRUE,
                                          createPsArgs = createPsArgs1,
                                          matchOnPs = TRUE,
                                          matchOnPsArgs = MatchOnPsArgs,
                                          fitOutcomeModel = TRUE,
                                          fitOutcomeModelArgs = fitOutcomeModelArgs1)
    
    a11 <- CohortMethod::createCmAnalysis(analysisId = 11,
                                          description = "Five-year, stratification",
                                          getDbCohortMethodDataArgs = getDbCmDataArgs,
                                          createStudyPopArgs = ITT,
                                          createPs = TRUE,
                                          createPsArgs = createPsArgs1,
                                          #trimByPs = TRUE,
                                          #trimByPsArgs = trimByPsArgs,
                                          stratifyByPs = TRUE,
                                          stratifyByPsArgs = stratifyByPsArgs,
                                          fitOutcomeModel = TRUE,
                                          fitOutcomeModelArgs = fitOutcomeModelArgs1)
    
    a12 <- CohortMethod::createCmAnalysis(analysisId = 12,
                                          description = "Five-year, without matching",
                                          getDbCohortMethodDataArgs = getDbCmDataArgs,
                                          createStudyPopArgs = ITT,
                                          createPs = FALSE,
                                          createPsArgs = NULL,
                                          #trimByPs = TRUE,
                                          #trimByPsArgs = trimByPsArgs,
                                          stratifyByPs = FALSE,
                                          stratifyByPsArgs = NULL,
                                          fitOutcomeModel = TRUE,
                                          fitOutcomeModelArgs = fitOutcomeModelArgs0)
    
    a13 <- CohortMethod::createCmAnalysis(analysisId = 13,
                                          description = "On-treatment, matching with blanking period",
                                          getDbCohortMethodDataArgs = getDbCmDataArgs,
                                          createStudyPopArgs = OnTreatmentWithBlanking,
                                          createPs = TRUE,
                                          createPsArgs = createPsArgs1,
                                          matchOnPs = TRUE,
                                          matchOnPsArgs = MatchOnPsArgs,
                                          fitOutcomeModel = TRUE,
                                          fitOutcomeModelArgs = fitOutcomeModelArgs1)
    
    a14 <- CohortMethod::createCmAnalysis(analysisId = 1142,
                                          description = "Five-year, matching with blanking period",
                                          getDbCohortMethodDataArgs = getDbCmDataArgs,
                                          createStudyPopArgs = ITTwithBlanking,
                                          createPs = TRUE,
                                          createPsArgs = createPsArgs1,
                                          matchOnPs = TRUE,
                                          matchOnPsArgs = MatchOnPsArgs,
                                          fitOutcomeModel = TRUE,
                                          fitOutcomeModelArgs = fitOutcomeModelArgs1)
    
    
    
    ##Interaction terms
    fitOutcomeModelArgsI1998 <- CohortMethod::createFitOutcomeModelArgs(useCovariates = FALSE,
                                                                        modelType = "cox",
                                                                        stratified = TRUE,
                                                                        prior = defaultPrior,
                                                                        interactionCovariateIds = 1998)
    
    fitOutcomeModelArgsI2998 <- CohortMethod::createFitOutcomeModelArgs(useCovariates = FALSE,
                                                                        modelType = "cox",
                                                                        stratified = TRUE,
                                                                        prior = defaultPrior,
                                                                        interactionCovariateIds = 2998)
    
    fitOutcomeModelArgsI3998 <- CohortMethod::createFitOutcomeModelArgs(useCovariates = FALSE,
                                                                        modelType = "cox",
                                                                        stratified = TRUE,
                                                                        prior = defaultPrior,
                                                                        interactionCovariateIds = 3998)
    
    fitOutcomeModelArgsI4998 <- CohortMethod::createFitOutcomeModelArgs(useCovariates = FALSE,
                                                                        modelType = "cox",
                                                                        stratified = TRUE,
                                                                        prior = defaultPrior,
                                                                        interactionCovariateIds = 4998)
    
    fitOutcomeModelArgsI5998 <- CohortMethod::createFitOutcomeModelArgs(useCovariates = FALSE,
                                                                        modelType = "cox",
                                                                        stratified = TRUE,
                                                                        prior = defaultPrior,
                                                                        interactionCovariateIds = 5998)
    
    fitOutcomeModelArgsI6998 <- CohortMethod::createFitOutcomeModelArgs(useCovariates = FALSE,
                                                                        modelType = "cox",
                                                                        stratified = TRUE,
                                                                        prior = defaultPrior,
                                                                        interactionCovariateIds = 6998)
    
    fitOutcomeModelArgsI9998 <- CohortMethod::createFitOutcomeModelArgs(useCovariates = FALSE,
                                                                        modelType = "cox",
                                                                        stratified = TRUE,
                                                                        prior = defaultPrior,
                                                                        interactionCovariateIds = subgroupCovariateIds)
    
    a19 <- CohortMethod::createCmAnalysis(analysisId = 19,
                                          description = "One-year outcome, matching, femal interaction",
                                          getDbCohortMethodDataArgs = getDbCmDataArgs,
                                          createStudyPopArgs = OneYearOutcome,
                                          createPs = TRUE,
                                          createPsArgs = createPsArgs1,
                                          matchOnPs = TRUE,
                                          matchOnPsArgs = MatchOnPsArgs,
                                          fitOutcomeModel = TRUE,
                                          fitOutcomeModelArgs = fitOutcomeModelArgsI1998)
    
    a29 <- CohortMethod::createCmAnalysis(analysisId = 29,
                                          description = "One-year outcome, matching, With 1 to 1 Matching, elderly interaction",
                                          getDbCohortMethodDataArgs = getDbCmDataArgs,
                                          createStudyPopArgs = OneYearOutcome,
                                          createPs = TRUE,
                                          createPsArgs = createPsArgs1,
                                          matchOnPs = TRUE,
                                          matchOnPsArgs = MatchOnPsArgs,
                                          fitOutcomeModel = TRUE,
                                          fitOutcomeModelArgs = fitOutcomeModelArgsI2998)
    
    a39 <- CohortMethod::createCmAnalysis(analysisId = 39,
                                          description = "One-year outcome, matching, black or aftrican race interaction",
                                          getDbCohortMethodDataArgs = getDbCmDataArgs,
                                          createStudyPopArgs = OneYearOutcome,
                                          createPs = TRUE,
                                          createPsArgs = createPsArgs1,
                                          matchOnPs = TRUE,
                                          matchOnPsArgs = MatchOnPsArgs,
                                          fitOutcomeModel = TRUE,
                                          fitOutcomeModelArgs = fitOutcomeModelArgsI3998)
    
    a49 <- CohortMethod::createCmAnalysis(analysisId = 49,
                                          description = "One-year outcome, matching, short-term MI interaction",
                                          getDbCohortMethodDataArgs = getDbCmDataArgs,
                                          createStudyPopArgs = OneYearOutcome,
                                          createPs = TRUE,
                                          createPsArgs = createPsArgs1,
                                          matchOnPs = TRUE,
                                          matchOnPsArgs = MatchOnPsArgs,
                                          fitOutcomeModel = TRUE,
                                          fitOutcomeModelArgs = fitOutcomeModelArgsI4998)
    
    a59 <- CohortMethod::createCmAnalysis(analysisId = 59,
                                          description = "One-year outcome, matching, concomitant PPI use interaction",
                                          getDbCohortMethodDataArgs = getDbCmDataArgs,
                                          createStudyPopArgs = OneYearOutcome,
                                          createPs = TRUE,
                                          createPsArgs = createPsArgs1,
                                          matchOnPs = TRUE,
                                          matchOnPsArgs = MatchOnPsArgs,
                                          fitOutcomeModel = TRUE,
                                          fitOutcomeModelArgs = fitOutcomeModelArgsI5998)
    
    a69 <- CohortMethod::createCmAnalysis(analysisId = 69,
                                          description = "One-year outcome, matching, maintenance aspirin dosage interaction",
                                          getDbCohortMethodDataArgs = getDbCmDataArgs,
                                          createStudyPopArgs = OneYearOutcome,
                                          createPs = TRUE,
                                          createPsArgs = createPsArgs1,
                                          matchOnPs = TRUE,
                                          matchOnPsArgs = MatchOnPsArgs,
                                          fitOutcomeModel = TRUE,
                                          fitOutcomeModelArgs = fitOutcomeModelArgsI6998)
    
    # a99 <- CohortMethod::createCmAnalysis(analysisId = 99,
    #                                       description = "Time to First Post Index Event within 1 year, With 1 to 1 Matching, all interaction",
    #                                       getDbCohortMethodDataArgs = getDbCmDataArgs,
    #                                       createStudyPopArgs = timeToFirstPostIndexEvent1Year,
    #                                       createPs = TRUE,
    #                                       createPsArgs = createPsArgs1,
    #                                       matchOnPs = TRUE,
    #                                       matchOnPsArgs = oneToOneMatchOnPsArgs,
    #                                       fitOutcomeModel = TRUE,
    #                                       fitOutcomeModelArgs = fitOutcomeModelArgsI6998)
    
    
    
    
    cmAnalysisList <- list(a1, a2, a3, a4, #a5, a6,
                           a7, a8, a9, a10, a11, a12, a13, a14, #a15, a16
                           a19, a29, a39, a49, a59, a69#, a99
    )
    #cmAnalysisList <- list(a1)
    
    CohortMethod::saveCmAnalysisList(cmAnalysisList, file.path(workFolder, "cmAnalysisList.json"))
    
}
