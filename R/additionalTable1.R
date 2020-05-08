# Copyright 2020 Observational Health Data Sciences and Informatics
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

# Generation additional table 1
#' @export
additionalResult <- function(connectionDetails = connectionDetails,
                             cdmDatabaseSchema = cdmDatabaseSchema,
                             cohortDatabaseSchema = cohortDatabaseSchema,
                             cohortTable = cohortTable,
                             oracleTempSchema = oracleTempSchema,
                             outputFolder = outputFolder,
                             databaseId = databaseId,
                             minCellCount = 10){
    start <- Sys.time()
    
    ####Cox assumption test####
    
    popList <- list.files(file.path(outputFolder,"cmOutput"), pattern = "^StratPop_l1_s1_p1.*s1_o.*.rds")
    for(i in 1:length(popList)){
        if (i==1) {coxTable <- data.frame()}
        
        popName <- popList[i]
        pop <- readRDS(file.path(outputFolder,"cmOutput",popName))
        exportName <- gsub("\\.rds","", gsub("StratPop_l1_s1_p1_","",popName))
        if(sum(pop$outcomeCount)!=0){
            try({
                fit <- survival::coxph(survival::Surv(survivalTime, outcomeCount)~treatment, data = pop)
                coxTest <- survival::cox.zph(fit)
                
                coxTable1 <- as.data.frame(coxTest$table)
                coxTable1$name <- exportName
                
                coxTable <- rbind(coxTable,coxTable1)
                
                #plotZph <- survminer::ggcoxzph(coxTest)
                
                #if(min(coxTable1$p, na.rm = T)<0.05) ggplot2::ggsave(file.path(outputFolder,"export",paste0(exportName,"_graph.pdf" )),print(plotZph))
            })
        }
        
        if (i == length(popList)) write.csv(coxTable, file.path(outputFolder,"export","cox_assumption.csv" ))
    }
    
    ####Baseline characteristics####
    
    # popList <- list.files(file.path(outputFolder,"cmOutput"), pattern = "StratPop_l1_s1_p1_t874_c929_s1_o1240.rds")
    # 
    # connectionDetails$schema <- cohortDatabaseSchema
    # connection <- DatabaseConnector::connect(connectionDetails)
    # 
    # for(i in 1:length(popList)){
    #     if (i==1){
    #         targetIds <- c()
    #         comparatorIds <- c()
    #     } 
    #     popName <- popList[i]
    #     
    #     exportName <- gsub("\\.rds","", gsub("StratPop_l1_s1_p1_","",popName))
    #     targetId <- as.numeric(gsub("_c.*.rds","",gsub("StratPop.*_t","",popName)))
    #     comparatorId <- as.numeric(gsub("_s.*.rds","",gsub("StratPop.*_c","",popName)))
    #     outcomeId <- as.numeric(gsub("t.*_s1_o","",exportName))
    #     pop <- readRDS(file.path(outputFolder,"cmOutput", popName))
    #     
    #     pop$cohortId <- NA
    #     pop$cohortId[pop$treatment==1] <- as.numeric(sprintf("%d%04d",targetId,outcomeId))
    #     pop$cohortId[pop$treatment==0] <- as.numeric(sprintf("%d%04d",comparatorId,outcomeId))
    #     
    #     popCohortTable <- data.frame(cohort_definition_id = pop$cohortId,
    #                               subject_id = pop$subjectId,
    #                               cohort_start_date = pop$cohortStartDate,
    #                               cohort_end_date = pop$cohortStartDate)
    #     
    #     
    #     DatabaseConnector::insertTable(connection = connection,
    #                                    tableName = cohortTable,
    #                                    data = popCohortTable,
    #                                    dropTableIfExists = F,
    #                                    createTable = F)
    #     targetIds <- c(targetId, as.numeric(sprintf("%d%04d",targetId,outcomeId)))
    #     comparatorIds <- c(comparatorId, as.numeric(sprintf("%d%04d",comparatorId,outcomeId)))
    #     }
    # targetName = "ticagrelor"
    # comparatorName = "clopidogrel"
    
    
    # exportFolder <- file.path(outputFolder, "export")
    # #Defining concept IDs
    # conditionGroupConceptIds <- 
    #     c(320128,201254,201826,432867,317309,319835,4232697,192359) #102
    # shorTermAcs <- c(315296, 444406, 438170, 434376, 312327) #104
    # 
    # drugGroupConceptIds <- c(19047423,19017067,1310149,43013024,36428260,40228152,21601784,21601823,21601665,21601744,21601461,21601855,21600095,21600713,21600744) #412
    # deviceConceptIds <- c(45772824) #604
    # #measConceptIds <- c(3038553000,3004249000,3012888000,3000963713,3004501840,3008631840,3048150842) #708
    # 
    
    # for( i in 1:length(targetIds)){
    #     targetCohortId <- targetIds[i]
    #     comparatorCohortId <- comparatorIds[i]
    #     targetCohortName = "ticagrelor"
    #     comparatorCohortName= "clopidogrel"
    #     
    #     for(endDay in c(0)){
    #         tableSpecification <- setTableSpecification(useDemographicsGender = T,
    #                                                     useDemographicsAge = F,
    #                                                     useDemographicsAgeGroup = T,
    #                                                     useDemographicsRace = T,
    #                                                     useDemographicsEthnicity = F,
    #                                                     useDemographicsIndexYear = T,
    #                                                     useDemographicsIndexMonth = F,
    #                                                     useDemographicsPriorObservationTime = FALSE,
    #                                                     useDemographicsPostObservationTime = FALSE,
    #                                                     useDemographicsTimeInCohort = FALSE,
    #                                                     useDemographicsIndexYearMonth = F,
    #                                                     conceptIdsConditionOccurrenceAnyTimePrior = c(),
    #                                                     conceptIdsConditionOccurrenceLongTerm = conditionGroupConceptIds,
    #                                                     conceptIdsConditionOccurrenceMediumTerm = c(),
    #                                                     conceptIdsConditionOccurrenceShortTerm = shorTermAcs,
    #                                                     conceptIdsConditionOccurrencePrimaryInpatientAnyTimePrior = c(),
    #                                                     conceptIdsConditionOccurrencePrimaryInpatientLongTerm = c(),
    #                                                     conceptIdsConditionOccurrencePrimaryInpatientMediumTerm = c(),
    #                                                     conceptIdsConditionOccurrencePrimaryInpatientShortTerm = c(),
    #                                                     conceptIdsConditionEraAnyTimePrior = c(),
    #                                                     conceptIdsConditionEraLongTerm = c(),
    #                                                     conceptIdsConditionEraMediumTerm = c(),
    #                                                     conceptIdsConditionEraShortTerm = c(),
    #                                                     conceptIdsConditionEraOverlapping = c(),
    #                                                     conceptIdsConditionEraStartLongTerm = c(),
    #                                                     conceptIdsConditionEraStartMediumTerm = c(),
    #                                                     conceptIdsConditionEraStartShortTerm = c(),
    #                                                     conceptIdsConditionGroupEraAnyTimePrior = c(),
    #                                                     conceptIdsConditionGroupEraLongTerm = conditionGroupConceptIds,
    #                                                     conceptIdsConditionGroupEraMediumTerm = c(),
    #                                                     conceptIdsConditionGroupEraShortTerm = shorTermAcs,
    #                                                     conceptIdsConditionGroupEraOverlapping = c(),
    #                                                     conceptIdsConditionGroupEraStartLongTerm = c(),
    #                                                     conceptIdsConditionGroupEraStartMediumTerm = c(),
    #                                                     conceptIdsConditionGroupEraStartShortTerm = c(),
    #                                                     conceptIdsDrugExposureAnyTimePrior = c(),
    #                                                     conceptIdsDrugExposureLongTerm = c(),
    #                                                     conceptIdsDrugExposureMediumTerm = c(),
    #                                                     conceptIdsDrugExposureShortTerm = c(),
    #                                                     conceptIdsDrugEraAnyTimePrior = c(),
    #                                                     conceptIdsDrugEraLongTerm = c(),
    #                                                     conceptIdsDrugEraMediumTerm = c(),
    #                                                     conceptIdsDrugEraShortTerm = drugGroupConceptIds,
    #                                                     conceptIdsDrugEraOverlapping = c(),
    #                                                     conceptIdsDrugEraStartLongTerm = c(),
    #                                                     conceptIdsDrugEraStartMediumTerm = c(),
    #                                                     conceptIdsDrugEraStartShortTerm = c(),
    #                                                     conceptIdsDrugGroupEraAnyTimePrior = c(),
    #                                                     conceptIdsDrugGroupEraLongTerm = c(),
    #                                                     conceptIdsDrugGroupEraMediumTerm = c(),
    #                                                     conceptIdsDrugGroupEraShortTerm = drugGroupConceptIds,
    #                                                     conceptIdsDrugGroupEraOverlapping = c(),
    #                                                     conceptIdsDrugGroupEraStartLongTerm = c(),
    #                                                     conceptIdsDrugGroupEraStartMediumTerm = c(),
    #                                                     conceptIdsDrugGroupEraStartShortTerm = c(),
    #                                                     conceptIdsProcedureOccurrenceAnyTimePrior = c(),
    #                                                     conceptIdsProcedureOccurrenceLongTerm = c(),
    #                                                     conceptIdsProcedureOccurrenceMediumTerm = c(),
    #                                                     conceptIdsProcedureOccurrenceShortTerm = c(),
    #                                                     conceptIdsDeviceExposureAnyTimePrior = c(),
    #                                                     conceptIdsDeviceExposureLongTerm = c(),
    #                                                     conceptIdsDeviceExposureMediumTerm = c(),
    #                                                     conceptIdsDeviceExposureShortTerm = deviceConceptIds,
    #                                                     conceptIdsMeasurementAnyTimePrior = c(),
    #                                                     conceptIdsMeasurementLongTerm = c(),
    #                                                     conceptIdsMeasurementMediumTerm = c(),
    #                                                     conceptIdsMeasurementShortTerm = c(),#measConceptIds,
    #                                                     conceptIdsMeasurementValueAnyTimePrior = c(),
    #                                                     conceptIdsMeasurementValueLongTerm = c(),
    #                                                     conceptIdsMeasurementValueMediumTerm = c(),
    #                                                     conceptIdsMeasurementValueShortTerm = c(), #measConceptIds,
    #                                                     conceptIdsMeasurementRangeGroupAnyTimePrior = c(),
    #                                                     conceptIdsMeasurementRangeGroupLongTerm = c(),
    #                                                     conceptIdsMeasurementRangeGroupMediumTerm = c(),
    #                                                     conceptIdsMeasurementRangeGroupShortTerm = c(),
    #                                                     conceptIdsObservationAnyTimePrior = c(),
    #                                                     conceptIdsObservationLongTerm = c(),
    #                                                     conceptIdsObservationMediumTerm = c(),
    #                                                     conceptIdsObservationShortTerm = c(),
    #                                                     useCharlsonIndex = F,
    #                                                     useDcsi = FALSE,
    #                                                     useChads2 = FALSE,
    #                                                     useChads2Vasc = FALSE,
    #                                                     useHfrs = F,#T,
    #                                                     useDistinctConditionCountLongTerm = FALSE,
    #                                                     useDistinctConditionCountMediumTerm = FALSE,
    #                                                     useDistinctConditionCountShortTerm = FALSE,
    #                                                     useDistinctIngredientCountLongTerm = FALSE,
    #                                                     useDistinctIngredientCountMediumTerm = FALSE,
    #                                                     useDistinctIngredientCountShortTerm = FALSE,
    #                                                     useDistinctProcedureCountLongTerm = FALSE,
    #                                                     useDistinctProcedureCountMediumTerm = FALSE,
    #                                                     useDistinctProcedureCountShortTerm = FALSE,
    #                                                     useDistinctMeasurementCountLongTerm = FALSE,
    #                                                     useDistinctMeasurementCountMediumTerm = FALSE,
    #                                                     useDistinctMeasurementCountShortTerm = FALSE,
    #                                                     useDistinctObservationCountLongTerm = FALSE,
    #                                                     useDistinctObservationCountMediumTerm = FALSE,
    #                                                     useDistinctObservationCountShortTerm = FALSE,
    #                                                     useVisitCountLongTerm = F,
    #                                                     useVisitCountMediumTerm = F,
    #                                                     useVisitCountShortTerm = F,
    #                                                     useVisitConceptCountLongTerm = F,
    #                                                     useVisitConceptCountMediumTerm = F,
    #                                                     useVisitConceptCountShortTerm = F,
    #                                                     longTermStartDays = -365,
    #                                                     mediumTermStartDays = -30,
    #                                                     shortTermStartDays = -7,
    #                                                     endDays = endDay) #you can try diverse time settings
    #         tryCatch({
    #             comparativeCharacterization(connectionDetails = connectionDetails,
    #                                         cdmDatabaseSchema = cdmDatabaseSchema,
    #                                         cohortDatabaseSchema = cohortDatabaseSchema,
    #                                         cohortTable = cohortTable,
    #                                         oracleTempSchema = oracleTempSchema,
    #                                         outputFolder = outputFolder,
    #                                         minCellCount = minCellCount,
    #                                         targetCohortId = targetCohortId,
    #                                         comparatorCohortId = comparatorCohortId,#7,
    #                                         outcomeCohortIds = NULL,
    #                                         tableSpecification = tableSpecification,
    #                                         sampleSize = NULL,
    #                                         output = "one column",
    #                                         percentDigits = 1, 
    #                                         valueDigits = 1,
    #                                         stdDiffDigits = 2,
    #                                         studyPopulationSetting = NULL,
    #                                         fileName = file.path(exportFolder, sprintf("base_char_t%s_c%s.csv",targetCohortId, comparatorCohortId)))
    #         },
    #         error = function(e) {
    #             ParallelLogger::logTrace(paste0(sprintf("Generating table 1 for cohort ID %s_%s is failed. The error message:", targetCohortId, comparatorCohortId), e))},
    #         finally = {
    #             ParallelLogger::logTrace('Done.')})
    #         
    #     }
    #     
    # }
    # Add all to zip file -------------------------------------------------------------------------------
    # ParallelLogger::logInfo("Adding results to zip file")
    # zipName <- file.path(exportFolder, paste0("Results_", databaseId, ".zip"))
    # files <- list.files(exportFolder, pattern = ".*\\.csv$")
    # oldWd <- setwd(exportFolder)
    # on.exit(setwd(oldWd), add = TRUE)
    # DatabaseConnector::createZipFile(zipFile = zipName, files = files)
    # ParallelLogger::logInfo("Results are ready for sharing at:", zipName)
    
    delta <- Sys.time() - start
    ParallelLogger::logInfo(paste("Generating additional result took",
                                  signif(delta, 3),
                                  attr(delta, "units")))
    
    
}
