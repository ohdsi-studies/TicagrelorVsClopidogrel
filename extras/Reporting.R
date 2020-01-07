#rm(list=ls())
library(ggplot2)
library(ggsci)
library(dplyr)

studyFolder <- "/Users/chan/data/ticagrelor/v1.3" #where the original results are stored
resultFolder <- "/Users/chan/data/ticagrelor/output" #where the figures/tables for the paper will be stored

source(file.path("inst/shiny/EvidenceExplorer","DataPulls.R"))
source(file.path("inst/shiny/EvidenceExplorer","PlotsAndTables.R"))
source(file.path("extra/","FuncitonsForReporting.R"))

targetColor <- rgb(255/255,99/255,71/255, alpha = 0.8)
comparatorColor <- rgb(30/255,144/255,255/255, alpha = 0.8)
targetColorFill <- rgb(255/255,99/255,71/255, alpha = 0.3)
comparatorColorFill <- rgb(30/255,144/255,255/255, alpha = 0.3)

databaseIds <- c("OptumPanTher","IQVIA - Hospital","HIRA")
DatabaseIdsUs<-c("OptumPanTher","IQVIA - Hospital")
databaseIdAndMeta<-c(databaseIds, "Meta-analysis")

#Settings for IDs

targetName = "Ticagrelor"
comparatorName = "Clopidogrel"
targetId = 874
comparatorId = 929
primaryAnalysisId = 1
outcomeId = 1240
journalTheme = "jama"

####Prepare the results####
# for(databaseId in databaseIds){
#     resultsZipFile <- file.path(outputFolder, #"export", 
#                                 paste0("Results", databaseId, ".zip"))
#     dataFolder <- file.path(outputFolder, "shinyData")
#     TicagrelorVsClopidogrel::prepareForEvidenceExplorer(resultsZipFile = resultsZipFile, dataFolder = dataFolder)
# }

##Meta-analysis
# shinyFolder <- "/Users/chan/data/ticagrelor/v1.3/shinyData"
# maExportFolder <- "/users/chan/data/ticagrelor/v1.3/MetaAnalysis"
# 
# TicagrelorVsClopidogrel::doMetaAnalysis(shinyFolder = "/Users/chan/data/ticagrelor/v1.3/shinyData",
#                                         maExportFolder = maExportFolder,
#                                         maxCores = 4,
#                                         #interactions=FALSE,
#                                         positiveControlOutcome = FALSE)

####Adding calibrated confidential intervals and re-write RDS####
#tcs<-unique(tcos[,c("targetId","comparatorId")])

# for(databaseId in databaseIds){
#     singleCohortMethodResult<-readRDS(file.path(studyFolder,"shinyData",sprintf("cohort_method_result_%s.rds",databaseId)))
#     colnames(singleCohortMethodResult)<-SqlRender::snakeCaseToCamelCase(colnames(singleCohortMethodResult))
#     tcos <- unique(singleCohortMethodResult[, c("targetId", "comparatorId", "outcomeId")])
#     tcos <- tcos[tcos$outcomeId %in% outcomeOfInterest$outcomeId, ]
#     tcs<-unique(tcos[,c("targetId","comparatorId")])
# 
#     for (analysisId in unique(cohortMethodAnalysis$analysisId)){
#         for (i in seq(nrow(tcs))){
#             tc<- tcs[i,]
#             index<-singleCohortMethodResult$targetId==tc$targetId&
#                 singleCohortMethodResult$comparatorId==tc$comparatorId&
#                 singleCohortMethodResult$analysisId==analysisId&
#                 singleCohortMethodResult$databaseId==databaseId&
#                 !is.na(singleCohortMethodResult$logRr) &
#                 !is.na(singleCohortMethodResult$seLogRr)
# 
#             if(sum(index, na.rm=T)==0) next
#             negativeData<-singleCohortMethodResult[index &
#                                                  singleCohortMethodResult$outcomeId %in% unique(negativeControlOutcome$outcomeId),]
#             null<-EmpiricalCalibration::fitNull(negativeData$logRr,
#                                                 negativeData$seLogRr)
# 
#             model<-EmpiricalCalibration::convertNullToErrorModel(null)
# 
#             calibratedCi<-EmpiricalCalibration::calibrateConfidenceInterval(logRr=singleCohortMethodResult[index,]$logRr,
#                                                                             seLogRr=singleCohortMethodResult[index,]$seLogRr,
#                                                                             model=model,
#                                                                             ciWidth = 0.95)
# 
#             singleCohortMethodResult[index,]$calibratedLogRr<-calibratedCi$logRr
#             singleCohortMethodResult[index,]$calibratedSeLogRr<-calibratedCi$seLogRr
#             singleCohortMethodResult[index,]$calibratedCi95Lb<-exp(calibratedCi$logLb95Rr)
#             singleCohortMethodResult[index,]$calibratedCi95Ub<-exp(calibratedCi$logUb95Rr)
#             singleCohortMethodResult[index,]$calibratedRr<-exp(calibratedCi$logRr)
# 
#         }
# 
# 
#     }
#     colnames(singleCohortMethodResult)<-SqlRender::camelCaseToSnakeCase(colnames(singleCohortMethodResult))
#     saveRDS(singleCohortMethodResult,file.path(studyFolder,"shinyData",sprintf("cohort_method_result_%s.rds",databaseId)))
# }

#load files into the environment
for (i in seq(length(databaseIdAndMeta))){
    databaseId = databaseIdAndMeta[i]
    dataFolder <- file.path(studyFolder,"shinyData")
    files <- list.files(dataFolder, pattern = sprintf("%s.rds", databaseId))
    files<-files[!grepl("tNA_cNA",files)]
    if (i==1){
        connection <- NULL
        positiveControlOutcome <- NULL
        
        splittableTables <- c("covariate_balance")#c("covariate_balance", "preference_score_dist", "kaplan_meier_dist")
        
        # Remove data already in global environment:
        tableNames <- gsub("(_t[0-9]+_c[0-9]+)|(_)[^_]*\\.rds", "", files) 
        camelCaseNames <- SqlRender::snakeCaseToCamelCase(tableNames)
        camelCaseNames <- unique(camelCaseNames)
        camelCaseNames <- camelCaseNames[!(camelCaseNames %in% SqlRender::snakeCaseToCamelCase(splittableTables))]
        rm(list = camelCaseNames)
        
        #load files
        lapply(files, loadFile)
    }else {
        lapply(files, loadFile)
    }
}

tcos <- unique(cohortMethodResult[, c("targetId", "comparatorId", "outcomeId")])

#####################
####Main Results#####
#####################

####Cohort per Years####
p <- plottingTrend(data = cohortCountPerYear,
                   databaseIds = databaseIds,
                   targetId = 874, #ticagrelor
                   comparatorId = 929, #clopidogrel
                   targetName = "ticagrelor",
                   theme = "jama")
if(!file.exists(file.path(resultFolder,"trend"))) dir.create(file.path(resultFolder,"trend"))
ggplot2::ggsave(file.path(resultFolder,"trend","trend.eps"), p, device = "eps", width = 12, height = 10, units = "cm", dpi = 320)
ggplot2::ggsave(file.path(resultFolder,"trend","trend.pdf"), p, device = "pdf", width = 12, height = 10, units = "cm", dpi = 320)
ggplot2::ggsave(file.path(resultFolder,"trend","trend.tiff"), p, device = "tiff", width = 12, height = 10, units = "cm", dpi = 320)

####Drug Adherence####

####baseline characteristics####
#prepare Table 1
#balanceAgg<-data.frame()
#databaseId<-databaseIds[1]

##Check whether there are balance more than 0.1

# balanceFile <-  list.files(file.path(studyFolder,"shinyData"),pattern = "covariate_balance", all.files=T,full.names = T)[1]
# for (balanceFile in list.files(file.path(studyFolder,"shinyData"),pattern = "covariate_balance", all.files=T,full.names = T)){
#     individualBalance<-readRDS(balanceFile)
#     individualBalance <- individualBalance[individualBalance$covariateId!=6998,]
#     
#     unbalancedData <-individualBalance[abs(individualBalance$std_diff_after)>=0.1,]
#     if(nrow(unbalancedData)) print(sprintf("%s has unbalanced data",balanceFile))
# }
# None of them has balance more than 0.1

for(databaseId in databaseIds){
    # balance<-covariateBalance [covariateBalance$databaseId ==databaseId&
    #                                covariateBalance$targetId==targetId &
    #                                covariateBalance$comparatorId==comparatorId &
    #                                covariateBalance$outcomeId==outcomeId&
    #                                covariateBalance$analysisId==primaryAnalysisId
    #                            ,
    #                            ]
    balance<-getBalance(databaseId,
                        studyFolder,
                        targetId,
                        comparatorId,
                        primaryAnalysisId,
                        outcomeId)
    
    ##Table 1
    Table1 <- prepareTable1(balance,
                            beforeLabel = "Before matching",
                            afterLabel = "After matching",
                            targetLabel = targetName,
                            comparatorLabel = comparatorName,
                            percentDigits = 1,
                            stdDiffDigits = 2,
                            output = "latex",
                            pathToCsv = file.path("inst/shiny/EvidenceExplorer","Table1Specs.csv"))
    Table1[,1]<-as.character(Table1[,1])
    Table1[,1]<-gsub(" \\(Unknown unit\\)","",Table1[,1])
    
    Table1<-labFormmating(Table1,percentDigits=1)
    if(!file.exists(file.path(resultFolder,"Table1"))) dir.create(file.path(resultFolder,"Table1"))
    
    write.csv(Table1,file.path(resultFolder, "Table1",sprintf("Table1_%s_t%d_c%d_o%d_a%d.csv",
                                                              databaseId,
                                                              targetId,
                                                              comparatorId,
                                                              outcomeId,
                                                              primaryAnalysisId
    )))
    
    ##Remove covariates of 'Maintainence Aspirin >= 300mg'
    balance <- balance[balance$covariateId!=6998,]
    
    #covariate balance scatter plot
    balancePlot<-plotCovariateBalanceScatterPlot(balance, beforeLabel = "Before matching", afterLabel = "After matching",
                                                 limits= c(0,0.4))
    #balancePlot<-CohortMethod::plotCovariateBalanceScatterPlot(balance, title = databaseId, showCovariateCountLabel = TRUE, showMaxLabel = TRUE)
    #add number of coaviates in the plot
    balancePlot<-balancePlot+ annotate("label", label = sprintf("Number of covariates: %s",
                                                                format(nrow(balance), big.mark=",", scientific=FALSE)), 
                                       x = -Inf, y = Inf, hjust=-0.2,vjust=2, color = "black")
    balancePlot+ggtitle(databaseId)
    
    
    assign(paste0("balancePlot","_",gsub("-","_",gsub(" ","",databaseId))), balancePlot, envir = .GlobalEnv)
    if(!file.exists(file.path(resultFolder,"balance_scatter_plot"))) dir.create(file.path(resultFolder,"balance_scatter_plot"))
    ggplot2::ggsave(file.path(resultFolder,"balance_scatter_plot",sprintf("balance_scatter_plot_%s_t%d_c%d_o%d_a%d.eps",
                                                                          databaseId,
                                                                          targetId,
                                                                          comparatorId,
                                                                          outcomeId,
                                                                          primaryAnalysisId)), 
                    balancePlot, device = "eps", width = 10, height = 10, units = "cm", dpi = 320)
    ggplot2::ggsave(file.path(resultFolder,"balance_scatter_plot",sprintf("balance_scatter_plot_%s_t%d_c%d_o%d_a%d.pdf",
                                                                          databaseId,
                                                                          targetId,
                                                                          comparatorId,
                                                                          outcomeId,
                                                                          primaryAnalysisId)), 
                    balancePlot, device = "pdf", width = 10, height = 10, units = "cm", dpi = 320)
    ggplot2::ggsave(file.path(resultFolder,"balance_scatter_plot",sprintf("balance_scatter_plot_%s_t%d_c%d_o%d_a%d.tiff",
                                                                          databaseId,
                                                                          targetId,
                                                                          comparatorId,
                                                                          outcomeId,
                                                                          primaryAnalysisId)), 
                    balancePlot, device = "tiff", width = 10, height = 10, units = "cm", dpi = 320)
    
    #balanceAgg <- rbind(balanceAgg,balance)
    
    ##
    # ggplot2::ggplot(balanceAgg, ggplot2::aes(x = absBeforeMatchingStdDiff, y = absAfterMatchingStdDiff)) +
    #     ggplot2::geom_point(color = rgb(0, 0, 0.8, alpha = 0.3), shape = 16, size = 2) +
    #     ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    #     ggplot2::geom_hline(yintercept = 0) +
    #     ggplot2::geom_vline(xintercept = 0) +
    #     ggplot2::scale_x_continuous(beforeLabel, limits = limits) +
    #     ggplot2::scale_y_continuous(afterLabel, limits = limits) +
    #     ggplot2::theme(fun.data=length, text = theme) + 
    #     #stat_summary(length,geom="label")+
    #     # annotate("label", label = sprintf("Number of covariates: %s",
    #     #                                   format(length(x), big.mark=",", scientific=FALSE)), 
    #     #          x = 0.05, y = 0.2, color = "black") +
    #     facet_wrap( facets=~databaseId, scale="free_x")
}
#gridExtra::grid.arrange(balancePlot_HIRA,balancePlot_IQVIA_Hospital,balancePlot_OptumPanther,ncol =3)

####Survival curve####
#Survival curve
for (databaseId in databaseIds){
    for (outcomeId in unique(outcomeOfInterest$outcomeId) ){
        outcomeName = unique(outcomeOfInterest$outcomeName[outcomeOfInterest$outcomeId==outcomeId])
        
        kaplanMeier = kaplanMeierDist[kaplanMeierDist$databaseId==databaseId&
                                          kaplanMeierDist$analysisId==analysisId&
                                          kaplanMeierDist$outcomeId==outcomeId&
                                          kaplanMeierDist$targetId == targetId&
                                          kaplanMeierDist$comparatorId == comparatorId
                                      ,]
        kpResult <- cohortMethodResult[cohortMethodResult$databaseId==databaseId&
                                           cohortMethodResult$analysisId==analysisId&
                                           cohortMethodResult$outcomeId==outcomeId&
                                           cohortMethodResult$targetId == targetId&
                                           cohortMethodResult$comparatorId == comparatorId
                                       ,]
        
        kaplanMeier$targetSurvival <- 1-kaplanMeier$targetSurvival
        kaplanMeier$targetSurvivalLb <-1-kaplanMeier$targetSurvivalLb
        kaplanMeier$targetSurvivalUb <-1-kaplanMeier$targetSurvivalUb
        kaplanMeier$comparatorSurvival <-1-kaplanMeier$comparatorSurvival
        kaplanMeier$comparatorSurvivalLb <-1-kaplanMeier$comparatorSurvivalLb
        kaplanMeier$comparatorSurvivalUb <-1-kaplanMeier$comparatorSurvivalUb
        if(is.na(unique(kpResult$p))) next
        if(length(unique(kpResult$p))>1) next
        
        if(unique(kpResult$p)<0.001){
            pValue =  sprintf("italic(P) < 0.001")
            if (journalTheme=="jama") pValue = sprintf("italic(P)<.001")
            pValue = sprintf("italic(P)<%s",".001")
        }else {
            #if (journalTheme=="jama") pNum <- sub("^(-?)0.", "\\1.", sprintf("%.3f",unique(kpResult$p))) 
            pValue =  sprintf("italic(P)==%#.3f",
                              unique(kpResult$p))
            
        }
        p<-plotKaplanMeier(kaplanMeier, 
                           targetName, 
                           comparatorName,
                           ylims = c(0,round(max(kaplanMeier$comparatorSurvivalUb,
                                                 kaplanMeier$targetSurvivalUb),2)+0.02),
                           xBreaks = NULL,#c(0,100,200,300),
                           targetColor = targetColor,
                           comparatorColor = comparatorColor,
                           targetColorFill = targetColorFill,
                           comparatorColorFill = comparatorColorFill,
                           pValue = pValue,
                           title = paste0(databaseId,"_",outcomeName))
        
        if(!file.exists(file.path(resultFolder,"kmplot"))) dir.create(file.path(resultFolder,"kmplot"))
        ggplot2::ggsave(file.path(resultFolder,"kmplot",sprintf("km_plot_%s_t%d_c%d_o%d_a%d.eps",
                                                                databaseId,
                                                                targetId,
                                                                comparatorId,
                                                                outcomeId,
                                                                analysisId)), p, device = "eps", width = 16, height = 12, units = "cm", dpi = 400)
        ggplot2::ggsave(file.path(resultFolder,"kmplot",sprintf("km_plot_%s_t%d_c%d_o%d_a%d.pdf",
                                                                databaseId,
                                                                targetId,
                                                                comparatorId,
                                                                outcomeId,
                                                                analysisId)), p, device = "pdf", width = 16, height = 12, units = "cm", dpi = 400)
        ggplot2::ggsave(file.path(resultFolder,"kmplot",sprintf("km_plot_%s_t%d_c%d_o%d_a%d.tiff",
                                                                databaseId,
                                                                targetId,
                                                                comparatorId,
                                                                outcomeId,
                                                                analysisId)), p, device = "tiff", width = 16, height = 12, units = "cm", dpi = 400)
    }
}

###Generate overall result and result of interest###
write.csv(cohortMethodResult,file.path(resultFolder,"overallResult.csv"))
resultOfInterest = cohortMethodResult[cohortMethodResult$outcomeId%in%unique(outcomeOfInterest$outcomeId),]
write.csv(resultOfInterest,file.path(resultFolder,"resultOfInterest.csv"))

####Plotting for Meta-analysis####
for(analysisId in unique(cohortMethodAnalysis$analysisId)){
    analysisName = unique(cohortMethodAnalysis$description[cohortMethodAnalysis$analysisId==analysisId])
    
    if(!dir.exists(file.path(resultFolder,"meta"))) dir.create(file.path(resultFolder,"meta"))
    if(!dir.exists(file.path(resultFolder,"limited_meta"))) dir.create(file.path(resultFolder,"limited_meta"))
    if(!dir.exists(file.path(resultFolder,"meta_cal"))) dir.create(file.path(resultFolder,"meta_cal"))
    
    if(!dir.exists(file.path(resultFolder,"meta",analysisName))) dir.create(file.path(resultFolder,"meta",analysisName))
    if(!dir.exists(file.path(resultFolder,"limited_meta",analysisName))) dir.create(file.path(resultFolder,"limited_meta",analysisName))
    if(!dir.exists(file.path(resultFolder,"meta_cal",analysisName))) dir.create(file.path(resultFolder,"meta_cal",analysisName))
    
    for(targetId in unique(tcos$targetId)){
        comparatorId <- cohortMethodResult$comparatorId[cohortMethodResult$targetId==targetId][1]
        for (outcomeId in unique(outcomeOfInterest$outcomeId) ){
            outcomeName = unique(outcomeOfInterest$outcomeName[outcomeOfInterest$outcomeId==outcomeId])
            
            resultOfInterest = cohortMethodResult[cohortMethodResult$targetId==targetId  & 
                                                      #cohortMethodResult$comparatorId==comparatorId& 
                                                      cohortMethodResult$outcomeId==outcomeId& 
                                                      cohortMethodResult$analysisId==analysisId&
                                                      cohortMethodResult$databaseId%in%databaseIds,]
            
            
            resultOfInterest<-unique(resultOfInterest)
            #if there is na in logRr of database, it should be removed
            resultOfInterest<-resultOfInterest[!is.na(resultOfInterest$logRr),]
            
            metaResult<-doMeta(data=resultOfInterest,
                               targetId = targetId,
                               comparatorId = comparatorId,
                               outcomeId = outcomeId,
                               analysisId = analysisId,
                               targetName = capitalize(targetName),
                               comparatorName = capitalize(comparatorName),
                               outcomeName = outcomeName,
                               calibration=F)
            
            tiff(file.path(resultFolder,"meta",analysisName,sprintf("meta_t%d_c%d_a%d_o%d_%s.tiff",targetId,comparatorId,analysisId,outcomeId,outcomeName) ),
                 width = 900*5,height = 720*5,
                 res = 500)
            forestPlotGenerator(metaResult)
            dev.off()
            
            tiff(file.path(resultFolder,"limited_meta",analysisName,sprintf("meta_limit_t%d_c%d_a%d_o%d_%s.tiff",targetId,comparatorId,analysisId,outcomeId,outcomeName) ),
                 width = 900*3,height = 720*3,
                 res = 300)
            forestPlotGenerator(metaResult, limited = T)
            dev.off()
            
            metaResultCalibrated<-doMeta(data=resultOfInterest,
                                         targetId = targetId,
                                         comparatorId = comparatorId,
                                         outcomeId = outcomeId,
                                         analysisId = analysisId,
                                         targetName = capitalize(targetName),
                                         comparatorName = capitalize(comparatorName),
                                         outcomeName = outcomeName,
                                         calibration=T)
            tiff(file.path(resultFolder,"meta_cal",analysisName,sprintf("meta_t%d_c%d_a%d_o%d_%s.tiff",targetId,comparatorId,analysisId,outcomeId,outcomeName) ),
                 width = 900*5,height = 720*5,
                 res = 500)
            forestPlotGenerator(metaResultCalibrated)
            dev.off()
            
            # tiff(file.path(resultFolder,"limited_meta_cal",analysisName,sprintf("meta_limit_t%d_c%d_a%d_o%d_%s.tiff",targetId,comparatorId,analysisId,outcomeId,outcomeName) ),
            #      width = 900*3,height = 720*3,
            #      res = 300)
            # forestPlotGenerator(metaResultCalibrated, limited = T)
            # dev.off()
        }
        
    }
    
}

##Plotting for meta-analysis for only US databases
for(analysisId in unique(cohortMethodAnalysis$analysisId)){
    analysisName = unique(cohortMethodAnalysis$description[cohortMethodAnalysis$analysisId==analysisId])
    
    if(!dir.exists(file.path(resultFolder,"meta_us"))) dir.create(file.path(resultFolder,"meta_us"))
    
    if(!dir.exists(file.path(resultFolder,"meta_us", analysisName))) dir.create(file.path(resultFolder,"meta_us", analysisName))
    
    for(targetId in unique(tcos$targetId)){
        comparatorId <- cohortMethodResult$comparatorId[cohortMethodResult$targetId==targetId][1]
        
        for (outcomeId in unique(outcomeOfInterest$outcomeId) ){
            outcomeName = unique(outcomeOfInterest$outcomeName[outcomeOfInterest$outcomeId==outcomeId])
            
            resultOfInterest = cohortMethodResult[cohortMethodResult$targetId==targetId  & 
                                                      cohortMethodResult$comparatorId==comparatorId& 
                                                      cohortMethodResult$outcomeId==outcomeId& 
                                                      cohortMethodResult$analysisId==analysisId&
                                                      cohortMethodResult$databaseId%in%DatabaseIdsUs,]
            
            
            resultOfInterest<-unique(resultOfInterest)
            #if there is na in logRr of database, it should be removed
            resultOfInterest<-resultOfInterest[!is.na(resultOfInterest$logRr),]
            if(!nrow(resultOfInterest)) next
            metaResult<-doMeta(data=resultOfInterest,
                               targetId = targetId,
                               comparatorId = comparatorId,
                               outcomeId = outcomeId,
                               analysisId = analysisId,
                               targetName = capitalize(targetName),
                               comparatorName = capitalize(comparatorName),
                               outcomeName = outcomeName)
            try({
                tiff(file.path(resultFolder,"meta_us",analysisName,sprintf("meta_t%d_c%d_a%d_o%d_%s.tiff",targetId,comparatorId,analysisId,outcomeId,outcomeName) ),
                     width = 900*5,height = 720*5,
                     res = 500)
                forestPlotGenerator(metaResult)
                dev.off()
            })
            
        }
    }
}

#write.csv(overallResult,file.path(resultFolder,"meta",sprintf("meta_t%d_c%d.csv",targetId,comparatorId)))

##US only meta-analysis
UsResult<-data.frame()
for(analysisId in unique(cohortMethodAnalysis$analysisId)){
    analysisName = unique(cohortMethodAnalysis$description[cohortMethodAnalysis$analysisId==analysisId])
    for (outcomeId in unique(outcomeOfInterest$outcomeId) ){
        
        outcomeName = unique(outcomeOfInterest$outcomeName[outcomeOfInterest$outcomeId==outcomeId])
        
        resultOfInterest = cohortMethodResult[cohortMethodResult$targetId==targetId  & 
                                                  cohortMethodResult$comparatorId==comparatorId& 
                                                  cohortMethodResult$outcomeId==outcomeId& 
                                                  cohortMethodResult$analysisId==analysisId&
                                                  cohortMethodResult$databaseId%in%DatabaseIdsUs,]
        if(!sum(!is.na(resultOfInterest$rr))) next #if there is no meaningful record skip
        
        resultOfInterest<-unique(resultOfInterest)
        #if there is na in logRr of database, it should be removed
        resultOfInterest<-resultOfInterest[!is.na(resultOfInterest$logRr),]
        
        metaResult<-doMeta(data=resultOfInterest,
                           targetId = targetId,
                           comparatorId = comparatorId,
                           outcomeId = outcomeId,
                           analysisId = analysisId,
                           targetName = capitalize(targetName),
                           comparatorName = capitalize(comparatorName),
                           outcomeName = outcomeName)
        metaD<-metaResult$meta
        
        metaDfSingle<-data.frame(targetId = targetId,
                                 comparatorId = comparatorId,
                                 outcomeId = outcomeId,
                                 analysisId = analysisId,
                                 rr = exp(metaD$TE.random),
                                 ci95Lb = exp(metaD$lower.random),
                                 ci95Ub = exp(metaD$upper.random),
                                 p = metaD$pval.random,
                                 i2 = NA,
                                 logRr = NA,
                                 seLogRr = NA,
                                 targetSubjects = sum(resultOfInterest$targetSubjects, na.rm =T),
                                 comparatorSubjects = sum(resultOfInterest$comparatorSubjects, na.rm =T),
                                 targetDays = sum(resultOfInterest$targetDays, na.rm =T),
                                 comparatorDays = sum(resultOfInterest$comparatorDays, na.rm =T),
                                 targetOutcomes = sum(resultOfInterest$targetOutcomes, na.rm =T),
                                 comparatorOutcomes = sum(resultOfInterest$comparatorOutcomes, na.rm =T),
                                 calibratedP = NA,
                                 calibratedRr = NA,
                                 calibratedCi95Lb = NA,
                                 calibratedCi95Ub = NA,
                                 calibratedLogRr= NA,
                                 calibratedSeLogRr = NA,
                                 databaseId = "US Only meta-analysis"
        )
        resultOfInterest<-rbind(resultOfInterest,metaDfSingle)
        UsResult <- rbind(UsResult,resultOfInterest)
        
    }
}
write.csv(UsResult,file.path(resultFolder,"US_Result.csv"))

####Two Dimensional Plotting####

##prepare the results
mainResults<-data.frame()
for (analysisId in unique(cohortMethodAnalysis$analysisId)){
    analysisName = unique(cohortMethodAnalysis$description[cohortMethodAnalysis$analysisId==analysisId])
    
    #interaction term analysis or no-matching analysis will not be included
    if (grepl("interaction",analysisName))next
    if (grepl("without matching",analysisName))next
    
    results <- cohortMethodResult[cohortMethodResult$analysisId==analysisId,]
    
    if (grepl("One-year",analysisName)) results$TAR <- "One-year"
    if (grepl("On-treatment",analysisName)) results$TAR <- "On-treatment"
    if (grepl("ITT",analysisName)) results$TAR <- "Five-year"
    
    if (grepl("one-to-one matching",analysisName)) results$Adjustment <- "1-to-1 PS matching"
    if (grepl("variable-ratio matching",analysisName)) results$Adjustment <- "Variable-ratio PS matching"
    if (grepl("stratification",analysisName)) results$Adjustment <- "PS stratification"
    
    if (grepl("with blanking period",analysisName)) results$blankingPeriod <- "after blanking period"
    if (!grepl("with blanking period",analysisName)) results$blankingPeriod <- ""
    mainResults <- rbind(mainResults,results)
    
}


###Plotting for NACE
###For primary endpoint
naceOutcomeId = 1240
naceNarrowOutcomeId = 1202
NarrowWithDeathOutcomeId = 653
targetAnalyses <- cohortMethodAnalysis$analysisId [!(grepl("interaction",cohortMethodAnalysis$description)|
                                                         grepl("without matching",cohortMethodAnalysis$description))]
targetAnalyses<-unique(targetAnalyses)

results<-data.frame()
for(analysisId in targetAnalyses){
    for (outcomeId in c(1240,1202,653)){
        analysisName = unique(cohortMethodAnalysis$description[cohortMethodAnalysis$analysisId==analysisId])
        outcomeName = unique(outcomeOfInterest$outcomeName[outcomeOfInterest$outcomeId==outcomeId])
        result<-mainResults[mainResults$outcomeId%in%c(outcomeId) & 
                                mainResults$targetId%in%targetId &
                                mainResults$analysisId%in%analysisId &
                                mainResults$databaseId=="Meta-analysis",]
        result$outcomeName = outcomeName
        result$analysisName = analysisName
        if(grepl("with blanking period", analysisName)) {
            result$outcomeName<-paste0(result$outcomeName," after blanking period")
        }
        results<-rbind(results,result)
    }
}
results$TAR<-factor(results$TAR,levels = c("One-year","Five-year","On-treatment"))

results$Adjustment<-factor(results$Adjustment,levels = c("1-to-1 PS matching",
                                                         "Variable-ratio PS matching",
                                                         "PS stratification"))

results<-results[!results$outcomeName%in%c("NACE (only primary condition) after blanking period",
                                           "NACE or mortality after blanking period"),]
results$outcomeName <- factor(results$outcomeName, levels = c("NACE", "NACE or mortality",
                                                              "NACE (only primary condition)", "NACE after blanking period"))

xLim = max(ceiling(median(results$ci95Ub, na.rm=TRUE)),ceiling(1/median(results$ci95Lb, na.rm=TRUE)),2, na.rm = TRUE)
xLim<-ifelse(xLim< (max( 1/results$rr,results$rr,na.rm=TRUE)),ceiling(max( 1/results$rr,results$rr,na.rm=TRUE)),xLim)
limits = c(1/xLim,xLim)
xLimits = c(0.85,1.3)

results <- results %>% mutate(#ci95Lb =ifelse(ci95Lb < limits[1], ci95Lb-1, ci95Lb), 
    #ci95Ub = ifelse(ci95Lb > limits[2], ci95Ub+1, ci95Ub),
    ci95LbOut = ifelse(ci95Lb < limits[1], rr - limits[1], NA), 
    ci95UbOut = ifelse(ci95Ub > limits[2], rr - limits[2], NA))

results$Significance<-factor(ifelse(results$p<0.05,"P<.05","Not significant"),
                             levels = c("P<.05","Not significant")
)

summaryP<-gridForest(results, breaks = c(0.9,1,1.1,1.2), outlierMoverLower= 0.03,outlierMoverUpper= 0.03, xLimits=xLimits)
outcomeName = "NACE"
outcomeId = 1240
if(!file.exists(file.path(resultFolder,"summary"))) dir.create(file.path(resultFolder,"summary"))
ggplot2::ggsave(file.path(resultFolder,"summary",sprintf("summary_%s_t%d_c%d_o%d_.eps",
                                                         outcomeName,
                                                         targetId,
                                                         comparatorId,
                                                         outcomeId)), summaryP, device = "eps" ,
                width = 20, height = 22, units = "cm", dpi = 320)

ggplot2::ggsave(file.path(resultFolder,"summary",sprintf("summary_%s_t%d_c%d_o%d_.pdf",
                                                         outcomeName,
                                                         targetId,
                                                         comparatorId,
                                                         outcomeId)), summaryP, device = "pdf" ,
                width = 20, height = 22, units = "cm", dpi = 320)

ggplot2::ggsave(file.path(resultFolder,"summary",sprintf("summary_%s_t%d_c%d_o%d_.tiff",
                                                         outcomeName,
                                                         targetId,
                                                         comparatorId,
                                                         outcomeId)), summaryP, device = "tiff" ,
                width = 20, height = 22, units = "cm", dpi = 320)

###p-value calibration
results$ci95Ub <- results$calibratedCi95Ub
results$ci95Lb <- results$calibratedCi95Lb
results$rr     <- results$calibratedRr
results$p <- results$calibratedP
xLim = max(ceiling(median(results$ci95Ub, na.rm=TRUE)),ceiling(1/median(results$ci95Lb, na.rm=TRUE)),2, na.rm = TRUE)
xLim<-ifelse(xLim< (max( 1/results$rr,results$rr,na.rm=TRUE)),ceiling(max( 1/results$rr,results$rr,na.rm=TRUE)),xLim)
limits = c(1/xLim,xLim)
limits = c(0.80,1.3)

results$ci95LbOut <- NA
results$ci95UbOut <- NA
results <- results %>% mutate(#ci95Lb =ifelse(ci95Lb < limits[1], ci95Lb-1, ci95Lb), 
    #ci95Ub = ifelse(ci95Lb > limits[2], ci95Ub+1, ci95Ub),
    ci95LbOut = ifelse(ci95Lb < limits[1], rr - limits[1], NA), 
    ci95UbOut = ifelse(ci95Ub > limits[2], rr - limits[2], NA))
results$Significance<-factor(ifelse(results$p<0.05,"P<.05","Not significant"),
                             levels = c("P<.05","Not significant")
)
summaryP <- gridForest(results)

outcomeName = "NACE"
outcomeId = 1240
if(!file.exists(file.path(resultFolder,"summary"))) dir.create(file.path(resultFolder,"summary"))
ggplot2::ggsave(file.path(resultFolder,"summary",sprintf("summary_%s_t%d_c%d_o%d_calibrated.eps",
                                                         outcomeName,
                                                         targetId,
                                                         comparatorId,
                                                         outcomeId)), summaryP, device = "eps" ,
                width = 20, height = 22, units = "cm", dpi = 320)

ggplot2::ggsave(file.path(resultFolder,"summary",sprintf("summary_%s_t%d_c%d_o%d_calibrated.pdf",
                                                         outcomeName,
                                                         targetId,
                                                         comparatorId,
                                                         outcomeId)), summaryP, device = "pdf" ,
                width = 20, height = 22, units = "cm", dpi = 320)

ggplot2::ggsave(file.path(resultFolder,"summary",sprintf("summary_%s_t%d_c%d_o%d_calibrated.tiff",
                                                         outcomeName,
                                                         targetId,
                                                         comparatorId,
                                                         outcomeId)), summaryP, device = "tiff" ,
                width = 20, height = 22, units = "cm", dpi = 320)

###NACE in limited cohort (2013 to 2015)
naceOutcomeId = 1240
naceNarrowOutcomeId = 1202
NarrowWithDeathOutcomeId = 653
targetAnalyses <- cohortMethodAnalysis$analysisId [!(grepl("interaction",cohortMethodAnalysis$description)|
                                                         grepl("without matching",cohortMethodAnalysis$description))]
targetAnalyses<-unique(targetAnalyses)

results<-data.frame()
for(analysisId in targetAnalyses){
    for (outcomeId in c(1240,1202,653)){
        analysisName = unique(cohortMethodAnalysis$description[cohortMethodAnalysis$analysisId==analysisId])
        outcomeName = unique(outcomeOfInterest$outcomeName[outcomeOfInterest$outcomeId==outcomeId])
        result<-mainResults[mainResults$outcomeId%in%c(outcomeId) & 
                                mainResults$targetId%in%c(1253) &
                                mainResults$analysisId%in%analysisId &
                                mainResults$databaseId=="Meta-analysis",]
        result$outcomeName = outcomeName
        result$analysisName = analysisName
        if(grepl("with blanking period", analysisName)) {
            result$outcomeName<-paste0(result$outcomeName," after blanking period")
        }
        results<-rbind(results,result)
    }
}
results$TAR<-factor(results$TAR,levels = c("One-year","Five-year","On-treatment"))

results$Adjustment<-factor(results$Adjustment,levels = c("1-to-1 PS matching",
                                                         "Variable-ratio PS matching",
                                                         "PS stratification"))

results<-results[!results$outcomeName%in%c("NACE (only primary condition) after blanking period",
                                           "NACE or mortality after blanking period"),]
results$outcomeName <- factor(results$outcomeName, levels = c("NACE", "NACE or mortality",
                                                              "NACE (only primary condition)", "NACE after blanking period"))

xLim = max(ceiling(median(results$ci95Ub, na.rm=TRUE)),ceiling(1/median(results$ci95Lb, na.rm=TRUE)),2, na.rm = TRUE)
xLim<-ifelse(xLim< (max( 1/results$rr,results$rr,na.rm=TRUE)),ceiling(max( 1/results$rr,results$rr,na.rm=TRUE)),xLim)
limits = c(1/xLim,xLim)
limits = c(0.80,1.3)
xLimits = c(0.80,1.3)

results <- results %>% mutate(#ci95Lb =ifelse(ci95Lb < limits[1], ci95Lb-1, ci95Lb), 
    #ci95Ub = ifelse(ci95Lb > limits[2], ci95Ub+1, ci95Ub),
    ci95LbOut = ifelse(ci95Lb < limits[1], rr - limits[1], NA), 
    ci95UbOut = ifelse(ci95Ub > limits[2], rr - limits[2], NA))

results$Significance<-factor(ifelse(results$p<0.05,"P<.05","Not significant"),
                             levels = c("P<.05","Not significant")
)

summaryP<-gridForest(results, breaks = c(0.8,0.9,1,1.1,1.2), outlierMoverLower= 0.02,outlierMoverUpper= 0.03, xLimits=xLimits)
outcomeName = "NACE"
outcomeId = 1240
if(!file.exists(file.path(resultFolder,"summary"))) dir.create(file.path(resultFolder,"summary"))
ggplot2::ggsave(file.path(resultFolder,"summary",sprintf("summary_%s_t%d_c%d_o%d_2013to2015.eps",
                                                         outcomeName,
                                                         targetId,
                                                         comparatorId,
                                                         outcomeId)), summaryP, device = "eps" ,
                width = 20, height = 22, units = "cm", dpi = 320)

ggplot2::ggsave(file.path(resultFolder,"summary",sprintf("summary_%s_t%d_c%d_o%d_2013to2015.pdf",
                                                         outcomeName,
                                                         targetId,
                                                         comparatorId,
                                                         outcomeId)), summaryP, device = "pdf" ,
                width = 20, height = 22, units = "cm", dpi = 320)

ggplot2::ggsave(file.path(resultFolder,"summary",sprintf("summary_%s_t%d_c%d_o%d_2013to2015.tiff",
                                                         outcomeName,
                                                         targetId,
                                                         comparatorId,
                                                         outcomeId)), summaryP, device = "tiff" ,
                width = 20, height = 22, units = "cm", dpi = 320)
####
###Plotting for Ischemic event
outcomeIdOfInterest = 1237
narrowOutcomeIdOfInterest = 1201
blankingPeriodAnalysisIds = c(5,6,7,12,13,14,19,20,21)
outcomeNameOfInterest <- "Ischemic event"
xLimits =c(0.85,1.3)
breaks = c(0.9,1,1.1,1.2)

results<-prepareGridForest(mainResults=mainResults,
                           cohortMethodAnalysis=cohortMethodAnalysis,
                           outcomeOfInterest=outcomeOfInterest,
                           outcomeIdOfInterest=outcomeIdOfInterest,
                           narrowOutcomeIdOfInterest=narrowOutcomeIdOfInterest,
                           outcomeNameOfInterest = outcomeNameOfInterest,
                           xLimits = xLimits,
                           breaks = breaks,
                           blankingPeriodAnalysisIds= c(5,6,7,12,13,14,19,20,21))

summaryP <- gridForest(results,breaks=breaks, outlierMoverLower= 0.02,outlierMoverUpper= 0.11, xLimits=xLimits)
outcomeName = gsub(" ","",outcomeNameOfInterest)
outcomeId = outcomeIdOfInterest
if(!file.exists(file.path(resultFolder,"summary"))) dir.create(file.path(resultFolder,"summary"))
ggplot2::ggsave(file.path(resultFolder,"summary",sprintf("summary_%s_t%d_c%d_o%d_.eps",
                                                         outcomeName,
                                                         targetId,
                                                         comparatorId,
                                                         outcomeId)), summaryP, device = "eps" ,
                width = 20, height = 22, units = "cm", dpi = 320)

ggplot2::ggsave(file.path(resultFolder,"summary",sprintf("summary_%s_t%d_c%d_o%d_.pdf",
                                                         outcomeName,
                                                         targetId,
                                                         comparatorId,
                                                         outcomeId)), summaryP, device = "pdf" ,
                width = 20, height = 22, units = "cm", dpi = 320)

ggplot2::ggsave(file.path(resultFolder,"summary",sprintf("summary_%s_t%d_c%d_o%d_.tiff",
                                                         outcomeName,
                                                         targetId,
                                                         comparatorId,
                                                         outcomeId)), summaryP, device = "tiff" ,
                width = 20, height = 22, units = "cm", dpi = 320)

###Plotting for Hemorrhagic event
outcomeIdOfInterest = 1239
narrowOutcomeIdOfInterest = 1200
blankingPeriodAnalysisIds = c(5,6,7,12,13,14,19,20,21)
outcomeNameOfInterest <- "Hemorrhagic event"
xLimits = c(0.9,2.7)
breaks = c(1.0,1.25,1.5,2,2.5)

results<-prepareGridForest(mainResults=mainResults,
                           cohortMethodAnalysis=cohortMethodAnalysis,
                           outcomeOfInterest=outcomeOfInterest,
                           outcomeIdOfInterest=outcomeIdOfInterest,
                           narrowOutcomeIdOfInterest=narrowOutcomeIdOfInterest,
                           outcomeNameOfInterest = outcomeNameOfInterest,
                           xLimits = xLimits,
                           breaks = breaks,
                           blankingPeriodAnalysisIds= c(5,6,7,12,13,14,19,20,21))

summaryP <- gridForest(results,breaks=breaks, outlierMoverLower= 0.02,outlierMoverUpper= 0.11, xLimits=xLimits)
outcomeName = "HemorrhagicEvent"
outcomeId = 1239
if(!file.exists(file.path(resultFolder,"summary"))) dir.create(file.path(resultFolder,"summary"))
ggplot2::ggsave(file.path(resultFolder,"summary",sprintf("summary_%s_t%d_c%d_o%d_.eps",
                                                         outcomeName,
                                                         targetId,
                                                         comparatorId,
                                                         outcomeId)), summaryP, device = "eps" ,
                width = 20, height = 22, units = "cm", dpi = 320)

ggplot2::ggsave(file.path(resultFolder,"summary",sprintf("summary_%s_t%d_c%d_o%d_.pdf",
                                                         outcomeName,
                                                         targetId,
                                                         comparatorId,
                                                         outcomeId)), summaryP, device = "pdf" ,
                width = 20, height = 22, units = "cm", dpi = 320)

ggplot2::ggsave(file.path(resultFolder,"summary",sprintf("summary_%s_t%d_c%d_o%d_.tiff",
                                                         outcomeName,
                                                         targetId,
                                                         comparatorId,
                                                         outcomeId)), summaryP, device = "tiff" ,
                width = 20, height = 22, units = "cm", dpi = 320)


##Plotting Ischemic stroke
outcomeIdOfInterest = 1233
narrowOutcomeIdOfInterest = 1180
blankingPeriodAnalysisIds = c(5,6,7,12,13,14,19,20,21)
outcomeNameOfInterest <- "Ischemic stroke"
xLimits = c(0.5,2.0)
breaks = c(0.5,0.75,1.0,1.5,2.0)

results<-prepareGridForest(mainResults=mainResults,
    cohortMethodAnalysis=cohortMethodAnalysis,
    outcomeOfInterest=outcomeOfInterest,
    outcomeIdOfInterest=1233,
    narrowOutcomeIdOfInterest=1180,
    outcomeNameOfInterest = outcomeNameOfInterest,
    xLimits = c(0.5,2.0),
    breaks = c(0.5,0.75,1.0,1.5,2.0),
    blankingPeriodAnalysisIds= c(5,6,7,12,13,14,19,20,21))

summaryP <- gridForest(results,breaks=breaks, outlierMoverLower= 0.02,outlierMoverUpper= 0.11, xLimits=xLimits)
summaryP
outcomeName = outcomeNameOfInterest
outcomeId = outcomeIdOfInterest
if(!file.exists(file.path(resultFolder,"summary"))) dir.create(file.path(resultFolder,"summary"))
ggplot2::ggsave(file.path(resultFolder,"summary",sprintf("summary_%s_t%d_c%d_o%d_.eps",
                                                         outcomeName,
                                                         targetId,
                                                         comparatorId,
                                                         outcomeId)), summaryP, device = "eps" ,
                width = 20, height = 22, units = "cm", dpi = 320)

ggplot2::ggsave(file.path(resultFolder,"summary",sprintf("summary_%s_t%d_c%d_o%d_.pdf",
                                                         outcomeName,
                                                         targetId,
                                                         comparatorId,
                                                         outcomeId)), summaryP, device = "pdf" ,
                width = 20, height = 22, units = "cm", dpi = 320)

ggplot2::ggsave(file.path(resultFolder,"summary",sprintf("summary_%s_t%d_c%d_o%d_.tiff",
                                                         outcomeName,
                                                         targetId,
                                                         comparatorId,
                                                         outcomeId)), summaryP, device = "tiff" ,
                width = 20, height = 22, units = "cm", dpi = 320)

##Plotting Recurrent acute MI
outcomeIdOfInterest = 1234
narrowOutcomeIdOfInterest = 1198
blankingPeriodAnalysisIds = c(5,6,7,12,13,14,19,20,21)
outcomeNameOfInterest <- "Acute myocardial infarction"
xLimits =c(0.75,1.4)
breaks = c(0.75,0.9,1,1.1,1.3)

results<-prepareGridForest(mainResults=mainResults,
                           cohortMethodAnalysis=cohortMethodAnalysis,
                           outcomeOfInterest=outcomeOfInterest,
                           outcomeIdOfInterest=outcomeIdOfInterest,
                           narrowOutcomeIdOfInterest=narrowOutcomeIdOfInterest,
                           outcomeNameOfInterest = outcomeNameOfInterest,
                           xLimits = xLimits,
                           breaks = breaks,
                           blankingPeriodAnalysisIds= c(5,6,7,12,13,14,19,20,21))
results$outcomeName<- gsub("Acute myocardial infarction","Recurrent acute MI", results$outcomeName)
summaryP <- gridForest(results,breaks=breaks, outlierMoverLower= 0.02,outlierMoverUpper= 0.04, xLimits=xLimits)
summaryP
outcomeName = outcomeNameOfInterest
outcomeId = outcomeIdOfInterest
if(!file.exists(file.path(resultFolder,"summary"))) dir.create(file.path(resultFolder,"summary"))
ggplot2::ggsave(file.path(resultFolder,"summary",sprintf("summary_%s_t%d_c%d_o%d_.eps",
                                                         outcomeName,
                                                         targetId,
                                                         comparatorId,
                                                         outcomeId)), summaryP, device = "eps" ,
                width = 20, height = 22, units = "cm", dpi = 320)

ggplot2::ggsave(file.path(resultFolder,"summary",sprintf("summary_%s_t%d_c%d_o%d_.pdf",
                                                         outcomeName,
                                                         targetId,
                                                         comparatorId,
                                                         outcomeId)), summaryP, device = "pdf" ,
                width = 20, height = 22, units = "cm", dpi = 320)

ggplot2::ggsave(file.path(resultFolder,"summary",sprintf("summary_%s_t%d_c%d_o%d_.tiff",
                                                         outcomeName,
                                                         targetId,
                                                         comparatorId,
                                                         outcomeId)), summaryP, device = "tiff" ,
                width = 20, height = 22, units = "cm", dpi = 320)

##Plotting Any revascularization
outcomeIdOfInterest = 1030
narrowOutcomeIdOfInterest = NULL
blankingPeriodAnalysisIds = c(5,6,7,12,13,14,19,20,21)
outcomeNameOfInterest <- "Revascularization"
xLimits =c(0.74,1.4)
breaks = c(0.75,0.9,1,1.1,1.3)

results<-prepareGridForest(mainResults=mainResults,
                           cohortMethodAnalysis=cohortMethodAnalysis,
                           outcomeOfInterest=outcomeOfInterest,
                           outcomeIdOfInterest=outcomeIdOfInterest,
                           narrowOutcomeIdOfInterest=narrowOutcomeIdOfInterest,
                           outcomeNameOfInterest = outcomeNameOfInterest,
                           xLimits = xLimits,
                           breaks = breaks,
                           blankingPeriodAnalysisIds= c(5,6,7,12,13,14,19,20,21))
summaryP <- gridForest(results,breaks=breaks, outlierMoverLower= 0.02,outlierMoverUpper= 0.04, xLimits=xLimits)
summaryP
outcomeName = outcomeNameOfInterest
outcomeId = outcomeIdOfInterest
if(!file.exists(file.path(resultFolder,"summary"))) dir.create(file.path(resultFolder,"summary"))
ggplot2::ggsave(file.path(resultFolder,"summary",sprintf("summary_%s_t%d_c%d_o%d_.eps",
                                                         outcomeName,
                                                         targetId,
                                                         comparatorId,
                                                         outcomeId)), summaryP, device = "eps" ,
                width = 20, height = 22, units = "cm", dpi = 320)

ggplot2::ggsave(file.path(resultFolder,"summary",sprintf("summary_%s_t%d_c%d_o%d_.pdf",
                                                         outcomeName,
                                                         targetId,
                                                         comparatorId,
                                                         outcomeId)), summaryP, device = "pdf" ,
                width = 20, height = 22, units = "cm", dpi = 320)

ggplot2::ggsave(file.path(resultFolder,"summary",sprintf("summary_%s_t%d_c%d_o%d_.tiff",
                                                         outcomeName,
                                                         targetId,
                                                         comparatorId,
                                                         outcomeId)), summaryP, device = "tiff" ,
                width = 20, height = 22, units = "cm", dpi = 320)

##Plotting Hemorrhagic stroke
outcomeIdOfInterest = 1235
narrowOutcomeIdOfInterest = 1190
blankingPeriodAnalysisIds = c(5,6,7,12,13,14,19,20,21)
outcomeNameOfInterest <- "Hemorrhagic stroke"
xLimits = c(0.75,3)
breaks = c(0.75,1.0,1.25,1.5,2.0,3.0)

results<-prepareGridForest(mainResults=mainResults,
                           cohortMethodAnalysis=cohortMethodAnalysis,
                           outcomeOfInterest=outcomeOfInterest,
                           outcomeIdOfInterest=outcomeIdOfInterest,
                           narrowOutcomeIdOfInterest=narrowOutcomeIdOfInterest,
                           outcomeNameOfInterest = outcomeNameOfInterest,
                           xLimits = xLimits,
                           breaks = breaks,
                           blankingPeriodAnalysisIds= c(5,6,7,12,13,14,19,20,21))
summaryP <- gridForest(results,breaks=breaks, outlierMoverLower= 0.045,outlierMoverUpper= 0.2, xLimits=xLimits)
summaryP
outcomeName = outcomeNameOfInterest
outcomeId = outcomeIdOfInterest
if(!file.exists(file.path(resultFolder,"summary"))) dir.create(file.path(resultFolder,"summary"))
ggplot2::ggsave(file.path(resultFolder,"summary",sprintf("summary_%s_t%d_c%d_o%d_.eps",
                                                         outcomeName,
                                                         targetId,
                                                         comparatorId,
                                                         outcomeId)), summaryP, device = "eps" ,
                width = 20, height = 22, units = "cm", dpi = 320)

ggplot2::ggsave(file.path(resultFolder,"summary",sprintf("summary_%s_t%d_c%d_o%d_.pdf",
                                                         outcomeName,
                                                         targetId,
                                                         comparatorId,
                                                         outcomeId)), summaryP, device = "pdf" ,
                width = 20, height = 22, units = "cm", dpi = 320)

ggplot2::ggsave(file.path(resultFolder,"summary",sprintf("summary_%s_t%d_c%d_o%d_.tiff",
                                                         outcomeName,
                                                         targetId,
                                                         comparatorId,
                                                         outcomeId)), summaryP, device = "tiff" ,
                width = 20, height = 22, units = "cm", dpi = 320)

##Plotting GI bleeding
outcomeIdOfInterest = 1236
narrowOutcomeIdOfInterest = 1197
blankingPeriodAnalysisIds = c(5,6,7,12,13,14,19,20,21)
outcomeNameOfInterest <- "GI bleeding"
xLimits = c(0.75,3.1)
breaks = c(0.75,1.0,1.5,2.0,3.0)

results<-prepareGridForest(mainResults=mainResults,
                           cohortMethodAnalysis=cohortMethodAnalysis,
                           outcomeOfInterest=outcomeOfInterest,
                           outcomeIdOfInterest=outcomeIdOfInterest,
                           narrowOutcomeIdOfInterest=narrowOutcomeIdOfInterest,
                           outcomeNameOfInterest = outcomeNameOfInterest,
                           xLimits = xLimits,
                           breaks = breaks,
                           blankingPeriodAnalysisIds= c(5,6,7,12,13,14,19,20,21))
summaryP <- gridForest(results,breaks=breaks, outlierMoverLower= 0.045,outlierMoverUpper= 0.2, xLimits=xLimits)
summaryP
outcomeName = outcomeNameOfInterest
outcomeId = outcomeIdOfInterest
if(!file.exists(file.path(resultFolder,"summary"))) dir.create(file.path(resultFolder,"summary"))
ggplot2::ggsave(file.path(resultFolder,"summary",sprintf("summary_%s_t%d_c%d_o%d_.eps",
                                                         outcomeName,
                                                         targetId,
                                                         comparatorId,
                                                         outcomeId)), summaryP, device = "eps" ,
                width = 20, height = 22, units = "cm", dpi = 320)

ggplot2::ggsave(file.path(resultFolder,"summary",sprintf("summary_%s_t%d_c%d_o%d_.pdf",
                                                         outcomeName,
                                                         targetId,
                                                         comparatorId,
                                                         outcomeId)), summaryP, device = "pdf" ,
                width = 20, height = 22, units = "cm", dpi = 320)

ggplot2::ggsave(file.path(resultFolder,"summary",sprintf("summary_%s_t%d_c%d_o%d_.tiff",
                                                         outcomeName,
                                                         targetId,
                                                         comparatorId,
                                                         outcomeId)), summaryP, device = "tiff" ,
                width = 20, height = 22, units = "cm", dpi = 320)

##Plotting mortality
outcomeIdOfInterest = 20
narrowOutcomeIdOfInterest = NULL
blankingPeriodAnalysisIds = c(5,6,7,12,13,14,19,20,21)
outcomeNameOfInterest <- "All-cause mortality"
xLimits =c(0.4,2)
breaks = c(0.5,0.75,1,1.5,2.0)

results<-prepareGridForest(mainResults=mainResults,
                           cohortMethodAnalysis=cohortMethodAnalysis,
                           outcomeOfInterest=outcomeOfInterest,
                           outcomeIdOfInterest=outcomeIdOfInterest,
                           narrowOutcomeIdOfInterest=narrowOutcomeIdOfInterest,
                           outcomeNameOfInterest = outcomeNameOfInterest,
                           xLimits = xLimits,
                           breaks = breaks,
                           blankingPeriodAnalysisIds= c(5,6,7,12,13,14,19,20,21))
summaryP <- gridForest(results,breaks=breaks, outlierMoverLower= 0.03,outlierMoverUpper= 0.15, xLimits=xLimits)
summaryP
outcomeName = outcomeNameOfInterest
outcomeId = outcomeIdOfInterest
if(!file.exists(file.path(resultFolder,"summary"))) dir.create(file.path(resultFolder,"summary"))
ggplot2::ggsave(file.path(resultFolder,"summary",sprintf("summary_%s_t%d_c%d_o%d_.eps",
                                                         outcomeName,
                                                         targetId,
                                                         comparatorId,
                                                         outcomeId)), summaryP, device = "eps" ,
                width = 20, height = 22, units = "cm", dpi = 320)

ggplot2::ggsave(file.path(resultFolder,"summary",sprintf("summary_%s_t%d_c%d_o%d_.pdf",
                                                         outcomeName,
                                                         targetId,
                                                         comparatorId,
                                                         outcomeId)), summaryP, device = "pdf" ,
                width = 20, height = 22, units = "cm", dpi = 320)

ggplot2::ggsave(file.path(resultFolder,"summary",sprintf("summary_%s_t%d_c%d_o%d_.tiff",
                                                         outcomeName,
                                                         targetId,
                                                         comparatorId,
                                                         outcomeId)), summaryP, device = "tiff" ,
                width = 20, height = 22, units = "cm", dpi = 320)

###Distribution of the risk estimates
library(dplyr)
library(ggplot2)
results <- read.csv("/Users/chan/data/ticagrelor/output/resultOfInterest.csv")
nace<- results %>% 
    #filter(databaseId %in% c("Meta-analysis")) %>%
    filter(outcomeId %in% c(1202,1240)) %>%
    filter(analysisId %in% c(1,2,3,5,6,7,8,9,10,12,13,14,15,16,17,19,20,21))

nace$databaseId <- factor(nace$databaseId, level = c("OptumPanTher","IQVIA - Hospital", "HIRA", "Meta-analysis"))
naceBeforeCal <- nace %>% select(targetId, comparatorId, outcomeId, analysisId,rr,ci95Lb,ci95Ub,p, databaseId) %>%
    mutate(Calibration="Before calibration")
naceAfterCal <- nace %>% 
    mutate(rr=calibratedRr, ci95Lb=calibratedCi95Lb, ci95Ub=calibratedCi95Ub,p=calibratedP) %>%
    select(targetId, comparatorId, outcomeId, analysisId,rr,ci95Lb,ci95Ub,p, databaseId) %>%
    mutate(Calibration="After calibration")
naceCal <- rbind(naceBeforeCal,naceAfterCal)

naceCal$Calibration <- factor(naceCal$Calibration,level = c("Before calibration", "After calibration"))

primaryRr<-naceCal %>% filter(targetId==874, analysisId==1,outcomeId==1240#, calibration == "before calibration"
)
customLimit = c(0.60,1.7)
customBreaks = c(0.60,0.75,0.9,1.0,1.1, 1.3,1.7)

RrDistr<-ggplot(naceCal, aes(x=rr,fill = Calibration, color = Calibration)) +
    geom_histogram(#fill="white",
        alpha = 0.3, position="identity", bins=50) +
    geom_vline(data = primaryRr, aes(xintercept=rr, color = Calibration)) +
    geom_vline(aes(xintercept=1.0), linetype="dashed") +
    facet_grid(databaseId~.)+
    ggplot2::theme_bw()+
    scale_x_continuous(trans=log10_trans(), limits= customLimit,breaks =customBreaks
    )+
    xlab('Hazard ratio')+ ylab("Count")
outcomeName="NACE"
if(!file.exists(file.path(resultFolder,"summary"))) dir.create(file.path(resultFolder,"summary"))
ggplot2::ggsave(file.path(resultFolder,"summary",sprintf("RrDistr_%s.eps",
                                                         outcomeName)), RrDistr, device = "eps" ,
                width = 24, height = 20, units = "cm", dpi = 320)

ggplot2::ggsave(file.path(resultFolder,"summary",sprintf("RrDistr_%s.pdf",
                                                         outcomeName)), RrDistr, device = "pdf" ,
                width = 24, height = 20, units = "cm", dpi = 320)

ggplot2::ggsave(file.path(resultFolder,"summary",sprintf("RrDistr_%s.tiff",
                                                         outcomeName)), RrDistr, device = "tiff" ,
                width = 24, height = 20, units = "cm", dpi = 320)
