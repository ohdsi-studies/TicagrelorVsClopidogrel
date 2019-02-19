outputFolder <- "s:/TicagrelorVsClopidogrel_narrow"

#KM plot, follow-up distribution of study population
studyPopList<-list.files(path = file.path(outputFolder,"cmOutput"), pattern = "StudyPop.*")
for (studyPopPath in studyPopList){
  studyPop<-readRDS(file.path(outputFolder,"cmOutput",studyPopPath))
  analysisName<-gsub(".rds","",gsub("StudyPop_","",studyPopPath))
  
  
  CohortMethod::plotKaplanMeier(population = studyPop, targetLabel= "Ticagrelor", comparatorLabel = "Clopidogrel",confidenceIntervals = TRUE,
                                fileName = file.path(outputFolder,"export",paste0("survivalPlot","_studyPop_" ,analysisName,".tiff")))
  
  followUpDist<-CohortMethod::getFollowUpDistribution(population = studyPop)
  write.csv(followUpDist, file.path(outputFolder,"export",paste0("followUpDist","_studyPop_" ,analysisName,".csv")))
  
  CohortMethod::plotFollowUpDistribution(population = studyPop,targetLabel= "Ticagrelor", comparatorLabel = "Clopidogrel",
                                         fileName = file.path(outputFolder,"export",paste0("FollowUpDistribution","_studyPop_" ,analysisName,".tiff")))
}
#attrition Diagram, KM plot, follow-up distribution of strata population
stratPopList<-list.files(path = file.path(outputFolder,"cmOutput"), pattern = "StratPop.*")
for (stratPopPath in stratPopList){
  stratPop<-readRDS(file.path(outputFolder,"cmOutput",stratPopPath))
  analysisName<-gsub(".rds","",gsub("StratPop_","",stratPopPath))
  
  CohortMethod::drawAttritionDiagram(stratPop, targetLabel = "Ticagrelor", comparatorLabel = "Clopidogrel", 
                                     fileName = file.path(outputFolder,"export",paste0("attritionDiagram",analysisName,".tiff")))
  
  CohortMethod::plotKaplanMeier(population = stratPop, targetLabel= "Ticagrelor", comparatorLabel = "Clopidogrel",confidenceIntervals = TRUE,
                                fileName = file.path(outputFolder,"export",paste0("survivalPlot","_stratPop_" ,analysisName,".tiff")))
  
  followUpDist<-CohortMethod::getFollowUpDistribution(population = stratPop)
  write.csv(followUpDist, file.path(outputFolder,"export",paste0("followUpDist","_stratPop_" ,analysisName,".csv")))
  
  CohortMethod::plotFollowUpDistribution(population = stratPop,targetLabel= "Ticagrelor", comparatorLabel = "Clopidogrel",
                                         fileName = file.path(outputFolder,"export",paste0("FollowUpDistribution","_stratPop_" ,analysisName,".tiff")))
}
#Tables for Baseline characteristics
fileName<-system.file("csv","tableSpecification.csv",package= "TicagrelorVsClopidogrel_narrow")
specification <-read.csv(fileName, stringsAsFactors = FALSE)
balanceList<-list.files(path = file.path(outputFolder,"balance"), pattern = "bal.*")
for (balancePath in balanceList){
  balance <- readRDS(file.path(outputFolder,"balance",balancePath))
  analysisName<-gsub(".rds","",gsub("bal_","",balancePath))
  cmTable1<-CohortMethod::createCmTable1(balance = balance, specifications = specification)
  write.csv(cmTable1,file.path(outputFolder,"export",paste0(analysisName,"_cmTable.csv")))
} 
