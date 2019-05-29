library(forestplot)
library(dplyr)

outputFolder<-""
analysisSummary <- read.csv(file.path(outputFolder,"cohort_method_result.csv"),stringsAsFactors = FALSE)
analysis <- read.csv(file.path(outputFolder,"cohort_method_analysis.csv"),stringsAsFactors = FALSE)

outcomes <- read.csv(file.path(outputFolder,"outcome_of_interest.csv"),stringsAsFactors = FALSE)
#covBal <- read.csv(file.path(outputFolder,"covariate_balance.csv"),stringsAsFactors = FALSE)

#Camel To Snake
colnames(analysisSummary)<-SqlRender::snakeCaseToCamelCase(colnames(analysisSummary))
colnames(analysis)<-SqlRender::snakeCaseToCamelCase(colnames(analysis))
colnames(outcomes)<-SqlRender::snakeCaseToCamelCase(colnames(outcomes))
#colnames(covBal)<-SqlRender::snakeCaseToCamelCase(colnames(covBal))


outcomeOfInterest <- outcomes$outcomeId
outcomeNameOfInterest <- c("NACE","IschemicEvent","ischemicStroke","Revascularization","AMI",
                           "HemorrhagicEvent","hemorrhagicStroke","giBleeding",
                           "anyDeath","dyspnea")
outcomeIdOfInterest<-outcomes$outcomeId[outcomes$outcomeName %in% outcomeNameOfInterest]
outcomeIdOrder<-outcomes$outcomeId [match(outcomeNameOfInterest,outcomes$outcomeName)]

outcomeNameOfInterestNarrow <- c("NACE_narrow","IschemicEvent_narrow","ischemicStroke_narrow","AMI_narrow",
                                 "HemorrhagicEvent_narrow","ICH_narrow","giBleeding_narrow")

outcomeNameOrder <- c("Net Adverse Clinical Event", 
                      
                      "Ischemic event",
                      "Ischemic stroke",
                      "Revascularization",
                      "Acute MI",
                      
                      "Hemorrhagic Event",
                      "Hemorrhagic stroke",
                      "GI bleeding",
                      
                      "Any death",
                      "Dyspnea")

analysisIds <- analysis$analysisId
analysisIdsOfInterest <- c(1,2,3,4,7,8,9,10,11,12,13,1142)
analysisIdOrder <- c(1,2,4,
                     7,8,13,
                     10,11,1142,
                     3,9,12)
# ##Trim strings for outcome Name 
# analysisSummary$outcomeName <- gsub("SCYou:","",analysisSummary$outcomeName)
# analysisSummary$outcomeName <- gsub("outcome","",analysisSummary$outcomeName)
# analysisSummary$outcomeName <- gsub("and primary condition and first event","",analysisSummary$outcomeName)
# analysisSummary$outcomeName <- gsub("and primary condition and all event","",analysisSummary$outcomeName)
# analysisSummary$outcomeName <- gsub("inpatient or ED","",analysisSummary$outcomeName)
# analysisSummary$outcomeName <- gsub(")","",analysisSummary$outcomeName)
# analysisSummary$outcomeName <- gsub('\\(',"",analysisSummary$outcomeName)
# analysisSummary$outcomeName <- trimws(analysisSummary$outcomeName)
# 
# ##Trim strings for analysis Name 
# analysisSummary$analysisDescription <- gsub("Time To First Post Index Event ","",analysisSummary$analysisDescription)
# analysisSummary$analysisDescription <- gsub("Time to First Post Index Event ","",analysisSummary$analysisDescription)


for(analysisIdOfInterest in analysisIds){
    #analysisIdOfInterest <- analysisIds[1]
    #analysisIdOfInterest=6
    
    data <- analysisSummary %>% 
        filter (analysisId == analysisIdOfInterest) %>%
        filter (outcomeId %in% outcomeIdOfInterest) %>%
        filter (targetId == 874 & comparatorId == 929 )
    
    #reorder the data according to the outcome order
    data <- data[ match(outcomeIdOrder,data$outcomeId),]
    data$outcomeName <- outcomeNameOrder
    
    tabletext <- cbind(
        c("Outcome","\n",as.character(data$outcomeName)), 
        
        c(sprintf("Target (n= %d)", data$targetSubjects[1]),"Event Rate, %/yr",
          sprintf("%.2f",data$targetOutcomes/data$targetDays*100*365) ),
        #c("\n","Mean followup days", round(data$targetDays/data$target,0)),
        
        c(sprintf("Comparator (n= %d)", data$comparatorSubjects[1]),"Event Rate, %/yr",
          sprintf("%.2f",data$comparatorOutcomes/data$comparatorDays*100*365)),
        #c("\n","Mean followup days", round(data$comparatorDays/data$comparator,0)), 
        c( "HR","(95% CI)", sprintf("%.2f \n (%.2f-%.2f)",data$rr, data$ci95Lb, data$ci95Ub) ),
        
        c(paste0("P"," value"),"\n", sprintf("%.3f",data$p) )
    )
    
    try({
        tiff(file.path(outputFolder,paste0("forestplot_Analysis",analysisIdOfInterest,".tiff")), width = 1050, height =600)
        forestplot::forestplot(labeltext=tabletext, 
                               graph.pos=5, 
                               graphwidth = unit(60,'mm'),
                               mean=c(NA,NA,data$rr), 
                               lower=c(NA,NA,data$ci95Lb), upper=c(NA,NA,data$ci95Ub),
                               title=analysis$description[analysis$analysisId==analysisIdOfInterest],
                               xlab="<--Ticagrelor Better--  --Clopidogrel Better--->",
                               xlog = TRUE,
                               clip=c(0.5,2.0), 
                               #xticks = c(0.5,1.0,2.0),
                               # hrzl_lines=list("3" = gpar(lwd=1, col="#99999922"), 
                               #                 "5" = gpar(lwd=60, lineend="butt", columns=c(2:6), col="#99999922"),
                               #                 "7" = gpar(lwd=60, lineend="butt", columns=c(2:6), col="#99999922"),
                               #                 "23" = gpar(lwd=60, lineend="butt", columns=c(2:6), col="#99999922"),
                               #                 "31" = gpar(lwd=60, lineend="butt", columns=c(2:6), col="#99999922")
                               #),
                               txt_gp=fpTxtGp(label=gpar(cex=1.1),
                                              ticks=gpar(cex=1.1),
                                              xlab=gpar(cex = 1.2),
                                              title=gpar(cex = 1.2)),
                               col=fpColors(box="black", lines="black", zero = "gray50"),
                               zero=1, cex=0.9, lineheight = "auto", boxsize=0.2, colgap=unit(6,"mm"),
                               lwd.ci=2, ci.vertices=TRUE, ci.vertices.height = 0.2
                               
        )
        dev.off()
    })
}

#From 2013 to 2015
for(analysisIdOfInterest in analysisIds){
    #From 2013 to 2015
    data <- analysisSummary %>% 
        filter (analysisId == analysisIdOfInterest) %>%
        filter (outcomeName %in% outcomeNameOfInterest) %>%
        filter (targetId == 1253 & comparatorId == 1254 )
    #reorder the data according to the outcome order
    data <- data[ match(outcomeNameOfInterest,data$outcomeName),]
    
    tabletext <- cbind(
        c("Outcome","\n",as.character(data$outcomeName)), 
        
        c(sprintf("Target (n= %d)", data$target[1]),"Event Rate, %/yr",sprintf("%.2f",data$eventsTarget/data$targetDays*100*365) ),
        #c("\n","Mean followup days", round(data$targetDays/data$target,0)),
        
        c(sprintf("Comparator (n= %d)", data$comparator[1]),"Event Rate, %/yr",sprintf("%.2f",data$eventsComparator/data$comparatorDays*100*365)),
        #c("\n","Mean followup days", round(data$comparatorDays/data$comparator,0)), 
        c( "HR","(95% CI)", sprintf("%.2f \n (%.2f-%.2f)",data$rr, data$ci95lb, data$ci95ub) ),
        
        c(paste0("P"," value"),"\n", sprintf("%.3f",data$p) )
    )
    
    try({
        tiff(file.path(outputFolder,paste0("forestplot_Analysis_2013_2015",analysisIdOfInterest,".tiff")), width = 1050, height =600)
        forestplot::forestplot(labeltext=tabletext, 
                               graph.pos=5, 
                               graphwidth = unit(60,'mm'),
                               mean=c(NA,NA,data$rr), 
                               lower=c(NA,NA,data$ci95lb), upper=c(NA,NA,data$ci95ub),
                               title=data$analysisDescription[1],
                               xlab="<--Ticagrelor Better--  --Clopidogrel Better--->",
                               xlog = TRUE,
                               clip=c(0.5,2.0), 
                               #xticks = c(0.5,1.0,2.0),
                               # hrzl_lines=list("3" = gpar(lwd=1, col="#99999922"), 
                               #                 "5" = gpar(lwd=60, lineend="butt", columns=c(2:6), col="#99999922"),
                               #                 "7" = gpar(lwd=60, lineend="butt", columns=c(2:6), col="#99999922"),
                               #                 "23" = gpar(lwd=60, lineend="butt", columns=c(2:6), col="#99999922"),
                               #                 "31" = gpar(lwd=60, lineend="butt", columns=c(2:6), col="#99999922")
                               #),
                               txt_gp=fpTxtGp(label=gpar(cex=1.1),
                                              ticks=gpar(cex=1.1),
                                              xlab=gpar(cex = 1.2),
                                              title=gpar(cex = 1.2)),
                               col=fpColors(box="black", lines="black", zero = "gray50"),
                               zero=1, cex=0.9, lineheight = "auto", boxsize=0.2, colgap=unit(6,"mm"),
                               lwd.ci=2, ci.vertices=TRUE, ci.vertices.height = 0.2
                               
        )
        dev.off()
    })
}

#From 2013 to 2015
for(analysisIdOfInterest in analysisIds){
    
    ##Narrow definition
    data <- analysisSummary %>% 
        filter (analysisId == analysisIdOfInterest) %>%
        filter (outcomeName %in% outcomeNameOfInterestNarrow)%>%
        filter (targetId == 874 & comparatorId == 929 )
    
    
    #reorder the data according to the outcome order
    data <- data[ match(outcomeNameOfInterestNarrow,data$outcomeName),]
    
    tabletext <- cbind(
        c("Outcome","\n",as.character(data$outcomeName)), 
        
        c(sprintf("Target (n= %d)", data$target[1]),"Event Rate, %/yr",sprintf("%.2f",data$eventsTarget/data$targetDays*100*365) ),
        #c("\n","Mean followup days", round(data$targetDays/data$target,0)),
        
        c(sprintf("Comparator (n= %d)", data$comparator[1]),"Event Rate, %/yr",sprintf("%.2f",data$eventsComparator/data$comparatorDays*100*365)),
        #c("\n","Mean followup days", round(data$comparatorDays/data$comparator,0)), 
        c( "HR","(95% CI)", sprintf("%.2f \n (%.2f-%.2f)",data$rr, data$ci95lb, data$ci95ub) ),
        
        c(paste0("P"," value"),"\n", sprintf("%.3f",data$p) )
    )
    
    try({
        tiff(file.path(outputFolder,paste0("forestplot_Analysis_Narrow",analysisIdOfInterest,".tiff")), width = 1050, height =600)
        forestplot::forestplot(labeltext=tabletext, 
                               graph.pos=5, 
                               graphwidth = unit(60,'mm'),
                               mean=c(NA,NA,data$rr), 
                               lower=c(NA,NA,data$ci95lb), upper=c(NA,NA,data$ci95ub),
                               title=data$analysisDescription[1],
                               xlab="<--Ticagrelor Better--  --Clopidogrel Better--->",
                               xlog = TRUE,
                               clip=c(0.5,2.0), 
                               #xticks = c(0.5,1.0,2.0),
                               # hrzl_lines=list("3" = gpar(lwd=1, col="#99999922"), 
                               #                 "5" = gpar(lwd=60, lineend="butt", columns=c(2:6), col="#99999922"),
                               #                 "7" = gpar(lwd=60, lineend="butt", columns=c(2:6), col="#99999922"),
                               #                 "23" = gpar(lwd=60, lineend="butt", columns=c(2:6), col="#99999922"),
                               #                 "31" = gpar(lwd=60, lineend="butt", columns=c(2:6), col="#99999922")
                               #),
                               txt_gp=fpTxtGp(label=gpar(cex=1.1),
                                              ticks=gpar(cex=1.1),
                                              xlab=gpar(cex = 1.2),
                                              title=gpar(cex = 1.2)),
                               col=fpColors(box="black", lines="black", zero = "gray50"),
                               zero=1, cex=0.9, lineheight = "auto", boxsize=0.2, colgap=unit(6,"mm"),
                               lwd.ci=2, ci.vertices=TRUE, ci.vertices.height = 0.2
                               
        )
        dev.off()
    })
    
}

for(outcomeIdSp in outcomeOfInterest){
    #outcomeIdSp <- outcomeOfInterest[2]
    data <- analysisSummary %>% 
        filter (outcomeId == outcomeIdSp) %>%
        filter (analysisId %in% analysisIdsOfInterest) %>%
        filter (targetId == 874 & comparatorId == 929 ) 
    
    data <- data[ match(analysisIdOrder,data$analysisId),]
    
    tabletext <- cbind(
        c("Analysis","\n",as.character(analysis$description[match(analysisIdOrder,analysis$analysisId)])), 
        
        c(sprintf("Target (n= %d)", data$targetSubjects[1]),"Event Rate, %/yr",
          sprintf("%.2f",data$targetOutcomes/data$targetDays*100*365) ),
        #c("\n","Mean followup days", round(data$targetDays/data$target,0)),
        
        c(sprintf("Comparator (n= %d)", data$comparatorSubjects[1]),"Event Rate, %/yr",
          sprintf("%.2f",data$comparatorOutcomes/data$comparatorDays*100*365)),
        #c("\n","Mean followup days", round(data$comparatorDays/data$comparator,0)), 
        c( "HR","(95% CI)", sprintf("%.2f \n (%.2f-%.2f)",data$rr, data$ci95Lb, data$ci95Ub) ),
        
        c(paste0("P"," value"),"\n", sprintf("%.3f",data$p) )
    )
    
    try({
        tiff(file.path(outputFolder,paste0("forestplot_Outcome",outcomes$outcomeName[outcomes$outcomeId==outcomeIdSp],".tiff")), width = 1300, height =600)
        forestplot::forestplot(labeltext=tabletext, 
                               graph.pos=5,
                               graphwidth = unit(60,'mm'),
                               mean=c(NA,NA,data$rr), 
                               lower=c(NA,NA,data$ci95Lb), upper=c(NA,NA,data$ci95Ub),
                               title=outcomes$outcomeName[outcomes$outcomeId==outcomeIdSp],
                               xlab="<--Ticagrelor Better--  --Clopidogrel Better--->",
                               xlog = TRUE,
                               clip=c(0.5,2.0), 
                               #xticks = c(0.5,1.0,2.0),
                               # hrzl_lines=list("3" = gpar(lwd=1, col="#99999922"), 
                               #                 "5" = gpar(lwd=60, lineend="butt", columns=c(2:6), col="#99999922"),
                               #                 "7" = gpar(lwd=60, lineend="butt", columns=c(2:6), col="#99999922"),
                               #                 "23" = gpar(lwd=60, lineend="butt", columns=c(2:6), col="#99999922"),
                               #                 "31" = gpar(lwd=60, lineend="butt", columns=c(2:6), col="#99999922")
                               #),
                               txt_gp=fpTxtGp(label=gpar(cex=1.1),
                                              ticks=gpar(cex=1.1),
                                              xlab=gpar(cex = 1.2),
                                              title=gpar(cex = 1.2)),
                               col=fpColors(box="black", lines="black", zero = "gray50"),
                               zero=1, cex=0.9, lineheight = "auto", boxsize=0.2, colgap=unit(6,"mm"),
                               lwd.ci=2, ci.vertices=TRUE, ci.vertices.height = 0.2
                               
        )
        dev.off()
    })
}

outputFolder<-"/Users/chan/OneDrive/Study/Ticagrelor/Result/181119"
cmOutput<-file.path(outputFolder, "cmOutput")
analysisSummary<-read.csv(file.path(outputFolder,"analysisSummary.csv"),stringsAsFactors =F)

outcomeOfInterest<-unique(analysisSummary$outcomeId[grepl("SCYou",analysisSummary$outcomeName)])

outcomeOrder <- c("Net Adverse Clinical Event", 
                  
                  "Ischemic event",
                  "Ischemic stroke",
                  "Revascularization",
                  "Acute myocardial infarction",
                  
                  "Hemorrhagic Event",
                  "Intracranial hemorrhage",
                  "GI bleeding",
                  
                  "SuddenCardiacDeath")

##Trim strings for outcome Name 
analysisSummary$outcomeName <- gsub("SCYou:","",analysisSummary$outcomeName)
analysisSummary$outcomeName <- gsub("SCYou","",analysisSummary$outcomeName)
analysisSummary$outcomeName <- gsub("outcome","",analysisSummary$outcomeName)
analysisSummary$outcomeName <- gsub("\\[","",analysisSummary$outcomeName)
analysisSummary$outcomeName <- gsub("]","",analysisSummary$outcomeName)
analysisSummary$outcomeName <- gsub(")","",analysisSummary$outcomeName)
analysisSummary$outcomeName <- gsub('\\(',"",analysisSummary$outcomeName)
analysisSummary$outcomeName <- trimws(analysisSummary$outcomeName)

##Trim strings for analysis Name 
analysisSummary$analysisDescription <- gsub("Time To First Post Index Event ","",analysisSummary$analysisDescription)
analysisSummary$analysisDescription <- gsub("Time to First Post Index Event ","",analysisSummary$analysisDescription)


for(analysisIdOfInterest in analysisIds){
    #analysisIdOfInterest <- analysisIds[1]
    data <- analysisSummary %>% 
        filter (analysisId == analysisIdOfInterest) %>%
        filter (outcomeId %in% outcomeOfInterest) %>%
        filter (targetId == 874 & comparatorId == 929 )
    
    #reorder the data according to the outcome order
    data <- data[ match(outcomeOrder,data$outcomeName),]
    
    tabletext <- cbind(
        c("Outcome","\n",as.character(data$outcomeName)), 
        
        c(sprintf("Target (n= %d)", data$target[1]),"Event Rate, %/yr",sprintf("%.2f",data$eventsTarget/data$targetDays*100*365,1) ),
        #c("\n","Mean followup days", round(data$targetDays/data$target,0)),
        
        c(sprintf("Comparator (n= %d)", data$comparator[1]),"Event Rate, %/yr",sprintf("%.2f",data$eventsComparator/data$comparatorDays*100*365,1)),
        #c("\n","Mean followup days", round(data$comparatorDays/data$comparator,0)), 
        c( "HR","(95% CI)", sprintf("%.2f \n (%.2f-%.2f)",data$rr, data$ci95lb, data$ci95ub) ),
        
        c(paste0("P"," value"),"\n", sprintf("%.3f",data$p) )
    )
    
    try({
        tiff(file.path(outputFolder,paste0("forestplot_Analysis",analysisIdOfInterest,".tiff")), width = 1050, height =600)
        forestplot::forestplot(labeltext=tabletext, 
                               graph.pos=5, 
                               graphwidth = unit(60,'mm'),
                               mean=c(NA,NA,data$rr), 
                               lower=c(NA,NA,data$ci95lb), upper=c(NA,NA,data$ci95ub),
                               title=data$analysisDescription[1],
                               xlab="<--Ticagrelor Better--  --Clopidogrel Better--->",
                               xlog = TRUE,
                               clip=c(0.5,2.0), 
                               #xticks = c(0.5,1.0,2.0),
                               # hrzl_lines=list("3" = gpar(lwd=1, col="#99999922"), 
                               #                 "5" = gpar(lwd=60, lineend="butt", columns=c(2:6), col="#99999922"),
                               #                 "7" = gpar(lwd=60, lineend="butt", columns=c(2:6), col="#99999922"),
                               #                 "23" = gpar(lwd=60, lineend="butt", columns=c(2:6), col="#99999922"),
                               #                 "31" = gpar(lwd=60, lineend="butt", columns=c(2:6), col="#99999922")
                               #),
                               txt_gp=fpTxtGp(label=gpar(cex=1.1),
                                              ticks=gpar(cex=1.1),
                                              xlab=gpar(cex = 1.2),
                                              title=gpar(cex = 1.2)),
                               col=fpColors(box="black", lines="black", zero = "gray50"),
                               zero=1, cex=0.9, lineheight = "auto", boxsize=0.2, colgap=unit(6,"mm"),
                               lwd.ci=2, ci.vertices=TRUE, ci.vertices.height = 0.2
                               
        )
        dev.off()
    })
    
}

for(outcomeIdSp in outcomeOfInterest){
    #outcomeIdSp <- outcomeOfInterest[3]
    data <- analysisSummary %>% 
        filter (outcomeId == outcomeIdSp) %>%
        filter (analysisId %in% analysisIds)
    
    tabletext <- cbind(
        c("Analysis","\n",as.character(data$analysisDescription)), 
        
        c("Cumulative outcome \n /Target population", "(%/year)", sprintf("%d/%d \n %.2f", data$eventsTarget,data$target,data$eventsTarget/data$targetDays*100*365)),
        
        c("Cumulative outcome \n /Comparator population", "(%/year)", sprintf("%d/%d \n %.2f", data$eventsComparator,data$comparator,data$eventsComparator/data$comparatorDays*100*365)),
        
        #c("\n","Mean followup days", round(data$comparatorDays/data$comparator,0)), 
        c( "HR","(95% CI)", sprintf("%.2f \n (%.2f-%.2f)",data$rr, data$ci95lb, data$ci95ub) ),
        
        c(paste0("P"," value"),"\n", sprintf("%.3f",data$p) )
    )
    try({
        tiff(file.path(outputFolder,paste0("forestplot_Outcome",outcomeIdSp,".tiff")), width = 1300, height =600)
        forestplot::forestplot(labeltext=tabletext, 
                               graph.pos=5,
                               graphwidth = unit(60,'mm'),
                               mean=c(NA,NA,data$rr), 
                               lower=c(NA,NA,data$ci95lb), upper=c(NA,NA,data$ci95ub),
                               title=data$outcomeName[1],
                               xlab="<--Ticagrelor Better--  --Clopidogrel Better--->",
                               xlog = TRUE,
                               clip=c(0.5,2.0), 
                               #xticks = c(0.5,1.0,2.0),
                               # hrzl_lines=list("3" = gpar(lwd=1, col="#99999922"), 
                               #                 "5" = gpar(lwd=60, lineend="butt", columns=c(2:6), col="#99999922"),
                               #                 "7" = gpar(lwd=60, lineend="butt", columns=c(2:6), col="#99999922"),
                               #                 "23" = gpar(lwd=60, lineend="butt", columns=c(2:6), col="#99999922"),
                               #                 "31" = gpar(lwd=60, lineend="butt", columns=c(2:6), col="#99999922")
                               #),
                               txt_gp=fpTxtGp(label=gpar(cex=1.1),
                                              ticks=gpar(cex=1.1),
                                              xlab=gpar(cex = 1.2),
                                              title=gpar(cex = 1.2)),
                               col=fpColors(box="black", lines="black", zero = "gray50"),
                               zero=1, cex=0.9, lineheight = "auto", boxsize=0.2, colgap=unit(6,"mm"),
                               lwd.ci=2, ci.vertices=TRUE, ci.vertices.height = 0.2
                               
        )
        dev.off()
    })
    
}
