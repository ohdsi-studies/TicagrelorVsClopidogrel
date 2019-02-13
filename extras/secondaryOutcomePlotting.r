library(forestplot)
library(dplyr)

outputFolder<-"C:/Users/apple/OneDrive/Study/Ticagrelor/Result/190124"

cmOutput<-file.path(outputFolder, "cmOutput")
outcomeModelRef<-readRDS(file.path(cmOutput,"outcomeModelReference.rds"))
analysisSummary<-read.csv(file.path(outputFolder,"analysisSummary.csv"),stringsAsFactors =F)


outcomeIds<-unique(outcomeModelRef$outcomeId)
analysisIds <- unique(outcomeModelRef$analysisId)
TcosOfInterest <- read.csv(system.file("settings","TcosOfInterest.csv", package = "TicagrelorVsClopidogrel"),stringsAsFactors =F)
outcomeOfInterest <- as.numeric(unlist(strsplit(TcosOfInterest$outcomeIds,';')))

outcomeOrder <- c("Net Adverse Clinical Event", 
                  
                  "Ischemic event",
                  "ischemic stroke",
                  "Revascularization",
                  "Acute MI",
                  
                  "Hemorrhagic Event",
                  "hemorrhagic stroke",
                  "GI bleeding",
                  
                  "dyspnea",
                  "any death")

for(analysisIdOfInterest in analysisIds){
    #analysisIdOfInterest <- analysisIds[1]
    data <- analysisSummary %>% 
        filter (analysisId == analysisIdOfInterest) %>%
        filter (outcomeId %in% outcomeOfInterest)
    
    #trim strings
    data$outcomeName <- gsub("SCYou:","",data$outcomeName)
    data$outcomeName <- gsub("outcome","",data$outcomeName)
    data$outcomeName <- gsub("and primary condition and first event","",data$outcomeName)
    data$outcomeName <- gsub("and primary condition and all event","",data$outcomeName)
    data$outcomeName <- gsub("inpatient or ED","",data$outcomeName)
    data$outcomeName <- gsub(")","",data$outcomeName)
    data$outcomeName <- gsub('\\(',"",data$outcomeName)
    data$outcomeName <- trimws(data$outcomeName)
    
    #reorder the data according to the outcome order
    data <- data[ match(outcomeOrder,data$outcomeName),]
    
    tabletext <- cbind(
        c("Outcome","\n",as.character(data$outcomeName)), 
        
        c(sprintf("Target (n= %d)", data$target[1]),"Event Rate, %/yr",sprintf("%.1f",data$eventsTarget/data$targetDays*100*365,1) ),
        #c("\n","Mean followup days", round(data$targetDays/data$target,0)),
        
        c(sprintf("Comparator (n= %d)", data$comparator[1]),"Event Rate, %/yr",sprintf("%.1f",data$eventsComparator/data$comparatorDays*100*365,1)),
        #c("\n","Mean followup days", round(data$comparatorDays/data$comparator,0)), 
        c( "HR","(95% CI)", sprintf("%.2f - %.2f",round(data$ci95lb,2), round(data$ci95ub,2)) ),
    
        c(paste0("P"," value"),"\n", sprintf("%.3f",data$p) )
    )
    
    
    tiff(file.path(outputFolder,paste0("forestplot_A",analysisIdOfInterest,".tiff")), width = 1000, height =750)
    forestplot::forestplot(labeltext=tabletext, 
                           graph.pos=5, 
                           mean=c(NA,NA,data$rr), 
                           lower=c(NA,NA,data$ci95lb), upper=c(NA,NA,data$ci95ub),
                           title=unique(data$analysisDescription),
                           xlab="     <---Ticagrelor Better---    ---Clopidogrel Better--->",
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
    
}