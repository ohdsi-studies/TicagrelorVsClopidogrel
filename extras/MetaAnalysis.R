#install.packages("forestplot")
library(EmpiricalCalibration)
library(meta)
library(forestplot)

# MetaResultFolder<-"/Users/chan/OneDrive/Study/OHDSI_HTN_combi/Results/metaanalysis"
# ResultFolder<-"/Users/chan/OneDrive/Study/OHDSI_HTN_combi/ExternalResults"
# outputFolder <- "/Users/chan/OneDrive/Study/OHDSI_HTN_combi/Results"

MetaResultFolder<-"C:/Users/apple/OneDrive/Study/OHDSI_HTN_combi/Results/metaanalysis"
ResultFolder<-"C:/Users/apple/OneDrive/Study/OHDSI_HTN_combi/ExternalResults"
outputFolder <- "C:/Users/apple/OneDrive/Study/OHDSI_HTN_combi/Results"
resultFolderList<-c(file.path(ResultFolder, "Optum StudyResults"),
                    file.path(ResultFolder, "CCAE"),
                    file.path(ResultFolder, "Medicare"),
                    file.path(ResultFolder, "Medicaid"),
                    file.path(ResultFolder, "Kor_NHIS_NSC"))

AnalysisIdSet<-c(30,180,365,730,18001,18002, 18011,18059,18061)
AnalysisNameSet<-c("30days",
                   "180days",
                   "365days",
                   "730days",
                   "Male",
                   "Female",
                   "without DM")
DataSourceSet<-c("CEDM","CCAE","Medicare","Medicaid","NHIS-NSC")

outcomeidset<-c(0, 2, 3,4, 4320)
outcomeNameSet<-c("mortality", "Myocardial Infarction", "Heart Failure", "Stroke", "MACCE")

#if (file.exists(file.path(ResultFolder,"meta_analysis.csv"))) file.remove(file.path(ResultFolder,"meta_analysis.csv"))
meta.summary<-data.frame()
for (i in seq(length(AnalysisIdSet))){
        #i<-2
        AnalysisId<-AnalysisIdSet[i]
        AnalysisName<-AnalysisNameSet[i]
        
        
    try({
            Optum<-read.csv(file.path(resultFolderList[1],AnalysisId,"tablesAndFigures/EmpiricalCalibration.csv"))
            CCAE<-read.csv(file.path(resultFolderList[2],AnalysisId,"tablesAndFigures/EmpiricalCalibration.csv"))
            medicare<-read.csv(file.path(resultFolderList[3],AnalysisId,"tablesAndFigures/EmpiricalCalibration.csv"))
            medicaid<-read.csv(file.path(resultFolderList[4],AnalysisId,"tablesAndFigures/EmpiricalCalibration.csv"))
            nhis<-read.csv(file.path(resultFolderList[5],AnalysisId,"tablesAndFigures/EmpiricalCalibration.csv"))
    })
        #AnalysisId<-18059
        if(AnalysisId==18059){
                AnalysisId<-18061
                AnalysisName<-"Younger than 60"
                Optum<-read.csv(file.path(resultFolderList[1],18061,"tablesAndFigures/EmpiricalCalibration.csv"))
                CCAE<-read.csv(file.path(resultFolderList[2],18059,"tablesAndFigures/EmpiricalCalibration.csv"))
                medicaid<-read.csv(file.path(resultFolderList[4],18061,"tablesAndFigures/EmpiricalCalibration.csv"))
                nhis<-read.csv(file.path(resultFolderList[5],18061,"tablesAndFigures/EmpiricalCalibration.csv"))
                dataSourceName<-c("CEDM",
                                  "CCAE",
                                  "Medicaid",
                                  "NHIS_NSC"
                )
                AnalysisId<-18059
        }
        #AnalysisId<-18061
        if (AnalysisId==18061){
                AnalysisId<-18059
                AnalysisName<-"60 or older"
                Optum<-read.csv(file.path(resultFolderList[1],18059,"tablesAndFigures/EmpiricalCalibration.csv"))
                CCAE<-read.csv(file.path(resultFolderList[2],18061,"tablesAndFigures/EmpiricalCalibration.csv"))
                medicare<-read.csv(file.path(resultFolderList[3],18059,"tablesAndFigures/EmpiricalCalibration.csv"))
                medicaid<-read.csv(file.path(resultFolderList[4],18059,"tablesAndFigures/EmpiricalCalibration.csv"))
                nhis<-read.csv(file.path(resultFolderList[5],18059,"tablesAndFigures/EmpiricalCalibration.csv"))
                
                AnalysisId<-18061
        }
                
    for (k in seq(length(outcomeidset))){
        #k<-1
        outcomeId <- outcomeidset[k]
        outcomeName <- outcomeNameSet[k]
        
        for (j in 1:3){
                #j<-1
                #j<-2
                #j<-3
            try(
                {
                    
            switch(j,
                   {targetid<-as.numeric(paste0(13,AnalysisId))
                   comparatorid<-as.numeric(paste0(14,AnalysisId))},
                   {targetid<-as.numeric(paste0(34,AnalysisId))
                   comparatorid<-as.numeric(paste0(13,AnalysisId))},
                   {targetid<-as.numeric(paste0(34,AnalysisId))
                   comparatorid<-as.numeric(paste0(14,AnalysisId))}
                   )
                        
                   #false_pos=sum(CCAE[( (CCAE$comparatorId==comparatorid)  &  (CCAE$targetId == targetid) & !(CCAE$outcomeId %in% c(outcomeidset, 1,6))), ]$p < 0.05, na.rm=TRUE)
                   #false_neg=sum(CCAE[( (CCAE$comparatorId==comparatorid)  &  (CCAE$targetId == targetid) & !(CCAE$outcomeId %in% c(outcomeidset, 1,6))), ]$p >= 0.05, na.rm=TRUE)
                   Optum_temp    <- Optum   [ Optum   $comparatorId==comparatorid & Optum   $targetId == targetid & Optum   $outcomeId == outcomeId, ]
                   CCAE_temp     <- CCAE    [ CCAE    $comparatorId==comparatorid & CCAE    $targetId == targetid & CCAE    $outcomeId == outcomeId, ]
                   medicare_temp <- medicare[ medicare$comparatorId==comparatorid & medicare$targetId == targetid & medicare$outcomeId == outcomeId, ]
                   medicaid_temp <- medicaid[ medicaid$comparatorId==comparatorid & medicaid$targetId == targetid & medicaid$outcomeId == outcomeId, ]
                   nhis_temp     <- nhis    [ nhis    $comparatorId==comparatorid & nhis    $targetId == targetid & nhis    $outcomeId == outcomeId, ]
                   
                   data <- rbind(Optum_temp, CCAE_temp, medicare_temp, medicaid_temp, nhis_temp)
                   
                   if(AnalysisId==18059) {
                           DataSourceSet<-c("CEDM","CCAE","Medicaid","NHIS-NSC") } else{
                                   DataSourceSet<-c("CEDM","CCAE","Medicare","Medicaid","NHIS-NSC")
                                   }
                   data$dataSourceName<-DataSourceSet
                   logRr=data$logRr
                   logLb95Ci = log( data$ci95lb )
                   logUb95Ci = log( data$ci95ub )
                   seLogRr <- (logUb95Ci-logLb95Ci) / (2 * qnorm(0.975))
                   
                   meta <- meta::metagen(logRr, seLogRr, studlab = DataSourceSet, sm = "RR", hakn = TRUE)
                   meta$n.e <- data$treated
                   meta$event.e <- data$eventsTreated
                   meta$event.rate.t <- round(with(data, eventsTreated   /(treatedDays   /365))*1000,1)
                   meta$person.year.t<-with(data, round((treatedDays   /365),0))
                   
                   meta$n.c <- data$comparator
                   meta$event.c <- data$eventsComparator
                   meta$event.rate.c <- round(with(data, eventsComparator/(comparatorDays/365))*1000,1)
                   meta$person.year.c<-with(data, round((comparatorDays/365),0))
                   
                   meta$HR <- with (data, paste0(sprintf("%.2f",round(rr,2)),
                                                 " (",sprintf("%.2f",round(ci95lb,2)), "-",
                                                 sprintf("%.2f",round(ci95ub,2)),")"))
                   #pdf(file.path(outputFolder,paste0(AnalysisId,"t",targetid,"c",comparatorid,"o",outcomeId,".pdf"))
                   #    ,paper = 'a4r',width=20, height=7)
                   
                   MetaForest<-forest.meta(meta,
                                       studlab=TRUE,
                                       pooled.events = TRUE,
                                       leftcols = c("studlab",
                                                    "n.e","event.e","person.year.t" ,"event.rate.t", 
                                                    "n.c","event.c","person.year.c","event.rate.c", 
                                                    "HR"),
                                       #lab.e = "Treated",
                                       #lab.c = "Comparator",
                                       lab.e.attach.to.col = c("n.e"),
                                       lab.c.attach.to.col = c("n.c"),
                                       leftlabs = c("Person-Years","Event rate", "Person-Years","Event rate", "HR (95% CI)"),
                                       rightcols = c("w.random"),
                                       comb.fixed = FALSE,
                                       comb.random = TRUE,
                                       text.random = "Overall",
                                       xlab = "Hazard Ratio (95% CI)"
                                       ,xlim = c(0.25,4.0)
                                       ,print.I2 = TRUE
                                       ,print.tau2 = FALSE
                                       #,print.Q = TRUE
                                       ,print.pval.Q = FALSE
                                       ,digits.I2 = 1
                                       #,digits.pval.Q = 3
                                       #,plotwidth ="5cm"
                           )
                   #dev.off()
                   
                   png(filename=file.path(outputFolder,paste0(AnalysisId,"t",targetid,"c",comparatorid,"o",outcomeId,".png"))
                   ,width=1200,height=300)
                   MetaForest<-forest.meta(meta,
                                           studlab=TRUE,
                                           pooled.events = TRUE,
                                           leftcols = c("studlab",
                                                        "n.e","event.e","person.year.t" ,"event.rate.t", 
                                                        "n.c","event.c","person.year.c","event.rate.c", 
                                                        "HR"),
                                           #lab.e = "Treated",
                                           #lab.c = "Comparator",
                                           lab.e.attach.to.col = c("n.e"),
                                           lab.c.attach.to.col = c("n.c"),
                                           leftlabs = c("Person-Years","Event rate", "Person-Years","Event rate", "HR (95% CI)"),
                                           rightcols = c("w.random"),
                                           comb.fixed = FALSE,
                                           comb.random = TRUE,
                                           text.random = "Overall",
                                           xlab = "Hazard Ratio (95% CI)",
                                           xlim = c(0.5,2)
                                           #,plotwidth ="10cm"
                   )
                   dev.off()
                   # rateTarget<-meta::metarate(event = eventsTreated,
                   #                                time = treatedDays/(365*1000), 
                   #                                data=data)
                   # rateComparator<-meta::metarate(event = eventsComparator,
                   #                            time = comparatorDays/(365*1000), 
                   #                            data=data)
                   # 
                   # exp(rateTarget$TE.random)                        
                   # exp(rateTarget$lower.random)
                   # exp(rateTarget$upper.random)
                   # 
                   # exp(rateComparator$TE.random)                        
                   # exp(rateComparator$lower.random)
                   # exp(rateComparator$upper.random)
                   
                   ##For calibrated
                   calibratedlogRr=data$logRr
                   calibratedlogLb95Ci = log(data$calibratedP_lb95ci)
                   calibratedlogUb95Ci = log(data$calibratedP_ub95ci)
                   
                   z = -0.862 + sqrt(0.743 -2.404 * log(data$p))
                   se = data$logRr / z
                   log(se)
                   CalLb95Ci = data$logRr - 1.96 * se
                   CalUb95Ci = data$logRr + 1.96 * se
                   data
                   exp(CalLb95Ci)
                   exp(CalUb95Ci)
                   
                   calibratedseLogRr <- (calibratedlogUb95Ci-calibratedlogLb95Ci) / (2 * qnorm(0.975))
                   meta <- meta::metagen(calibratedlogRr, calibratedseLogRr, studlab = DataSourceSet, sm = "RR", hakn = TRUE)
                   
                   meta$HR <- with (data, paste0(sprintf("%.2f",round(rr,2)),
                                                 " (",sprintf("%.2f",round(calibratedP_lb95ci,2)), "-",
                                                 sprintf("%.2f",round(calibratedP_ub95ci,2)),")"))
                   #pdf(file.path(outputFolder,paste0("Cal",AnalysisId,"t",targetid,"c",comparatorid,"o",outcomeId,".pdf"))
                   #    ,paper = 'a4r',width=20, height=7)
                   
                   
                   Cal.MetaForest<-forest.meta(meta,
                                           studlab=TRUE,
                                           pooled.events = TRUE,
                                           leftcols = c("studlab", "HR"),
                                           #lab.e = "Treated",
                                           #lab.c = "Comparator",
                                           lab.e.attach.to.col = c("n.e"),
                                           lab.c.attach.to.col = c("n.c"),
                                           leftlabs = c("HR (95% CI)"),
                                           rightcols = c("w.random"),
                                           comb.fixed = FALSE,
                                           comb.random = TRUE,
                                           text.random = "Overall",
                                           xlab = "Hazard Ratio (95% CI)"
                                           ,xlim = c(0.25,4.0)
                                           ,print.I2 = TRUE
                                           ,print.tau2 = FALSE
                                           #,print.Q = TRUE
                                           ,print.pval.Q = FALSE
                                           ,digits.I2 = 1
                                           #,digits.pval.Q = 3
                                           #,plotwidth ="5cm"
                   )
                   dev.off()
                   
                   png(file.path(outputFolder,paste0("Cal",AnalysisId,"t",targetid,"c",comparatorid,"o",outcomeId,".png"))
                       ,width=1200,height=500)
                   Cal.MetaForest<-forest.meta(meta,
                                               studlab=TRUE,
                                               pooled.events = TRUE,
                                               leftcols = c("studlab", "HR"),
                                               #lab.e = "Treated",
                                               #lab.c = "Comparator",
                                               lab.e.attach.to.col = c("n.e"),
                                               lab.c.attach.to.col = c("n.c"),
                                               leftlabs = c("HR (95% CI)"),
                                               rightcols = c("w.random"),
                                               comb.fixed = FALSE,
                                               comb.random = TRUE,
                                               text.random = "Overall",
                                               xlab = "Hazard Ratio (95% CI)"
                                               ,xlim = c(0.25,4.0)
                                               ,print.I2 = TRUE
                                               ,print.tau2 = FALSE
                                               #,print.Q = TRUE
                                               ,print.pval.Q = FALSE
                                               ,digits.I2 = 1
                                               #,digits.pval.Q = 3
                                               #,plotwidth ="5cm"
                   )
                   dev.off()
                   # df=data.frame(AnalysisId = AnalysisId,
                   #               AnalysisName = AnalysisName,
                   #               
                   #               outcomeid = outcomeid,
                   #               outcomeName = outcomeName,
                   #               
                   #               targetid = targetid,
                   #               targetName = unique(as.character(data$targetName)),
                   #               totalNoTarget = sum(data$treated),
                   #               totalDaysTarget = sum(data$treatedDays),
                   #               eventTarget = sum(data$eventsTreated),
                   #               #metarateTarget = 
                   #               
                   #               comparatorid = comparatorid,
                   #               comparatorName = unique(as.character(data$comparatorName)),
                   #               totalNoComparator = sum(data$comparator),
                   #               totalDaysComparator = sum(data$comparatorDays),
                   #               eventComparator = sum(data$eventsComparator),
                   #               
                   #               HazardRatio = round(exp(s$random$TE),3),
                   #               lower.ci = round(exp(s$random$lower),3),
                   #               upper.ci=round(exp(s$random$upper),3),
                   #               p.value = round(s$random$p,3),
                   #               
                   #               calibrated.lower.ci = round(exp(calibrated.s$random$lower),3),
                   #               calibrated.upper.ci=round(exp(calibrated.s$random$upper),3),
                   #               calibrated.p.value = round(calibrated.s$random$p,3)
                   #               )
                   # meta.summary<-rbind(meta.summary,df)
                   # 
                   # ##Meta-analysis for Incidence rate Difference 
                   # metaIRD<-metainc(event.e = eventsTreated, 
                   #                  time.e = treatedDays/(365*1000),
                   #                  event.c = eventsComparator,
                   #                  time.c=comparatorDays/(365*1000),
                   #                  data=result_temp,
                   #                  method = "MH", #Haenszel 
                   #                  sm = "IRD"
                   # )
                   # ird.df=data.frame(IRD = metaIRD$TE.random,
                   #                   lower.ci = metaIRD$lower.random,
                   #                   upper.ci = metaIRD$upper.random,
                   #                   IRD.p = metaIRD$pval.random,
                   #                   I2 = metaIRD$I2
                   #                   )
                   # 
                   # meta.IRD.summary<-rbind(meta.IRD.summary,ird.df)
                   # 
                   # 
                   # pdf(file.path(outputFolder,paste0(AnalysisId,"t",targetid,"c",comparatorid,"o",outcomeid,".pdf")))
                   # plotMetaAnalysisForest(logRr = logRr,
                   #                        logLb95Ci = logLb95Ci,
                   #                        logUb95Ci = logUb95Ci,
                   #                        labels = dataSourceName,
                   #                        limits = c(0.2, 5)
                   # )
                   # dev.off()
                   # png(file.path(outputFolder,paste0(AnalysisId,"t",targetid,"c",comparatorid,"o",outcomeid,".png")))
                   # plotMetaAnalysisForest(logRr = logRr,
                   #                        logLb95Ci = logLb95Ci,
                   #                        logUb95Ci = logUb95Ci,
                   #                        labels = dataSourceName,
                   #                        limits = c(0.2, 5))
                   # dev.off()
                   # 
                   # pdf(file.path(outputFolder,"calibrated",paste0(AnalysisId,"t",targetid,"c",comparatorid,"o",outcomeid,".pdf")))
                   # plotMetaAnalysisForest(logRr = logRr,
                   #                        logLb95Ci = calibratedlogLb95Ci,
                   #                        logUb95Ci = calibratedlogUb95Ci,
                   #                        labels = dataSourceName,
                   #                        limits = c(0.2, 5)
                   # )
                   # dev.off()
                   # png(file.path(outputFolder,"calibrated",paste0(AnalysisId,"t",targetid,"c",comparatorid,"o",outcomeid,".png")))
                   # plotMetaAnalysisForest(logRr = logRr,
                   #                        logLb95Ci = calibratedlogLb95Ci,
                   #                        logUb95Ci = calibratedlogUb95Ci,
                   #                        labels = dataSourceName,
                   #                        limits = c(0.2, 5))
                   # dev.off()
                }
            )
        }
    }
    
}

primaryOutcome<-c(0)
AnalysisId <- 180
targetIdSet    <- c(13180, 34180, 34180)
comparatorIdSet<- c(14180, 13180, 14180)
outcomeOfInterest <- c(0,1,2,3,4,6,4320)


for (i in 1:3){
        targetid<-targetIdSet[i]
        comparatorid<-comparatorIdSet[i]
        
        for (DataSource in DataSourceSet){
                DataPath= switch(DataSource,"CCAE" = "CCAE", "Optum" = "Optum StudyResults", "Medicare" = "Medicare", "Medicaid" = "Medicaid",
                                 "NHIS-NSC"="Kor_NHIS_NSC")
                #print(file.path(ResultFolder, file.path(DataPath, AnalysisId,"tablesAndFigures","EmpiricalCalibration.csv")))
                result<-read.csv(file.path(ResultFolder, file.path(DataPath, AnalysisId,"tablesAndFigures","EmpiricalCalibration.csv")))
                
                #filter negative outcomes only
                Neg.result<-result %>% filter (targetId == targetid, comparatorId == comparatorid, !outcomeId %in% outcomeOfInterest)
                Pri.result<-result %>% filter (targetId == targetid, comparatorId == comparatorid, outcomeId  %in% primaryOutcome)
                
                null <- EmpiricalCalibration::fitMcmcNull(Neg.result$logRr, Neg.result$seLogRr)
                EmpiricalCalibration::plotCalibrationEffect(logRrNegatives = Neg.result$logRr,
                                                            seLogRrNegatives = Neg.result$seLogRr,
                                                            logRrPositives = Pri.result$logRr,
                                                            seLogRrPositives = Pri.result$seLogRr,
                                                            null = null,
                                                            fileName = file.path(outputFolder,
                                                                                 paste0("NegPlot_",DataSource,"_a",AnalysisId, "_t", targetid, "_c", comparatorid, ".png")
                                                            ))
                
        }
}

plotMetaAnalysisForest <- function(logRr, 
                                   logLb95Ci, 
                                   logUb95Ci, 
                                   labels, 
                                   xLabel = "Relative risk", 
                                   limits = c(0.1, 10), 
                                   hakn = TRUE,
                                   fileName = NULL) {
        seLogRr <- (logUb95Ci-logLb95Ci) / (2 * qnorm(0.975))
        meta <- meta::metagen(logRr, seLogRr, studlab = labels, sm = "RR", hakn = hakn)
        #meta$w.random/sum(meta$w.random)
        s <- summary(meta)
        print(s)
        rnd <- s$random
        summaryLabel <- sprintf("Summary (I\u00B2 = %.2f)", s$I2$TE)
        d1 <- data.frame(logRr = -100,
                         logLb95Ci = -100,
                         logUb95Ci = -100,
                         name = "Source",
                         type = "header")
        d2 <- data.frame(logRr = logRr,
                         logLb95Ci = logLb95Ci,
                         logUb95Ci = logUb95Ci,
                         name = labels,
                         type = "db")
        d3 <- data.frame(logRr = rnd$TE,
                         logLb95Ci = rnd$lower,
                         logUb95Ci = rnd$upper,
                         name = summaryLabel,
                         type = "ma")
        
        d <- rbind(d1, d2, d3)
        d$name <- factor(d$name, levels = c(summaryLabel, rev(as.character(labels)), "Source"))
        
        breaks <- c(0.1, 0.25, 0.5, 1, 2, 4, 6, 8, 10)
        p <- ggplot2::ggplot(d,ggplot2::aes(x = exp(logRr), y = name, xmin = exp(logLb95Ci), xmax = exp(logUb95Ci))) +
                ggplot2::geom_vline(xintercept = breaks, colour = "#AAAAAA", lty = 1, size = 0.2) +
                ggplot2::geom_vline(xintercept = 1, size = 0.5) +
                ggplot2::geom_errorbarh(height = 0.15) +
                ggplot2::geom_point(size=3, shape = 23, ggplot2::aes(fill=type)) +
                ggplot2::scale_fill_manual(values = c("#000000", "#000000", "#FFFFFF")) +
                ggplot2::scale_x_continuous(xLabel, trans = "log10", breaks = breaks, labels = breaks) +
                ggplot2::coord_cartesian(xlim = limits) +
                ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                               panel.grid.minor = ggplot2::element_blank(),
                               panel.background = ggplot2::element_blank(),
                               legend.position = "none",
                               panel.border = ggplot2::element_blank(),
                               axis.text.y = ggplot2::element_blank(),
                               axis.title.y = ggplot2::element_blank(),
                               axis.ticks = ggplot2::element_blank(),
                               plot.margin = grid::unit(c(0,0,0.1,0), "lines"))
        
        labels <- paste0(formatC(exp(d$logRr),  digits = 2, format = "f"),
                         " (",
                         formatC(exp(d$logLb95Ci), digits = 2, format = "f"),
                         "-",
                         formatC(exp(d$logUb95Ci), digits = 2, format = "f"),
                         ")")
        
        labels <- data.frame(y = rep(d$name, 2),
                             x = rep(1:2, each = nrow(d)),
                             label = c(as.character(d$name), labels),
                             stringsAsFactors = FALSE)
        labels$label[nrow(d) + 1] <-  paste(xLabel,"(95% CI)")
        data_table <- ggplot2::ggplot(labels, ggplot2::aes(x = x, y = y, label = label)) +
                ggplot2::geom_text(size = 4, hjust=0, vjust=0.5) +
                ggplot2::geom_hline(ggplot2::aes(yintercept=nrow(d) - 0.5)) +
                ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                               panel.grid.minor = ggplot2::element_blank(),
                               legend.position = "none",
                               panel.border = ggplot2::element_blank(),
                               panel.background = ggplot2::element_blank(),
                               axis.text.x = ggplot2::element_text(colour="white"),
                               axis.text.y = ggplot2::element_blank(),
                               axis.ticks = ggplot2::element_line(colour="white"),
                               plot.margin = grid::unit(c(0,0,0.1,0), "lines")) +
                ggplot2::labs(x="",y="") +
                ggplot2::coord_cartesian(xlim=c(1,3))
        
        plot <- gridExtra::grid.arrange(data_table, p, ncol=2)
        
        if (!is.null(fileName))
                ggplot2::ggsave(fileName, plot, width = 7, height = 1 + length(logRr) * 0.3, dpi = 400)
        return(plot)
}



##For younger patients

AnalysisId<-18061
AnalysisName<-"Younger than 60"
CCAE<-read.csv(file.path(resultFolderList[1],18059,"tablesAndFigures/EmpiricalCalibration.csv"))
Optum<-read.csv(file.path(resultFolderList[2],18061,"tablesAndFigures/EmpiricalCalibration.csv"))
medicaid<-read.csv(file.path(resultFolderList[4],18061,"tablesAndFigures/EmpiricalCalibration.csv"))
nhis<-read.csv(file.path(resultFolderList[5],18061,"tablesAndFigures/EmpiricalCalibration.csv"))
dataSourceName<-c("CEDM",
                  "CCAE",
                  "Medicaid",
                  "NHIS_NSC"
)


logRr=result_temp$logRr
logLb95Ci = log( result_temp$ci95lb )
logUb95Ci = log( result_temp$ci95ub )

seLogRr <- (logUb95Ci-logLb95Ci) / (2 * qnorm(0.975))
meta <- meta::metagen(logRr, seLogRr, studlab = dataSourceName, sm = "RR", hakn = TRUE)
s <- summary(meta)


str(meta)

s$I2$TE
s$I2$lower
s$I2$upper

AnalysisId<-18059
for (k in seq(length(outcomeidset))){
    #k<-1
    outcomeid <- outcomeidset[k]
    outcomeName <- outcomeNameSet[k]
    
    
    CCAE_outcome<-CCAE[CCAE$outcomeId == outcomeid,]
    Optum_outcome<-Optum[Optum$outcomeId == outcomeid,]
    medicaid_outcome<-medicaid[medicaid$outcomeId == outcomeid, ]
    nhis_outcome<-nhis[nhis$outcomeId == outcomeid,]
    
    
    for (j in 1:3){
        try(
            {
                
                switch(j,
                       {targetid<-as.numeric(paste0(13,18059))
                       comparatorid<-as.numeric(paste0(14,18059))
                       
                       #false_pos=sum(CCAE[( (CCAE$comparatorId==comparatorid)  &  (CCAE$targetId == targetid) & !(CCAE$outcomeId %in% c(outcomeidset, 1,6))), ]$p < 0.05, na.rm=TRUE)
                       #false_neg=sum(CCAE[( (CCAE$comparatorId==comparatorid)  &  (CCAE$targetId == targetid) & !(CCAE$outcomeId %in% c(outcomeidset, 1,6))), ]$p >= 0.05, na.rm=TRUE)
                       
                       CCAE_temp <- CCAE_outcome[ (CCAE_outcome$comparatorId==comparatorid & CCAE_outcome$targetId == targetid), ]
                       Optum_temp <- Optum_outcome[ (Optum_outcome$comparatorId==comparatorid & Optum_outcome$targetId == targetid), ]
                       
                       medicaid_temp <- medicaid_outcome[ (medicaid_outcome$comparatorId==comparatorid & medicaid_outcome$targetId == targetid), ]
                       nhis_temp <- nhis_outcome[ (nhis_outcome$comparatorId==comparatorid & nhis_outcome$targetId == targetid), ]
                       
                       result_temp <- rbind(Optum_temp, CCAE_temp, medicaid_temp, nhis_temp)
                       result_temp$dataSourceName<-dataSourceName
                       
                       logRr=result_temp$logRr
                       logLb95Ci = log( result_temp$ci95lb )
                       logUb95Ci = log( result_temp$ci95ub )
                       
                       seLogRr <- (logUb95Ci-logLb95Ci) / (2 * qnorm(0.975))
                       meta <- meta::metagen(logRr, seLogRr, studlab = dataSourceName, sm = "RR", hakn = TRUE)
                       s <- summary(meta)
                       
                       calibratedlogRr=result_temp$logRr
                       calibratedlogLb95Ci = log( result_temp$calibratedP_lb95ci )
                       calibratedlogUb95Ci = log( result_temp$calibratedP_ub95ci )
                       
                       calibratedseLogRr <- (calibratedlogUb95Ci-calibratedlogLb95Ci) / (2 * qnorm(0.975))
                       meta <- meta::metagen(logRr, calibratedseLogRr, studlab = dataSourceName, sm = "RR", hakn = TRUE)
                       calibrated.s <- summary(meta)
                       
                       df=data.frame(AnalysisId = AnalysisId,
                                     AnalysisName = AnalysisName,
                                     
                                     outcomeid = outcomeid,
                                     outcomeName = outcomeName,
                                     
                                     targetid = targetid,
                                     targetName = unique(as.character(result_temp$targetName)),
                                     totalNoTarget = sum(result_temp$treated),
                                     totalDaysTarget = sum(result_temp$treatedDays),
                                     eventTarget = sum(result_temp$eventsTreated),
                                     
                                     comparatorid = comparatorid,
                                     comparatorName = unique(as.character(result_temp$comparatorName)),
                                     totalNoComparator = sum(result_temp$comparator),
                                     totalDaysComparator = sum(result_temp$comparatorDays),
                                     eventComparator = sum(result_temp$eventsComparator),
                                     
                                     HazardRatio = round(exp(s$random$TE),3),
                                     lower.ci = round(exp(s$random$lower),3),
                                     upper.ci=round(exp(s$random$upper),3),
                                     p.value = round(s$random$p,3),
                                     
                                     calibrated.lower.ci = round(exp(calibrated.s$random$lower),3),
                                     calibrated.upper.ci=round(exp(calibrated.s$random$upper),3),
                                     calibrated.p.value = round(calibrated.s$random$p,3)
                       )
                       meta.summary<-rbind(meta.summary,df)
                       
                       pdf(file.path(ResultFolder,paste0(AnalysisId,"t",targetid,"c",comparatorid,"o",outcomeid,".pdf")))
                       plotMetaAnalysisForest(logRr = logRr,
                                              logLb95Ci = logLb95Ci,
                                              logUb95Ci = logUb95Ci,
                                              labels = dataSourceName,
                                              limits = c(0.2, 5)
                       )
                       dev.off()
                       png(file.path(ResultFolder,paste0(AnalysisId,"t",targetid,"c",comparatorid,"o",outcomeid,".png")))
                       plotMetaAnalysisForest(logRr = logRr,
                                              logLb95Ci = logLb95Ci,
                                              logUb95Ci = logUb95Ci,
                                              labels = dataSourceName,
                                              limits = c(0.2, 5))
                       dev.off()
                       
                       pdf(file.path(ResultFolder,"calibrated",paste0(AnalysisId,"t",targetid,"c",comparatorid,"o",outcomeid,".pdf")))
                       plotMetaAnalysisForest(logRr = logRr,
                                              logLb95Ci = calibratedlogLb95Ci,
                                              logUb95Ci = calibratedlogUb95Ci,
                                              labels = dataSourceName,
                                              limits = c(0.2, 5)
                       )
                       dev.off()
                       png(file.path(ResultFolder,"calibrated",paste0(AnalysisId,"t",targetid,"c",comparatorid,"o",outcomeid,".png")))
                       plotMetaAnalysisForest(logRr = logRr,
                                              logLb95Ci = calibratedlogLb95Ci,
                                              logUb95Ci = calibratedlogUb95Ci,
                                              labels = dataSourceName,
                                              limits = c(0.2, 5))
                       dev.off()
                       
                       },
                       {
                           targetid<-as.numeric(paste0(34,AnalysisId))
                           comparatorid<-as.numeric(paste0(13,AnalysisId))
                           
                           
                           #false_pos=sum(CCAE[( (CCAE$comparatorId==comparatorid)  &  (CCAE$targetId == targetid) & !(CCAE$outcomeId %in% c(outcomeidset, 1,6))), ]$p < 0.05, na.rm=TRUE)
                           #false_neg=sum(CCAE[( (CCAE$comparatorId==comparatorid)  &  (CCAE$targetId == targetid) & !(CCAE$outcomeId %in% c(outcomeidset, 1,6))), ]$p >= 0.05, na.rm=TRUE)
                           
                           CCAE_temp <- CCAE_outcome[ (CCAE_outcome$comparatorId==comparatorid & CCAE_outcome$targetId == targetid), ]
                           Optum_temp <- Optum_outcome[ (Optum_outcome$comparatorId==comparatorid & Optum_outcome$targetId == targetid), ]
                           medicaid_temp <- medicaid_outcome[ (medicaid_outcome$comparatorId==comparatorid & medicaid_outcome$targetId == targetid), ]
                           nhis_temp <- nhis_outcome[ (nhis_outcome$comparatorId==comparatorid & nhis_outcome$targetId == targetid), ]
                           
                           result_temp <- rbind(CCAE_temp, Optum_temp, medicaid_temp, nhis_temp)
                           result_temp$dataSourceName<-dataSourceName
                           
                           logRr=result_temp$logRr
                           logLb95Ci = log( result_temp$ci95lb )
                           logUb95Ci = log( result_temp$ci95ub )
                           
                           seLogRr <- (logUb95Ci-logLb95Ci) / (2 * qnorm(0.975))
                           meta <- meta::metagen(logRr, seLogRr, studlab = dataSourceName, sm = "RR", hakn = TRUE)
                           s <- summary(meta)
                           
                           calibratedlogRr=result_temp$logRr
                           calibratedlogLb95Ci = log( result_temp$calibratedP_lb95ci )
                           calibratedlogUb95Ci = log( result_temp$calibratedP_ub95ci )
                           
                           calibratedseLogRr <- (calibratedlogUb95Ci-calibratedlogLb95Ci) / (2 * qnorm(0.975))
                           meta <- meta::metagen(logRr, calibratedseLogRr, studlab = dataSourceName, sm = "RR", hakn = TRUE)
                           calibrated.s <- summary(meta)
                           
                           df=data.frame(AnalysisId = AnalysisId,
                                         AnalysisName = AnalysisName,
                                         
                                         outcomeid = outcomeid,
                                         outcomeName = outcomeName,
                                         
                                         targetid = targetid,
                                         targetName = unique(as.character(result_temp$targetName)),
                                         totalNoTarget = sum(result_temp$treated),
                                         totalDaysTarget = sum(result_temp$treatedDays),
                                         eventTarget = sum(result_temp$eventsTreated),
                                         
                                         comparatorid = comparatorid,
                                         comparatorName = unique(as.character(result_temp$comparatorName)),
                                         totalNoComparator = sum(result_temp$comparator),
                                         totalDaysComparator = sum(result_temp$comparatorDays),
                                         eventComparator = sum(result_temp$eventsComparator),
                                         
                                         HazardRatio = round(exp(s$random$TE),3),
                                         lower.ci = round(exp(s$random$lower),3),
                                         upper.ci=round(exp(s$random$upper),3),
                                         p.value = round(s$random$p,3),
                                         
                                         calibrated.lower.ci = round(exp(calibrated.s$random$lower),3),
                                         calibrated.upper.ci=round(exp(calibrated.s$random$upper),3),
                                         calibrated.p.value = round(calibrated.s$random$p,3)
                           )
                           meta.summary<-rbind(meta.summary,df)
                           # if (file.exists(file.path(ResultFolder,"meta_analysis.csv"))){
                           #     write.table(df, file.path(ResultFolder,"meta_analysis.csv"),sep=",",col.names = FALSE, append=TRUE)
                           # }else {
                           #     write.table(df, file.path(ResultFolder,"meta_analysis.csv"),sep=",", col.names = TRUE, append=TRUE)
                           # }
                           
                           pdf(file.path(ResultFolder,paste0(AnalysisId,"t",targetid,"c",comparatorid,"o",outcomeid,".pdf")))
                           plotMetaAnalysisForest(logRr = logRr,
                                                  logLb95Ci = logLb95Ci,
                                                  logUb95Ci = logUb95Ci,
                                                  labels = dataSourceName,
                                                  limits = c(0.2, 5)
                           )
                           dev.off()
                           png(file.path(ResultFolder,paste0(AnalysisId,"t",targetid,"c",comparatorid,"o",outcomeid,".png")))
                           plotMetaAnalysisForest(logRr = logRr,
                                                  logLb95Ci = logLb95Ci,
                                                  logUb95Ci = logUb95Ci,
                                                  labels = dataSourceName,
                                                  limits = c(0.2, 5))
                           dev.off()
                           
                           pdf(file.path(ResultFolder,"calibrated",paste0(AnalysisId,"t",targetid,"c",comparatorid,"o",outcomeid,".pdf")))
                           plotMetaAnalysisForest(logRr = logRr,
                                                  logLb95Ci = calibratedlogLb95Ci,
                                                  logUb95Ci = calibratedlogUb95Ci,
                                                  labels = dataSourceName,
                                                  limits = c(0.2, 5)
                           )
                           dev.off()
                           png(file.path(ResultFolder,"calibrated",paste0(AnalysisId,"t",targetid,"c",comparatorid,"o",outcomeid,".png")))
                           plotMetaAnalysisForest(logRr = logRr,
                                                  logLb95Ci = calibratedlogLb95Ci,
                                                  logUb95Ci = calibratedlogUb95Ci,
                                                  labels = dataSourceName,
                                                  limits = c(0.2, 5))
                           dev.off()
                           
                       },
                       {targetid<-as.numeric(paste0(34,AnalysisId))
                       comparatorid<-as.numeric(paste0(14,AnalysisId))
                       
                       #false_pos=sum(CCAE[( (CCAE$comparatorId==comparatorid)  &  (CCAE$targetId == targetid) & !(CCAE$outcomeId %in% c(outcomeidset, 1,6))), ]$p < 0.05, na.rm=TRUE)
                       #false_neg=sum(CCAE[( (CCAE$comparatorId==comparatorid)  &  (CCAE$targetId == targetid) & !(CCAE$outcomeId %in% c(outcomeidset, 1,6))), ]$p >= 0.05, na.rm=TRUE)
                       
                       CCAE_temp <- CCAE_outcome[ (CCAE_outcome$comparatorId==comparatorid & CCAE_outcome$targetId == targetid), ]
                       Optum_temp <- Optum_outcome[ (Optum_outcome$comparatorId==comparatorid & Optum_outcome$targetId == targetid), ]
                       medicaid_temp <- medicaid_outcome[ (medicaid_outcome$comparatorId==comparatorid & medicaid_outcome$targetId == targetid), ]
                       nhis_temp <- nhis_outcome[ (nhis_outcome$comparatorId==comparatorid & nhis_outcome$targetId == targetid), ]
                       
                       result_temp <- rbind(CCAE_temp, Optum_temp, medicaid_temp, nhis_temp)
                       result_temp$dataSourceName<-dataSourceName
                       
                       logRr=result_temp$logRr
                       logLb95Ci = log( result_temp$ci95lb )
                       logUb95Ci = log( result_temp$ci95ub )
                       
                       seLogRr <- (logUb95Ci-logLb95Ci) / (2 * qnorm(0.975))
                       meta <- meta::metagen(logRr, seLogRr, studlab = dataSourceName, sm = "RR", hakn = TRUE)
                       s <- summary(meta)
                       
                       calibratedlogRr=result_temp$logRr
                       calibratedlogLb95Ci = log( result_temp$calibratedP_lb95ci )
                       calibratedlogUb95Ci = log( result_temp$calibratedP_ub95ci )
                       
                       calibratedseLogRr <- (calibratedlogUb95Ci-calibratedlogLb95Ci) / (2 * qnorm(0.975))
                       meta <- meta::metagen(logRr, calibratedseLogRr, studlab = dataSourceName, sm = "RR", hakn = TRUE)
                       calibrated.s <- summary(meta)
                       
                       df=data.frame(AnalysisId = AnalysisId,
                                     AnalysisName = AnalysisName,
                                     
                                     outcomeid = outcomeid,
                                     outcomeName = outcomeName,
                                     
                                     targetid = targetid,
                                     targetName = unique(as.character(result_temp$targetName)),
                                     totalNoTarget = sum(result_temp$treated),
                                     totalDaysTarget = sum(result_temp$treatedDays),
                                     eventTarget = sum(result_temp$eventsTreated),
                                     
                                     comparatorid = comparatorid,
                                     comparatorName = unique(as.character(result_temp$comparatorName)),
                                     totalNoComparator = sum(result_temp$comparator),
                                     totalDaysComparator = sum(result_temp$comparatorDays),
                                     eventComparator = sum(result_temp$eventsComparator),
                                     
                                     HazardRatio = round(exp(s$random$TE),3),
                                     lower.ci = round(exp(s$random$lower),3),
                                     upper.ci=round(exp(s$random$upper),3),
                                     p.value = round(s$random$p,3),
                                     
                                     calibrated.lower.ci = round(exp(calibrated.s$random$lower),3),
                                     calibrated.upper.ci=round(exp(calibrated.s$random$upper),3),
                                     calibrated.p.value = round(calibrated.s$random$p,3)
                       )
                       meta.summary<-rbind(meta.summary,df)
                       # if (file.exists(file.path(ResultFolder,"meta_analysis.csv"))){
                       #     write.table(df, file.path(ResultFolder,"meta_analysis.csv"),sep=",",col.names = FALSE, append=TRUE)
                       # }else {
                       #     write.table(df, file.path(ResultFolder,"meta_analysis.csv"),sep=",", col.names = TRUE, append=TRUE)
                       # }
                       
                       pdf(file.path(ResultFolder,paste0(AnalysisId,"t",targetid,"c",comparatorid,"o",outcomeid,".pdf")))
                       plotMetaAnalysisForest(logRr = logRr,
                                              logLb95Ci = logLb95Ci,
                                              logUb95Ci = logUb95Ci,
                                              labels = dataSourceName,
                                              limits = c(0.2, 5)
                       )
                       dev.off()
                       png(file.path(ResultFolder,paste0(AnalysisId,"t",targetid,"c",comparatorid,"o",outcomeid,".png")))
                       plotMetaAnalysisForest(logRr = logRr,
                                              logLb95Ci = logLb95Ci,
                                              logUb95Ci = logUb95Ci,
                                              labels = dataSourceName,
                                              limits = c(0.2, 5))
                       dev.off()
                       
                       pdf(file.path(ResultFolder,"calibrated",paste0(AnalysisId,"t",targetid,"c",comparatorid,"o",outcomeid,".pdf")))
                       plotMetaAnalysisForest(logRr = logRr,
                                              logLb95Ci = calibratedlogLb95Ci,
                                              logUb95Ci = calibratedlogUb95Ci,
                                              labels = dataSourceName,
                                              limits = c(0.2, 5)
                       )
                       dev.off()
                       png(file.path(ResultFolder,"calibrated",paste0(AnalysisId,"t",targetid,"c",comparatorid,"o",outcomeid,".png")))
                       plotMetaAnalysisForest(logRr = logRr,
                                              logLb95Ci = calibratedlogLb95Ci,
                                              logUb95Ci = calibratedlogUb95Ci,
                                              labels = dataSourceName,
                                              limits = c(0.2, 5))
                       dev.off()
                       
                       }
                )
                
            }
        )
    }
}


#For older patients
AnalysisId<-18059
AnalysisName<-"60 or Older"
CCAE<-read.csv(file.path(resultFolderList[1],18061,"tablesAndFigures/EmpiricalCalibration.csv"))
Optum<-read.csv(file.path(resultFolderList[2],18059,"tablesAndFigures/EmpiricalCalibration.csv"))
medicare<-read.csv(file.path(resultFolderList[3],18059,"tablesAndFigures/EmpiricalCalibration.csv"))
medicaid<-read.csv(file.path(resultFolderList[4],18059,"tablesAndFigures/EmpiricalCalibration.csv"))
nhis<-read.csv(file.path(resultFolderList[5],18059,"tablesAndFigures/EmpiricalCalibration.csv"))
dataSourceName<-c("CCAE",
                  "Optum",
                  "Medicare",
                  "Medicaid",
                  "NHIS_NSC"
)

AnalysisId<-18061
for (k in seq(length(outcomeidset))){
    #k<-1
    outcomeid <- outcomeidset[k]
    outcomeName <- outcomeNameSet[k]
    
    CCAE_outcome<-CCAE[CCAE$outcomeId == outcomeid,]
    Optum_outcome<-Optum[Optum$outcomeId == outcomeid,]
    medicare_outcome<-medicare[medicare$outcomeId == outcomeid,]
    medicaid_outcome<-medicaid[medicaid$outcomeId == outcomeid, ]
    nhis_outcome<-nhis[nhis$outcomeId == outcomeid,]
    
    
    for (j in 1:3){
        try(
            {
                
                switch(j,
                       {targetid<-as.numeric(paste0(13,18061))
                       comparatorid<-as.numeric(paste0(14,18061))
                       
                       #false_pos=sum(CCAE[( (CCAE$comparatorId==comparatorid)  &  (CCAE$targetId == targetid) & !(CCAE$outcomeId %in% c(outcomeidset, 1,6))), ]$p < 0.05, na.rm=TRUE)
                       #false_neg=sum(CCAE[( (CCAE$comparatorId==comparatorid)  &  (CCAE$targetId == targetid) & !(CCAE$outcomeId %in% c(outcomeidset, 1,6))), ]$p >= 0.05, na.rm=TRUE)
                       
                       CCAE_temp <- CCAE_outcome[ (CCAE_outcome$comparatorId==comparatorid & CCAE_outcome$targetId == targetid), ]
                       Optum_temp <- Optum_outcome[ (Optum_outcome$comparatorId==comparatorid & Optum_outcome$targetId == targetid), ]
                       medicare_temp <- medicare_outcome[ (medicare_outcome$comparatorId==comparatorid & medicare_outcome$targetId == targetid), ]
                       medicaid_temp <- medicaid_outcome[ (medicaid_outcome$comparatorId==comparatorid & medicaid_outcome$targetId == targetid), ]
                       nhis_temp <- nhis_outcome[ (nhis_outcome$comparatorId==comparatorid & nhis_outcome$targetId == targetid), ]
                       
                       result_temp <- rbind(CCAE_temp, Optum_temp, medicare_temp, medicaid_temp, nhis_temp)
                       result_temp$dataSourceName<-dataSourceName
                       
                       logRr=result_temp$logRr
                       logLb95Ci = log( result_temp$ci95lb )
                       logUb95Ci = log( result_temp$ci95ub )
                       
                       seLogRr <- (logUb95Ci-logLb95Ci) / (2 * qnorm(0.975))
                       meta <- meta::metagen(logRr, seLogRr, studlab = dataSourceName, sm = "RR", hakn = TRUE)
                       s <- summary(meta)
                       
                       calibratedlogRr=result_temp$logRr
                       calibratedlogLb95Ci = log( result_temp$calibratedP_lb95ci )
                       calibratedlogUb95Ci = log( result_temp$calibratedP_ub95ci )
                       
                       calibratedseLogRr <- (calibratedlogUb95Ci-calibratedlogLb95Ci) / (2 * qnorm(0.975))
                       meta <- meta::metagen(logRr, calibratedseLogRr, studlab = dataSourceName, sm = "RR", hakn = TRUE)
                       calibrated.s <- summary(meta)
                       
                       df=data.frame(AnalysisId = AnalysisId,
                                     AnalysisName = AnalysisName,
                                     
                                     outcomeid = outcomeid,
                                     outcomeName = outcomeName,
                                     
                                     targetid = targetid,
                                     targetName = unique(as.character(result_temp$targetName)),
                                     totalNoTarget = sum(result_temp$treated),
                                     totalDaysTarget = sum(result_temp$treatedDays),
                                     eventTarget = sum(result_temp$eventsTreated),
                                     
                                     comparatorid = comparatorid,
                                     comparatorName = unique(as.character(result_temp$comparatorName)),
                                     totalNoComparator = sum(result_temp$comparator),
                                     totalDaysComparator = sum(result_temp$comparatorDays),
                                     eventComparator = sum(result_temp$eventsComparator),
                                     
                                     HazardRatio = round(exp(s$random$TE),3),
                                     lower.ci = round(exp(s$random$lower),3),
                                     upper.ci=round(exp(s$random$upper),3),
                                     p.value = round(s$random$p,3),
                                     
                                     calibrated.lower.ci = round(exp(calibrated.s$random$lower),3),
                                     calibrated.upper.ci=round(exp(calibrated.s$random$upper),3),
                                     calibrated.p.value = round(calibrated.s$random$p,3)
                       )
                       meta.summary<-rbind(meta.summary,df)
                       
                       pdf(file.path(ResultFolder,paste0(AnalysisId,"t",targetid,"c",comparatorid,"o",outcomeid,".pdf")))
                       plotMetaAnalysisForest(logRr = logRr,
                                              logLb95Ci = logLb95Ci,
                                              logUb95Ci = logUb95Ci,
                                              labels = dataSourceName,
                                              limits = c(0.2, 5)
                       )
                       dev.off()
                       png(file.path(ResultFolder,paste0(AnalysisId,"t",targetid,"c",comparatorid,"o",outcomeid,".png")))
                       plotMetaAnalysisForest(logRr = logRr,
                                              logLb95Ci = logLb95Ci,
                                              logUb95Ci = logUb95Ci,
                                              labels = dataSourceName,
                                              limits = c(0.2, 5))
                       dev.off()
                       
                       pdf(file.path(ResultFolder,"calibrated",paste0(AnalysisId,"t",targetid,"c",comparatorid,"o",outcomeid,".pdf")))
                       plotMetaAnalysisForest(logRr = logRr,
                                              logLb95Ci = calibratedlogLb95Ci,
                                              logUb95Ci = calibratedlogUb95Ci,
                                              labels = dataSourceName,
                                              limits = c(0.2, 5)
                       )
                       dev.off()
                       png(file.path(ResultFolder,"calibrated",paste0(AnalysisId,"t",targetid,"c",comparatorid,"o",outcomeid,".png")))
                       plotMetaAnalysisForest(logRr = logRr,
                                              logLb95Ci = calibratedlogLb95Ci,
                                              logUb95Ci = calibratedlogUb95Ci,
                                              labels = dataSourceName,
                                              limits = c(0.2, 5))
                       dev.off()
                       
                       },
                       {
                           targetid<-as.numeric(paste0(34,18061))
                           comparatorid<-as.numeric(paste0(13,18061))
                           
                           
                           #false_pos=sum(CCAE[( (CCAE$comparatorId==comparatorid)  &  (CCAE$targetId == targetid) & !(CCAE$outcomeId %in% c(outcomeidset, 1,6))), ]$p < 0.05, na.rm=TRUE)
                           #false_neg=sum(CCAE[( (CCAE$comparatorId==comparatorid)  &  (CCAE$targetId == targetid) & !(CCAE$outcomeId %in% c(outcomeidset, 1,6))), ]$p >= 0.05, na.rm=TRUE)
                           
                           CCAE_temp <- CCAE_outcome[ (CCAE_outcome$comparatorId==comparatorid & CCAE_outcome$targetId == targetid), ]
                           Optum_temp <- Optum_outcome[ (Optum_outcome$comparatorId==comparatorid & Optum_outcome$targetId == targetid), ]
                           medicare_temp <- medicare_outcome[ (medicare_outcome$comparatorId==comparatorid & medicare_outcome$targetId == targetid), ]
                           medicaid_temp <- medicaid_outcome[ (medicaid_outcome$comparatorId==comparatorid & medicaid_outcome$targetId == targetid), ]
                           nhis_temp <- nhis_outcome[ (nhis_outcome$comparatorId==comparatorid & nhis_outcome$targetId == targetid), ]
                           
                           result_temp <- rbind(CCAE_temp, Optum_temp, medicare_temp, medicaid_temp, nhis_temp)
                           result_temp$dataSourceName<-dataSourceName
                           
                           logRr=result_temp$logRr
                           logLb95Ci = log( result_temp$ci95lb )
                           logUb95Ci = log( result_temp$ci95ub )
                           
                           seLogRr <- (logUb95Ci-logLb95Ci) / (2 * qnorm(0.975))
                           meta <- meta::metagen(logRr, seLogRr, studlab = dataSourceName, sm = "RR", hakn = TRUE)
                           s <- summary(meta)
                           
                           calibratedlogRr=result_temp$logRr
                           calibratedlogLb95Ci = log( result_temp$calibratedP_lb95ci )
                           calibratedlogUb95Ci = log( result_temp$calibratedP_ub95ci )
                           
                           calibratedseLogRr <- (calibratedlogUb95Ci-calibratedlogLb95Ci) / (2 * qnorm(0.975))
                           meta <- meta::metagen(logRr, calibratedseLogRr, studlab = dataSourceName, sm = "RR", hakn = TRUE)
                           calibrated.s <- summary(meta)
                           
                           df=data.frame(AnalysisId = AnalysisId,
                                         AnalysisName = AnalysisName,
                                         
                                         outcomeid = outcomeid,
                                         outcomeName = outcomeName,
                                         
                                         targetid = targetid,
                                         targetName = unique(as.character(result_temp$targetName)),
                                         totalNoTarget = sum(result_temp$treated),
                                         totalDaysTarget = sum(result_temp$treatedDays),
                                         eventTarget = sum(result_temp$eventsTreated),
                                         
                                         comparatorid = comparatorid,
                                         comparatorName = unique(as.character(result_temp$comparatorName)),
                                         totalNoComparator = sum(result_temp$comparator),
                                         totalDaysComparator = sum(result_temp$comparatorDays),
                                         eventComparator = sum(result_temp$eventsComparator),
                                         
                                         HazardRatio = round(exp(s$random$TE),3),
                                         lower.ci = round(exp(s$random$lower),3),
                                         upper.ci=round(exp(s$random$upper),3),
                                         p.value = round(s$random$p,3),
                                         
                                         calibrated.lower.ci = round(exp(calibrated.s$random$lower),3),
                                         calibrated.upper.ci=round(exp(calibrated.s$random$upper),3),
                                         calibrated.p.value = round(calibrated.s$random$p,3)
                           )
                           meta.summary<-rbind(meta.summary,df)
                           # if (file.exists(file.path(ResultFolder,"meta_analysis.csv"))){
                           #     write.table(df, file.path(ResultFolder,"meta_analysis.csv"),sep=",",col.names = FALSE, append=TRUE)
                           # }else {
                           #     write.table(df, file.path(ResultFolder,"meta_analysis.csv"),sep=",", col.names = TRUE, append=TRUE)
                           # }
                           
                           pdf(file.path(ResultFolder,paste0(AnalysisId,"t",targetid,"c",comparatorid,"o",outcomeid,".pdf")))
                           plotMetaAnalysisForest(logRr = logRr,
                                                  logLb95Ci = logLb95Ci,
                                                  logUb95Ci = logUb95Ci,
                                                  labels = dataSourceName,
                                                  limits = c(0.2, 5)
                           )
                           dev.off()
                           png(file.path(ResultFolder,paste0(AnalysisId,"t",targetid,"c",comparatorid,"o",outcomeid,".png")))
                           plotMetaAnalysisForest(logRr = logRr,
                                                  logLb95Ci = logLb95Ci,
                                                  logUb95Ci = logUb95Ci,
                                                  labels = dataSourceName,
                                                  limits = c(0.2, 5))
                           dev.off()
                           
                           pdf(file.path(ResultFolder,"calibrated",paste0(AnalysisId,"t",targetid,"c",comparatorid,"o",outcomeid,".pdf")))
                           plotMetaAnalysisForest(logRr = logRr,
                                                  logLb95Ci = calibratedlogLb95Ci,
                                                  logUb95Ci = calibratedlogUb95Ci,
                                                  labels = dataSourceName,
                                                  limits = c(0.2, 5)
                           )
                           dev.off()
                           png(file.path(ResultFolder,"calibrated",paste0(AnalysisId,"t",targetid,"c",comparatorid,"o",outcomeid,".png")))
                           plotMetaAnalysisForest(logRr = logRr,
                                                  logLb95Ci = calibratedlogLb95Ci,
                                                  logUb95Ci = calibratedlogUb95Ci,
                                                  labels = dataSourceName,
                                                  limits = c(0.2, 5))
                           dev.off()
                           
                       },
                       {targetid<-as.numeric(paste0(34,18061))
                       comparatorid<-as.numeric(paste0(14,18061))
                       
                       #false_pos=sum(CCAE[( (CCAE$comparatorId==comparatorid)  &  (CCAE$targetId == targetid) & !(CCAE$outcomeId %in% c(outcomeidset, 1,6))), ]$p < 0.05, na.rm=TRUE)
                       #false_neg=sum(CCAE[( (CCAE$comparatorId==comparatorid)  &  (CCAE$targetId == targetid) & !(CCAE$outcomeId %in% c(outcomeidset, 1,6))), ]$p >= 0.05, na.rm=TRUE)
                       
                       CCAE_temp <- CCAE_outcome[ (CCAE_outcome$comparatorId==comparatorid & CCAE_outcome$targetId == targetid), ]
                       Optum_temp <- Optum_outcome[ (Optum_outcome$comparatorId==comparatorid & Optum_outcome$targetId == targetid), ]
                       medicare_temp <- medicare_outcome[ (medicare_outcome$comparatorId==comparatorid & medicare_outcome$targetId == targetid), ]
                       medicaid_temp <- medicaid_outcome[ (medicaid_outcome$comparatorId==comparatorid & medicaid_outcome$targetId == targetid), ]
                       nhis_temp <- nhis_outcome[ (nhis_outcome$comparatorId==comparatorid & nhis_outcome$targetId == targetid), ]
                       
                       result_temp <- rbind(CCAE_temp, Optum_temp, medicare_temp, medicaid_temp, nhis_temp)
                       result_temp$dataSourceName<-dataSourceName
                       
                       logRr=result_temp$logRr
                       logLb95Ci = log( result_temp$ci95lb )
                       logUb95Ci = log( result_temp$ci95ub )
                       
                       seLogRr <- (logUb95Ci-logLb95Ci) / (2 * qnorm(0.975))
                       meta <- meta::metagen(logRr, seLogRr, studlab = dataSourceName, sm = "RR", hakn = TRUE)
                       s <- summary(meta)
                       
                       calibratedlogRr=result_temp$logRr
                       calibratedlogLb95Ci = log( result_temp$calibratedP_lb95ci )
                       calibratedlogUb95Ci = log( result_temp$calibratedP_ub95ci )
                       
                       calibratedseLogRr <- (calibratedlogUb95Ci-calibratedlogLb95Ci) / (2 * qnorm(0.975))
                       meta <- meta::metagen(logRr, calibratedseLogRr, studlab = dataSourceName, sm = "RR", hakn = TRUE)
                       calibrated.s <- summary(meta)
                       
                       df=data.frame(AnalysisId = AnalysisId,
                                     AnalysisName = AnalysisName,
                                     
                                     outcomeid = outcomeid,
                                     outcomeName = outcomeName,
                                     
                                     targetid = targetid,
                                     targetName = unique(as.character(result_temp$targetName)),
                                     totalNoTarget = sum(result_temp$treated),
                                     totalDaysTarget = sum(result_temp$treatedDays),
                                     eventTarget = sum(result_temp$eventsTreated),
                                     
                                     comparatorid = comparatorid,
                                     comparatorName = unique(as.character(result_temp$comparatorName)),
                                     totalNoComparator = sum(result_temp$comparator),
                                     totalDaysComparator = sum(result_temp$comparatorDays),
                                     eventComparator = sum(result_temp$eventsComparator),
                                     
                                     HazardRatio = round(exp(s$random$TE),3),
                                     lower.ci = round(exp(s$random$lower),3),
                                     upper.ci=round(exp(s$random$upper),3),
                                     p.value = round(s$random$p,3),
                                     
                                     calibrated.lower.ci = round(exp(calibrated.s$random$lower),3),
                                     calibrated.upper.ci=round(exp(calibrated.s$random$upper),3),
                                     calibrated.p.value = round(calibrated.s$random$p,3)
                       )
                       meta.summary<-rbind(meta.summary,df)
                       # if (file.exists(file.path(ResultFolder,"meta_analysis.csv"))){
                       #     write.table(df, file.path(ResultFolder,"meta_analysis.csv"),sep=",",col.names = FALSE, append=TRUE)
                       # }else {
                       #     write.table(df, file.path(ResultFolder,"meta_analysis.csv"),sep=",", col.names = TRUE, append=TRUE)
                       # }
                       
                       pdf(file.path(ResultFolder,paste0(AnalysisId,"t",targetid,"c",comparatorid,"o",outcomeid,".pdf")))
                       plotMetaAnalysisForest(logRr = logRr,
                                              logLb95Ci = logLb95Ci,
                                              logUb95Ci = logUb95Ci,
                                              labels = dataSourceName,
                                              limits = c(0.2, 5)
                       )
                       dev.off()
                       png(file.path(ResultFolder,paste0(AnalysisId,"t",targetid,"c",comparatorid,"o",outcomeid,".png")))
                       plotMetaAnalysisForest(logRr = logRr,
                                              logLb95Ci = logLb95Ci,
                                              logUb95Ci = logUb95Ci,
                                              labels = dataSourceName,
                                              limits = c(0.2, 5))
                       dev.off()
                       
                       pdf(file.path(ResultFolder,"calibrated",paste0(AnalysisId,"t",targetid,"c",comparatorid,"o",outcomeid,".pdf")))
                       plotMetaAnalysisForest(logRr = logRr,
                                              logLb95Ci = calibratedlogLb95Ci,
                                              logUb95Ci = calibratedlogUb95Ci,
                                              labels = dataSourceName,
                                              limits = c(0.2, 5)
                       )
                       dev.off()
                       png(file.path(ResultFolder,"calibrated",paste0(AnalysisId,"t",targetid,"c",comparatorid,"o",outcomeid,".png")))
                       plotMetaAnalysisForest(logRr = logRr,
                                              logLb95Ci = calibratedlogLb95Ci,
                                              logUb95Ci = calibratedlogUb95Ci,
                                              labels = dataSourceName,
                                              limits = c(0.2, 5))
                       dev.off()
                       
                       }
                )
                
            }
        )
    }
}

write.csv(meta.summary,file.path(ResultFolder,"meta_analysis.csv"))


##Meta-analysis for Incidence Rate Difference

meta.IRD.summary<-data.frame()

AnalysisIdSet<-c(30,180,365,730,18001,18002, 18011)
AnalysisNameSet<-c("30days",
                   "180days",
                   "365days",
                   "730days",
                   "Male",
                   "Female",
                   "without DM"
)

outcomeidset<-c(0, 2, 3,4, 4320)
outcomeNameSet<-c("mortality", "Myocardial Infarction", "Heart Failure", "Stroke", "MACCE")

for (i in seq(length(AnalysisIdSet))){
        #i<-1
        AnalysisId<-AnalysisIdSet[i]
        AnalysisName<-AnalysisNameSet[i]
        try({
                CCAE<-read.csv(file.path(resultFolderList[1],AnalysisId,"tablesAndFigures/EmpiricalCalibration.csv"))
                Optum<-read.csv(file.path(resultFolderList[2],AnalysisId,"tablesAndFigures/EmpiricalCalibration.csv"))
                medicare<-read.csv(file.path(resultFolderList[3],AnalysisId,"tablesAndFigures/EmpiricalCalibration.csv"))
                medicaid<-read.csv(file.path(resultFolderList[4],AnalysisId,"tablesAndFigures/EmpiricalCalibration.csv"))
                nhis<-read.csv(file.path(resultFolderList[5],AnalysisId,"tablesAndFigures/EmpiricalCalibration.csv"))
        })
        
        
        
        for (k in seq(length(outcomeidset))){
                #k<-1
                outcomeid <- outcomeidset[k]
                outcomeName <- outcomeNameSet[k]
                
                
                CCAE_outcome<-CCAE[CCAE$outcomeId == outcomeid,]
                Optum_outcome<-Optum[Optum$outcomeId == outcomeid,]
                medicare_outcome<-medicare[medicare$outcomeId == outcomeid,]
                medicaid_outcome<-medicaid[medicaid$outcomeId == outcomeid, ]
                nhis_outcome<-nhis[nhis$outcomeId == outcomeid,]
                
                
                for (j in 1:3){
                        #j<-1
                        try(
                                {
                                        
                                        switch(j,
                                               {targetid<-as.numeric(paste0(13,AnalysisId))
                                               comparatorid<-as.numeric(paste0(14,AnalysisId))},
                                               {targetid<-as.numeric(paste0(34,AnalysisId))
                                               comparatorid<-as.numeric(paste0(13,AnalysisId))
                                               },
                                               {targetid<-as.numeric(paste0(34,AnalysisId))
                                               comparatorid<-as.numeric(paste0(14,AnalysisId))})
                                        
                                               
                                               CCAE_temp <- CCAE_outcome[ (CCAE_outcome$comparatorId==comparatorid & CCAE_outcome$targetId == targetid), ]
                                               Optum_temp <- Optum_outcome[ (Optum_outcome$comparatorId==comparatorid & Optum_outcome$targetId == targetid), ]
                                               medicare_temp <- medicare_outcome[ (medicare_outcome$comparatorId==comparatorid & medicare_outcome$targetId == targetid), ]
                                               medicaid_temp <- medicaid_outcome[ (medicaid_outcome$comparatorId==comparatorid & medicaid_outcome$targetId == targetid), ]
                                               nhis_temp <- nhis_outcome[ (nhis_outcome$comparatorId==comparatorid & nhis_outcome$targetId == targetid), ]
                                               
                                               result_temp <- rbind(CCAE_temp, Optum_temp, medicare_temp, medicaid_temp, nhis_temp)
                                               result_temp$dataSourceName<-dataSourceName
                                               
                                               ##Meta-analysis for Incidence rate Difference per 1000 person-year 
                                               metaIRD<-metainc(event.e = eventsTreated, 
                                                                time.e = treatedDays/(365*1000),
                                                                event.c = eventsComparator,
                                                                time.c=comparatorDays/(365*1000),
                                                                data=result_temp,
                                                                method = "MH", #Mantel-Haenszel Test
                                                                sm = "IRD"
                                               )
                                               ird.df=data.frame(AnalysisId = AnalysisId,
                                                                 AnalysisName = AnalysisName,
                                                                 
                                                                 outcomeid = outcomeid,
                                                                 outcomeName = outcomeName,
                                                                 
                                                                 targetid = targetid,
                                                                 targetName = unique(as.character(result_temp$targetName)),
                                                                 totalNoTarget = sum(result_temp$treated),
                                                                 totalDaysTarget = sum(result_temp$treatedDays),
                                                                 eventTarget = sum(result_temp$eventsTreated),
                                                                 
                                                                 comparatorid = comparatorid,
                                                                 comparatorName = unique(as.character(result_temp$comparatorName)),
                                                                 totalNoComparator = sum(result_temp$comparator),
                                                                 totalDaysComparator = sum(result_temp$comparatorDays),
                                                                 eventComparator = sum(result_temp$eventsComparator),
                                                                 IRD = metaIRD$TE.random,
                                                                 lower.ci = metaIRD$lower.random,
                                                                 upper.ci = metaIRD$upper.random,
                                                                 IRD.p = metaIRD$pval.random,
                                                                 I2 = metaIRD$I2
                                               )
                                               
                                               meta.IRD.summary<-rbind(meta.IRD.summary,ird.df)
                                               
                                               }
                                        )
                                        
                                }
                }
}

##For younger patients

AnalysisId<-18061
AnalysisName<-"Younger than 60"
CCAE<-read.csv(file.path(resultFolderList[1],18059,"tablesAndFigures/EmpiricalCalibration.csv"))
Optum<-read.csv(file.path(resultFolderList[2],18061,"tablesAndFigures/EmpiricalCalibration.csv"))
medicaid<-read.csv(file.path(resultFolderList[4],18061,"tablesAndFigures/EmpiricalCalibration.csv"))
nhis<-read.csv(file.path(resultFolderList[5],18061,"tablesAndFigures/EmpiricalCalibration.csv"))
dataSourceName<-c("CCAE",
                  "Optum",
                  "Medicaid",
                  "NHIS_NSC"
)
AnalysisId<-18059
for (k in seq(length(outcomeidset))){
        #k<-1
        outcomeid <- outcomeidset[k]
        outcomeName <- outcomeNameSet[k]
        
        
        CCAE_outcome<-CCAE[CCAE$outcomeId == outcomeid,]
        Optum_outcome<-Optum[Optum$outcomeId == outcomeid,]
        medicaid_outcome<-medicaid[medicaid$outcomeId == outcomeid, ]
        nhis_outcome<-nhis[nhis$outcomeId == outcomeid,]
        
        
        for (j in 1:3){
                #j<-1
                try(
                        {
                                
                                switch(j,
                                       {targetid<-as.numeric(paste0(13,AnalysisId))
                                       comparatorid<-as.numeric(paste0(14,AnalysisId))},
                                       {targetid<-as.numeric(paste0(34,AnalysisId))
                                       comparatorid<-as.numeric(paste0(13,AnalysisId))
                                       },
                                       {targetid<-as.numeric(paste0(34,AnalysisId))
                                       comparatorid<-as.numeric(paste0(14,AnalysisId))})
                                
                                
                                CCAE_temp <- CCAE_outcome[ (CCAE_outcome$comparatorId==comparatorid & CCAE_outcome$targetId == targetid), ]
                                Optum_temp <- Optum_outcome[ (Optum_outcome$comparatorId==comparatorid & Optum_outcome$targetId == targetid), ]
                                medicaid_temp <- medicaid_outcome[ (medicaid_outcome$comparatorId==comparatorid & medicaid_outcome$targetId == targetid), ]
                                nhis_temp <- nhis_outcome[ (nhis_outcome$comparatorId==comparatorid & nhis_outcome$targetId == targetid), ]
                                
                                result_temp <- rbind(CCAE_temp, Optum_temp, medicaid_temp, nhis_temp)
                                result_temp$dataSourceName<-dataSourceName
                                
                                ##Meta-analysis for Incidence rate Difference 
                                metaIRD<-metainc(event.e = eventsTreated, 
                                                 time.e = treatedDays/(365*1000),
                                                 event.c = eventsComparator,
                                                 time.c=comparatorDays/(365*1000),
                                                 data=result_temp,
                                                 method = "MH", #Haenszel 
                                                 sm = "IRD"
                                )
                                ird.df=data.frame(AnalysisId = AnalysisId,
                                                  AnalysisName = AnalysisName,
                                                  
                                                  outcomeid = outcomeid,
                                                  outcomeName = outcomeName,
                                                  
                                                  targetid = targetid,
                                                  targetName = unique(as.character(result_temp$targetName)),
                                                  totalNoTarget = sum(result_temp$treated),
                                                  totalDaysTarget = sum(result_temp$treatedDays),
                                                  eventTarget = sum(result_temp$eventsTreated),
                                                  
                                                  comparatorid = comparatorid,
                                                  comparatorName = unique(as.character(result_temp$comparatorName)),
                                                  totalNoComparator = sum(result_temp$comparator),
                                                  totalDaysComparator = sum(result_temp$comparatorDays),
                                                  eventComparator = sum(result_temp$eventsComparator),
                                                  IRD = metaIRD$TE.random,
                                                  lower.ci = metaIRD$lower.random,
                                                  upper.ci = metaIRD$upper.random,
                                                  IRD.p = metaIRD$pval.random,
                                                  I2 = metaIRD$I2
                                )
                                
                                meta.IRD.summary<-rbind(meta.IRD.summary,ird.df)
                                
                        }
                )
                
        }
}

AnalysisId<-18059
AnalysisName<-"60 or Older"
CCAE<-read.csv(file.path(resultFolderList[1],18061,"tablesAndFigures/EmpiricalCalibration.csv"))
Optum<-read.csv(file.path(resultFolderList[2],18059,"tablesAndFigures/EmpiricalCalibration.csv"))
medicare<-read.csv(file.path(resultFolderList[3],18059,"tablesAndFigures/EmpiricalCalibration.csv"))
medicaid<-read.csv(file.path(resultFolderList[4],18059,"tablesAndFigures/EmpiricalCalibration.csv"))
nhis<-read.csv(file.path(resultFolderList[5],18059,"tablesAndFigures/EmpiricalCalibration.csv"))
dataSourceName<-c("CCAE",
                  "Optum",
                  "Medicare",
                  "Medicaid",
                  "NHIS_NSC"
)

AnalysisId<-18061
for (k in seq(length(outcomeidset))){
        #k<-1
        outcomeid <- outcomeidset[k]
        outcomeName <- outcomeNameSet[k]
        
        
        CCAE_outcome<-CCAE[CCAE$outcomeId == outcomeid,]
        Optum_outcome<-Optum[Optum$outcomeId == outcomeid,]
        medicare_outcome<-medicare[medicare$outcomeId == outcomeid,]
        medicaid_outcome<-medicaid[medicaid$outcomeId == outcomeid, ]
        nhis_outcome<-nhis[nhis$outcomeId == outcomeid,]
        
        
        for (j in 1:3){
                #j<-1
                try(
                        {
                                
                                switch(j,
                                       {targetid<-as.numeric(paste0(13,AnalysisId))
                                       comparatorid<-as.numeric(paste0(14,AnalysisId))},
                                       {targetid<-as.numeric(paste0(34,AnalysisId))
                                       comparatorid<-as.numeric(paste0(13,AnalysisId))
                                       },
                                       {targetid<-as.numeric(paste0(34,AnalysisId))
                                       comparatorid<-as.numeric(paste0(14,AnalysisId))})
                                
                                
                                CCAE_temp <- CCAE_outcome[ (CCAE_outcome$comparatorId==comparatorid & CCAE_outcome$targetId == targetid), ]
                                Optum_temp <- Optum_outcome[ (Optum_outcome$comparatorId==comparatorid & Optum_outcome$targetId == targetid), ]
                                medicare_temp <- medicare_outcome[ (medicare_outcome$comparatorId==comparatorid & medicare_outcome$targetId == targetid), ]
                                medicaid_temp <- medicaid_outcome[ (medicaid_outcome$comparatorId==comparatorid & medicaid_outcome$targetId == targetid), ]
                                nhis_temp <- nhis_outcome[ (nhis_outcome$comparatorId==comparatorid & nhis_outcome$targetId == targetid), ]
                                
                                result_temp <- rbind(CCAE_temp, Optum_temp, medicare_temp, medicaid_temp, nhis_temp)
                                result_temp$dataSourceName<-dataSourceName
                                
                                ##Meta-analysis for Incidence rate Difference 
                                metaIRD<-metainc(event.e = eventsTreated, 
                                                 time.e = treatedDays/(365*1000),
                                                 event.c = eventsComparator,
                                                 time.c=comparatorDays/(365*1000),
                                                 data=result_temp,
                                                 method = "MH", #Haenszel 
                                                 sm = "IRD"
                                )
                                ird.df=data.frame(AnalysisId = AnalysisId,
                                                  AnalysisName = AnalysisName,
                                                  
                                                  outcomeid = outcomeid,
                                                  outcomeName = outcomeName,
                                                  
                                                  targetid = targetid,
                                                  targetName = unique(as.character(result_temp$targetName)),
                                                  totalNoTarget = sum(result_temp$treated),
                                                  totalDaysTarget = sum(result_temp$treatedDays),
                                                  eventTarget = sum(result_temp$eventsTreated),
                                                  
                                                  comparatorid = comparatorid,
                                                  comparatorName = unique(as.character(result_temp$comparatorName)),
                                                  totalNoComparator = sum(result_temp$comparator),
                                                  totalDaysComparator = sum(result_temp$comparatorDays),
                                                  eventComparator = sum(result_temp$eventsComparator),
                                                  IRD = metaIRD$TE.random,
                                                  lower.ci = metaIRD$lower.random,
                                                  upper.ci = metaIRD$upper.random,
                                                  IRD.p = metaIRD$pval.random,
                                                  I2 = metaIRD$I2
                                )
                                
                                meta.IRD.summary<-rbind(meta.IRD.summary,ird.df)
                                
                        }
                )
                
        }
}
write.csv(meta.IRD.summary,file.path(ResultFolder,"meta_analysis_IRD.csv"))
