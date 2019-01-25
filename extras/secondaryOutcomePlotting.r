library(forestplot)

outputFolder<-"C:/Users/apple/OneDrive/Study/Ticagrelor/Data"
analysisSummary<-read.csv(file.path(outputFolder,"analysisSummary.csv"))

summ<-analysisSummary[grepl("On-treatment PS matching", analysisSummary$analysisDescription),]
summ<-summ[grepl("SCYou", summ$outcomeName),]

data<-summ
tabletext <- cbind(
    c("Subgroup","\n",as.character(data$outcomeName)), 
    #c("No. of Patients (%)","\n",np), 
    #c("4-Yr Cum. Event Rate\n PCI","\n",data$PCI.Group), 
    #c("4-Yr Cum. Event Rate\n Medical Therapy","\n",data$Medical.Therapy.Group), 
    c(paste0(italic("P")," value"),"\n", sprintf("%.3f",round(data$p,3)) )
    )

#png(file.path(outputFolder,"secondary_outcome.png"),width=960, height=640)
forestplot::forestplot(labeltext=tabletext, 
                       graph.pos=3, 
                       mean=c(NA,NA,summ$rr), 
                       lower=c(NA,NA,summ$ci95lb), upper=c(NA,NA,summ$ci95ub),
                       title="Hazard Ratio",
                       xlab="     <---Ticagrelor Better---    ---Clopidogrel Better--->",
                       # hrzl_lines=list("3" = gpar(lwd=1, col="#99999922"), 
                       #                 "5" = gpar(lwd=60, lineend="butt", columns=c(2:6), col="#99999922"),
                       #                 "7" = gpar(lwd=60, lineend="butt", columns=c(2:6), col="#99999922"),
                       #                 "23" = gpar(lwd=60, lineend="butt", columns=c(2:6), col="#99999922"),
                       #                 "31" = gpar(lwd=60, lineend="butt", columns=c(2:6), col="#99999922")
                       #),
                       txt_gp=fpTxtGp(label=gpar(cex=1.25),
                                      ticks=gpar(cex=1.1),
                                      xlab=gpar(cex = 1.2),
                                      title=gpar(cex = 1.2)),
                       col=fpColors(box="black", lines="black", zero = "gray50"),
                       zero=1, cex=0.9, lineheight = "auto", boxsize=0.2, colgap=unit(6,"mm"),
                       lwd.ci=2, ci.vertices=TRUE, ci.vertices.height = 0.2
                       )
#dev.off()

#Future order
c("Net Adverse Clinical Event",
  
  "Overall ischemic event",
  "  Any revascularization",
  "  Acute myocardial infarction",
  "  Ischemic stroke",
  
  "Overall hemorrhagic event",
  "  Intracranial hemorrhage",
  "  Gastrointestional bleeding")