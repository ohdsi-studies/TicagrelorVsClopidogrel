#requires
#library(ggplot2)
#library(ggsci)
#library(dplyr)
#theme can be 'nejm' 'jama' and 'lancet'

# Load data from data folder:
loadFile <- function(file) {
    # file = files[13]
    tableName <- gsub("(_t[0-9]+_c[0-9]+)|(_)[^_]*\\.rds", "", file) 
    camelCaseName <- SqlRender::snakeCaseToCamelCase(tableName)
    if (!(tableName %in% splittableTables)) {
        newData <- readRDS(file.path(dataFolder, file))
        newData <- data.frame(lapply(newData, function(x){
            if(is.factor(x)) {
                as.character(x)
            } else {x}
        }), stringsAsFactors=FALSE)
        colnames(newData) <- SqlRender::snakeCaseToCamelCase(colnames(newData))
        if (exists(camelCaseName, envir = .GlobalEnv)) {
            existingData <- get(camelCaseName, envir = .GlobalEnv)
            newData <- rbind(existingData, newData)
        }
        assign(camelCaseName, newData, envir = .GlobalEnv)
    }
    invisible(NULL)
}

plottingTrend<-function(data,
                        databaseIds,
                        targetId,
                        comparatorId,
                        targetName = NULL,
                        theme = "jama"){
    
    target<- cohortCountPerYear %>% 
        filter(cohortdefinitionid==targetId) %>% 
        arrange(year) %>% 
        mutate(target=cohortcount) %>%
        select(target,year,databaseid)
    
    comparator<- cohortCountPerYear %>% 
        filter(cohortdefinitionid==comparatorId) %>% 
        arrange(year) %>% 
        mutate(comparator=cohortcount) %>%
        select(comparator,year,databaseid)
    
    pts<-dplyr::inner_join(target, comparator, by = c("year","databaseid"))
    pts$total = pts$target+pts$comparator
    
    pts$databaseId <- factor(pts$databaseid,levels = databaseIds) # make databaseId factor with an order
    
    ####Proportion Plotting####
    p<-ggplot(data = pts,
              aes(x=year, y=target/total*100, 
                  group = databaseId, 
                  colour = databaseId
              )) + 
        geom_line(stat="identity",
                  size = 1
        ) + 
        geom_point(size=2) +
        scale_x_continuous(breaks= seq(2011,2019,1)) +
        #ggtitle(sprintf("Proportion of %s users among study population per year",targetName)) +
        labs(x="Year", y = "Proportion (%)")+
        theme_bw()+ #remove background
        theme(axis.text=element_text(size=11), #The size of the numbers on axis
              axis.title=element_text(size=12) #The size of the title of the axis
              )+
        theme (panel.border = element_blank(), axis.line = element_line())+#only x and y axis, not box
        theme (panel.grid.major.x = element_blank() , #remove vertical grid line
               panel.grid.minor.x = element_blank() , #remove vertical grid line
               panel.grid.major.y = element_line( size=.1, color="gray" )#,
               #panel.grid.minor.y = element_line( size=.05, color="gray" ) 
               )+
        theme (legend.position="bottom", legend.direction="horizontal",
               legend.title = element_blank(),
               legend.text=element_text(size=12) #the size of the legend
               ) 

    # theme( # remove the vertical grid lines
    #     panel.grid.major.x = element_blank() ,
    #     # explicitly set the horizontal lines (or they will disappear too)
    #     panel.grid.major.y = element_line( size=.1, color="black" ) 
    # )
                   
    
    if(is.null(theme)) theme <- ""
    if(theme=="jama")p <- p + ggsci::scale_color_jama()
    if(theme=="lancet")p <- p + ggsci::scale_color_lancet()
    if(theme=="nejm")p <- p + ggsci::scale_color_nejm()
    
    
    return(p)
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

##get balances
#databaseId = databaseIds[1]

targetId = 874 #ticagrelor
comparatorId = 929 #clopidogrel
analysisId = 1
outcomeId = 1240

getBalance <- function(databaseId,
                       studyFolder,
                       targetId,
                       comparatorId,
                       analysisId,
                       outcomeId){
    pathToRds<-file.path(studyFolder,"shinyData",
                         sprintf("covariate_balance_t%d_c%d_%s.rds",targetId,comparatorId,databaseId))
    balance <- readRDS(pathToRds)
    
    pathToRds<-file.path(studyFolder,"shinyData",
                         sprintf("covariate_%s.rds",databaseId))
    covariate <- readRDS(pathToRds)
    
    colnames(balance)<-SqlRender::snakeCaseToCamelCase(colnames(balance))
    colnames(covariate)<-SqlRender::snakeCaseToCamelCase(colnames(covariate))
    
    balance <- balance[balance$analysisId == analysisId & balance$outcomeId == outcomeId, ]
    balance <- merge(balance, covariate[,c("covariateId", "covariateAnalysisId", "covariateName")])
    balance <- balance[ c("covariateId",
                          "covariateName",
                          "covariateAnalysisId", 
                          "targetMeanBefore", 
                          "comparatorMeanBefore", 
                          "stdDiffBefore", 
                          "targetMeanAfter", 
                          "comparatorMeanAfter",
                          "stdDiffAfter")]
    colnames(balance) <- c("covariateId",
                           "covariateName",
                           "analysisId",
                           "beforeMatchingMeanTreated",
                           "beforeMatchingMeanComparator",
                           "beforeMatchingStdDiff",
                           "afterMatchingMeanTreated",
                           "afterMatchingMeanComparator",
                           "afterMatchingStdDiff")
    balance$absBeforeMatchingStdDiff <- abs(balance$beforeMatchingStdDiff)
    balance$absAfterMatchingStdDiff <- abs(balance$afterMatchingStdDiff)
    return(balance)
    
}

#remove 'unknown unit'
labFormmating<-function(Table1,percentDigits){
    UndoFormatPercent <- function(x) {
        #result <- format(sprintf("%.1f",round(x/100,2)), digits = 3, justify = "right")
        result <- sprintf(paste0("%5.",percentDigits,"f"), round(x/100, percentDigits))
        result <- gsub("^-", "<", result)
        result <- gsub("NA", "", result)
        return(result)
    }
    
    loc<-which(grepl("Body mass index|BP|Hemoglobin|Glucose|Cholesterol|Creatinine|Creatine kinase" ,Table1[,1]))
    Table1[loc,2]<-
        UndoFormatPercent(as.numeric(Table1[loc,2]))
    Table1[loc,3]<-
        UndoFormatPercent(as.numeric(Table1[loc,3]))
    Table1[loc,5]<-
        UndoFormatPercent(as.numeric(Table1[loc,5]))
    Table1[loc,6]<-
        UndoFormatPercent(as.numeric(Table1[loc,6]))
    
    return(Table1)
}

prepareTable1 <- function(balance,
                          beforeLabel = "Before stratification",
                          afterLabel = "After stratification",
                          targetLabel = "Target",
                          comparatorLabel = "Comparator",
                          percentDigits = 1,
                          stdDiffDigits = 2,
                          output = "latex",
                          pathToCsv = "Table1Specs.csv") {
    if (output == "latex") {
        space <- " "
    } else {
        space <- "&nbsp;"
    }
    specifications <- read.csv(pathToCsv, stringsAsFactors = FALSE)
    
    fixCase <- function(label) {
        idx <- (toupper(label) == label)
        if (any(idx)) {
            label[idx] <- paste0(substr(label[idx], 1, 1),
                                 tolower(substr(label[idx], 2, nchar(label[idx]))))
        }
        return(label)
    }
    
    formatPercent <- function(x) {
        result <- format(round(100 * x, percentDigits), digits = percentDigits + 1, justify = "right")
        result <- gsub("^-", "<", result)
        result <- gsub("NA", "", result)
        result <- gsub(" ", space, result)
        return(result)
    }
    
    formatStdDiff <- function(x) {
        result <- format(round(x, stdDiffDigits), digits = stdDiffDigits + 1, justify = "right")
        result <- gsub("NA", "", result)
        result <- gsub(" ", space, result)
        return(result)
    }
    
    resultsTable <- data.frame()
    for (i in 1:nrow(specifications)) {
        if (specifications$analysisId[i] == "") {
            resultsTable <- rbind(resultsTable,
                                  data.frame(Characteristic = specifications$label[i], value = ""))
        } else {
            idx <- balance$analysisId == specifications$analysisId[i]
            if (any(idx)) {
                if (specifications$covariateIds[i] != "") {
                    covariateIds <- as.numeric(strsplit(specifications$covariateIds[i], ";")[[1]])
                    idx <- balance$covariateId %in% covariateIds
                } else {
                    covariateIds <- NULL
                }
                if (any(idx)) {
                    balanceSubset <- balance[idx, ]
                    if (is.null(covariateIds)) {
                        balanceSubset <- balanceSubset[order(balanceSubset$covariateId), ]
                    } else {
                        balanceSubset <- merge(balanceSubset, data.frame(covariateId = covariateIds,
                                                                         rn = 1:length(covariateIds)))
                        balanceSubset <- balanceSubset[order(balanceSubset$rn, balanceSubset$covariateId), ]
                    }
                    balanceSubset$covariateName <- fixCase(gsub("^.*: ", "", balanceSubset$covariateName))
                    if (specifications$covariateIds[i] == "" || length(covariateIds) > 1) {
                        resultsTable <- rbind(resultsTable, data.frame(Characteristic = specifications$label[i],
                                                                       beforeMatchingMeanTreated = NA,
                                                                       beforeMatchingMeanComparator = NA,
                                                                       beforeMatchingStdDiff = NA,
                                                                       afterMatchingMeanTreated = NA,
                                                                       afterMatchingMeanComparator = NA,
                                                                       afterMatchingStdDiff = NA,
                                                                       stringsAsFactors = FALSE))
                        resultsTable <- rbind(resultsTable, data.frame(Characteristic = paste0(space,
                                                                                               space,
                                                                                               space,
                                                                                               space,
                                                                                               balanceSubset$covariateName),
                                                                       beforeMatchingMeanTreated = balanceSubset$beforeMatchingMeanTreated,
                                                                       beforeMatchingMeanComparator = balanceSubset$beforeMatchingMeanComparator,
                                                                       beforeMatchingStdDiff = balanceSubset$beforeMatchingStdDiff,
                                                                       afterMatchingMeanTreated = balanceSubset$afterMatchingMeanTreated,
                                                                       afterMatchingMeanComparator = balanceSubset$afterMatchingMeanComparator,
                                                                       afterMatchingStdDiff = balanceSubset$afterMatchingStdDiff,
                                                                       stringsAsFactors = FALSE))
                    } else {
                        resultsTable <- rbind(resultsTable, data.frame(Characteristic = specifications$label[i],
                                                                       beforeMatchingMeanTreated = balanceSubset$beforeMatchingMeanTreated,
                                                                       beforeMatchingMeanComparator = balanceSubset$beforeMatchingMeanComparator,
                                                                       beforeMatchingStdDiff = balanceSubset$beforeMatchingStdDiff,
                                                                       afterMatchingMeanTreated = balanceSubset$afterMatchingMeanTreated,
                                                                       afterMatchingMeanComparator = balanceSubset$afterMatchingMeanComparator,
                                                                       afterMatchingStdDiff = balanceSubset$afterMatchingStdDiff,
                                                                       stringsAsFactors = FALSE))
                    }
                }
            }
        }
    }
    resultsTable$beforeMatchingMeanTreated <- formatPercent(resultsTable$beforeMatchingMeanTreated)
    resultsTable$beforeMatchingMeanComparator <- formatPercent(resultsTable$beforeMatchingMeanComparator)
    resultsTable$beforeMatchingStdDiff <- formatStdDiff(resultsTable$beforeMatchingStdDiff)
    resultsTable$afterMatchingMeanTreated <- formatPercent(resultsTable$afterMatchingMeanTreated)
    resultsTable$afterMatchingMeanComparator <- formatPercent(resultsTable$afterMatchingMeanComparator)
    resultsTable$afterMatchingStdDiff <- formatStdDiff(resultsTable$afterMatchingStdDiff)
    
    headerRow <- as.data.frame(t(rep("", ncol(resultsTable))))
    colnames(headerRow) <- colnames(resultsTable)
    headerRow$beforeMatchingMeanTreated <- targetLabel
    headerRow$beforeMatchingMeanComparator <- comparatorLabel
    headerRow$afterMatchingMeanTreated <- targetLabel
    headerRow$afterMatchingMeanComparator <- comparatorLabel
    
    subHeaderRow <- as.data.frame(t(rep("", ncol(resultsTable))))
    colnames(subHeaderRow) <- colnames(resultsTable)
    subHeaderRow$Characteristic <- "Characteristic"
    subHeaderRow$beforeMatchingMeanTreated <- "%"
    subHeaderRow$beforeMatchingMeanComparator <- "%"
    subHeaderRow$beforeMatchingStdDiff <- "Std. diff"
    subHeaderRow$afterMatchingMeanTreated <- "%"
    subHeaderRow$afterMatchingMeanComparator <- "%"
    subHeaderRow$afterMatchingStdDiff <- "Std. diff"
    
    resultsTable <- rbind(headerRow, subHeaderRow, resultsTable)
    
    colnames(resultsTable) <- rep("", ncol(resultsTable))
    colnames(resultsTable)[2] <- beforeLabel
    colnames(resultsTable)[5] <- afterLabel
    return(resultsTable)
}

plotPs <- function(ps, targetName, comparatorName) {
    ps <- rbind(data.frame(x = ps$preferenceScore, y = ps$targetDensity, group = targetName),
                data.frame(x = ps$preferenceScore, y = ps$comparatorDensity, group = comparatorName))
    ps$group <- factor(ps$group, levels = c(as.character(targetName), as.character(comparatorName)))
    theme <- ggplot2::element_text(colour = "#000000", size = 12, margin = ggplot2::margin(0, 0.5, 0, 0.1, "cm"))
    plot <- ggplot2::ggplot(ps,
                            ggplot2::aes(x = x, y = y, color = group, group = group, fill = group)) +
        ggplot2::geom_density(stat = "identity") +
        ggplot2::scale_fill_manual(values = c(rgb(0.8, 0, 0, alpha = 0.5),
                                              rgb(0, 0, 0.8, alpha = 0.5))) +
        ggplot2::scale_color_manual(values = c(rgb(0.8, 0, 0, alpha = 0.5),
                                               rgb(0, 0, 0.8, alpha = 0.5))) +
        ggplot2::scale_x_continuous("Preference score", limits = c(0, 1)) +
        ggplot2::scale_y_continuous("Density") +
        ggplot2::theme(legend.title = ggplot2::element_blank(),
                       panel.grid.major = ggplot2::element_blank(),
                       panel.grid.minor = ggplot2::element_blank(),
                       legend.position = "top",
                       legend.text = theme,
                       axis.text = theme,
                       axis.title = theme)
    return(plot)
}



plotCovariateBalanceScatterPlot <- function(balance, 
                                            beforeLabel = "Before stratification", 
                                            afterLabel = "After stratification",
                                            limits = NULL) {
    if(is.null(limits)){limits <- c(min(c(balance$absBeforeMatchingStdDiff, balance$absAfterMatchingStdDiff),
                                   na.rm = TRUE),
                               max(c(balance$absBeforeMatchingStdDiff, balance$absAfterMatchingStdDiff),
                                   na.rm = TRUE))}
    theme <- ggplot2::element_text(colour = "#000000", size = 12)
    plot <- ggplot2::ggplot(balance, ggplot2::aes(x = absBeforeMatchingStdDiff, y = absAfterMatchingStdDiff)) +
        ggplot2::geom_point(color = rgb(0, 0, 0.8, alpha = 0.3), shape = 16, size = 2) +
        ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
        ggplot2::geom_hline(yintercept = 0) +
        ggplot2::geom_vline(xintercept = 0) +
        ggplot2::scale_x_continuous(beforeLabel, limits = limits) +
        ggplot2::scale_y_continuous(afterLabel, limits = limits) +
        ggplot2::theme(text = theme)
    
    return(plot)
}


plotKaplanMeier <- function(kaplanMeier, 
                            targetName, 
                            comparatorName,
                            ylims = NULL,
                            xBreaks = NULL,
                            targetColor = NULL,
                            comparatorColor = NULL,
                            targetColorFill = NULL,
                            comparatorColorFill = NULL,
                            pValue = NULL,
                            title = NULL) {
    data <- rbind(data.frame(time = kaplanMeier$time,
                             s = kaplanMeier$targetSurvival,
                             lower = kaplanMeier$targetSurvivalLb,
                             upper = kaplanMeier$targetSurvivalUb,
                             strata = paste0(" ", targetName, "    ")),
                  data.frame(time = kaplanMeier$time,
                             s = kaplanMeier$comparatorSurvival,
                             lower = kaplanMeier$comparatorSurvivalLb,
                             upper = kaplanMeier$comparatorSurvivalUb,
                             strata = paste0(" ", comparatorName)))
    
    xlims <- c(-max(data$time)/40, max(data$time))
    if(is.null(ylims)){
        ylims <- c(min(data$lower), max(data$upper))
    }
    xLabel <- "Follow-Up Duration (Days)"
    yLabel <- "Cumulative Incidence"
    if(is.null(xBreaks)){xBreaks <- kaplanMeier$time[!is.na(kaplanMeier$targetAtRisk)]}
    
    if(is.null(targetColor)|
       is.null(comparatorColor)|
       is.null(targetColorFill)|
       is.null(comparatorColorFill)
    ){
        targetColor = rgb(0.8, 0, 0, alpha = 0.8)
        comparatorColor = rgb(0, 0, 0.8, alpha = 0.8)
        targetColorFill = rgb(0.8, 0, 0, alpha = 0.3)
        comparatorColorFill = rgb(0, 0, 0.8, alpha = 0.3)
    }
    
    plot <- ggplot2::ggplot(data, ggplot2::aes(x = time,
                                               y = s,
                                               color = strata,
                                               fill = strata,
                                               ymin = lower,
                                               ymax = upper)) +
        ggplot2::geom_ribbon(color = rgb(0, 0, 0, alpha = 0)) +
        ggplot2::geom_step(size = 1) +
        ggplot2::scale_color_manual(values = c(rgb(255/255,99/255,71/255, alpha = 0.8),
                                               rgb(30/255,144/255,255/255, alpha = 0.8))) +
        ggplot2::scale_fill_manual(values = c(rgb(255/255,99/255,71/255, alpha = 0.3),
                                              rgb(30/255,144/255,255/255, alpha = 0.3))) +
        # ggplot2::scale_color_manual(values = c(rgb(0.8, 0, 0, alpha = 0.8),
        #                                        rgb(0, 0, 0.8, alpha = 0.8))) +
        # ggplot2::scale_fill_manual(values = c(rgb(0.8, 0, 0, alpha = 0.3),
        #                                       rgb(0, 0, 0.8, alpha = 0.3))) +
        ggplot2::scale_x_continuous(xLabel, limits = xlims, breaks = xBreaks) +
        ggplot2::scale_y_continuous(yLabel, limits = ylims) +
        theme_bw()+ #remove background
        theme (panel.border = element_blank(), axis.line = element_line())+#only x and y axis, not box
        theme (panel.grid.major.x = element_blank() , #remove vertical grid line
               panel.grid.minor.x = element_blank()  #remove vertical grid line
        )+
        ggplot2::theme(legend.title = ggplot2::element_blank(),
                       legend.position = "top",
                       legend.key.size = ggplot2::unit(1, "lines"),
                       plot.title = ggplot2::element_text(hjust = 0.5)) +
        ggplot2::theme(axis.title.y = ggplot2::element_text(vjust = -10)) 
    
    if(!is.null(pValue)){
        plot <- plot+ggplot2::annotate("text", label = pValue, parse =T,
                                       x=Inf,y=-Inf,hjust=2,vjust=-2, color = "black")
    }
    if(!is.null(title)){
        plot <- plot+ggplot2::ggtitle(title)
    }
    
    
    targetAtRisk <- kaplanMeier$targetAtRisk[!is.na(kaplanMeier$targetAtRisk)]
    comparatorAtRisk <- kaplanMeier$comparatorAtRisk[!is.na(kaplanMeier$comparatorAtRisk)]
    labels <- data.frame(x = c(0, xBreaks, xBreaks),
                         y = as.factor(c("Number at risk",
                                         rep(targetName, length(xBreaks)),
                                         rep(comparatorName, length(xBreaks)))),
                         label = c("",
                                   formatC(targetAtRisk, big.mark = ",", mode = "integer"),
                                   formatC(comparatorAtRisk, big.mark = ",", mode = "integer")))
    labels$y <- factor(labels$y, levels = c(comparatorName, targetName, "Number at risk"))
    dataTable <- ggplot2::ggplot(labels, ggplot2::aes(x = x, y = y, label = label)) + ggplot2::geom_text(size = 3.5, vjust = 0.5) + ggplot2::scale_x_continuous(xLabel,
                                                                                                                                                                limits = xlims,
                                                                                                                                                                breaks = xBreaks) + ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                                                                                                                                                                                                   panel.grid.minor = ggplot2::element_blank(),
                                                                                                                                                                                                   legend.position = "none",
                                                                                                                                                                                                   panel.border = ggplot2::element_blank(),
                                                                                                                                                                                                   panel.background = ggplot2::element_blank(),
                                                                                                                                                                                                   axis.text.x = ggplot2::element_text(color = "white"),
                                                                                                                                                                                                   axis.title.x = ggplot2::element_text(color = "white"),
                                                                                                                                                                                                   axis.title.y = ggplot2::element_blank(),
                                                                                                                                                                                                   axis.ticks = ggplot2::element_line(color = "white"))
    plots <- list(plot, dataTable)
    grobs <- widths <- list()
    for (i in 1:length(plots)) {
        grobs[[i]] <- ggplot2::ggplotGrob(plots[[i]])
        widths[[i]] <- grobs[[i]]$widths[2:5]
    }
    maxwidth <- do.call(grid::unit.pmax, widths)
    for (i in 1:length(grobs)) {
        grobs[[i]]$widths[2:5] <- as.list(maxwidth)
    }
    plot <- gridExtra::grid.arrange(grobs[[1]], grobs[[2]], heights = c(400, 100))
    
    
    return(plot)
}



plotScatter <- function(controlResults) {
    size <- 2
    labelY <- 0.7
    d <- rbind(data.frame(yGroup = "Uncalibrated",
                          logRr = controlResults$logRr,
                          seLogRr = controlResults$seLogRr,
                          ci95Lb = controlResults$ci95Lb,
                          ci95Ub = controlResults$ci95Ub,
                          trueRr = controlResults$effectSize),
               data.frame(yGroup = "Calibrated",
                          logRr = controlResults$calibratedLogRr,
                          seLogRr = controlResults$calibratedSeLogRr,
                          ci95Lb = controlResults$calibratedCi95Lb,
                          ci95Ub = controlResults$calibratedCi95Ub,
                          trueRr = controlResults$effectSize))
    d <- d[!is.na(d$logRr), ]
    d <- d[!is.na(d$ci95Lb), ]
    d <- d[!is.na(d$ci95Ub), ]
    if (nrow(d) == 0) {
        return(NULL)
    }
    d$Group <- as.factor(d$trueRr)
    d$Significant <- d$ci95Lb > d$trueRr | d$ci95Ub < d$trueRr
    temp1 <- aggregate(Significant ~ Group + yGroup, data = d, length)
    temp2 <- aggregate(Significant ~ Group + yGroup, data = d, mean)
    temp1$nLabel <- paste0(formatC(temp1$Significant, big.mark = ","), " estimates")
    temp1$Significant <- NULL
    
    temp2$meanLabel <- paste0(formatC(100 * (1 - temp2$Significant), digits = 1, format = "f"),
                              "% of CIs include ",
                              temp2$Group)
    temp2$Significant <- NULL
    dd <- merge(temp1, temp2)
    dd$tes <- as.numeric(as.character(dd$Group))
    
    breaks <- c(0.1, 0.25, 0.5, 1, 2, 4, 6, 8, 10)
    theme <- ggplot2::element_text(colour = "#000000", size = 12)
    themeRA <- ggplot2::element_text(colour = "#000000", size = 12, hjust = 1)
    themeLA <- ggplot2::element_text(colour = "#000000", size = 12, hjust = 0)
    
    d$Group <- paste("True hazard ratio =", d$Group)
    dd$Group <- paste("True hazard ratio =", dd$Group)
    alpha <- 1 - min(0.95 * (nrow(d)/nrow(dd)/50000)^0.1, 0.95)
    plot <- ggplot2::ggplot(d, ggplot2::aes(x = logRr, y = seLogRr), environment = environment()) +
        ggplot2::geom_vline(xintercept = log(breaks), colour = "#AAAAAA", lty = 1, size = 0.5) +
        ggplot2::geom_abline(ggplot2::aes(intercept = (-log(tes))/qnorm(0.025), slope = 1/qnorm(0.025)),
                             colour = rgb(0.8, 0, 0),
                             linetype = "dashed",
                             size = 1,
                             alpha = 0.5,
                             data = dd) +
        ggplot2::geom_abline(ggplot2::aes(intercept = (-log(tes))/qnorm(0.975), slope = 1/qnorm(0.975)),
                             colour = rgb(0.8, 0, 0),
                             linetype = "dashed",
                             size = 1,
                             alpha = 0.5,
                             data = dd) +
        ggplot2::geom_point(size = size,
                            color = rgb(0, 0, 0, alpha = 0.05),
                            alpha = alpha,
                            shape = 16) +
        ggplot2::geom_hline(yintercept = 0) +
        ggplot2::geom_label(x = log(0.15),
                            y = 0.9,
                            alpha = 1,
                            hjust = "left",
                            ggplot2::aes(label = nLabel),
                            size = 5,
                            data = dd) +
        ggplot2::geom_label(x = log(0.15),
                            y = labelY,
                            alpha = 1,
                            hjust = "left",
                            ggplot2::aes(label = meanLabel),
                            size = 5,
                            data = dd) +
        ggplot2::scale_x_continuous("Hazard ratio",
                                    limits = log(c(0.1, 10)),
                                    breaks = log(breaks),
                                    labels = breaks) +
        ggplot2::scale_y_continuous("Standard Error", limits = c(0, 1)) +
        ggplot2::facet_grid(yGroup ~ Group) +
        ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                       panel.background = ggplot2::element_blank(),
                       panel.grid.major = ggplot2::element_blank(),
                       axis.ticks = ggplot2::element_blank(),
                       axis.text.y = themeRA,
                       axis.text.x = theme,
                       axis.title = theme,
                       legend.key = ggplot2::element_blank(),
                       strip.text.x = theme,
                       strip.text.y = theme,
                       strip.background = ggplot2::element_blank(),
                       legend.position = "none")
    
    return(plot)
}

getControlResults <- function(connection, targetId, comparatorId, analysisId, databaseId) {
    results <- cohortMethodResult[cohortMethodResult$targetId == targetId &
                                      cohortMethodResult$comparatorId == comparatorId &
                                      cohortMethodResult$analysisId == analysisId &
                                      cohortMethodResult$databaseId == databaseId, ]
    results$effectSize <- NA
    idx <- results$outcomeId %in% negativeControlOutcome$outcomeId
    results$effectSize[idx] <- 1
    if (!is.null(positiveControlOutcome)) {
        idx <- results$outcomeId %in% positiveControlOutcome$outcomeId
        results$effectSize[idx] <- positiveControlOutcome$effectSize[match(results$outcomeId[idx],
                                                                           positiveControlOutcome$outcomeId)]
    }
    results <- results[!is.na(results$effectSize), ]
    return(results)
}

uncapitalize <- function(x) {
    if (is.character(x)) {
        substr(x, 1, 1) <- tolower(substr(x, 1, 1))
    }
    x
}

capitalize <- function(x) {
    substr(x, 1, 1) <- toupper(substr(x, 1, 1))
    x
}

forestPlotGenerator<-function(metaResult,
                              space = "             ",
                              limited=F){
    meta=metaResult$meta
    targetName = metaResult$targetName
    comparatorName = metaResult$comparatorName
    ### Initial setting for jama
    #meta::settings.meta("jama") #meta::settings.meta("meta")
    
    ### decrease margins so the full space is used
    #par(mar=c(5,5,1,2))
    
    xLim= ceiling(max (1/exp(meta$lower.random),exp(meta$upper.random)))
    if(limited){
        leftCols = c("studlab",
                     "effect","ci")
        leftLabs= c("Source", "HR","95% CI")
    }else{
        leftCols = c("studlab",
                     "n.e","event.e" ,#"event.rate.t", 
                     "n.c","event.c",#"event.rate.c",
                     "effect","ci"#,"HR"
        )
        leftLabs= c("Source", "Total","Event","Total","Event","HR","95% CI")
    }
    
    forestPlot<-meta::forest.meta(meta,
                                  studlab=TRUE,
                                  
                                  #overall=TRUE,
                                  #pooled.totals=meta$comb.random, 
                                  pooled.total =TRUE,
                                  pooled.events=TRUE,
                                  leftcols = leftCols,
                                  rightcols=F,
                                  #col.study="black",
                                  #col.square="gray",
                                  #col.inside="white",
                                  #col.diamond="gray",
                                  #col.diamond.lines="black",
                                  leftlabs = leftLabs,
                                  lab.e = paste0(space,targetName),
                                  lab.c = paste0(space,comparatorName),
                                  lab.e.attach.to.col = c("n.e"),
                                  lab.c.attach.to.col = c("n.c"),
                                  # leftlabs = c("Event rate", "Event rate"#, "HR (95% CI)"
                                  #              ),
                                  # rightcols = c("w.random"),
                                  fontsize=12,
                                  comb.fixed = FALSE,
                                  comb.random = TRUE,
                                  text.random = "Overall",
                                  col.diamond.random = "royalblue",
                                  col.diamond.lines = "black",
                                  #xlab = "Hazard Ratio (95% CI)",
                                  
                                  digits = 2,
                                  digits.pval =3,
                                  digits.I2 = 1,
                                  just.studlab="left",
                                  #just.addcols ="right",
                                  just.addcols.left= "right",
                                  #just.addcols.right= "right",
                                  just = "center",
                                  xlim = c(round(1/xLim,2),xLim),
                                  plotwidth ="8cm",
                                  #layout="JAMA",
                                  spacing =1,
                                  addrow.overall=TRUE,
                                  print.I2 = TRUE,
                                  # overall.hetstat = T,
                                  # hetstat= F,
                                  # print.I2=F,
                                  print.pval.I2=F,
                                  print.tau2 = F,
                                  # print.Q	=F,
                                  print.pval.Q = F,
                                  # zero.pval = F,
                                  # print.Rb  = F,
                                  #smlab = "",
                                  #sortvar=TE,
                                  label.lef = sprintf("Favors\n%s",capitalize(targetName)),
                                  label.right = sprintf("Favors\n%s",capitalize(comparatorName)),
                                  scientific.pval = F,#meta::gs("scientific.pval"), 
                                  big.mark =","#meta::gs("big.mark),
                                  
    )
    
    
    
    # forestPlot<-meta::forest.meta(meta,
    #                               studlab=TRUE,
    #                               #overall=TRUE,
    #                               #pooled.totals=meta$comb.random, 
    #                               pooled.events=TRUE,
    #                               leftcols = c("studlab",
    #                                            "n.e","event.e" ,#"event.rate.t", 
    #                                            "n.c","event.c",#"event.rate.c",
    #                                            "effect","ci"#,"HR"
    #                               ),
    #                               #col.study="black",
    #                               #col.square="gray",
    #                               #col.inside="white",
    #                               #col.diamond="gray",
    #                               #col.diamond.lines="black",
    #                               lab.e = paste0(space,targetName),
    #                               lab.c = paste0(space,comparatorName),
    #                               lab.e.attach.to.col = c("n.e"),
    #                               lab.c.attach.to.col = c("n.c"),
    #                               # leftlabs = c("Event rate", "Event rate"#, "HR (95% CI)"
    #                               #              ),
    #                               # rightcols = c("w.random"),
    #                               digits = 3,
    #                               comb.fixed = FALSE,
    #                               comb.random = TRUE,
    #                               text.random = "Overall",
    #                               xlab = "Hazard Ratio (95% CI)",
    #                               just.studlab="left",
    #                               just.addcols ="right",
    #                               xlim = c(0.5,2),
    #                               plotwidth ="8cm",
    #                               layout="JAMA",
    #                               spacing =1,
    #                               addrow.overall=TRUE,
    #                               test.overall.random =F,
    #                               overall.hetstat = F,
    #                               print.tau2 = F,
    #                               print.Q	=F,
    #                               print.pval.Q = F,
    #                               label.lef = sprintf("Favors\n%s",capitalize(targetName)),
    #                               label.right = sprintf("Favors\n%s",capitalize(comparatorName)),
    #                               scientific.pval = T,#meta::gs("scientific.pval"), 
    #                               big.mark =" "#meta::gs("big.mark)
    # )
    return(forestPlot)
}

doMeta <- function(data,
                   targetId = 874,
                   comparatorId = 929,
                   outcomeId = 1236,
                   analysisId = 1,
                   targetName = "ticagrelor",
                   comparatorName = "clopidogrel",
                   outcomeName = "GI bleeding",
                   calibration=F){
    
    databaseIds <- data$databaseId
    if(calibration){
        logRr = data$calibratedLogRr
        logLb95Ci = log( data$calibratedCi95Lb )
        logUb95Ci = log( data$calibratedCi95Ub )
        seLogRr = data$calibratedSeLogRr
    }else{
        logRr = data$logRr
        logLb95Ci = log( data$ci95Lb )
        logUb95Ci = log( data$ci95Ub )
        #seLogRr = (logUb95Ci-logLb95Ci) / (2 * qnorm(0.975))
        seLogRr = data$seLogRr
    }
    
    meta <- meta::metagen(TE = logRr, 
                          seTE = seLogRr, 
                          studlab = databaseIds, 
                          sm = "HR", 
                          hakn = FALSE,
                          comb.fixed = TRUE,
                          comb.random = TRUE
    )
    
    meta$n.e <- data$targetSubjects
    meta$event.e <- data$targetOutcomes
    meta$event.rate.t <- round(with(data, targetOutcomes/(targetDays/365))*1000,1)
    meta$person.year.t <- with(data, round((targetDays/365),0))
    
    meta$n.c <- data$comparatorSubjects
    meta$event.c <- data$comparatorOutcomes
    meta$event.rate.c <- round(with(data, comparatorOutcomes/(comparatorDays/365))*1000,1)
    meta$person.year.c<-with(data, round((comparatorDays/365),0))
    
    return (list(meta=meta,
                 databaseIds = databaseIds,
                 targetId = targetId,
                 comparatorId = comparatorId,
                 outcomeId = outcomeId,
                 analysisId = analysisId,
                 targetName = targetName,
                 comparatorName = comparatorName,
                 outcomeName = outcomeName
    ))
}
outcomeIdOfInterest = 1233
narrowOutcomeIdOfInterest = 1180
blankingPeriodAnalysisIds = c(5,6,7,12,13,14,19,20,21)
outcomeName <- "Ischemic stroke"
limits = c(0.5,2.0)
breaks = c(0.5,0.75,1.0,1.5,2.0)

prepareGridForest <- function(mainResults,
                              cohortMethodAnalysis,
                              outcomeOfInterest,
                              outcomeIdOfInterest,
                              narrowOutcomeIdOfInterest=NULL,
                              outcomeNameOfInterest = "",
                              xLimits = c(0.5,2.0),
                              breaks = c(0.5,0.75,1.0,1.5,2.0),
                              blankingPeriodAnalysisIds= c(5,6,7,12,13,14,19,20,21)){
    targetAnalyses <- cohortMethodAnalysis$analysisId [!(grepl("interaction",cohortMethodAnalysis$description)|
                                                             grepl("without matching",cohortMethodAnalysis$description))]
    targetAnalyses<-unique(targetAnalyses)
    
    results<-data.frame()
    for(analysisId in targetAnalyses){
        for (outcomeId in c(outcomeIdOfInterest,narrowOutcomeIdOfInterest)){
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
    results$outcomeName<-gsub("conditino","condition",results$outcomeName)
    results<-results[ !((results$analysisId%in%blankingPeriodAnalysisIds) & (results$outcomeId%in%narrowOutcomeIdOfInterest)),]
    
    results$TAR<-factor(results$TAR,levels = c("One-year","Five-year","On-treatment"))
    
    results$Adjustment<-factor(results$Adjustment,levels = c("1-to-1 PS matching",
                                                             "Variable-ratio PS matching",
                                                             "PS stratification"))
    uniqueOutcomeNames<-unique(results$outcomeName)
    primaryConditionOutcomeName<-uniqueOutcomeNames[grepl("primary", uniqueOutcomeNames)]
    blankingPeriodOutcomeName<-uniqueOutcomeNames[grepl("blanking", uniqueOutcomeNames)]
    
    if(!is.null(narrowOutcomeIdOfInterest)){
        outcomeNameOrder <- c(outcomeNameOfInterest, primaryConditionOutcomeName,blankingPeriodOutcomeName)
    } else{
        outcomeNameOrder <- c(outcomeNameOfInterest,blankingPeriodOutcomeName)
    }
    results$outcomeName <- factor(results$outcomeName, levels = outcomeNameOrder)
    
    xLim = max(ceiling(median(results$ci95Ub, na.rm=TRUE)),ceiling(1/median(results$ci95Lb, na.rm=TRUE)),2, na.rm = TRUE)
    xLim<-ifelse(xLim< (max( 1/results$rr,results$rr,na.rm=TRUE)),ceiling(max( 1/results$rr,results$rr,na.rm=TRUE)),xLim)
    limits = c(1/xLim,xLim)
    limits = xLimits
    
    results <- results %>% mutate(#ci95Lb =ifelse(ci95Lb < limits[1], ci95Lb-1, ci95Lb), 
        #ci95Ub = ifelse(ci95Lb > limits[2], ci95Ub+1, ci95Ub),
        ci95LbOut = ifelse(ci95Lb < limits[1], rr - limits[1], NA), 
        ci95UbOut = ifelse(ci95Ub > limits[2], rr - limits[2], NA))
    results$Significance<-factor(ifelse(results$p<0.05,"P<.05","Not significant"),
                                 levels = c("P<.05","Not significant")
    )
    
    return(results)
    
}


gridForest <- function(results, breaks = c(0.9,1,1.1,1.2), outlierMoverLower= 0.03,outlierMoverUpper= 0.1, xLimits=c(0.85,1.3)){
    #hrExpression<-expression("Hazard Ratio (95% Confidence Interval) \n Favor Clopidogrel     Favor Tiacgrelor")
    hrExpression <- "Hazard Ratio (95% Confidence Interval)"
    shapeValue = c(17,21)#shape for closed and open center
    
    if (min(as.numeric(results$Significance))==2) shapeValue = c(21,17)
    resultPlot<-ggplot2::ggplot(data=results,
                    aes(x = Adjustment,y = rr, ymin =  ci95Lb, ymax = ci95Ub, shape =  Significance))+
        ggplot2::geom_pointrange(aes(col=Adjustment, shape=Significance), size = 0.6
        )+
        scale_shape_manual(values=shapeValue)+ #shape for closed and open center
        geom_hline(yintercept=1, linetype="dotted")+
        #ggplot2::geom_hline(aes(fill=Adjustment),yintercept =1, linetype=2)+
        xlab('Definition of the Outcomes')+ ylab(hrExpression)+
        ggplot2::geom_errorbar(aes(ymin=ci95Lb, ymax=ci95Ub,col=Adjustment),width=0.2,cex=1)+ 
        geom_segment(aes(x = Adjustment, xend = Adjustment, y = rr, yend = rr - ci95LbOut-outlierMoverLower,col=Adjustment), 
                     arrow = ggplot2::arrow(angle=45,
                                            unit (0.3,"cm")),
                     size=1, show.legend=FALSE)+
        geom_segment(aes(x = Adjustment, xend = Adjustment, y = rr, yend = rr - ci95UbOut+outlierMoverUpper,col=Adjustment), 
                     arrow = ggplot2::arrow(angle=45,
                                            unit (0.3,"cm")),
                     size=1, show.legend=FALSE)+
        #ggplot2::theme(axis.text.x=element_blank())+
        ggplot2::facet_grid(outcomeName~TAR)+
        #facet_wrap(~matching,strip.position="left",nrow=9,scales = "free_y") +
        ggplot2::theme(plot.title=element_text(size=18,face="bold"),
                       axis.text.y=element_blank(),
                       axis.ticks.y=element_blank(),
                       axis.text.x=element_text(face="bold"),
                       axis.title=element_text(size=18,face="bold"),
                       strip.text.y = element_text(hjust=0,vjust = 1,angle=180,face="bold"),
                       strip.text.x = element_blank()
        )+
        ggplot2::coord_flip(ylim = xLimits)+scale_y_continuous(trans='log10', breaks = breaks
        )+
        ggplot2::theme_bw()+
        ggplot2::theme(axis.text.y=element_blank(),
                       panel.grid.major.y = element_blank(),
                       axis.ticks.y=element_blank())+
        scale_x_discrete(limits=rev(levels(results$Adjustment)))
    return(resultPlot)
}
