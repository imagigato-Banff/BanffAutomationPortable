## Figure made on  R version 4.1.3 (2022-03-10)

##load the packages 
library(ggplot2)
library(ggfortify)
library(pec)
library(survival)
library(Hmisc)
library(survminer)
library(readr)


## Main Figure 4
##import the data
Figure4_data <- read_csv("Figure4_data.csv")
##run the cox model to obtain the hazard ratio and the p value
Figure4_data$CATEGORY= as.factor(Figure4_data$CATEGORY)
summary(coxph(Surv(TIME,GRAFT_LOSS) ~ CATEGORY, data=Figure4_data))    

##run the code to create the plot
Figure4 <-  ggsurvplot(
    fit = survfit(Surv(TIME, GRAFT_LOSS) ~ CATEGORY, data = Figure4_data), 
    xlab = "Time post-biopsy (years)", xlim = c(0,3),break.x.by=1,  
    risk.table = TRUE, cumevents = FALSE, risk.table.y.text = FALSE, conf.int = FALSE,
    pval=c("Log-rank p value<0.0001"),pval.coord=c(2,0.6), pval.size=8,
    cumevents.y.text = FALSE,risk.table.height = 0.10,  palette=c("#66CCFF", "#3399FF", "#FF6699", "#CC0033"),
    font.x = c(13, "bold"), legend.labs = c("No rejection/no rejection. reference HR=1", 
                                            "Rejection/rejection. HR=4.7, 95% CI: 3.1-7.1, p<0.0001", 
                                            "Rejection/no rejection. HR=0.9, 95% CI: 0.1-6.7, p=0.941",
                                            "No rejection/rejection. HR=6.4, 95% CI: 3.9-10.6, p<0.0001"), 
    font.y = c(13, "bold"), font.main= c(18, "bold"), 
    font.legend=c(13, "bold"), 
    legend.title=c("Legend: Original diagnosis (pathologist)/reclassified diagnosis (Banff Automation System)"),
    legend=c(0.5,0.25),
    ylab = "Graft survival probability")
Figure4$table <- Figure4$table + theme_cleantable()
Figure4 


