## Exploración de Datos: Kaplan-Meier
# Author: Christian Badillo
# Date: 02/12/2025

# Datos.
myData <- readxl::read_xls("data_cancer.xls")

# Ver los datos.
View(myData)

# Variables de Importancia.
survivalTime <- as.numeric(myData$`Overall Survival (Months)`)
censoring <- ifelse(myData$`Patient's Vital Status` == "Died of Disease", 1, 0)
radioTherapy <- factor(myData$`Radio Therapy`, levels=c("Yes", "No"), labels = c("Yes", "No"))
surgeryType <- factor(myData$`Type of Breast Surgery`, levels=c("Mastectomy", "Breast Conserving"), labels = c("Mastectomy", "Breast Conserving"))

# -------------------------------------------------------------------------------------------------
# -------------------------------- Kaplan-Meier sin Grupos ----------------------------------------
# -------------------------------------------------------------------------------------------------

library(survival)
library(survminer)

model <- Surv(survivalTime, censoring)

fit <- survfit(model ~ 1, data = myData)

ggsurvplot(fit, data = myData)

# -------------------------------------------------------------------------------------------------
# --------------------------- Kaplan-Meier grupo: Radio Therapy ------------------------------------
# -------------------------------------------------------------------------------------------------

fit.rt <- survfit(model ~ radioTherapy, data = myData)

ggsurvplot(fit.rt, data = myData, pval.method = TRUE, pval = TRUE, 
           log.rank.weights = "n", 
           censor = T,
           conf.int = T,
           conf.int.style = "ribbon",
           conf.int.alpha = 0.18,
           pval.size = 7,
           pval.method.size = 8
           )

# -------------------------------------------------------------------------------------------------
# ---------------------- Kaplan-Meier grupo: Type of Breast Surgery -------------------------------
# -------------------------------------------------------------------------------------------------

fit.tbs <- survfit(model ~ surgeryType, data = myData)

ggsurvplot(fit.tbs, data = myData, pval.method = TRUE, pval = TRUE, 
           log.rank.weights = "n", 
           censor = T,
           conf.int = T,
           conf.int.style = "ribbon",
           conf.int.alpha = 0.18,
           pval.size = 7,
           pval.method.size = 8
)
