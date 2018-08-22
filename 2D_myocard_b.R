# ------------------------------------------------------------
# A random forest prediction using Zimbabwe Standard DHS 2015
# Zimbabwe 2015 selected because it has IR, MR, and AR
# ky71_flattened.dta
#
# Eric Chow, 2018-05-14
# ------------------------------------------------------------

rm(list=ls(all=TRUE))
gc()
library(tidyverse)
library(readxl)
library(forestplot)
library(foreign)
library(epiR)
library(stringr)

setwd("~/QSU/stress")
source("~/QSU/qsu.R")

# read dataset from QSU server
# stress <- data.frame(read_xlsx("/Volumes/QSU/Datasets/Cardiology/Vedant_Pargaonkar/stress_test/data/merged_ECG_stress\ study_5_25\ -\ Subset_8_14.XLSX"))

# read deid data from data
stress <- read.dta("data/stress.dta")
names(stress) <- c("sex", "ecg", "echo", "comp", "any_abnorm", "ed", "mvd", "mb")
head(stress)

# --------------------------------------------------
# turn abnormalities into factors
stress$any_abnorm <- as.factor(stress$any_abnorm)
stress$ed <- as.factor(stress$ed)
stress$mvd <- as.factor(stress$mvd)
stress$mb <- as.factor(stress$mb)

levels(stress$any_abnorm) <- c("healthy", "disease")
levels(stress$ed) <- c("healthy", "disease")
levels(stress$mvd) <- c("healthy", "disease")
levels(stress$mb) <- c("healthy", "disease")

# --------------------------------------------------
# turn tests into factors

stress$ecg <- as.factor(stress$ecg)
stress$echo <- as.factor(stress$echo)
stress$comp <- as.factor(stress$comp)

levels(stress$ecg ) <- c("neg[-]", "pos[+]")
levels(stress$echo) <- c("neg[-]", "pos[+]")
levels(stress$comp) <- c("neg[-]", "pos[+]")




# stressm <- stress[stress$sex == 1, ]
# stressm[,c("echo","any_abnorm")]

# get se, sp, ppv, npv
t_any_echo <- table(stress$echo, stress$any_abnorm)[2:1,2:1]
t_ed_echo <- table(stress$echo, stress$ed)[2:1,2:1]
t_mvd_echo <- table(stress$echo, stress$mvd)[2:1,2:1]
t_mb_echo <- table(stress$echo, stress$mb)[2:1,2:1]

t_any_ecg <- table(stress$ecg, stress$any_abnorm)[2:1,2:1]
t_ed_ecg <- table(stress$ecg, stress$ed)[2:1,2:1]
t_mvd_ecg <- table(stress$ecg, stress$mvd)[2:1,2:1]
t_mb_ecg <- table(stress$ecg, stress$mb)[2:1,2:1]

t_any_comp <- table(stress$comp, stress$any_abnorm)[2:1,2:1]
t_ed_comp <- table(stress$comp, stress$ed)[2:1,2:1]
t_mvd_comp <- table(stress$comp, stress$mvd)[2:1,2:1]
t_mb_comp <- table(stress$comp, stress$mb)[2:1,2:1]


# -----------------------------------------------------------------------------
# a function, given the table above, returns a clean string
# ex:
# get_stat(t_mb_echo, "se")
get_stat <- function(table, stat = "se") {
    # the Se, Sp, PPV, NPV
    se <- 100*round(epi.tests(table)$elements$sensitivity,3)
    sp <- 100*round(epi.tests(table)$elements$specificity,3)
    ppv <- 100*round(epi.tests(table)$elements$pv.positive,3)
    npv <- 100*round(epi.tests(table)$elements$pv.negative,3)

    se_text <- sprintf("%3.1f%% [%3.1f%% - %3.1f%%]", se$est, se$lower, se$upper)
    sp_text <- sprintf("%3.1f%% [%3.1f%% - %3.1f%%]", sp$est, sp$lower, sp$upper)
    ppv_text <- sprintf("%3.1f%% [%3.1f%% - %3.1f%%]", ppv$est, ppv$lower, ppv$upper)
    npv_text <- sprintf("%3.1f%% [%3.1f%% - %3.1f%%]", npv$est, npv$lower, npv$upper)

    if (stat == "se") {
        result <- se_text
    }
    if (stat == "sp") {
        result <- sp_text
    }
    if (stat == "ppv") {
        result <- ppv_text
    }
    if (stat == "npv") {
        result <- npv_text
    }
    return(result)
}

get_est <- function(table, stat= "se", est = 1) {
    # the Se, Sp, PPV, NPV
    se <- round(epi.tests(table)$elements$sensitivity,3)
    sp <- round(epi.tests(table)$elements$specificity,3)
    ppv <- round(epi.tests(table)$elements$pv.positive,3)
    npv <- round(epi.tests(table)$elements$pv.negative,3)


        if (stat == "se") {
            this_stat <- se
        }
        if (stat == "sp") {
            this_stat <- sp
        }
        if (stat == "ppv") {
            this_stat <- ppv
        }
        if (stat == "npv") {
            this_stat <- npv
        }

        return(as.numeric(this_stat[est]))
}

# get_est(t_any_comp, stat="se", est =1)
#


                   # the labels -----------
labeltext <- cbind(c("Sensitivity",
                     "Echo",
                     "ECG",
                     "Combined",

                     "Specificity",
                     "Echo",
                     "ECG",
                     "Combined",

                     "PPV",
                     "Echo",
                     "ECG",
                     "Combined",

                     "NPV",
                     "Echo",
                     "ECG",
                     "Combined"
                     ),

                   # the data ------------
                   c("Percent [95% CI]",
                    get_stat(t_any_echo, "se"),
                    get_stat(t_any_ecg, "se"),
                    get_stat(t_any_comp, "se"),

                    "",
                    get_stat(t_any_echo, "sp"),
                    get_stat(t_any_ecg, "sp"),
                    get_stat(t_any_comp, "sp"),

                    "",
                    get_stat(t_any_echo, "ppv"),
                    get_stat(t_any_ecg, "ppv"),
                    get_stat(t_any_comp, "ppv"),

                    "",
                    get_stat(t_any_echo, "npv"),
                    get_stat(t_any_ecg, "npv"),
                    get_stat(t_any_comp, "npv")
                     ))

mean_points <-  c(NA,
        get_est(t_any_echo, stat="se", est =1),
        get_est(t_any_ecg, stat="se", est =1),
        get_est(t_any_comp, stat="se", est =1),

        NA,
        get_est(t_any_echo, stat="sp", est =1),
        get_est(t_any_ecg, stat="sp", est =1),
        get_est(t_any_comp, stat="sp", est =1),

        NA,
        get_est(t_any_echo, stat="ppv", est =1),
        get_est(t_any_ecg, stat="ppv", est =1),
        get_est(t_any_comp, stat="ppv", est =1),

        NA,
        get_est(t_any_echo, stat="npv", est =1),
        get_est(t_any_ecg, stat="npv", est =1),
        get_est(t_any_comp, stat="npv", est =1)
       )

lower_points <-  c(NA,
       get_est(t_any_echo, stat="se", est =2),
       get_est(t_any_ecg, stat="se", est =2),
       get_est(t_any_comp, stat="se", est =2),

       NA,
       get_est(t_any_echo, stat="sp", est =2),
       get_est(t_any_ecg, stat="sp", est =2),
       get_est(t_any_comp, stat="sp", est =2),

       NA,
       get_est(t_any_echo, stat="ppv", est =2),
       get_est(t_any_ecg, stat="ppv", est =2),
       get_est(t_any_comp, stat="ppv", est =2),

       NA,
       get_est(t_any_echo, stat="npv", est =2),
       get_est(t_any_ecg, stat="npv", est =2),
       get_est(t_any_comp, stat="npv", est =2)
      )

upper_points <-  c(NA,
      get_est(t_any_echo, stat="se", est =3),
      get_est(t_any_ecg, stat="se", est =3),
      get_est(t_any_comp, stat="se", est =3),

      NA,
      get_est(t_any_echo, stat="sp", est =3),
      get_est(t_any_ecg, stat="sp", est =3),
      get_est(t_any_comp, stat="sp", est =3),

      NA,
      get_est(t_any_echo, stat="ppv", est =3),
      get_est(t_any_ecg, stat="ppv", est =3),
      get_est(t_any_comp, stat="ppv", est =3),

      NA,
      get_est(t_any_echo, stat="npv", est =3),
      get_est(t_any_ecg, stat="npv", est =3),
      get_est(t_any_comp, stat="npv", est =3)
     )

# ------------------------------------------------------------------------------
pdf("figs/fig2A_any_abnormality.pdf")
    forestplot(labeltext = labeltext,
        mean  = mean_points,
        lower = lower_points,
        upper = upper_points,
        is.summary = c(TRUE, rep(FALSE,3),TRUE, rep(FALSE,3),TRUE, rep(FALSE,3),TRUE, rep(FALSE,3)),
        graph.pos="right",
        xlab = "Proportion",
        title = "Diagnostic Properties of Stress Testing: Any Abnormality",
        hrzl_lines = TRUE,
        lwd.ci = 1.4, ci.vertices = TRUE,
        col = fpColors(lines="dodgerblue3", box="dodgerblue3"),
        grid = TRUE,
        zero = 1, lwd.zero = 1.2,
        boxsize = 0.25, xticks = (0:10/10), xtick.digits=1)
dev.off()
