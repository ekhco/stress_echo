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

setwd("~/QSU/stress_echo")
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




stressm <- stress[stress$sex == 1, ]
stressf <- stress[stress$sex == 0, ]
# stressm[,c("echo","any_abnorm")]

# -----------------------------------------------------------
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

# -----------------------------------------------------------
# get se, sp, ppv, npv - male
tm_any_echo <- table(stressm$echo, stressm$any_abnorm)[2:1,2:1]
tm_ed_echo <- table(stressm$echo, stressm$ed)[2:1,2:1]
tm_mvd_echo <- table(stressm$echo, stressm$mvd)[2:1,2:1]
tm_mb_echo <- table(stressm$echo, stressm$mb)[2:1,2:1]

tm_any_ecg <- table(stressm$ecg, stressm$any_abnorm)[2:1,2:1]
tm_ed_ecg <- table(stressm$ecg, stressm$ed)[2:1,2:1]
tm_mvd_ecg <- table(stressm$ecg, stressm$mvd)[2:1,2:1]
tm_mb_ecg <- table(stressm$ecg, stressm$mb)[2:1,2:1]

tm_any_comp <- table(stressm$comp, stressm$any_abnorm)[2:1,2:1]
tm_ed_comp <- table(stressm$comp, stressm$ed)[2:1,2:1]
tm_mvd_comp <- table(stressm$comp, stressm$mvd)[2:1,2:1]
tm_mb_comp <- table(stressm$comp, stressm$mb)[2:1,2:1]

# -----------------------------------------------------------
# get se, sp, ppv, npv - female
tf_any_echo <- table(stressf$echo, stressf$any_abnorm)[2:1,2:1]
tf_ed_echo <- table(stressf$echo, stressf$ed)[2:1,2:1]
tf_mvd_echo <- table(stressf$echo, stressf$mvd)[2:1,2:1]
tf_mb_echo <- table(stressf$echo, stressf$mb)[2:1,2:1]

tf_any_ecg <- table(stressf$ecg, stressf$any_abnorm)[2:1,2:1]
tf_ed_ecg <- table(stressf$ecg, stressf$ed)[2:1,2:1]
tf_mvd_ecg <- table(stressf$ecg, stressf$mvd)[2:1,2:1]
tf_mb_ecg <- table(stressf$ecg, stressf$mb)[2:1,2:1]

tf_any_comp <- table(stressf$comp, stressf$any_abnorm)[2:1,2:1]
tf_ed_comp <- table(stressf$comp, stressf$ed)[2:1,2:1]
tf_mvd_comp <- table(stressf$comp, stressf$mvd)[2:1,2:1]
tf_mb_comp <- table(stressf$comp, stressf$mb)[2:1,2:1]


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
                     "Stress echo",
                     "Stress ECG",
                     "Comprehensive",

                     "Specificity",
                     "Stress echo",
                     "Stress ECG",
                     "Comprehensive",

                     "PPV",
                     "Stress echo",
                     "Stress ECG",
                     "Comprehensive",

                     "NPV",
                     "Stress echo",
                     "Stress ECG",
                     "Comprehensive"
                     ),

                   # the data ------------
                   c("Both sexes [95% CI]",
                    get_stat(t_ed_echo, "se"),
                    get_stat(t_ed_ecg, "se"),
                    get_stat(t_ed_comp, "se"),

                    "",
                    get_stat(t_ed_echo, "sp"),
                    get_stat(t_ed_ecg, "sp"),
                    get_stat(t_ed_comp, "sp"),

                    "",
                    get_stat(t_ed_echo, "ppv"),
                    get_stat(t_ed_ecg, "ppv"),
                    get_stat(t_ed_comp, "ppv"),

                    "",
                    get_stat(t_ed_echo, "npv"),
                    get_stat(t_ed_ecg, "npv"),
                    get_stat(t_ed_comp, "npv")
                     ))

mean_points_m <-  c(NA,
        get_est(tm_ed_echo, stat="se", est =1),
        get_est(tm_ed_ecg, stat="se", est =1),
        get_est(tm_ed_comp, stat="se", est =1),

        NA,
        get_est(tm_ed_echo, stat="sp", est =1),
        get_est(tm_ed_ecg, stat="sp", est =1),
        get_est(tm_ed_comp, stat="sp", est =1),

        NA,
        get_est(tm_ed_echo, stat="ppv", est =1),
        get_est(tm_ed_ecg, stat="ppv", est =1),
        get_est(tm_ed_comp, stat="ppv", est =1),

        NA,
        get_est(tm_ed_echo, stat="npv", est =1),
        get_est(tm_ed_ecg, stat="npv", est =1),
        get_est(tm_ed_comp, stat="npv", est =1)
       )

mean_points_f <-  c(NA,
       get_est(tf_ed_echo, stat="se", est =1),
       get_est(tf_ed_ecg, stat="se", est =1),
       get_est(tf_ed_comp, stat="se", est =1),

       NA,
       get_est(tf_ed_echo, stat="sp", est =1),
       get_est(tf_ed_ecg, stat="sp", est =1),
       get_est(tf_ed_comp, stat="sp", est =1),

       NA,
       get_est(tf_ed_echo, stat="ppv", est =1),
       get_est(tf_ed_ecg, stat="ppv", est =1),
       get_est(tf_ed_comp, stat="ppv", est =1),

       NA,
       get_est(tf_ed_echo, stat="npv", est =1),
       get_est(tf_ed_ecg, stat="npv", est =1),
       get_est(tf_ed_comp, stat="npv", est =1)
      )





lower_points_m <-  c(NA,
       get_est(tm_ed_echo, stat="se", est =2),
       get_est(tm_ed_ecg, stat="se", est =2),
       get_est(tm_ed_comp, stat="se", est =2),

       NA,
       get_est(tm_ed_echo, stat="sp", est =2),
       get_est(tm_ed_ecg, stat="sp", est =2),
       get_est(tm_ed_comp, stat="sp", est =2),

       NA,
       get_est(tm_ed_echo, stat="ppv", est =2),
       get_est(tm_ed_ecg, stat="ppv", est =2),
       get_est(tm_ed_comp, stat="ppv", est =2),

       NA,
       get_est(tm_ed_echo, stat="npv", est =2),
       get_est(tm_ed_ecg, stat="npv", est =2),
       get_est(tm_ed_comp, stat="npv", est =2)
      )

lower_points_f <-  c(NA,
     get_est(tf_ed_echo, stat="se", est =2),
     get_est(tf_ed_ecg, stat="se", est =2),
     get_est(tf_ed_comp, stat="se", est =2),

     NA,
     get_est(tf_ed_echo, stat="sp", est =2),
     get_est(tf_ed_ecg, stat="sp", est =2),
     get_est(tf_ed_comp, stat="sp", est =2),

     NA,
     get_est(tf_ed_echo, stat="ppv", est =2),
     get_est(tf_ed_ecg, stat="ppv", est =2),
     get_est(tf_ed_comp, stat="ppv", est =2),

     NA,
     get_est(tf_ed_echo, stat="npv", est =2),
     get_est(tf_ed_ecg, stat="npv", est =2),
     get_est(tf_ed_comp, stat="npv", est =2)
    )



upper_points_m <-  c(NA,
      get_est(tm_ed_echo, stat="se", est =3),
      get_est(tm_ed_ecg, stat="se", est =3),
      get_est(tm_ed_comp, stat="se", est =3),

      NA,
      get_est(tm_ed_echo, stat="sp", est =3),
      get_est(tm_ed_ecg, stat="sp", est =3),
      get_est(tm_ed_comp, stat="sp", est =3),

      NA,
      get_est(tm_ed_echo, stat="ppv", est =3),
      get_est(tm_ed_ecg, stat="ppv", est =3),
      get_est(tm_ed_comp, stat="ppv", est =3),

      NA,
      get_est(tm_ed_echo, stat="npv", est =3),
      get_est(tm_ed_ecg, stat="npv", est =3),
      get_est(tm_ed_comp, stat="npv", est =3)
     )

 upper_points_f <-  c(NA,
       get_est(tf_ed_echo, stat="se", est =3),
       get_est(tf_ed_ecg, stat="se", est =3),
       get_est(tf_ed_comp, stat="se", est =3),

       NA,
       get_est(tf_ed_echo, stat="sp", est =3),
       get_est(tf_ed_ecg, stat="sp", est =3),
       get_est(tf_ed_comp, stat="sp", est =3),

       NA,
       get_est(tf_ed_echo, stat="ppv", est =3),
       get_est(tf_ed_ecg, stat="ppv", est =3),
       get_est(tf_ed_comp, stat="ppv", est =3),

       NA,
       get_est(tf_ed_echo, stat="npv", est =3),
       get_est(tf_ed_ecg, stat="npv", est =3),
       get_est(tf_ed_comp, stat="npv", est =3)
      )
                   # the labels -----------
labeltext <- cbind(c("Sensitivity",
                     "Stress echo",
                     "Stress ECG",
                     "Comprehensive",

                     "Specificity",
                     "Stress echo",
                     "Stress ECG",
                     "Comprehensive",

                     "PPV",
                     "Stress echo",
                     "Stress ECG",
                     "Comprehensive",

                     "NPV",
                     "Stress echo",
                     "Stress ECG",
                     "Comprehensive"
                     ),

                   # the data ------------
                   c("Both sexes [95% CI]",
                    get_stat(t_ed_echo, "se"),
                    get_stat(t_ed_ecg, "se"),
                    get_stat(t_ed_comp, "se"),

                    "",
                    get_stat(t_ed_echo, "sp"),
                    get_stat(t_ed_ecg, "sp"),
                    get_stat(t_ed_comp, "sp"),

                    "",
                    get_stat(t_ed_echo, "ppv"),
                    get_stat(t_ed_ecg, "ppv"),
                    get_stat(t_ed_comp, "ppv"),

                    "",
                    get_stat(t_ed_echo, "npv"),
                    get_stat(t_ed_ecg, "npv"),
                    get_stat(t_ed_comp, "npv")
                     ))

mean_points_m <-  c(NA,
        get_est(tm_ed_echo, stat="se", est =1),
        get_est(tm_ed_ecg, stat="se", est =1),
        get_est(tm_ed_comp, stat="se", est =1),

        NA,
        get_est(tm_ed_echo, stat="sp", est =1),
        get_est(tm_ed_ecg, stat="sp", est =1),
        get_est(tm_ed_comp, stat="sp", est =1),

        NA,
        get_est(tm_ed_echo, stat="ppv", est =1),
        get_est(tm_ed_ecg, stat="ppv", est =1),
        get_est(tm_ed_comp, stat="ppv", est =1),

        NA,
        get_est(tm_ed_echo, stat="npv", est =1),
        get_est(tm_ed_ecg, stat="npv", est =1),
        get_est(tm_ed_comp, stat="npv", est =1)
       )

mean_points_f <-  c(NA,
       get_est(tf_ed_echo, stat="se", est =1),
       get_est(tf_ed_ecg, stat="se", est =1),
       get_est(tf_ed_comp, stat="se", est =1),

       NA,
       get_est(tf_ed_echo, stat="sp", est =1),
       get_est(tf_ed_ecg, stat="sp", est =1),
       get_est(tf_ed_comp, stat="sp", est =1),

       NA,
       get_est(tf_ed_echo, stat="ppv", est =1),
       get_est(tf_ed_ecg, stat="ppv", est =1),
       get_est(tf_ed_comp, stat="ppv", est =1),

       NA,
       get_est(tf_ed_echo, stat="npv", est =1),
       get_est(tf_ed_ecg, stat="npv", est =1),
       get_est(tf_ed_comp, stat="npv", est =1)
      )





lower_points_m <-  c(NA,
       get_est(tm_ed_echo, stat="se", est =2),
       get_est(tm_ed_ecg, stat="se", est =2),
       get_est(tm_ed_comp, stat="se", est =2),

       NA,
       get_est(tm_ed_echo, stat="sp", est =2),
       get_est(tm_ed_ecg, stat="sp", est =2),
       get_est(tm_ed_comp, stat="sp", est =2),

       NA,
       get_est(tm_ed_echo, stat="ppv", est =2),
       get_est(tm_ed_ecg, stat="ppv", est =2),
       get_est(tm_ed_comp, stat="ppv", est =2),

       NA,
       get_est(tm_ed_echo, stat="npv", est =2),
       get_est(tm_ed_ecg, stat="npv", est =2),
       get_est(tm_ed_comp, stat="npv", est =2)
      )

lower_points_f <-  c(NA,
     get_est(tf_ed_echo, stat="se", est =2),
     get_est(tf_ed_ecg, stat="se", est =2),
     get_est(tf_ed_comp, stat="se", est =2),

     NA,
     get_est(tf_ed_echo, stat="sp", est =2),
     get_est(tf_ed_ecg, stat="sp", est =2),
     get_est(tf_ed_comp, stat="sp", est =2),

     NA,
     get_est(tf_ed_echo, stat="ppv", est =2),
     get_est(tf_ed_ecg, stat="ppv", est =2),
     get_est(tf_ed_comp, stat="ppv", est =2),

     NA,
     get_est(tf_ed_echo, stat="npv", est =2),
     get_est(tf_ed_ecg, stat="npv", est =2),
     get_est(tf_ed_comp, stat="npv", est =2)
    )



upper_points_m <-  c(NA,
      get_est(tm_ed_echo, stat="se", est =3),
      get_est(tm_ed_ecg, stat="se", est =3),
      get_est(tm_ed_comp, stat="se", est =3),

      NA,
      get_est(tm_ed_echo, stat="sp", est =3),
      get_est(tm_ed_ecg, stat="sp", est =3),
      get_est(tm_ed_comp, stat="sp", est =3),

      NA,
      get_est(tm_ed_echo, stat="ppv", est =3),
      get_est(tm_ed_ecg, stat="ppv", est =3),
      get_est(tm_ed_comp, stat="ppv", est =3),

      NA,
      get_est(tm_ed_echo, stat="npv", est =3),
      get_est(tm_ed_ecg, stat="npv", est =3),
      get_est(tm_ed_comp, stat="npv", est =3)
     )

 upper_points_f <-  c(NA,
       get_est(tf_ed_echo, stat="se", est =3),
       get_est(tf_ed_ecg, stat="se", est =3),
       get_est(tf_ed_comp, stat="se", est =3),

       NA,
       get_est(tf_ed_echo, stat="sp", est =3),
       get_est(tf_ed_ecg, stat="sp", est =3),
       get_est(tf_ed_comp, stat="sp", est =3),

       NA,
       get_est(tf_ed_echo, stat="ppv", est =3),
       get_est(tf_ed_ecg, stat="ppv", est =3),
       get_est(tf_ed_comp, stat="ppv", est =3),

       NA,
       get_est(tf_ed_echo, stat="npv", est =3),
       get_est(tf_ed_ecg, stat="npv", est =3),
       get_est(tf_ed_comp, stat="npv", est =3)
      )


# ------------------------------------------------------------------------------
pdf("figs/fig2B_endo_dysfn_color.pdf")

    par(mfrow=c(1,1))
    forestplot(labeltext = labeltext,
        mean  = cbind(mean_points_f, mean_points_m),
        lower = cbind(lower_points_f, lower_points_m),
        upper = cbind(upper_points_f, upper_points_m),
        is.summary = c(TRUE, rep(FALSE,3),TRUE, rep(FALSE,3),TRUE, rep(FALSE,3),TRUE, rep(FALSE,3)),
        graph.pos="right",
        xlab = "Proportion",
        title = "", #"Diagnostic Properties of Stress Testing: Any Abnormality",
        legend = c("female", "male"),
        fn.ci_norm = c(fpDrawCircleCI, fpDrawNormalCI),
        line.margin=0.25, colgap = unit(4,"mm"), graphwidth = unit(60,"mm"),
        hrzl_lines = TRUE,
        lwd.ci = 1.4, lty.ci = c(5,1),  ci.vertices = TRUE, ci.vertices.height = 0.03,
        col = fpColors(lines=c("tomato", "dodgerblue3"), box=c("tomato", "dodgerblue3")),
        grid = TRUE,
        zero = 1, lwd.zero = 1.2,
        boxsize = 0.18, xticks = (0:10/10), xtick.digits=1)

dev.off()

pdf("figs/fig2B_endo_dysfn_bw.pdf")

    forestplot(labeltext = labeltext,
        mean  = cbind(mean_points_f, mean_points_m),
        lower = cbind(lower_points_f, lower_points_m),
        upper = cbind(upper_points_f, upper_points_m),
        is.summary = c(TRUE, rep(FALSE,3),TRUE, rep(FALSE,3),TRUE, rep(FALSE,3),TRUE, rep(FALSE,3)),
        graph.pos="right",
        xlab = "Proportion",
        title = "", #"Diagnostic Properties of Stress Testing: Any Abnormality",
        legend = c("female", "male"),
        fn.ci_norm = c(fpDrawCircleCI, fpDrawNormalCI),
        line.margin=0.25, colgap = unit(4,"mm"), graphwidth = unit(60,"mm"),
        hrzl_lines = TRUE,
        lwd.ci = 1.4, lty.ci = c(5,1),  ci.vertices = TRUE, ci.vertices.height = 0.03,
        col = fpColors(lines=c("black", "darkgrey"), box=c("black", "darkgrey")),
        grid = TRUE,
        zero = 1, lwd.zero = 1.2,
        boxsize = 0.18, xticks = (0:10/10), xtick.digits=1)

dev.off()
