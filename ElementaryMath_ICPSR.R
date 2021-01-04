########################################################################################################
# Analysis of Effective Programs in Elementary Mathematics
########################################################################################################
# Authors: Marta Pellegrini, Cynthia Lake, Amanda Neitzel, & Robert Slavin
# Contact: marta.pellegrini@unifi.it

# This file analyzes the included studies in the Elementary Mathematics systematic review, including
# preparing the data for analysis and meta-regressions.

########################################################################################################
# Initial Set-up
########################################################################################################
# Clear workspace
rm(list=ls(all=TRUE))

# Load packages
test<-require(plyr)   #rename()
if (test == FALSE) {
  install.packages("plyr")
  require(plyr)
}
test<-require(tableone)   #CreateTableOne()
if (test == FALSE) {
  install.packages("tableone")
  require(tableone)
}
test<-require(metafor)   #rma()
if (test == FALSE) {
  install.packages("metafor")
  require(metafor)
}
test<-require(clubSandwich)   #coeftest()
if (test == FALSE) {
  install.packages("clubSandwich")
  require(clubSandwich)
}
test<-require(flextable)   
if (test == FALSE) {
  install.packages("flextable")
  require(flextable)
}
test<-require(officer)   
if (test == FALSE) {
  install.packages("officer")
  require(officer)
}
rm(test)

########################################################################################################
# Load data
########################################################################################################
# load findings and studies
findings <- read.csv("public_use_dataset_Studies.csv")
studies <- read.csv("public_use_dataset_Findings.csv")

########################################################################################################
# Prep data
########################################################################################################
# merge dataframes
full <- merge(studies, findings, by = c("Program", "Authors", "Year"), all = TRUE, suffixes = c(".s", ".f"))

# rm(findings, studies)

########################################################################################################
# Prep data: create variables for analyses
########################################################################################################
################################################################
# Create unique identifiers (ES, study, program)
################################################################
full$ESId <- as.numeric(rownames(full))
full$StudyID <- paste(full$Program, full$Authors, full$Year, sep = ": ")
full$StudyID <- as.numeric(as.factor(full$StudyID))
full$ProgramID <- as.numeric(as.factor(full$Program))

################################################################
# Create dummies, categorical variables, and centered variables
################################################################
### create any missing categorical variables ###
    # used for descriptives and marginal means
# PD implementation type
full$imp_type <- full$Table.topic
full$imp_type[which(full$Table.topic!="PD focused on curriculum" & full$Table.topic!="PD focused on software")] <- "Not PD implementation"

# Curriculum type
full$curr_type <- full$Table.topic
full$curr_type[which(full$Table.topic!="Curricula with tech" & full$Table.topic!="Curricula no tech")] <- "Not Curriculum"

# Big Category
full$big_cat <- full$Table.topic
full$big_cat[which(full$big_cat == "PD focused on curriculum" | full$big_cat == "PD focused on software")] <- "PD focused on implementation"
full$big_cat[which(full$big_cat == "Curricula no tech" | full$big_cat == "Curricula with tech")] <- "Curricula"
full$big_cat[which(full$Tutoring.Type == "Cross-age")] <- "Cross-age"
full$big_cat[which(full$Tutoring.Type == "Online one-to-one")] <- "Online one-to-one"

### create dummy variables ###
# table/category
full$tbl_tutoring <- 0
full$tbl_tutoring[which(full$big_cat=="Tutoring")] <- 1
full$tbl_online <- 0
full$tbl_online[which(full$Tutoring.Type=="Online one-to-one")] <- 1
full$tbl_crossage <- 0
full$tbl_crossage[which(full$Tutoring.Type=="Cross-age")] <- 1
full$PD_class <- 0
full$PD_class[which(full$Table.topic=="PD for classroom")] <- 1
full$PD_content <- 0
full$PD_content[which(full$Table.topic=="PD content and pedagogy")] <- 1
full$PD_imp <- 0
full$PD_imp[which(full$Table.topic=="PD focused on curriculum" | full$Table.topic=="PD focused on software")] <- 1
full$imp_curr <- 0
full$imp_curr[which(full$Table.topic=="PD focused on curriculum")] <- 1
full$imp_softw <- 0
full$imp_softw[which(full$Table.topic=="PD focused on software")] <- 1
full$imp_none <- 0
full$imp_none[which(full$PD_imp==0)] <- 1
full$tbl_curr <- 0
full$tbl_curr[which(full$Table.topic=="Curricula with tech" | full$Table.topic=="Curricula no tech")] <- 1
full$curr_tech <- 0
full$curr_tech[which(full$Table.topic=="Curricula with tech")] <- 1
full$curr_notech <- 0
full$curr_notech[which(full$Table.topic=="Curricula no tech")] <- 1
full$curr_none <- 0
full$curr_none[which(full$tbl_curr==0)] <- 1
full$tbl_benchmark <- 0
full$tbl_benchmark[which(full$Table.topic=="Benchmark assessments")] <- 1

# grade level
full$grade_k2 <- 0
full$grade_k2[which(full$Grade.Band == "K-2")] <- 1
full$grade_36 <- 0
full$grade_36[which(full$Grade.Band == "3-6")] <- 1
full$grade_mixed <- 0
full$grade_mixed[which(full$Grade.Band == "mixed")] <- 1

# design: assignment type   
full$randomized <- 0
full$randomized[which(full$Randomized == 1)] <-1
full$matched <- 0
full$matched[which(full$Randomized == 0)] <- 1

# design: assignment level
full$clustered <- 0
full$clustered[which(full$Clustered == 1)] <-1
full$individual <- 0
full$individual[which(full$Clustered == 0)] <-1

# publication status 
full$published <- 0 
full$published[which(full$Published == 1)] <-1
full$unpublished <- 0 
full$unpublished[which(full$Published == 0)] <-1

#technology 
full$technology_yes <- 0
full$technology_yes[which(full$Technology == 1)] <-1
full$technology_no <- 0
full$technology_no[which(full$Technology == 0)] <-1

# duration
full$duration_lessoneyear <- 0
full$duration_lessoneyear[which(full$Duration.Band == "less than one year")] <- 1
full$duration_oneyear <- 0
full$duration_oneyear[which(full$Duration.Band == "one year")] <- 1
full$duration_moreoneyear <- 0
full$duration_moreoneyear[which(full$Duration.Band == "more than one year")] <- 1

# FRL status
full$FRL_high <- 0
full$FRL_high[which(full$Level.Poverty == "HighPoverty")] <- 1
full$FRL_low <- 0
full$FRL_low[which(full$Level.Poverty == "LowPoverty")] <- 1
full$FRL_all <- 0
full$FRL_all[which(full$Level.Poverty == "AllPoverty")] <- 1

# low achiever status
# full$Level.Achievement[which(full$big_cat == "Tutoring")] <- "TutoringAchiever"
full$achiever_low <- 0
full$achiever_low[which(full$Level.Achievement == "LowAchiever")] <- 1
full$achiever_high <- 0
full$achiever_high[which(full$Level.Achievement == "HighAchiever")] <- 1
full$achiever_all <- 0
full$achiever_all[which(full$Level.Achievement == "AllAchiever")] <- 1
full$achiever_tutoring <- 0
full$achiever_tutoring[which(full$Level.Achievement == "TutoringAchiever")] <- 1

# tutoring: provider: FOR EXPLORATORY TUTORING MODEL
full$Tutoring.Provider[which(full$Tutoring.Provider=="Cross-age")] <- "NotTutoring"
full$Tutoring.Provider[which(full$Tutoring.Provider=="Online one-to-one")] <- "NotTutoring"
full$provider_parap <- 0
full$provider_parap[which(full$Tutoring.Provider == "Paraprofessionals")] <- 1
full$provider_teacher <- 0
full$provider_teacher[which(full$Tutoring.Provider == "Teachers")] <- 1
full$provider_volunteer <- 0
full$provider_volunteer[which(full$Tutoring.Provider == "PaidVolunteers")] <- 1
full$provider_nottut <- 0
full$provider_nottut[which(full$Tutoring.Provider == "NotTutoring")] <- 1

# tutoring: type
full$type_individual <- 0
full$type_individual[which(full$Tutoring.Type == "One-to-one")] <- 1
full$type_smallgroup <- 0
full$type_smallgroup[which(full$Tutoring.Type == "One-to-small")] <- 1
full$type_nottut <- 0
full$type_nottut[which(full$Tutoring.Type == "NotTutoring" | full$Tutoring.Type == "Cross-age" | full$Tutoring.Type == "Online one-to-one")] <- 1

# fix table topic
full$Table.topic[which(full$Tutoring.Type == "Cross-age")] <- "Cross-age"
full$Table.topic[which(full$Tutoring.Type == "Online one-to-one")] <- "Online one-to-one"

# tutoring: type: categorical
full$Tutoring.Type[which(full$Tutoring.Type=="Cross-age")] <- "NotTutoring"
full$Tutoring.Type[which(full$Tutoring.Type=="Online one-to-one")] <- "NotTutoring"

# setting
full$international <- 0
full$international[which(full$International==1)] <- 1
full$america <- 0
full$america[which(full$International==0)] <- 1

### center variables ###
# table/category
full$tbl_tutoring.c <- full$tbl_tutoring - mean(full$tbl_tutoring)
full$tbl_crossage.c <- full$tbl_crossage - mean(full$tbl_crossage)
full$tbl_online.c <- full$tbl_online - mean(full$tbl_online)
full$PD_class.c <- full$PD_class - mean(full$PD_class)
full$PD_content.c <- full$PD_content - mean(full$PD_content)
full$PD_imp.c <- full$PD_imp - mean(full$PD_imp)
full$imp_curr.c <- full$imp_curr - mean(full$imp_curr)
full$imp_softw.c <- full$imp_softw - mean(full$imp_softw)
full$imp_none.c <- full$imp_none - mean(full$imp_none)
full$tbl_curr.c <- full$tbl_curr - mean(full$tbl_curr)
full$curr_tech.c <- full$curr_tech - mean(full$curr_tech)
full$curr_notech.c <- full$curr_notech - mean(full$curr_notech)
full$curr_none.c <- full$curr_none - mean(full$curr_none)
full$tbl_benchmark.c <- full$tbl_benchmark - mean(full$tbl_benchmark)

# grade level
full$grade_k2.c <- full$grade_k2 - mean(full$grade_k2)
full$grade_36.c <- full$grade_36 - mean(full$grade_36)
full$grade_mixed.c <- full$grade_mixed - mean(full$grade_mixed)

# design: assignment type
full$randomized.c <- full$randomized - mean(full$randomized)
full$matched.c <- full$matched - mean(full$matched)

# design: assignment level
full$clustered.c <- full$clustered - mean(full$clustered)
full$individual.c <- full$individual - mean(full$individual)

# publication status
full$published.c <- full$published - mean(full$published)
full$unpublished.c <- full$unpublished - mean(full$unpublished)

#technology
full$technology_yes.c <- full$technology_yes - mean(full$technology_yes)
full$technology_no.c <- full$technology_no - mean(full$technology_no)

# duration
full$duration_lessoneyear.c <- full$duration_lessoneyear - mean(full$duration_lessoneyear)
full$duration_oneyear.c <- full$duration_oneyear - mean(full$duration_oneyear)
full$duration_moreoneyear.c <- full$duration_moreoneyear - mean(full$duration_moreoneyear)

# FRL status
full$FRL_high.c <- full$FRL_high - mean(full$FRL_high)
full$FRL_low.c <- full$FRL_low - mean(full$FRL_low)
full$FRL_all.c <- full$FRL_all - mean(full$FRL_all)

# low achiever status
full$achiever_low.c <- full$achiever_low - mean(full$achiever_low)
full$achiever_high.c <- full$achiever_high - mean(full$achiever_high)
full$achiever_all.c <- full$achiever_all - mean(full$achiever_all)
full$achiever_tutoring.c <- full$achiever_tutoring - mean(full$achiever_tutoring)

# tutoring: provider: FOR EXPLORATORY TUTORING MODEL
full$provider_parap.c <- full$provider_parap - mean(full$provider_parap)
full$provider_teacher.c <- full$provider_teacher - mean(full$provider_teacher)
full$provider_volunteer.c <- full$provider_volunteer - mean(full$provider_volunteer)
full$provider_nottut.c <- full$provider_nottut - mean(full$provider_nottut)

# tutoring: type
full$type_individual.c <- full$type_individual - mean(full$type_individual)
full$type_smallgroup.c <- full$type_smallgroup - mean(full$type_smallgroup)
full$type_nottut.c <- full$type_nottut - mean(full$type_nottut)

# setting
full$international.c <- full$international - mean(full$international)
full$america.c <- full$america - mean(full$america)

################################################################
# Correct effect sizes for clustering ala Hedges 2007
################################################################
# calculate cluster-corrected variance
# correct for clustering
full$icc <- NA
full$icc[which(full$Clustered == 1)] <- 0.2   # assumption from WWC

full$Effect.Size.adj <- full$Effect.Size * (sqrt(1-((2*(full$Total.Cluster-1)*full$icc)/(full$Total.N-2))))
full$Effect.Size.orig <- full$Effect.Size
full$Effect.Size[which(full$Clustered==1)] <- full$Effect.Size.adj[which(full$Clustered==1)]

################################################################
# Create study-level data and effect sizes
################################################################
# create weighted ESs (for study-level ES calculations)
# weighted by sample size

full$wes <- full$Effect.Size * full$Total.N
full$totalweight <- full$Total.N

# aggregate data to study-level, the re-combine
study_levela <- unique(full[c("Program", "Authors", "Year", "Table.number", "Table.topic", "Design", "Publication.Status", "StudyID", "Tutoring.Provider", "Tutoring.Type", "Tutoring.ExtraTime")])
study_levelb <- aggregate(full[c("International", "Treatment.Cluster", "Control.Cluster", "Treatment.N", "Control.N", 
                                 "Clustered", "Randomized", "Total.N", "Total.Cluster", "ProgramID")], by = list(full$StudyID), FUN = mean, na.rm = TRUE)
study_levelb <- rename(study_levelb, c("Group.1" = "StudyID"))
study_levelc <- aggregate(full$Effect.Size, by = list(full$StudyID), FUN = length)
study_levelc <- rename(study_levelc, c("x" = "num.findings"))
study_levelc <- rename(study_levelc, c("Group.1" = "StudyID"))
study_leveld <- aggregate(full[c("wes", "totalweight")], by = list(full$StudyID), FUN = sum, na.rm = TRUE)
study_leveld <- rename(study_leveld, c("Group.1" = "StudyID"))
study_level <- merge(study_levela, study_levelb, by = "StudyID", all = TRUE)
study_level <- merge(study_level, study_levelc, by = "StudyID", all = TRUE)
study_level <- merge(study_level, study_leveld, by = "StudyID", all = TRUE)

rm(study_levela)
rm(study_levelb)
rm(study_levelc)
rm(study_leveld)

# create study-level ES
study_level$Effect.Size <- study_level$wes/study_level$totalweight

# merge that back into the findings (for final tables)
study_level$StudyES <- study_level$Effect.Size
study_level$StudyES <- format(round(study_level$StudyES, 2), nsmall = 2)
study_level$StudyES[which(study_level$StudyES > 0)] <- paste("+", study_level$StudyES, sep = "")
full <- merge(full, study_level[c("StudyID", "num.findings", "StudyES")], by = "StudyID", all = TRUE)

################################################################
# Calculate meta-analytic variables (effect-size level for RVE)
################################################################
#calculate standard errors
full$se<-sqrt(((full$Treatment.N+full$Control.N)/(full$Treatment.N*full$Control.N))+((full$Effect.Size*full$Effect.Size)/(2*(full$Treatment.N+full$Control.N))))

#calculate variance
full$var<-full$se*full$se

full$Treatment.Cluster.n <- NA
full$Treatment.Cluster.n[which(full$Clustered == 1)] <- round(full$Treatment.N[which(full$Clustered == 1)]/full$Treatment.Cluster[which(full$Clustered == 1)], 0)
full$Control.Cluster.n <- NA
full$Control.Cluster.n[which(full$Clustered == 1)] <- round(full$Control.N[which(full$Clustered == 1)]/full$Control.Cluster[which(full$Clustered == 1)], 0)

full$ntilde <- NA
full$ntilde <- ((full$Control.N * full$Treatment.Cluster * full$Treatment.Cluster.n * full$Treatment.Cluster.n)/(full$Treatment.N * full$Total.N)) +
  ((full$Treatment.N * full$Control.Cluster * full$Control.Cluster.n * full$Control.Cluster.n)/(full$Control.N * full$Total.N))

full$n.TU <- NA
full$n.TU <- ((full$Treatment.N * full$Treatment.N) - (full$Treatment.Cluster * full$Treatment.Cluster.n * full$Treatment.Cluster.n))/(full$Treatment.N * (full$Treatment.Cluster - 1))
full$n.CU <- NA
full$n.CU <- ((full$Control.N * full$Control.N) - (full$Control.Cluster * full$Control.Cluster.n * full$Control.Cluster.n))/(full$Control.N * (full$Control.Cluster - 1))

full$AT <- NA
full$AT <-((full$Treatment.N^2 * full$Treatment.Cluster * full$Treatment.Cluster.n^2) + ((full$Treatment.Cluster * full$Treatment.Cluster.n^2)^2) - (2 * full$Treatment.N * full$Treatment.Cluster.n^3 * full$Treatment.Cluster))/(full$Treatment.N^2)

full$AC <- NA
full$AC <-((full$Control.N^2 * full$Control.Cluster * full$Control.Cluster.n^2) + ((full$Control.Cluster * full$Control.Cluster.n^2)^2) - (2 * full$Control.N * full$Control.Cluster.n^3 * full$Control.Cluster))/(full$Control.N^2)

full$A <- full$AT + full$AC

full$B <- NA
full$B <- full$n.TU * (full$Treatment.Cluster - 1) + full$n.CU * (full$Control.Cluster -1)

full$var.adj <- NA
full$var.adj <- ((full$Treatment.N + full$Control.N)/(full$Treatment.N * full$Control.N)) * (1 + (full$ntilde - 1) * full$icc) + ((((full$Total.N - 2)*(1-full$icc)^2) + full$A * full$icc^2 + 2*full$icc*(1-full$icc))*full$Effect.Size^2)/(2*(full$Total.N-2)*((full$Total.N - 2)-full$icc*(full$Total.N-2-full$B)))
full$var.old <- full$var
full$var[which(full$Clustered == 1)] <- full$var.adj[which(full$Clustered == 1)]

################################################################
# Calculate meta-analytic variables (study-level for program means)
################################################################
#calculate standard errors
study_level$se<-sqrt(((study_level$Treatment.N+study_level$Control.N)/(study_level$Treatment.N*study_level$Control.N))+((study_level$Effect.Size*study_level$Effect.Size)/(2*(study_level$Treatment.N+study_level$Control.N))))

#calculate variance
study_level$var<-study_level$se*study_level$se

# calculate cluster-corrected variance
# correct for clustering
study_level$icc <- NA
study_level$icc[which(study_level$Clustered == 1)] <- 0.2   # could look this up and specify

study_level$Treatment.Cluster.n <- NA
study_level$Treatment.Cluster.n[which(study_level$Clustered == 1)] <- round(study_level$Treatment.N[which(study_level$Clustered == 1)]/study_level$Treatment.Cluster[which(study_level$Clustered == 1)], 0)
study_level$Control.Cluster.n <- NA
study_level$Control.Cluster.n[which(study_level$Clustered == 1)] <- round(study_level$Control.N[which(study_level$Clustered == 1)]/study_level$Control.Cluster[which(study_level$Clustered == 1)], 0)

study_level$ntilde <- NA
study_level$ntilde <- ((study_level$Control.N * study_level$Treatment.Cluster * study_level$Treatment.Cluster.n * study_level$Treatment.Cluster.n)/(study_level$Treatment.N * study_level$Total.N)) +
  ((study_level$Treatment.N * study_level$Control.Cluster * study_level$Control.Cluster.n * study_level$Control.Cluster.n)/(study_level$Control.N * study_level$Total.N))

study_level$n.TU <- NA
study_level$n.TU <- ((study_level$Treatment.N * study_level$Treatment.N) - (study_level$Treatment.Cluster * study_level$Treatment.Cluster.n * study_level$Treatment.Cluster.n))/(study_level$Treatment.N * (study_level$Treatment.Cluster - 1))
study_level$n.CU <- NA
study_level$n.CU <- ((study_level$Control.N * study_level$Control.N) - (study_level$Control.Cluster * study_level$Control.Cluster.n * study_level$Control.Cluster.n))/(study_level$Control.N * (study_level$Control.Cluster - 1))

study_level$AT <- NA
study_level$AT <-((study_level$Treatment.N^2 * study_level$Treatment.Cluster * study_level$Treatment.Cluster.n^2) + ((study_level$Treatment.Cluster * study_level$Treatment.Cluster.n^2)^2) - (2 * study_level$Treatment.N * study_level$Treatment.Cluster.n^3 * study_level$Treatment.Cluster))/(study_level$Treatment.N^2)

study_level$AC <- NA
study_level$AC <-((study_level$Control.N^2 * study_level$Control.Cluster * study_level$Control.Cluster.n^2) + ((study_level$Control.Cluster * study_level$Control.Cluster.n^2)^2) - (2 * study_level$Control.N * study_level$Control.Cluster.n^3 * study_level$Control.Cluster))/(study_level$Control.N^2)

study_level$A <- study_level$AT + study_level$AC

study_level$B <- NA
study_level$B <- study_level$n.TU * (study_level$Treatment.Cluster - 1) + study_level$n.CU * (study_level$Control.Cluster -1)

study_level$var.adj <- NA
study_level$var.adj <- ((study_level$Treatment.N + study_level$Control.N)/(study_level$Treatment.N * study_level$Control.N)) * (1 + (study_level$ntilde - 1) * study_level$icc) + ((((study_level$Total.N - 2)*(1-study_level$icc)^2) + study_level$A * study_level$icc^2 + 2*study_level$icc*(1-study_level$icc))*study_level$Effect.Size^2)/(2*(study_level$Total.N-2)*((study_level$Total.N - 2)-study_level$icc*(study_level$Total.N-2-study_level$B)))
study_level$var.old <- study_level$var
study_level$var[which(study_level$Clustered == 1)] <- study_level$var.adj[which(study_level$Clustered == 1)]

########################################################################################################
# Analysis
########################################################################################################
#################################################################################
# Descriptive Statistics
#################################################################################
# number of programs:
length(unique(full$Program))

# identify variables for descriptive tables (study-level and outcome-level)
vars_study <- c("Total.N", "Table.topic", "Design", "Publication.Status", "International", "Tutoring.Provider", "Tutoring.Type", "Tutoring.ExtraTime")
vars_outcome <- c("Grade.Band", "Level.Achievement", "Level.Poverty")

# create the table "chunks"
table_study_df <- as.data.frame(print(CreateTableOne(vars = vars_study, data = study_level, includeNA = TRUE, factorVars = c("International")), showAllLevels = TRUE))
table_outcome_df <- as.data.frame(print(CreateTableOne(vars = vars_outcome, data = full, includeNA = TRUE), showAllLevels = TRUE))

rm(vars_study, vars_outcome)

#################################################################################
# Means for Programs   ###need to have study-level outcomes for these calculations
#################################################################################
#create new dataframes for storing results
meanES <- data.frame(comparison = character(0), all.es = numeric(0), all.p = numeric (0), all.k = numeric(0), df = numeric(0))

# determine number of studies per program
studycount <- aggregate(StudyID ~ Program, data = full, function(x) length(unique(x)))
# limit to those with more than one study
studycount <- subset(studycount, studycount$StudyID > 1)
# get list of programs with more than one study
programs <- unique(studycount$Program)

for(i in 1:length(programs)) {
  # i <- 1
  program <- programs[i]
  program.FE <- rma(study_level$Effect.Size, study_level$var, data = study_level, method="FE", subset = (Program == program))
  meanES <- rbind(meanES, data.frame(comparison = program, all.es = program.FE$b, all.p = program.FE$pval, all.n = program.FE$k, df = NA))
}
rm(studycount, programs, program, program.FE)

#################################################################################
# Null Model
#################################################################################
V_list <- impute_covariance_matrix(vi = full$var,  #known correlation vector
                                   cluster = full$StudyID, #study ID
                                   r = 0.70) #assumed correlation 

MVnull <- rma.mv(yi=Effect.Size, #effect size
                 V = V_list, #variance (tHIS IS WHAt CHANGES FROM HEmodel)
                 random = ~1 | StudyID/ESId, #nesting structure
                 test= "t", #use t-tests
                 data=full, #define data
                 method="REML") #estimate variances using REML


#t-tests of each covariate #
MVnull.coef <- coef_test(MVnull,#estimation model above
                         cluster=full$StudyID, #define cluster IDs
                         vcov = "CR2") #estimation method (CR2 is best)

#################################################################################
# Metaregression
#################################################################################
terms <- c("PD_class.c", "PD_content.c", "PD_imp.c", "tbl_curr.c", "tbl_benchmark.c", "tbl_online.c", "tbl_crossage.c", "tbl_tutoring.c",
           "imp_curr.c", "imp_softw.c", "imp_none.c",
           "curr_tech.c", "curr_notech.c", "curr_none",
           "grade_k2.c", "grade_36.c", "grade_mixed.c",
           "matched.c", "randomized.c",
           "international.c", "america.c",
           "FRL_low.c", "FRL_high.c", "FRL_all.c",
           "achiever_low.c", "achiever_high.c", "achiever_tutoring.c", "achiever_all.c",
           # "provider_volunteer.c", "provider_parap.c", "provider_nottut.c", "provider_teacher.c",
           "type_smallgroup.c", "type_individual.c", "type_nottut.c") #,

formula <- reformulate(termlabels = c(terms))

MVfull <- rma.mv(yi=Effect.Size, #effect size
                 V = V_list, #variance (tHIS IS WHAt CHANGES FROM HEmodel)
                 mods = formula, #ADD COVS HERE
                 random = ~1 | StudyID/ESId, #nesting structure
                 test= "t", #use t-tests
                 data=full, #define data
                 method="REML") #estimate variances using REML

#t-tests of each covariate #
MVfull.coef <- coef_test(MVfull,#estimation model above
                         cluster=full$StudyID, #define cluster IDs
                         vcov = "CR2") #estimation method (CR2 is best)

#################################################################################
# Calculating Marginal Means
#################################################################################
# re-run model for each moderator to get marginal means for each #

# set up table to store results
means <- data.frame(moderator = character(0), group = character(0), beta = numeric(0), SE = numeric(0), 
                    tstat = numeric(0), df = numeric(0), p_Satt = numeric(0))

# This is a bit of a pain because the fully centered model isn't relevant for 
# moderators that are specific to only certain categories.  For example, the mean
# for a tutoring study with the average level of content-focused PD doesn't make 
# sense.  So we break this into:
    # 1) Moderators for all studies
    # 2) Moderators for tutoring studies
    # 3) Moderators for PD studies
    # 4) Moderators for curriculum studies
# This is further complicated because we need the average values for each category,
# plus the averages for each subcategory within that category, so that adds
    # 5) Means for categories
# We will get these still using the SAME model, just changing whether the variables
# are centered or not and whether we use an intercept or not.  It's important to
# note these are not different models.  You could, technically, calculate each
# of these means from the fully centered model estimated above, but that's a lot
# of work.  This is simpler and less prone to errors.  Each is done below, and
# the specific changes are outlined in a word document.

##### 1) Moderators for all studies #####
mods <- c("as.factor(Grade.Band)", "as.factor(Randomized)", #"as.factor(Clustered)", 
          "as.factor(International)", "as.factor(Level.Poverty)", "as.factor(Level.Achievement)")

for(i in 1:length(mods)){
  # i <- 1
  formula <- reformulate(termlabels = c(mods[i], terms, "-1"))   # Worth knowing - if you duplicate terms, it keeps the first one
  mod_means <- rma.mv(yi=Effect.Size, #effect size
                      V = V_list, #variance (tHIS IS WHAt CHANGES FROM HEmodel)
                      mods = formula, #ADD COVS HERE
                      random = ~1 | StudyID/ESId, #nesting structure
                      test= "t", #use t-tests
                      data=full, #define data
                      method="REML") #estimate variances using REML
  coef_mod_means <- as.data.frame(coef_test(mod_means,#estimation model above
                                            cluster=full$StudyID, #define cluster IDs
                                            vcov = "CR2")) #estimation method (CR2 is best)
  # limit to relevant rows (the means you are interested in)
  coef_mod_means$moderator <- gsub(x = mods[i], pattern = "as.factor", replacement = "")
  coef_mod_means$group <- rownames(coef_mod_means)
  rownames(coef_mod_means) <- c()
  coef_mod_means <- subset(coef_mod_means, substr(start = 1, stop = nchar(mods[i]), x = coef_mod_means$group)== mods[i])
  coef_mod_means$group <- substr(x = coef_mod_means$group, start = nchar(mods[i])+1, stop = nchar(coef_mod_means$group))
  means <- dplyr::bind_rows(means, coef_mod_means)
}

##### 2) Moderators for tutoring studies #####
# This gets you the moderators for the average tutoring studies (Group Size across all studies, etc)
mods <- c("as.factor(full$Tutoring.Type)")

for(i in 1:length(mods)) {
  # i <- 1
  # Provider
  changes <- c("PD_class", "PD_content", "PD_imp", "tbl_curr", "tbl_benchmark", "tbl_online", "tbl_crossage", "tbl_tutoring", 
               "imp_curr", "imp_softw", "imp_none", 
               "curr_tech", "curr_notech", "curr_none", 
               "achiever_high", "achiever_tutoring", "achiever_all", "achiever_low")
  
  formula <- reformulate(termlabels = c(mods[i], changes, terms, "-1"))   # Worth knowing - if you duplicate terms, it keeps the first one
  mod_means <- rma.mv(yi=Effect.Size, #effect size
                      V = V_list, #variance (tHIS IS WHAt CHANGES FROM HEmodel)
                      mods = formula, #ADD COVS HERE
                      random = ~1 | StudyID/ESId, #nesting structure
                      test= "t", #use t-tests
                      data=full, #define data
                      method="REML") #estimate variances using REML
  coef_mod_means <- as.data.frame(coef_test(mod_means,#estimation model above
                                            cluster=full$StudyID, #define cluster IDs
                                            vcov = "CR2")) #estimation method (CR2 is best)
  # limit to relevant rows (the means you are interested in)
  coef_mod_means$moderator <- gsub(x = mods[i], pattern = "as.factor", replacement = "")
  coef_mod_means$group <- rownames(coef_mod_means)
  rownames(coef_mod_means) <- c()
  coef_mod_means <- subset(coef_mod_means, substr(start = 1, stop = nchar(mods[i]), x = coef_mod_means$group)== mods[i])
  coef_mod_means$group <- substr(x = coef_mod_means$group, start = nchar(mods[i])+1, stop = nchar(coef_mod_means$group))
  means <- dplyr::bind_rows(means, coef_mod_means)
}

##### 2b) Exploratory model for tutoring provider #####
# This gets you the moderators adding provider for the average tutoring studies (Provider across all studies, etc)
mods <- c("as.factor(full$Tutoring.Provider)")

for(i in 1:length(mods)) {
  # i <- 1
  # Provider
  changes <- c("PD_class", "PD_content", "PD_imp", "tbl_curr", "tbl_benchmark", "tbl_online", "tbl_crossage", "tbl_tutoring", 
               "imp_curr", "imp_softw", "imp_none", 
               "curr_tech", "curr_notech", "curr_none", 
               "achiever_high", "achiever_tutoring", "achiever_all", "achiever_low")
  
  formula <- reformulate(termlabels = c(mods[i], changes, terms, "-1"))   # Worth knowing - if you duplicate terms, it keeps the first one
  mod_means <- rma.mv(yi=Effect.Size, #effect size
                      V = V_list, #variance (tHIS IS WHAt CHANGES FROM HEmodel)
                      mods = formula, #ADD COVS HERE
                      random = ~1 | StudyID/ESId, #nesting structure
                      test= "t", #use t-tests
                      data=full, #define data
                      method="REML") #estimate variances using REML
  coef_mod_means <- as.data.frame(coef_test(mod_means,#estimation model above
                                            cluster=full$StudyID, #define cluster IDs
                                            vcov = "CR2")) #estimation method (CR2 is best)
  # limit to relevant rows (the means you are interested in)
  coef_mod_means$moderator <- gsub(x = mods[i], pattern = "as.factor", replacement = "")
  coef_mod_means$moderator <- gsub(x = coef_mod_means$moderator, pattern = "full\\$", replacement = "Exploratory: ")
  coef_mod_means$group <- rownames(coef_mod_means)
  rownames(coef_mod_means) <- c()
  coef_mod_means <- subset(coef_mod_means, substr(start = 1, stop = nchar(mods[i]), x = coef_mod_means$group)== mods[i])
  coef_mod_means$group <- substr(x = coef_mod_means$group, start = nchar(mods[i])+1, stop = nchar(coef_mod_means$group))
  means <- dplyr::bind_rows(means, coef_mod_means)
}

# This gets you the averages within subcategories (the averages for Providers X Type)
# get with Provider as category
mods <- c("as.factor(full$Tutoring.Provider)")

# repeat twice: once for small group, once for individual
### There is likely a more elegant way to do this, but I'm in a hurry

# Individual first (as reference for the Tutoring Provider Means)
for(i in 1:length(mods)) {
  # i <- 1
  # Provider
  changes <- c("PD_class", "PD_content", "PD_imp", "tbl_curr", "tbl_benchmark", "tbl_online", "tbl_crossage", "tbl_tutoring",
               "imp_curr", "imp_softw", "imp_none",
               "curr_tech", "curr_notech", "curr_none",
               "achiever_high", "achiever_tutoring", "achiever_all", "achiever_low", 
               "type_smallgroup", "type_nottut", "type_individual")

  formula <- reformulate(termlabels = c(mods[i], changes, terms, "-1"))   # Worth knowing - if you duplicate terms, it keeps the first one
  mod_means <- rma.mv(yi=Effect.Size, #effect size
                      V = V_list, #variance (tHIS IS WHAt CHANGES FROM HEmodel)
                      mods = formula, #ADD COVS HERE
                      random = ~1 | StudyID/ESId, #nesting structure
                      test= "t", #use t-tests
                      data=full, #define data
                      method="REML") #estimate variances using REML
  coef_mod_means <- as.data.frame(coef_test(mod_means,#estimation model above
                                            cluster=full$StudyID, #define cluster IDs
                                            vcov = "CR2")) #estimation method (CR2 is best)
  # limit to relevant rows (the means you are interested in)
  coef_mod_means$moderator <- "Exploratory: Tutoring Group Size: Individual"
  coef_mod_means$group <- rownames(coef_mod_means)
  rownames(coef_mod_means) <- c()
  coef_mod_means <- subset(coef_mod_means, substr(start = 1, stop = nchar(mods[i]), x = coef_mod_means$group)== mods[i])
  coef_mod_means$group <- substr(x = coef_mod_means$group, start = nchar(mods[i])+1, stop = nchar(coef_mod_means$group))
  coef_mod_means <- subset(coef_mod_means, coef_mod_means$group=="Paraprofessionals" | coef_mod_means$group == "Teachers")
  means <- dplyr::bind_rows(means, coef_mod_means)
}

# Small Group next (as reference for the Tutoring Provider Means)
for(i in 1:length(mods)) {
  # i <- 1
  # Provider
  changes <- c("PD_class", "PD_content", "PD_imp", "tbl_curr", "tbl_benchmark", "tbl_online", "tbl_crossage", "tbl_tutoring",
               "imp_curr", "imp_softw", "imp_none",
               "curr_tech", "curr_notech", "curr_none",
               "achiever_high", "achiever_tutoring", "achiever_all", "achiever_low", 
               "type_individual", "type_nottut", "type_smallgroup")

  formula <- reformulate(termlabels = c(mods[i], changes, terms, "-1"))   # Worth knowing - if you duplicate terms, it keeps the first one
  mod_means <- rma.mv(yi=Effect.Size, #effect size
                      V = V_list, #variance (tHIS IS WHAt CHANGES FROM HEmodel)
                      mods = formula, #ADD COVS HERE
                      random = ~1 | StudyID/ESId, #nesting structure
                      test= "t", #use t-tests
                      data=full, #define data
                      method="REML") #estimate variances using REML
  coef_mod_means <- as.data.frame(coef_test(mod_means,#estimation model above
                                            cluster=full$StudyID, #define cluster IDs
                                            vcov = "CR2")) #estimation method (CR2 is best)
  # limit to relevant rows (the means you are interested in)
  coef_mod_means$moderator <- "Exploratory: Tutoring Group Size: Small Group"
  coef_mod_means$group <- rownames(coef_mod_means)
  rownames(coef_mod_means) <- c()
  coef_mod_means <- subset(coef_mod_means, substr(start = 1, stop = nchar(mods[i]), x = coef_mod_means$group)== mods[i])
  coef_mod_means$group <- substr(x = coef_mod_means$group, start = nchar(mods[i])+1, stop = nchar(coef_mod_means$group))
  coef_mod_means <- subset(coef_mod_means, coef_mod_means$group=="Paraprofessionals" | coef_mod_means$group == "Teachers")
  means <- dplyr::bind_rows(means, coef_mod_means)
}

##### 3) Moderators for PD implementation studies #####
# This gets you the moderators for the average PD studies (Type across all studies, etc)
mods <- c("as.factor(full$imp_type)")

for(i in 1:length(mods)) {
  # i <- 1
  # Provider
  changes <- c("PD_class", "PD_content", "tbl_curr", "tbl_benchmark", "tbl_online", "tbl_crossage", "tbl_tutoring", "PD_imp",
               "curr_tech", "curr_notech", "curr_none", 
               # "provider_volunteer", "provider_parap", "provider_nottut", "provider_teacher",
               "type_smallgroup", "type_individual", "type_nottut") #,
# "achiever_low", "achiever_high", "achiever_tutoring", "achiever_all")
  
  formula <- reformulate(termlabels = c(mods[i], changes, terms, "-1"))   # Worth knowing - if you duplicate terms, it keeps the first one
  mod_means <- rma.mv(yi=Effect.Size, #effect size
                      V = V_list, #variance (tHIS IS WHAt CHANGES FROM HEmodel)
                      mods = formula, #ADD COVS HERE
                      random = ~1 | StudyID/ESId, #nesting structure
                      test= "t", #use t-tests
                      data=full, #define data
                      method="REML") #estimate variances using REML
  coef_mod_means <- as.data.frame(coef_test(mod_means,#estimation model above
                                            cluster=full$StudyID, #define cluster IDs
                                            vcov = "CR2")) #estimation method (CR2 is best)
  # limit to relevant rows (the means you are interested in)
  coef_mod_means$moderator <- gsub(x = mods[i], pattern = "as.factor", replacement = "")
  coef_mod_means$group <- rownames(coef_mod_means)
  rownames(coef_mod_means) <- c()
  coef_mod_means <- subset(coef_mod_means, substr(start = 1, stop = nchar(mods[i]), x = coef_mod_means$group)== mods[i])
  coef_mod_means$group <- substr(x = coef_mod_means$group, start = nchar(mods[i])+1, stop = nchar(coef_mod_means$group))
  means <- dplyr::bind_rows(means, coef_mod_means)
}

# This gets you the average across two subcategories (PD_software & PD_curriculum together)



##### 4) Moderators for curriculum studies #####
mods <- c("as.factor(full$curr_type)")

for(i in 1:length(mods)) {
  # i <- 1
  # Provider
  changes <- c("PD_class", "PD_content", "PD_imp", "tbl_benchmark", "tbl_crossage", "tbl_online", "tbl_tutoring", "tbl_curr", 
               "imp_curr", "imp_softw", "imp_none", 
                # "provider_volunteer", "provider_parap", "provider_nottut", "provider_teacher",
               "type_smallgroup", "type_individual", "type_nottut") #,
# "achiever_low", "achiever_high", "achiever_tutoring", "achiever_all")
  
  formula <- reformulate(termlabels = c(mods[i], changes, terms, "-1"))   # Worth knowing - if you duplicate terms, it keeps the first one
  mod_means <- rma.mv(yi=Effect.Size, #effect size
                      V = V_list, #variance (tHIS IS WHAt CHANGES FROM HEmodel)
                      mods = formula, #ADD COVS HERE
                      random = ~1 | StudyID/ESId, #nesting structure
                      test= "t", #use t-tests
                      data=full, #define data
                      method="REML") #estimate variances using REML
  coef_mod_means <- as.data.frame(coef_test(mod_means,#estimation model above
                                            cluster=full$StudyID, #define cluster IDs
                                            vcov = "CR2")) #estimation method (CR2 is best)
  # limit to relevant rows (the means you are interested in)
  coef_mod_means$moderator <- gsub(x = mods[i], pattern = "as.factor", replacement = "")
  coef_mod_means$group <- rownames(coef_mod_means)
  rownames(coef_mod_means) <- c()
  coef_mod_means <- subset(coef_mod_means, substr(start = 1, stop = nchar(mods[i]), x = coef_mod_means$group)== mods[i])
  coef_mod_means$group <- substr(x = coef_mod_means$group, start = nchar(mods[i])+1, stop = nchar(coef_mod_means$group))
  means <- dplyr::bind_rows(means, coef_mod_means)
}


##### 5) Means for categories #####
    # You need to work through these, uncentering any moderators that don't apply to that category
    # Tutoring:
changes <- c("PD_class", "PD_content", "PD_imp", "tbl_benchmark", "tbl_curr", "tbl_online", "tbl_crossage", "tbl_tutoring", 
             "imp_curr", "imp_softw", "imp_none",
             "curr_tech", "curr_notech", "curr_none",  
             "achiever_high", "achiever_tutoring", "achiever_all", "achiever_low")
formula <- reformulate(termlabels = c(changes, terms))   # Worth knowing - if you duplicate terms, it keeps the first one
mod_means <- rma.mv(yi=Effect.Size, #effect size
                    V = V_list, #variance (tHIS IS WHAt CHANGES FROM HEmodel)
                    mods = formula, #ADD COVS HERE
                    random = ~1 | StudyID/ESId, #nesting structure
                    test= "t", #use t-tests
                    data=full, #define data
                    method="REML") #estimate variances using REML
coef_mod_means <- as.data.frame(coef_test(mod_means,#estimation model above
                                          cluster=full$StudyID, #define cluster IDs
                                          vcov = "CR2")) #estimation method (CR2 is best)
# limit to relevant rows (the means you are interested in)
coef_mod_means$moderator <- "Category"
coef_mod_means$group <- rownames(coef_mod_means)
rownames(coef_mod_means) <- c()
coef_mod_means <- subset(coef_mod_means, coef_mod_means$group == "intrcpt")
coef_mod_means$group <- "Tutoring"
means <- dplyr::bind_rows(means, coef_mod_means)

    # PD: Class:
changes <- c("PD_content", "PD_imp", "tbl_benchmark", "tbl_curr", "tbl_crossage", "tbl_online", "tbl_tutoring", "PD_class", 
             "imp_curr", "imp_softw", "imp_none",
             "curr_tech", "curr_notech", "curr_none", 
             # "provider_volunteer", "provider_parap", "provider_teacher", "provider_nottut",
             "type_smallgroup", "type_individual", "type_nottut") #,

formula <- reformulate(termlabels = c(changes, terms))   # Worth knowing - if you duplicate terms, it keeps the first one
mod_means <- rma.mv(yi=Effect.Size, #effect size
                    V = V_list, #variance (tHIS IS WHAt CHANGES FROM HEmodel)
                    mods = formula, #ADD COVS HERE
                    random = ~1 | StudyID/ESId, #nesting structure
                    test= "t", #use t-tests
                    data=full, #define data
                    method="REML") #estimate variances using REML
coef_mod_means <- as.data.frame(coef_test(mod_means,#estimation model above
                                          cluster=full$StudyID, #define cluster IDs
                                          vcov = "CR2")) #estimation method (CR2 is best)
# limit to relevant rows (the means you are interested in)
coef_mod_means$moderator <- "Category"
coef_mod_means$group <- rownames(coef_mod_means)
rownames(coef_mod_means) <- c()
coef_mod_means <- subset(coef_mod_means, coef_mod_means$group == "intrcpt")
coef_mod_means$group <- "PD: Class"
means <- dplyr::bind_rows(means, coef_mod_means)

# PD: Content:
changes <- c("PD_class", "PD_imp", "tbl_benchmark", "tbl_curr", "tbl_online", "tbl_crossage", "tbl_tutoring", "PD_content", 
             "imp_curr", "imp_softw", "imp_none",
             "curr_tech", "curr_notech", "curr_none", 
             # "provider_volunteer", "provider_parap", "provider_teacher", "provider_nottut",
             "type_smallgroup", "type_individual", "type_nottut") #,
# "extratime_yes", "extratime_notpossible", "extratime_nottut", "extratime_no")

formula <- reformulate(termlabels = c(changes, terms))   # Worth knowing - if you duplicate terms, it keeps the first one
mod_means <- rma.mv(yi=Effect.Size, #effect size
                    V = V_list, #variance (tHIS IS WHAt CHANGES FROM HEmodel)
                    mods = formula, #ADD COVS HERE
                    random = ~1 | StudyID/ESId, #nesting structure
                    test= "t", #use t-tests
                    data=full, #define data
                    method="REML") #estimate variances using REML
coef_mod_means <- as.data.frame(coef_test(mod_means,#estimation model above
                                          cluster=full$StudyID, #define cluster IDs
                                          vcov = "CR2")) #estimation method (CR2 is best)
# limit to relevant rows (the means you are interested in)
coef_mod_means$moderator <- "Category"
coef_mod_means$group <- rownames(coef_mod_means)
rownames(coef_mod_means) <- c()
coef_mod_means <- subset(coef_mod_means, coef_mod_means$group == "intrcpt")
coef_mod_means$group <- "PD: Content"
means <- dplyr::bind_rows(means, coef_mod_means)


# PD: Implementation:
changes <- c("PD_class", "PD_content", "tbl_benchmark", "tbl_curr", "tbl_online", "tbl_crossage", "tbl_tutoring", "PD_imp", 
             "curr_tech", "curr_notech", "curr_none", 
             # "provider_volunteer", "provider_parap", "provider_teacher", "provider_nottut",
             "type_smallgroup", "type_individual", "type_nottut") #,
# "extratime_yes", "extratime_notpossible", "extratime_nottut", "extratime_no")

formula <- reformulate(termlabels = c(changes, terms))   # Worth knowing - if you duplicate terms, it keeps the first one
mod_means <- rma.mv(yi=Effect.Size, #effect size
                    V = V_list, #variance (tHIS IS WHAt CHANGES FROM HEmodel)
                    mods = formula, #ADD COVS HERE
                    random = ~1 | StudyID/ESId, #nesting structure
                    test= "t", #use t-tests
                    data=full, #define data
                    method="REML") #estimate variances using REML
coef_mod_means <- as.data.frame(coef_test(mod_means,#estimation model above
                                          cluster=full$StudyID, #define cluster IDs
                                          vcov = "CR2")) #estimation method (CR2 is best)
# limit to relevant rows (the means you are interested in)
coef_mod_means$moderator <- "Category"
coef_mod_means$group <- rownames(coef_mod_means)
rownames(coef_mod_means) <- c()
coef_mod_means <- subset(coef_mod_means, coef_mod_means$group == "intrcpt")
coef_mod_means$group <- "PD: Implementation"
means <- dplyr::bind_rows(means, coef_mod_means)


    # Curriculum
changes <- c("PD_class", "PD_imp", "tbl_benchmark", "tbl_online", "tbl_crossage", "tbl_tutoring", "PD_content", "tbl_curr", 
             "imp_curr", "imp_softw", "imp_none",
             # "provider_volunteer", "provider_parap", "provider_nottut", "provider_teacher",
             "type_smallgroup", "type_individual", "type_nottut") #,
# "extratime_yes", "extratime_notpossible", "extratime_nottut", "extratime_no")

formula <- reformulate(termlabels = c(changes, terms))   # Worth knowing - if you duplicate terms, it keeps the first one
mod_means <- rma.mv(yi=Effect.Size, #effect size
                    V = V_list, #variance (tHIS IS WHAt CHANGES FROM HEmodel)
                    mods = formula, #ADD COVS HERE
                    random = ~1 | StudyID/ESId, #nesting structure
                    test= "t", #use t-tests
                    data=full, #define data
                    method="REML") #estimate variances using REML
coef_mod_means <- as.data.frame(coef_test(mod_means,#estimation model above
                                          cluster=full$StudyID, #define cluster IDs
                                          vcov = "CR2")) #estimation method (CR2 is best)
# limit to relevant rows (the means you are interested in)
coef_mod_means$moderator <- "Category"
coef_mod_means$group <- rownames(coef_mod_means)
rownames(coef_mod_means) <- c()
coef_mod_means <- subset(coef_mod_means, coef_mod_means$group == "intrcpt")
coef_mod_means$group <- "Curriculum"
means <- dplyr::bind_rows(means, coef_mod_means)

    # Benchmark Assessments
changes <- c("PD_class", "PD_imp", "tbl_online", "tbl_crossage", "tbl_tutoring", "PD_content", "tbl_curr", "tbl_benchmark", 
             "imp_curr", "imp_softw", "imp_none",
             "curr_tech", "curr_notech", "curr_none", 
             # "provider_volunteer", "provider_parap", "provider_nottut", "provider_teacher",
             "type_smallgroup", "type_individual", "type_nottut") #,
# "extratime_yes", "extratime_notpossible", "extratime_nottut", "extratime_no")

formula <- reformulate(termlabels = c(changes, terms))   # Worth knowing - if you duplicate terms, it keeps the first one
mod_means <- rma.mv(yi=Effect.Size, #effect size
                    V = V_list, #variance (tHIS IS WHAt CHANGES FROM HEmodel)
                    mods = formula, #ADD COVS HERE
                    random = ~1 | StudyID/ESId, #nesting structure
                    test= "t", #use t-tests
                    data=full, #define data
                    method="REML") #estimate variances using REML
coef_mod_means <- as.data.frame(coef_test(mod_means,#estimation model above
                                          cluster=full$StudyID, #define cluster IDs
                                          vcov = "CR2")) #estimation method (CR2 is best)
# limit to relevant rows (the means you are interested in)
coef_mod_means$moderator <- "Category"
coef_mod_means$group <- rownames(coef_mod_means)
rownames(coef_mod_means) <- c()
coef_mod_means <- subset(coef_mod_means, coef_mod_means$group == "intrcpt")
coef_mod_means$group <- "Benchmark Assessments"
means <- dplyr::bind_rows(means, coef_mod_means)

    # CrossAge
changes <- c("PD_class", "PD_imp", "tbl_online", "tbl_tutoring", "PD_content", "tbl_curr", "tbl_benchmark", "tbl_crossage", 
             "imp_curr", "imp_softw", "imp_none",
             "curr_tech", "curr_notech", "curr_none", 
             # "provider_volunteer", "provider_parap", "provider_nottut", "provider_teacher",
             "type_smallgroup", "type_individual", "type_nottut") #,
# "extratime_yes", "extratime_notpossible", "extratime_nottut", "extratime_no")

formula <- reformulate(termlabels = c(changes, terms))   # Worth knowing - if you duplicate terms, it keeps the first one
mod_means <- rma.mv(yi=Effect.Size, #effect size
                    V = V_list, #variance (tHIS IS WHAt CHANGES FROM HEmodel)
                    mods = formula, #ADD COVS HERE
                    random = ~1 | StudyID/ESId, #nesting structure
                    test= "t", #use t-tests
                    data=full, #define data
                    method="REML") #estimate variances using REML
coef_mod_means <- as.data.frame(coef_test(mod_means,#estimation model above
                                          cluster=full$StudyID, #define cluster IDs
                                          vcov = "CR2")) #estimation method (CR2 is best)
# limit to relevant rows (the means you are interested in)
coef_mod_means$moderator <- "Category"
coef_mod_means$group <- rownames(coef_mod_means)
rownames(coef_mod_means) <- c()
coef_mod_means <- subset(coef_mod_means, coef_mod_means$group == "intrcpt")
coef_mod_means$group <- "CrossAge Tutoring"
means <- dplyr::bind_rows(means, coef_mod_means)

# CrossAge
changes <- c("PD_class", "PD_imp", "tbl_tutoring", "PD_content", "tbl_curr", "tbl_benchmark", "tbl_crossage", "tbl_online", 
             "imp_curr", "imp_softw", "imp_none",
             "curr_tech", "curr_notech", "curr_none", 
             # "provider_volunteer", "provider_parap", "provider_nottut", "provider_teacher",
             "type_smallgroup", "type_individual", "type_nottut") #,
# "extratime_yes", "extratime_notpossible", "extratime_nottut", "extratime_no")

formula <- reformulate(termlabels = c(changes, terms))   # Worth knowing - if you duplicate terms, it keeps the first one
mod_means <- rma.mv(yi=Effect.Size, #effect size
                    V = V_list, #variance (tHIS IS WHAt CHANGES FROM HEmodel)
                    mods = formula, #ADD COVS HERE
                    random = ~1 | StudyID/ESId, #nesting structure
                    test= "t", #use t-tests
                    data=full, #define data
                    method="REML") #estimate variances using REML
coef_mod_means <- as.data.frame(coef_test(mod_means,#estimation model above
                                          cluster=full$StudyID, #define cluster IDs
                                          vcov = "CR2")) #estimation method (CR2 is best)
# limit to relevant rows (the means you are interested in)
coef_mod_means$moderator <- "Category"
coef_mod_means$group <- rownames(coef_mod_means)
rownames(coef_mod_means) <- c()
coef_mod_means <- subset(coef_mod_means, coef_mod_means$group == "intrcpt")
coef_mod_means$group <- "Online Tutoring"
means <- dplyr::bind_rows(means, coef_mod_means)


#################################################################################
# Heterogeneity
#################################################################################
# 95% prediction intervals
PI_upper <- MVfull$b[1] + (1.96*sqrt(MVfull$sigma2[1] + MVfull$sigma2[2]))
PI_lower <- MVfull$b[1] - (1.96*sqrt(MVfull$sigma2[1] + MVfull$sigma2[2]))

########################################################################################################
# Output
########################################################################################################
#################################################################################
# Format Output   
#################################################################################
# Descriptives Table
table_study_df$Category <- row.names(table_study_df)
rownames(table_study_df) <- c()
table_study_df <- table_study_df[c("Category", "level", "Overall")]
table_study_df$Category[which(substr(table_study_df$Category, 1, 1)=="X")] <- NA
table_study_df$Category <- gsub(pattern = "\\..mean..SD..", replacement = "", x = table_study_df$Category)
table_study_df$Overall <- gsub(pattern = "\\( ", replacement = "\\(", x = table_study_df$Overall)
table_study_df$Overall <- gsub(pattern = "\\) ", replacement = "\\)", x = table_study_df$Overall)
table_study_df$Category <- gsub(pattern = "\\....", replacement = "", x = table_study_df$Category)
table_study_df$Category[which(table_study_df$Category=="n")] <- "Total Studies"

table_outcome_df$Category <- row.names(table_outcome_df)
rownames(table_outcome_df) <- c()
table_outcome_df <- table_outcome_df[c("Category", "level", "Overall")]
table_outcome_df$Category[which(substr(table_outcome_df$Category, 1, 1)=="X")] <- NA
table_outcome_df$Overall <- gsub(pattern = "\\( ", replacement = "\\(", x = table_outcome_df$Overall)
table_outcome_df$Overall <- gsub(pattern = "\\) ", replacement = "\\)", x = table_outcome_df$Overall)
table_outcome_df$Category <- gsub(pattern = "\\....", replacement = "", x = table_outcome_df$Category)
table_outcome_df$Category[which(table_outcome_df$Category=="n")] <- "Total Effect Sizes"

# Program Means Table
meanES$all.es.orig <- meanES$all.es
meanES$all.es <- format(round(meanES$all.es, 2), nsmall = 2)
meanES$all.es[which(meanES$all.es.orig > 0)] <- paste("+", meanES$all.es[which(meanES$all.es.orig > 0)], sep = "")
meanES$all.p <- format(round(meanES$all.p, 3), nsmall = 3)
rownames(meanES) <- c()
meanES$df <- NULL
meanES$all.es.orig<- NULL

# MetaRegression Table
MVnull.coef$coef <- row.names(MVnull.coef)
MVnull.coef <- MVnull.coef[c("coef", "beta", "SE", "tstat", "df", "p_Satt")]

MVfull.coef$coef <- row.names(MVfull.coef)
MVfull.coef <- MVfull.coef[c("coef", "beta", "SE", "tstat", "df", "p_Satt")]

# Marginal Means Table
means$moderator <- gsub(pattern = "\\(", replacement = "", x = means$moderator)
means$moderator <- gsub(pattern = "\\)", replacement = "", x = means$moderator)
means$moderator <- gsub(pattern = "\\`", replacement = "", x = means$moderator)
means$moderator <- gsub(pattern = "full\\$", replacement = "", x = means$moderator)

#################################################################################
# Saving Output   
#################################################################################
myreport <- read_docx()

# Descriptive Table
myreport <- body_add_par(x = myreport, value = "Table 1: Descriptives", style = "Normal")
descriptives_study <- flextable(head(table_study_df, n=nrow(table_study_df)))
descriptives_study <- add_header_lines(descriptives_study, values = c("Study Level"), top = FALSE)
descriptives_study <- theme_vanilla(descriptives_study)
myreport <- body_add_flextable(x = myreport, descriptives_study)

descriptives_outcome <- flextable(head(table_outcome_df, n=nrow(table_outcome_df)))
descriptives_outcome <- delete_part(descriptives_outcome, part = "header")
descriptives_outcome <- add_header_lines(descriptives_outcome, values = c("Outcome Level"))
descriptives_outcome <- theme_vanilla(descriptives_outcome)
myreport <- body_add_flextable(x = myreport, descriptives_outcome)
myreport <- body_add_par(x = myreport, value = "", style = "Normal")

# Program Means Table
myft <- flextable(head(meanES, n=nrow(meanES)))
myft <- theme_vanilla(myft)
myft <- autofit(myft)
myreport <- body_add_par(x = myreport, value = "Program Mean Effect Sizes", style = "Normal")
myreport <- body_add_flextable(x = myreport, myft)
myreport <- body_add_par(x = myreport, value = "", style = "Normal")

# MetaRegression Table
model_null <- flextable(head(MVnull.coef, n=nrow(MVnull.coef)))
colkeys <- c("beta", "SE", "tstat", "df")
model_null <- colformat_num(model_null,  j = colkeys, digits = 2)
model_null <- colformat_num(model_null,  j = c("p_Satt"), digits = 3)
#model_null <- autofit(model_null)
model_null <- add_header_lines(model_null, values = c("Null Model"), top = FALSE)
model_null <- theme_vanilla(model_null)
myreport <- body_add_par(x = myreport, value = "Table 2: Model Results", style = "Normal")
myreport <- body_add_flextable(x = myreport, model_null)
#myreport <- body_add_par(x = myreport, value = "", style = "Normal")

model_full <- flextable(head(MVfull.coef, n=nrow(MVfull.coef)))
model_full <- colformat_num(model_full,  j = colkeys, digits = 2)
model_full <- colformat_num(model_full,  j = c("p_Satt"), digits = 3)
#model_full <- autofit(model_full)
model_full <- delete_part(model_full, part = "header")
model_full <- add_header_lines(model_full, values = c("Meta-Regression"))
model_full <- theme_vanilla(model_full)
myreport <- body_add_flextable(x = myreport, model_full)
myreport <- body_add_par(x = myreport, value = "", style = "Normal")

# Marginal Means
marginalmeans <- flextable(head(means, n=nrow(means)))
marginalmeans <- colformat_num(marginalmeans,  j = colkeys, digits = 2)
marginalmeans <- colformat_num(marginalmeans,  j = c("p_Satt"), digits = 3)
rm(colkeys)
marginalmeans <- theme_vanilla(marginalmeans)
marginalmeans <- merge_v(marginalmeans, j = c("moderator"))
myreport <- body_add_par(x = myreport, value = "Marginal Means", style = "Normal")
myreport <- body_add_flextable(x = myreport, marginalmeans)
myreport <- body_add_par(x = myreport, value = "", style = "Normal")

# Study Level Means
studymeans <- study_level[c("Program", "Authors", "Year", "Table.number", 
                            "Table.topic", "num.findings", "StudyES")]
studymeans <- flextable(head(studymeans, n=nrow(studymeans)))
studymeans <- merge_v(studymeans, j = c("Program"))
myreport <- body_add_par(x = myreport, value = "Study Means", style = "Normal")
myreport <- body_add_flextable(x = myreport, studymeans)
myreport <- body_add_par(x = myreport, value = "", style = "Normal")

# Write to word doc
file = paste("ElementaryMathResults.docx", sep = "")
print(myreport, file)
