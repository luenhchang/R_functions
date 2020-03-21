# ------------------------------------------------------------------------------------------------------
# Program           : variable-label.R
# Modified from     : 
# Author            : Chang
# Date created      : 20190104
# Purpose           : Create two column dataframes with var.name and var.label for use in tables or graphs. Avoid doing this in other script files so that the labels won't vary from time to time. Purpose similar to your centralised SAS proc format file
# Function external : 
#------------------------------------------------------------------------------------------------------
# Run dependency    : 
# Type File
#-------------------------------------------------------------------------------------------------------
# Outpu paste0()

#-------------------------------------------------------------------------------------------------------
# Sys.Date()  History
#-------------------------------------------------------------------------------------------------------
# 2019010
#-------------------------------------------------------------------------------------------------------

#-------------------------------------------------------------------------------------------------
# Create variable labels per target phenotypes used in manuscript 2
## Vector lenght and order of individual variable names should match those of the labels to create
#-------------------------------------------------------------------------------------------------
# Variable names per target phenotypes used in manuscript 2
## Number= 22
targ.pheno.var.name.manu2 <- c(paste0("everDrug",c(1:9))
                               ,"SU_cannabis_ever"
                               ,"SU_cannabis_onset"
                               ,"SU_DSM4alcoholAbuse_ori"
                               ,"SU_DSM4alcoholDepend_ori"
                               ,paste0("SU_DSM5alcoholUD_",c("ori","0or1vs2or3"))
                               ,"SU_DSM4cannabisAbuse_ori"
                               ,"SU_cannabis_abuse_onset"
                               ,"SU_DSM4cannabisDepend_ori"
                               ,"SU_cannabis_dependence_onset"
                               ,paste0("SU_DSM5cannabisUD_",c("ori","0or1vs2or3"))
                               ,"SU_cannabis_use_disorder_onset") # length(targ.pheno.var.name.manu2)

# Variable labels per target phenotypes used in manuscript 2
## Number= 22
targ.pheno.label.manu2 <- c(paste0("Ever used ",c("cocaine" 
                                                  ,"amphetamine"
                                                  ,"inhalants"
                                                  ,"sedatives"
                                                  ,"hallucinogens"
                                                  ,"opioids"
                                                  ,"ecstasy"
                                                  ,"prescription pain killers"
                                                  ,"prescription stimulants"
                                                  ,"cannabis"))
                            ,"Age at onset of cannabis use"
                            ,"Alcohol abuse"
                            ,"Alcohol dependence"
                            ,paste0("DSM5 AUD ",c("4 point scale","0 1 vs 2 3"))
                            ,"Cannabis abuse"
                            ,"Age at onset of cannabis abuse"
                            ,"Cannabis dependence"
                            ,"Age at onset of cannabis dependence"
                            ,paste0("DSM5 CUD ",c("4 point scale","0 1 vs 2 3"))
                            ,"Age at onset of CUD") # length(targ.pheno.label.manu2) 22

#-------------------------------------------------------------------------------------------------
# Create variable labels per target phenotypes used in manuscript 3
## Vector lenght and order of individual variable names should match those of the labels to create
#-------------------------------------------------------------------------------------------------

## Target phenotype variable names grouped to manuscript 3
## Number: 15
targ.pheno.var.name.manu3 <- c(# GSCAN phenotypes in middle-aged adults
  paste0("GSCAN_",c("Q2_recode","Q4","Q1","Q3_recode","Q6_recode","Q5_Drinks_per_week"))
  # SUD in middle-aged adults
  ,"alcdep4","nicdep4","ftnd_dep"
  # Conduct disorder, antisocial personality disorder, other mental disorders in adults
  ,"dsmiv_conductdx","aspddx4","depdx","panic4","sp_dsm4","mania_scrn")
## Target phenotype variable labels grouped to manuscript 3
## Number included= 15 (total=31)
targ.pheno.label.manu3 <- c(# GSCAN phenotypes in middle-aged adults
  "Smoking initiation" # GSCAN_Q2_recode
  ,"Age at starting regular smoking" # GSCAN_Q4
  ,"Cigarettes per day" # GSCAN_Q1
  ,"Smoking cessation"  # GSCAN_Q3_recode
  ,"Drinkers versus non-drinkers" # GSCAN_Q6_recode
  ,"Drinks per week in active drinkers" # GSCAN_Q5_Drinks_per_week
  ,paste0("DSM-IV ",c("alcohol dependence","nicotine dependence"))
  ,"FTND-based nicotine dependence"
  ,paste0("DSM-IV ",c("conduct disorder","antisocial personality disorder","depressive disorder","panic disorder","social phobia"))
  ,"Mania screen")

#-------------------------------------------------------------------------------------------
# Create a data.frame with variable names and variable labels created above
#-------------------------------------------------------------------------------------------
## value Numb_target_pheno_predicted is for creating a column summation row in the section belows. The name should be consistent with row.last.part1.manu2 and row.last.part1.manu3

targ.pheno.var.labels <- data.frame(var.name=c(targ.pheno.var.name.manu2
                                               ,targ.pheno.var.name.manu3
                                               ,"Numb_target_pheno_predicted")
                                    ,var.label=c(targ.pheno.label.manu2
                                                 ,targ.pheno.label.manu3
                                                 ,"Number of target phenotypes predicted")
                                    ,stringsAsFactors = F) # dim(targ.pheno.var.labels) 38  2


discovery.trait.var.labels <- data.frame(disco.trait.name=c("si","ai","cpd","sc","dpw")
                                         ,disco.trait.label=c("Smoking initiation"
                                                              ,"Age at starting regular smoking"
                                                              ,"Cigarettes per day"
                                                              ,"Smoking cessation"
                                                              ,"Drinks per week in active drinkers")
                                         ,stringsAsFactors = F)

