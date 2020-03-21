##################################################################################
# Filename: Rfunction_harmonise-exposures-and-outcome_two-sample-MR.R
# Modified from: zMR_step06-03_two-sample-MR.R
# Program author: Chang
# Purpose: (1) Harmonise exposure (clumped GWAS of trait 1) and outcome (QCed GWAS of trait 2), (2) run MR analysis on harmonised data, (3) Generate MR report HTML files 
# Date created: 20190405
# How to use this file in R: source(paste0(locRFunction,"Rfunction_harmonise-exposures-and-outcome_two-sample-MR.R"))
# Reference: https://elifesciences.org/articles/34408
# Note: To be eligible for inclusion in MR-Base, studies should provide the following information for each SNP: the beta coefficient and standard error from a regression model (typically an additive model) and the modelled effect and non-effect alleles. This is the minimum information required for implementation of 2SMR. The following information is also sought but is not essential: effect allele frequency, sample size, p-values for SNP-phenotype associations, p-values for Hardy–Weinberg equilibrium, p-values for Cochran's Q test for between study heterogeneity (if a GWAS meta-analysis) and metrics of imputation quality, such as info or r2 scores (for imputed SNPs).
# file directory: 
#-----------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------
# Sys.time()  Update
#-----------------------------------------------------------------------------------------
# 20190830    Changed package loading to library(package) without lib.loc, because module load R/3.5.1 is used in the shell script that calls this R script
# 20190823    Added generation of MR reports
# 20190408    Performed MR on 180 pairs of exposure and outcome  
#-----------------------------------------------------------------------------------------

#-------------------------------------------
# Folder locations under my home directory
#-------------------------------------------
homeDir <- "/mnt/backedup/home/lunC/";
locRFunction <- paste0(homeDir,"scripts/RFunctions/")

# Load packages
library("dplyr")
library("TwoSampleMR")
#library("dplyr",lib.loc="/software/R/R-3.4.1/lib64/R/library")
#library("dplyr",lib.loc="/software/R/R-3.5.1/lib64/R/library")
#library("TwoSampleMR",lib.loc="/software/R/R-3.5.1/lib64/R/library")
#library("TwoSampleMR",lib.loc="/mnt/backedup/home/lunC/R/x86_64-pc-linux-gnu-library/3.4")

source(paste0(locRFunction,"RFunction_import_export_single_file.R"))

#------------------------------------------------------------------------------------------------
# Create a function that (1) harmonises an exposure and outcome using TwoSampleMR::harmonise_data, (2) runs two sample MR using TwoSampleMR::mr(), (3) generates MR reports, (4) run horizontal pleiotropy tests
#------------------------------------------------------------------------------------------------
## Number of arguments: 66-41+1 = 26
Run.two.sample.Mendelian.randomisation <- function(  exposure.consortium
                                                    ,exposure.trait
                                                    ,exposure.file.path
                                                    ,exposure.file.delimiter
                                                    ,exposure.file.colname.for.SNP
                                                    ,exposure.file.colname.for.beta
                                                    ,exposure.file.colname.for.standard.error
                                                    ,exposure.file.colname.for.effect.allele
                                                    ,exposure.file.colname.for.other.allele
                                                    ,exposure.file.colname.for.P.value
                                                    ,exposure.clumping.p1
                                                    ,exposure.clumping.p2
                                                    ,outcome.consortium
                                                    ,outcome.trait
                                                    ,outcome.file.path
                                                    ,outcome.file.delimiter=here.outcome.file.delimiter
                                                    ,outcome.file.colname.for.SNP
                                                    ,outcome.file.colname.for.beta
                                                    ,outcome.file.colname.for.standard.error
                                                    ,outcome.file.colname.for.effect.allele
                                                    ,outcome.file.colname.for.other.allele
                                                    ,outcome.file.colname.for.P.value
                                                    ,filePath.output.harmonised.data
                                                    ,filePath.output.MR.analysis
                                                    ,folderPath.output.MR.reports
                                                    ,filePath.output.horizontal.pleiotropy.test) {
  # Purpose: Read a clumped GWAS of trait 1 as an exposure, QCed GWAS of trait 2 as an outcome, harmonise these two data sets, export the harmonised data as a tsv file
  #
  # Args: 
  # exposure.consortium                       Source of trait 1 GWAS data
  # exposure.trait                            Name of trait 1
  # exposure.file.path                        File path of clumped GWAS file of trait 1 used as an exposure
  # exposure.file.delimiter                   Delimiter of the exposure file, must be either space or tab
  # exposure.file.colname.for.SNP             Column name that identifies SNP RS ID
  # exposure.file.colname.for.beta            Column name that identifies SNP beta coefficient on trait 1
  # exposure.file.colname.for.standard.error  Column name that identifies standard error
  # exposure.file.colname.for.effect.allele   Column name that identifies effect alleles 
  # exposure.file.colname.for.other.allele    Column name that identifies the other allele
  # exposure.file.colname.for.P.value         Column name that identifies the association P value of the SNP
  # exposure.clumping.p1                      LD clumping criteria of p1 value
  # exposure.clumping.p2                      LD clumping criteria of p2 value
  
  # outcome.consortium                        Source of trait 2 GWAS data
  # outcome.trait                             Name of trait 2
  # outcome.file.path                         File path of QCed GWAS of trait 2 used as the outcome
  # outcome.file.delimiter                    Delimiter of the outcome file, must be either space or tab.
  # outcome.file.colname.for.SNP              Column name that identifies SNP RS ID
  # outcome.file.colname.for.beta             Column name that identifies SNP beta coefficient on trait 2
  # outcome.file.colname.for.standard.error   Column name that identifies standard error
  # outcome.file.colname.for.effect.allele    Column name that identifies effect alleles 
  # outcome.file.colname.for.other.allele     Column name that identifies the other allele
  # outcome.file.colname.for.P.value          Column name that identifies the association P value of the SNP
  
  # filePath.output.harmonised.data           Path of output file with harmonised data
  # filePath.output.MR.analysis               Path of output file with all MR analysis results
  # folderPath.output.MR.reports              Folder path where MR report HTML is exported 
  
  #-------------------------------------------------------------------------------------------#
  # Part 1: code testing with fixed values for function arguments
  ## Comment this part out when running MR analysis using the function 
  #-------------------------------------------------------------------------------------------#
  # Specify the function arguments using values printed at /mnt/lustre/working/lab_nickm/lunC/MR_ICC_GSCAN_201806/two-sample-MR/input/harmonised-data/pbs_output/harmonise_exposure_outcome_pair2.pbs.out
  
  # exposure.file.path="/mnt/lustre/working/lab_nickm/lunC/MR_ICC_GSCAN_201806/data/noICC_results/QC4_GWAS_from_clumped_SNPs/GWAS_from-clumped-SNPs_ai_noICC_LDWindow-kb-10000_R2-0.01_p1-1e-5_p2-1e-5"
  # exposure.consortium="GSCAN"
  # exposure.trait="AI"
  # exposure.file.delimiter="space"
  # exposure.file.colname.for.SNP="SNP"
  # exposure.file.colname.for.beta="BETA"
  # exposure.file.colname.for.standard.error="SE"
  # exposure.file.colname.for.effect.allele="ALT"
  # exposure.file.colname.for.other.allele="REF"
  # exposure.file.colname.for.P.value="PVALUE"
  # exposure.clumping.p1="1e-5"
  # exposure.clumping.p2="1e-5"
  # 
  # outcome.file.path="/mnt/lustre/working/lab_nickm/lunC/MR_ICC_GSCAN_201806/data/noICC_results/QC3_remove_ambiguousSNPs_indel/cpd_noICC.ambiguousSNPRemoved"
  # outcome.consortium="GSCAN"
  # outcome.trait="CPD"
  # outcome.file.delimiter="tab"
  # outcome.file.colname.for.SNP="RSID"
  # outcome.file.colname.for.beta="BETA"
  # outcome.file.colname.for.standard.error="SE"
  # outcome.file.colname.for.effect.allele="ALT"
  # outcome.file.colname.for.other.allele="REF"
  # outcome.file.colname.for.P.value="PVALUE"
  # filePath.output.harmonised.data="/mnt/lustre/working/lab_nickm/lunC/MR_ICC_GSCAN_201806/two-sample-MR/input/harmonised-data/harmonised-data_exposure-clumped-ai-noICC-LDWindow-kb-10000-R2-0.01-p1-1e-5-p2-1e-5_outcome-GSCAN-CPD.tsv"
  # filePath.output.MR.analysis="/mnt/lustre/working/lab_nickm/lunC/MR_ICC_GSCAN_201806/two-sample-MR/output/MR-analysis_exposure-GSCAN-AI-1e-5-1e-5_outcome-GSCAN-CPD.tsv"
  # folderPath.output.MR.reports="/mnt/lustre/working/lab_nickm/lunC/MR_ICC_GSCAN_201806/two-sample-MR/output_MR-reports"
  # filePath.output.horizontal.pleiotropy.test="/mnt/lustre/working/lab_nickm/lunC/MR_ICC_GSCAN_201806/two-sample-MR/output_horizontal-pleiotropy/MR-Egger-intercept_exposure-GSCAN-AI-1e-5-1e-5_outcome-GSCAN-CPD.tsv"

  #-------------------------------------------------------------------------------------------#
  # Harmonise data
  #-------------------------------------------------------------------------------------------#
  
  # Import a clumped GWAS file (exposure) as a data.frame
  if (exposure.file.delimiter=="space"){
    ImportASpaceSeparatedFile(input.file.path = exposure.file.path
                              ,data.name = "df.exposure") # dim(df.exposure) 64 13
  } else if (exposure.file.delimiter=="tab"){
    ImportATabSeparatedFile(input.file.path = exposure.file.path
                            ,data.name = "df.exposure")
  } else {
    print("File delimiter must be either space or tab")
  }
  

  # Format the existing data.frame of clumped GWAS to exposure type
  exposure_data <- TwoSampleMR::format_data( dat=df.exposure
                                            ,type="exposure"
                                            ,header = TRUE
                                            ,snp_col = exposure.file.colname.for.SNP
                                            ,beta_col = exposure.file.colname.for.beta
                                            ,se_col = exposure.file.colname.for.standard.error
                                            ,effect_allele_col = exposure.file.colname.for.effect.allele
                                            ,other_allele_col = exposure.file.colname.for.other.allele
                                            ,pval_col = exposure.file.colname.for.P.value) # dim(exposure_data) 64 11
  
  # Overwrite the exposure column with the trait name subset from the input file path
  exposure_data$exposure <- rep(exposure.trait, nrow(exposure_data)) 
  
  # Specify the delimiter of outcome file
  if (outcome.file.delimiter=="space"){
    outcome.file.delimiter.symbol <- " "
  } else if (outcome.file.delimiter=="tab"){
    outcome.file.delimiter.symbol <- "\t"
  } else {
    print("File delimiter must be either space or tab")
  }
  
  # Use tryCatch() here as some iterations won't run because of not enough SNPs but we want the loopig continues even there is an error
  tryCatch({
    # Extract exposure SNPs from outcome GWAS file
    outcome_data <- TwoSampleMR::read_outcome_data(filename = outcome.file.path 
                                                   ,snps = exposure_data$SNP
                                                   ,sep = outcome.file.delimiter.symbol 
                                                   ,snp_col = outcome.file.colname.for.SNP 
                                                   ,beta_col = outcome.file.colname.for.beta 
                                                   ,se_col = outcome.file.colname.for.standard.error 
                                                   ,effect_allele_col = outcome.file.colname.for.effect.allele 
                                                   ,other_allele_col = outcome.file.colname.for.other.allele 
                                                   ,pval_col = outcome.file.colname.for.P.value ) # dim(outcome_data) 64 12
    
    # Overwrite the outcome column with the phenotype name
    outcome_data$outcome <- rep(outcome.trait,nrow(outcome_data))
    
    # Harmonise exposure data and outcome data
    ## The exposure data and outcome data are now obtained, but it is important to harmonise the effects. This means that the effect of a SNP on the exposure and the effect of that SNP on the outcome must each correspond to the same allele.
    harmo_data <- TwoSampleMR::harmonise_data( exposure_dat=exposure_data
                                              ,outcome_dat = outcome_data)
    
    # Convert all factor columns to character columns
    harmo_data[] <- lapply(harmo_data, function(x) if(is.factor(x)) as.character(x) else x)
    
    # Export harmonised data
    # ExportFileTabSeparated(data=harmo_data
    #                        ,output.file.path=filePath.output.harmonised.data)
    
    #--------------------------------------------------------------------------
    # Run all MR analysis
    #--------------------------------------------------------------------------
    # all.MR.analyses <- TwoSampleMR::mr(harmo_data) # dim(all.MR.analyses) 1 9
    # 
    # # Add additional information to the MR analysis data
    # all.MR.analyses$exposure.consortium <- exposure.consortium
    # all.MR.analyses$exposure.clumping.p1 <- exposure.clumping.p1
    # all.MR.analyses$exposure.clumping.p2 <- exposure.clumping.p2
    # all.MR.analyses$outcome.consortium <- outcome.consortium
    # 
    # # Convert all factor columns to character columns
    # all.MR.analyses[] <- lapply(all.MR.analyses, function(x) if(is.factor(x)) as.character(x) else x)
    
    # Export MR analysis results
    # ExportFileTabSeparated(data=all.MR.analyses
    #                        ,output.file.path = filePath.output.MR.analysis)
    #-----------------------------------------------------------------------------------------------
    # Generate a MR report per harmonised data. This report HTML contains every MR analysis and plot
    #-----------------------------------------------------------------------------------------------
    # Recode exposure and outcome, as mr_report uses these two column values as output file name removing all non-letter string
    # exposure.modified <- paste0("Exposure",exposure.consortium,exposure.trait,exposure.clumping.p1)
    # outcome.modified <- paste0("Outcome",outcome.consortium,outcome.trait)
    # harmo_data2 <- harmo_data %>% dplyr::mutate(exposure=exposure.modified
    #                                             ,outcome=outcome.modified)
    # # Run mr_report
    # TwoSampleMR::mr_report(dat= harmo_data2
    #                        ,output_path = folderPath.output.MR.reports
    #                        ,output_type = "html"
    #                        , author = "Chang"
    #                        , study="MR-report")
    
    #-----------------------------------------------------------------------------------------------
    # Run horizontal pleiotropy 
    #-----------------------------------------------------------------------------------------------
    hori.pleio <- TwoSampleMR::mr_pleiotropy_test(dat=harmo_data)
    
    # Add grouping variables to the result data
    hori.pleio$exposure.consortium <- exposure.consortium
    hori.pleio$exposure.clumping.p1 <- exposure.clumping.p1
    hori.pleio$exposure.clumping.p2 <- exposure.clumping.p2
    hori.pleio$outcome.consortium <- outcome.consortium
    
    # Convert all factor columns to character columns
    hori.pleio[] <- lapply(hori.pleio, function(x) if(is.factor(x)) as.character(x) else x)

    # Export MR analysis results
    ExportFileTabSeparated(data=hori.pleio
                          ,output.file.path = filePath.output.horizontal.pleiotropy.test)
  
  # End tryCatch() function  
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
                         
}

#-------------------------------------------------------------------------------------------------------#
#-------------------------------------------This is the end of this file--------------------------------#
#-------------------------------------------------------------------------------------------------------#  
