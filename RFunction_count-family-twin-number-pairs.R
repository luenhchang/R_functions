###############################################################################################
# program name      : RFunction_count-family-twin-number-pairs.R
# modifiied from    : PRS_UKB_201711_step15-01-01_make-input-files-for-1varGREML.R
# purpose           : Count number of families, twin pairs, parents, sib in the target samples
# programmer  	    : Chang
# date created	    : 20181227
# external function : nil
# Internal function : 
# note			    : 
#---------------------------------------------------------------------------------------
# run dependency  : 
# Type  File
#---------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------
# Sys.Date() History
#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------

# Locations of main folders
homeDir <- "/mnt/backedup/home/lunC/";
locRFunction <- paste0(homeDir,"scripts/RFunctions/")

CountFamilyTwinNumberPairsWithZygosity <- function(data=tem.uniqueID
                                                   ,input.var.name.family.ID="FAMID"
                                                   ,input.var.name.person.ID="ID"
                                                   ,input.var.name.zygosity="ZYGOSITY"
                                                   ,input.var.name.sex="nSEX"
                                                   ,input.var.name.date.of.birth="DOB"
                                                   ,sample.name="Manuscript 2"
                                                   ,out.file.path=paste0(locPheno,"family-twin-information_manuscript2.tsv")
){
  # Count number of people in the input data, number of families, number of twin individuals and pairs. Input data must have columns for familyID, ID, zygosity (6 groups), sex and date of birth
  
  # Copy columns from input data 
  data$FAMID <- data[,input.var.name.family.ID]
  data$ID <- data[,input.var.name.person.ID]
  data$zygosity <- data[,input.var.name.zygosity]
  data$sex <- data[,input.var.name.sex]
  data$DOB <- data[,input.var.name.date.of.birth]
  
  ## Extract last 2 characters of the ID column
  library(magrittr)
  data %<>% mutate(ID.suffix=substr(ID,nchar(ID)-2+1,nchar(ID))) # dim(data) 2463 8
  
  # Collaspe 6 groups ZYGOSITY to 5 groups
  data$ZYGOSITY.5.groups <- ifelse(data$zygosity %in% c(5,6),5
                                   ,data$zygosity) # dim(data) 2463 9
  
  # Count number of twin and twin pairs per family and zygosity
  ## Be aware of conflict in same-named function summarise() of package dplyr and plyr
  # [R floor Function](http://www.endmemo.com/program/R/floor.php)
  data2 <- data %>%  
    filter(!is.na(ZYGOSITY.5.groups)) %>%  # Only twins have values in ZYGOSITY
    dplyr::group_by(FAMID,ZYGOSITY.5.groups,DOB, add=TRUE) %>% # multiple group columns
    dplyr::summarise(numb.twin.per.family.zygosity= n()
                     ,numb.twin.pairs.per.family.zygosity= floor(n()/2)) # dim(data2) 1130 5
  
  # Cross table of frequ of ID.suffix and ZYGOSITY
  # Convert table to data.frame [How to convert a table to a data frame](https://stackoverflow.com/questions/10758961/how-to-convert-a-table-to-a-data-frame)
  cross.table.IDsuffix.zygosity <- as.data.frame.matrix(table(factor(data$ID.suffix,levels = c("01","02","50","51","52","53","54"))
                                                              ,factor(data$zygosity,levels = c(1:6,NA))
                                                              ,exclude = FALSE))
  
  # Non-twins
  data3 <- data %>%  
    filter(is.na(ZYGOSITY.5.groups)) %>%  # Non twins have no values in ZYGOSITY
    dplyr::summarise(numb.non.twins= n()) # dim(data3)
  
  # Add all the counts to one dataset
  family.twins <- data.frame(target.sample=sample.name
                             ,numb.people=length(data$ID) # 2463
                             ,numb.family=length(unique(data$FAMID)) # 1163 families
                             ,numb.twins=sum(data2$numb.twin.per.family.zygosity) # 1977 twins
                             ,numb.twin.pairs=sum(data2$numb.twin.pairs.per.family.zygosity)
                             # 835 twin pairs 
                             ,numb.non.twins=data3$numb.non.twins
                             ,stringsAsFactors = F)
  
  # Export file to input file folder
  ExportFileTabSeparated(data=family.twins
                         ,output.file.path = out.file.path)
}

#-------------------------------------------------------------------
# Input file without zygosity, DOB information
#-------------------------------------------------------------------

## https://genepi.qimr.edu.au/intranet/general/common_codes.html#H36
## Suffix Subject
##--------------------------------------
## 03	    Father
## 04	    Mother
## 05	    Spouse of Twin 1
## 08-24	Children of Twin 1
## 25	    Spouse of Twin 2
## 28-49	Children of Twin 2
## 50+	  Siblings of Twins
## 65-72	Adopted siblings of twins
## 73	    Adopted Father
## 74	    Adopted Mother
## 75-79	Maternal half-siblings
## 80-84	Paternal half-siblings
## 85	    Second spouse of Twin 01
## 86	    Second spouse of Twin 02
##--------------------------------------

CountFamilyTwinNumberPairsNoZygosityNoDOB <- function(data=data.manu3.uniqueID
                                                      ,input.var.name.family.ID="FAMID"
                                                      ,input.var.name.person.ID="ID"
                                                      ,input.var.name.sex="sex"
                                                      ,sample.name="Manuscript 3"
                                                      ,out.file.path=paste0(locPheno,"count-twins-and-their-relatives_manuscript3.tsv")
){
  # Count number of people in the input data, number of families, number of twin individuals and pairs. Input data must have columns for familyID, ID, zygosity (6 groups), sex and date of birth
  
  # Copy columns from input data 
  data$FAMID <- data[,input.var.name.family.ID]
  data$ID <- data[,input.var.name.person.ID]
  data$sex <- data[,input.var.name.sex]

  ## Extract last 2 characters of the ID column
  library(magrittr,lib.loc = "/software/R/R-3.4.1/lib64/R/library")
  
  data %<>% mutate(ID.suffix=substr(ID,nchar(ID)-2+1,nchar(ID))) # dim(data) 13999     5
  
  # Count number of twin and twin pairs per family and zygosity
  ## Be aware of conflict in same-named function summarise() of package dplyr and plyr
  # [R floor Function](http://www.endmemo.com/program/R/floor.php)
  count.twins <- data %>%  
    filter(ID.suffix %in% c("01","02")) %>%  # Only twins have values in ZYGOSITY
    dplyr::group_by(FAMID, add=TRUE) %>% # multiple group columns
    dplyr::summarise(numb.twin.per.family= n()
                     ,numb.twin.pairs.per.family= floor(n()/2)) # dim(count.twins) 3710 3
  
  ## Count number of IDs who are parents of twins
  count.parents <- data %>%  
    filter(ID.suffix %in% c("03","04")) %>%
    dplyr::summarise(numb.parents= n()) # 2047
    
  ## Count number of IDs who are spouses of twins
  ### How do I filter a range of numbers in R? https://stackoverflow.com/questions/51107901/how-do-i-filter-a-range-of-numbers-in-r
  count.spouses <- data %>%  
    filter(ID.suffix %in% c("05","25")) %>%
    dplyr::summarise(numb.spouses= n()) # 23
  
  ## Count number of IDs who are offsprings of twins
  count.offsprings <- data %>%  
    filter(ID.suffix %in% c("08","09","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49")) %>%
    dplyr::summarise(numb.offsprings= n()) #151

  ## Count number of IDs who are siblings of twins
  count.siblings <- data %>%  
    filter(ID.suffix %in% c("50","51","52","53","54","55","56","57","58","59","60","61","62","63","64")) %>%
    dplyr::summarise(numb.siblings= n()) #5191
  
  # Add all the counts to one dataset
  twins.and.their.relatives <- data.frame(target.sample=sample.name
                             ,numb.people=length(data$ID) # 14004
                             ,numb.family=length(unique(data$FAMID)) # 1163 families
                             ,numb.twins=sum(count.twins$numb.twin.per.family) # 1977 twins
                             ,numb.twin.pairs=sum(count.twins$numb.twin.pairs.per.family)
                             # 835 twin pairs 
                             ,numb.parents=count.parents$numb.parents
                             ,numb.siblings=count.siblings$numb.siblings
                             ,numb.offsprings=count.offsprings$numb.offsprings
                             ,numb.spouses=count.spouses$numb.spouses
                             ,stringsAsFactors = F)
  
  # Export file to input file folder
  ExportFileTabSeparated(data=twins.and.their.relatives
                         ,output.file.path = out.file.path)
}