# ---------------------------------------------------------------------------------------
# Program           : RFunction_partial-string-matching.R
# Modified from     : 
# Author            : Tony Hirst
# Date created      : 20190418
# Purpose           : Merge two data sets based on partial string matching in their merging keys
# Note              : Functions here find merging key A that is partailly matched merging key B. The function returns errors if no partial match is found. May further handle the error with tryCatch
# Reference         : https://www.r-bloggers.com/merging-data-sets-based-on-partially-matched-data-elements/
# Function internal : signature(), partialMatch()
# How to run this   : source(paste0(locRFunction,"RFunction_partial-string-matching.R"))
#----------------------------------------------------------------------------------------------
# Run dependency    : 
# Type File
#---------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------
# Sys.Date() History
#----------------------------------------------------------------------------------------
# 
#----------------------------------------------------------------------------------------

# Dowlonad two example data CSV files using URLs
#Data loaded in from downloaded files as:
##PercentageUsingTheNet
##ccode
# install.packages("RCurl")
library(RCurl)
URL.1 <- "http://s.telegraph.co.uk/graphics/conrad/PercentageUsingTheNet.csv"
x <- getURL(URL.1, ssl.verifypeer = FALSE)
PercentageUsingTheNet <- read.csv(textConnection(x)) # dim(PercentageUsingTheNet) 176 3

ccode <- data.frame(Country.Name=c("ANDORRA","ANGOLA","ANGUILLA","ANTARCTICA","ANTIGUA AND BARBUDA","ARGENTINA","ARMENIA","ARUBA","AUSTRALIA","AUSTRIA","AZERBAIJAN","BAHAMAS","BAHRAIN")
                    ,ISO.3166.1.alpha.2.code=c("AD","AO","AI","AQ","AG","AR","AM","AW","AU","AT","AZ","BS","BH")
                    ,stringsAsFactors = F)

##Here's where the algorithm starts...
##I'm going to generate a signature from country names to reduce some of the minor differences between strings
##In this case, convert all characters to lower case, sort the words alphabetically, and then concatenate them with no spaces.
##So for example, United Kingdom would become kingdomunited
##We might also remove stopwords such as 'the' and 'of'.
signature <- function(x){
  sig=paste(sort(unlist(strsplit(tolower(x)," "))),collapse='')
  return(sig)
}

partialMatch <- function(x,y,levDist=0.1){
  xx=data.frame(sig=sapply(x, signature),row.names=NULL)
  yy=data.frame(sig=sapply(y, signature),row.names=NULL)
  xx$raw=x
  yy$raw=y
  xx=subset(xx,subset=(sig!=''))
  xy=merge(xx,yy,by='sig',all=T)
  matched=subset(xy,subset=(!(is.na(raw.x)) & !(is.na(raw.y))))
  matched$pass="Duplicate"
  todo=subset(xy,subset=(is.na(raw.y)),select=c(sig,raw.x))
  colnames(todo)=c('sig','raw')
  todo$partials= as.character(sapply(todo$sig, agrep, yy$sig,max.distance = levDist,value=T))
  todo=merge(todo,yy,by.x='partials',by.y='sig')
  partial.matched=subset(todo,subset=(!(is.na(raw.x)) & !(is.na(raw.y))),select=c("sig","raw.x","raw.y"))
  partial.matched$pass="Partial"
  matched=rbind(matched,partial.matched)
  un.matched=subset(todo,subset=(is.na(raw.x)),select=c("sig","raw.x","raw.y"))
  if (nrow(un.matched)>0){
    un.matched$pass="Unmatched"
    matched=rbind(matched,un.matched)
  }
  matched=subset(matched,select=c("raw.x","raw.y","pass"))
  
  return(matched)
}

#A rogue character in @coneee's data file borked things for me, so let's do a character code conversion first
# Convert Character Vector between Encodings
PercentageUsingTheNet$ECONOMY=iconv(PercentageUsingTheNet$ECONOMY)

matches <- partialMatch(PercentageUsingTheNet$ECONOMY,ccode$Country.Name)

a=PercentageUsingTheNet
b=ccode

# Merge the original data set with the ISO country code country name keys
aa=merge(a,matches,by.x='ECONOMY',by.y='raw.x',all.x=T)

# Merge in the ISO country codes
aa=merge(aa,b,by.x='raw.y',by.y='Country.Name',all.x=T)
aa=subset(aa,select=c('ECONOMY','RANK','PERCENTAGE.OF.INDIVIDUALS.USING.THE.INTERNET.2011','ISO.3166.1.alpha.2.code'))
