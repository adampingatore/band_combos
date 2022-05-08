## Juncos Band Combo Automation
## for UCLA Junco Lab
## Adam Pingatore
## Feb 20, 2022

require(stats)
require(gtools)
require(qdap)

rm(list=ls())
setwd("C:/Users/aping/Desktop/220313_chick_band_materials")

## Load in data
banding <- read.csv(file="220313_banding_data_protected.csv")

###################
## HOUSEKEEPING ##
###################

## explore data
head(banding)
colnames(banding)
head(banding$color.band..XoYleft..right.)

## rename combo column
names(banding)[7] <- "combo"


## band names
bands <- c("DG","LG","Y","R","DB","LB","O","W","B","L",
           "BW","HPDB","LGL","RY","LW","DBR","RW")

## band digit codes --> Wilmer or others: see spreadsheet "banding_housekeeping"
bands_num <- c("1","2","3","4","5","6","7","8","9","10",
               "9&8","11&5","2&10","4&3","10&8",
               "5&4","4&8")



## Determining which bands are still in use
colnames(banding)
unique(banding$Year)

recent <- banding[which(banding$Year == "2020"|
                          banding$Year == "2021"|
                          banding$Year == "2022"),]

## cleaning up
recentA <- gsub(" ", "", recent$combo, fixed = TRUE)
recentB <- strsplit(recentA, split="[,,/]")

## split up bands, add to vector
bandvec <- vector()
for(i in 1:length(recentB)){
  banded <- recentB[[i]]
  bandvec <- append(bandvec, banded)
}
recent_bands <- unique(bandvec)

## see if I missed any bands/included bands no longer in use
which(recent_bands %in% bands)
recent_bands[-which(recent_bands %in% bands)] ## in recent bands, not in my list --> OK (ltd use)
bands[-which(bands %in% recent_bands)] ## in my list, not in recent bands --> OK



################
## RIGHT LEG ##
################

## all permutations of bands
raw_right <- permutations(n=length(bands_num),
                          r=2,
                          v=bands_num)

## add a / between the permutations
rightA <- apply(raw_right, 1, function(x)paste(x, collapse='/'))


## find which permutations contain the same color
samecol <- vector()
for(i in 1:length(rightA)){
  div <- strsplit(rightA[i], split="[&,/]")[[1]]
  if(length(div) == length(unique(div))){
    next
  } else {
    samecol <- append(samecol, i)
  }
}

## delete combos with the same color
rightB <- rightA[-samecol]
head(rightB)


## Convert numbers back to names
rightF <- mgsub(bands_num,bands,rightB)
head(rightF)


###############
## LEFT LEG ##
##############

## already defined these higher in the code, just doing it here again for clarity
bands <- c("DG","LG","Y","R","DB","LB","O","W","B","L",
           "BW","HPDB","LGL","RY","LW","DBR","RW")


## each band over aluminum
leftA <- paste(bands, '/Al', sep="")
leftA



###################
## COMBINE LEGS ##
##################

## all permutations of two vectors, in a grid format
bothA <- expand.grid(leftA, rightF)

## combine the columns, using a comma as separation
bothB <- paste(bothA$Var1, bothA$Var2, sep=",")
head(bothB)


######################
## CHECK NEW        ##
## AGAINST EXISTING ##
######################

head(banding$combo)

## get rid of leading/trailing/internal spaces in existing data
usedA <- trimws(x=banding$combo, which=c("both"))
usedB <- gsub(" ", "", usedA, fixed = TRUE)
head(usedB)


## exploring matches between new and existing combos
any(bothB %in% usedB) ## any new combos in existing data?
which(bothB %in% usedB)
length(which(bothB %in% usedB)) ## how many new combos in existing data?

## which new combos have already been used
overlap <- which(toupper(bothB) %in% toupper(usedB))


## remove new combos which have been used
bothC <- bothB[-overlap]

## making sure it worked (FALSE if it worked)
any(bothC %in% usedB)

## over 3000 new & unique combos generated
length(bothC)


## Improve formatting for new combos
head(bothC)
bothD <- sub("([,])", "\\1 \\2", bothC)
head(bothD)



## generating 50 random combos for QC (uncaption if want to test)

## --> will output a spreadsheet of new band combos
## --> the code works, but can copy these combos and Ctrl+F in the 
##     current datasheet to check for peace of mund

#rand <- floor(runif(50, min=0, max=length(bothD)))
#write.csv(x=bothD[rand], file="testing.csv")


## rename columns
final_combos<-as.data.frame(bothD)
names(final_combos)[1] <- "combo"

## get current date, for naming spreadsheet
date <- format(Sys.Date(), format="%y%m%d")

## output to csv
write.csv(x=final_combos, file=paste(date, "_new_combos", ".csv", sep=""))
