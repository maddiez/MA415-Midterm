library(foreign)
library(dplyr)
library(data.table)
library(plyr)
v <- read.dbf("viol.DBF")
aFac <- read.dbf("lookups/acc.dbf")
admpay <- read.dbf("admpay.DBF")
hzs <- read.dbf("lookups/hzs.dbf")
occ <- read.dbf("lookups/occ.dbf")
osha <- read.dbf("debt.dbf")
history <- read.dbf("prog.dbf")
nep <- read.dbf("lookups/neptable.dbf")
relact <- read.dbf("relact.dbf")
std <- read.dbf("lookups/std.dbf")
fda <- read.dbf("lookups/fda.dbf")

#mode function
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]}


a <- read.dbf("accid.DBF")
#Remove state (because all MA), Name (not relevent for data analysis), and RELINSP (doesn't help identify, already have activity no)
a <- a %>% select(-c(SITESTATE, NAME, RELINSP) )  
#What kind of occupations?
a$OCC_CODE <- as.numeric(as.character(a$OCC_CODE))

#create subset of only accidents filed w occupation code
a_occupations <- subset(a, OCC_CODE > 000)
a_occupations$OCCUPATION <- 0
a_occupations <- as.data.table(a_occupations)
occ <- as.data.table(occ)
setkey(a_occupations, "OCC_CODE")
setkey(occ, "code")
logStats[occ]

#age?
a$AGE <- as.numeric(as.character(a$AGE))
a_age <- subset(a, AGE > 0)

#compare degree 1 = fatality:
a_fatality <- subset(a, DEGREE == 1)
#remove all entries with 43 = "other"
a_fatality <- a_fatality[grep("43", aFac$CATEGORY, invert = TRUE),]
#prep accident factors for only source optons
aFac_Source <- aFac[grep("SOUR", aFac$CATEGORY, invert = FALSE),]
#for mode
getmode(a_fatality$SOURCE)
plot(a_fatality$SOURCE)
########find way to put in corresponding source
