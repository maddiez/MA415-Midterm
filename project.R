v <- read.dbf("viol.DBF")
admpay <- read.dbf("admpay.DBF")
hzs <- read.dbf("lookups/hzs.dbf")
history <- read.dbf("history.dbf")
nep <- read.dbf("lookups/neptable.dbf")
relact <- read.dbf("relact.dbf")
std <- read.dbf("lookups/std.dbf")
fda <- read.dbf("lookups/fda.dbf")






library(foreign)
library(dplyr)
library(data.table)
library(plyr)
library(ggplot2)

occ <- read.dbf("lookups/occ.dbf")
a <- read.dbf("accid.DBF")
aFac <- read.dbf("lookups/acc.dbf")

#Remove state (because all MA), Name (not relevent for data analysis), and RELINSP (doesn't help identify, already have activity no)
a <- a %>% select(-c(SITESTATE, NAME, RELINSP, AGE, SEX) )  

#Create Degree corresponding codes
Deg <- c("Non - Hospital Injuries", "Hospital Injuries", "Fatalities")
DEGREE <- c("3", "2", "1")
DegDatabase <- data.frame(Deg, DEGREE)

#replace degree codes with actual injuries
a <- left_join(a, DegDatabase, by="DEGREE")

#repeat replacements for other cols with codes into lookups
aFac_Nat <- aFac[grep("NATU", aFac$CATEGORY, invert = FALSE),]
aFac_Nat <- setNames(aFac_Nat, c("CATEGORY","NATURE","Nature"))
a <- left_join(a, aFac_Nat, by="NATURE")

aFac_Body <- aFac[grep("BODY", aFac$CATEGORY, invert = FALSE),]
aFac_Body <- setNames(aFac_Body, c("CATEGORY","BODYPART","Body Part"))
a <- left_join(a, aFac_Body, by="BODYPART")

aFac_Source <- aFac[grep("SOUR", aFac$CATEGORY, invert = FALSE),]
aFac_Source <- setNames(aFac_Source, c("CATEGORY","SOURCE","Source"))
a <- left_join(a, aFac_Source, by="SOURCE")

aFac_Event <- aFac[grep("EVENT", aFac$CATEGORY, invert = FALSE),]
aFac_Event <- setNames(aFac_Event, c("CATEGORY","EVENT","Event"))
a <- left_join(a, aFac_Event, by="EVENT")

aFac_Env <- aFac[grep("ENVIR", aFac$CATEGORY, invert = FALSE),]
aFac_Env <- setNames(aFac_Env, c("CATEGORY","ENVIRON","Enviro"))
a <- left_join(a, aFac_Env, by="ENVIRON")

aFac_Human <- aFac[grep("HUMAN", aFac$CATEGORY, invert = FALSE),]
aFac_Human <- setNames(aFac_Human, c("CATEGORY","HUMAN","Human"))
a <- left_join(a, aFac_Human, by="HUMAN")

aFac_Task <- aFac[grep("TASK", aFac$CATEGORY, invert = FALSE),]
aFac_Task <- setNames(aFac_Task, c("CATEGORY","TASK","Task"))
a <- left_join(a, aFac_Task, by="TASK")

#Clean up extra columns 
a <- a %>% select(-c(BODYPART, CATEGORY.x, CATEGORY.x.x, CATEGORY.x.x.x, CATEGORY.y, CATEGORY.y.y, CATEGORY.y.y.y, NATURE, DEGREE, SOURCE, EVENT, ENVIRON, HUMAN, TASK, HAZSUB) )  

a$OCC_CODE <- as.numeric(as.character(a$OCC_CODE))

#create subset of only accidents filed w occupation code
a_occupations <- subset(a, OCC_CODE > 000 & OCC_CODE <999)
occ$CODE <- as.numeric(as.character(occ$CODE))

Occupation <- c("Transportation and Material Moving")
OCC_CODE <- c(occ[461:502,1])
Occ53 <- data.frame(OCC_CODE, Occupation)

Occupation <- c("Production")
OCC_CODE <- c(occ[359:460,1])
Occ51 <- data.frame(OCC_CODE, Occupation)

Occupation <- c("Installation, Maintenance, and Repair")
OCC_CODE <- c(occ[297:323,1], occ[133:154,1])
Occ49 <- data.frame(OCC_CODE, Occupation)

Occupation <- c("Construction and Extraction")
OCC_CODE <- c(occ[324:358,1])
Occ47 <- data.frame(OCC_CODE, Occupation)

Occupation <- c("Farming, Fishing, and Forestry")
OCC_CODE <- c(occ[276:296,1])
Occ45 <- data.frame(OCC_CODE, Occupation)

Occupation <- c("Personal Care and Service")
OCC_CODE <- c(occ[267:276,1])
Occ39 <- data.frame(OCC_CODE, Occupation)

Occupation <- c("Building and Grounds Cleaning and Maintenance")
OCC_CODE <- c(occ[262:266,1])
Occ37 <- data.frame(OCC_CODE, Occupation)

Occupation <- c("Healthcare Support")
OCC_CODE <- c(occ[259:262,1])
Occ31 <- data.frame(OCC_CODE, Occupation)

Occupation <- c("Food Preparation and Serving Related")
OCC_CODE <- c(occ[250:259,1])
Occ35 <- data.frame(OCC_CODE, Occupation)

Occupation <- c("Office and Administrative Support")
OCC_CODE <- c(occ[239:249,1])
Occ43 <- data.frame(OCC_CODE, Occupation)

Occupation <- c("Office and Administrative Support")
OCC_CODE <- c(occ[182:233,1])
Occ44 <- data.frame(OCC_CODE, Occupation)

Occupation <- c("Personal Care and Service")
OCC_CODE <- c(occ[234:239,1])
Occ39 <- data.frame(OCC_CODE, Occupation)

Occupation <- c("Sales")
OCC_CODE <- c(occ[155:182,1])
Occ41 <- data.frame(OCC_CODE, Occupation)

Occupation <- c("Arts, Design, Entertainment, Sports, and Media")
OCC_CODE <- c(occ[120:132, 1])
Occ27 <- data.frame(OCC_CODE, Occupation)

Occupation <- c("Community and Social Service")
OCC_CODE <- c(occ[71:119,1])
Occ21 <- data.frame(OCC_CODE, Occupation)

Occupation <- c("Healthcare Practitioners and Technical")
OCC_CODE <- c(occ[56:70,1])
Occ29 <- data.frame(OCC_CODE, Occupation)

Occupation <- c("Life, Physical, and Social Science")
OCC_CODE <- c(occ[41:55,1])
Occ19 <- data.frame(OCC_CODE, Occupation)

Occupation <- c("Architecture and Engineering")
OCC_CODE <- c(occ[27:40,1])
Occ17 <- data.frame(OCC_CODE, Occupation)

Occupation <- c("Business and Financial Operations")
OCC_CODE <- c(occ[15:26,1])
Occ13 <- data.frame(OCC_CODE, Occupation)

Occupation <- c("Management")
OCC_CODE <- c(occ[1:14,1])
Occ11 <- data.frame(OCC_CODE, Occupation)

OccDatabase <- rbind(Occ19, Occ37, Occ11, Occ13, Occ44, Occ17, Occ29, Occ21, Occ27, Occ41, Occ39, Occ35, Occ43, Occ53, Occ51, Occ49, Occ47, Occ45, Occ31)
a_occupations <- left_join(a_occupations, OccDatabase, by="OCC_CODE")

#Choose only Occupation and degree
occ_with_deg <- a_occupations %>% select(c(Occupation, Deg) ) 

#Use plyr count function
occ_with_deg  <- count_(occ_with_deg , c('Occupation', 'Deg'))

#Reorder with descending counts
occ_with_deg <-  occ_with_deg [with(occ_with_deg , order(-n)), ]
occ_with_deg <- setnames(occ_with_deg, "Deg", "Degree")
occ_with_deg <- setnames(occ_with_deg, "n", "Occurances")


library(knitr)
kable(occ_with_deg, caption = "The number of occurances for degrees of injuries in each Occupation")
a_occupations$Deg <- factor(a_occupations$Deg, levels = c("Non - Hospital Injuries", "Hospital Injuries", "Fatalities"))

p = ggplot(data = a_occupations, aes(x = Occupation, fill = factor(Deg))) 
p + geom_bar() + coord_flip() + scale_fill_brewer(palette = 2) + labs(fill = "Degree of Injury") + theme(legend.justification=c(1,0), legend.position=c(1,0)) +ggtitle("Occupations Accident Occurances") 


osha <- read.dbf("osha.DBF")

sic <- read.dbf("lookups/sic.dbf")
naics <- read.dbf("lookups/naics.dbf")

cleanosha <- osha %>% select(c(SIC, TOTALVIOLS, INSPTYPE))
cleanosha <- left_join(cleanosha, sic, by="SIC")
cleanosha <- cleanosha %>% select(-c(SIC))

cleanosha$INSPTYPE <- as.character(cleanosha$INSPTYPE)

cleanosha <- subset(cleanosha, TOTALVIOLS > 0)
fatcat <- filter(cleanosha, INSPTYPE == "A")
complaint <- filter(cleanosha, INSPTYPE == "B")

fatcat <- ddply(fatcat,"INDUSTRY",numcolwise(sum))
complaint <- ddply(complaint,"INDUSTRY",numcolwise(sum))
fatcat <-  fatcat [with(fatcat , order(-TOTALVIOLS)), ]
complaint <-  complaint [with(complaint , order(-TOTALVIOLS)), ]
fatcat <- setNames(fatcat, c("Industry","Total Violations"))
complaint <- setNames(complaint, c("Industry","Total Violations"))


f1 <- head(fatcat,100)
c1 <- head(complaint,100)
x <- left_join(f1,c1, by="Industry")
