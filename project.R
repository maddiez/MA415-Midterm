library(foreign)
library(dplyr)
library(data.table)
library(plyr)
library(ggplot2)
v <- read.dbf("viol.DBF")
aFac <- read.dbf("lookups/acc.dbf")
admpay <- read.dbf("admpay.DBF")
hzs <- read.dbf("lookups/hzs.dbf")
occ <- read.dbf("lookups/occ.dbf")
#osha <- read.dbf("debt.dbf")
history <- read.dbf("history.dbf")
nep <- read.dbf("lookups/neptable.dbf")
relact <- read.dbf("relact.dbf")
std <- read.dbf("lookups/std.dbf")
fda <- read.dbf("lookups/fda.dbf")

#mode function
#getmode <- function(v) {
#  uniqv <- unique(v)
# uniqv[which.max(tabulate(match(v, uniqv)))]}

a <- read.dbf("accid.DBF")
#Remove state (because all MA), Name (not relevent for data analysis), and RELINSP (doesn't help identify, already have activity no)
a <- a %>% select(-c(SITESTATE, NAME, RELINSP) )  
#What kind of occupations?
a$OCC_CODE <- as.numeric(as.character(a$OCC_CODE))

#create subset of only accidents filed w occupation code
a_occupations <- subset(a, OCC_CODE > 000 & OCC_CODE <999)
a_occupations <- a_occupations %>% select(-c(SEX, SOURCE, AGE, ENVIRON, HUMAN, HAZSUB, TASK, EVENT, BODYPART, NATURE) )  
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

Deg <- c("Non - Hospital Injuries", "Hospital Injuries", "Fatalities")
DEGREE <- c("3", "2", "1")
DegDatabase <- data.frame(Deg, DEGREE)
a_occupations <- left_join(a_occupations, DegDatabase, by="DEGREE")


a_occupations$Deg <- factor(a_occupations$Deg, levels = c("Non - Hospital Injuries", "Hospital Injuries", "Fatalities"))

ggplot(data = a_occupations, aes(x = Occupation, fill = factor(Deg))) + 
  geom_bar() + coord_flip() + scale_fill_brewer(palette = 12) + labs(fill = "Degree of Injury")

p <- ggplot(NULL, aes(Occupation)) +   
  geom_bar(aes(fill = "Non - Hospital Injuries"), data = a_nohosp, alpha = 0.5) + 
  geom_bar(aes(fill = "Hospital Injuries"), data = a_hospinj, alpha = 0.5) + 
  geom_bar(aes(fill = "Fatalities"), data = a_fatality, alpha = 0.5)
p + theme(legend.position="top")+theme(axis.text.x = element_text(size=10,angle = 60, hjust = 0.5)) + coord_flip() + scale_fill_brewer(palette = 1) 
+ scale_x_discrete(labels = c("Architecture and Engineering" = "Arch/Eng", "Building and Grounds Cleaning and Maintenance" = "Building Maint.", "Construction and Extraction" = "Construc./Extrac.", "Farming, Fishing, and Forestry" = "Farm/Fish/Fores.", "Food Preparation and Serving Related" = "Food Prep/Serv.", "Installation, Maintenance, and Repair" = "Install./Repair", "Management" = "Mgmt.", "Office and Administrative Support"="Admin. Supp.", "Production" = "Prod.", "Sales" = "Sales", "Transportation and Material Moving" = "Transpo."))







a_fatality <- subset(a_occupations, DEGREE == 1)
a_hospinj <- subset(a_occupations, DEGREE == 2)
a_nohosp <- subset(a_occupations, DEGREE == 3)
p <- ggplot(NULL, aes(Occupation)) +   
  geom_bar(aes(fill = "Non - Hospital Injuries"), data = a_nohosp, alpha = 0.5) + 
  geom_bar(aes(fill = "Hospital Injuries"), data = a_hospinj, alpha = 0.5) + 
  geom_bar(aes(fill = "Fatalities"), data = a_fatality, alpha = 0.5)
p + theme(legend.position="top")+theme(axis.text.x = element_text(size=10,angle = 60, hjust = 0.5)) + coord_flip() + scale_fill_brewer(palette = 1) 
  + scale_x_discrete(labels = c("Architecture and Engineering" = "Arch/Eng", "Building and Grounds Cleaning and Maintenance" = "Building Maint.", "Construction and Extraction" = "Construc./Extrac.", "Farming, Fishing, and Forestry" = "Farm/Fish/Fores.", "Food Preparation and Serving Related" = "Food Prep/Serv.", "Installation, Maintenance, and Repair" = "Install./Repair", "Management" = "Mgmt.", "Office and Administrative Support"="Admin. Supp.", "Production" = "Prod.", "Sales" = "Sales", "Transportation and Material Moving" = "Transpo."))

d <- rbind(a_fatality, a_hospinj, a_nohosp)
p <- ggplot(d, aes(Occupation, fill=name)) + geom_bar(position = "dodge")
p + theme(legend.position="top")+theme(axis.text.x = element_text(size=10,angle = 60, hjust = 0.5))+ scale_x_discrete(labels = c("Architecture and Engineering" = "Arch/Eng", "Building and Grounds Cleaning and Maintenance" = "Building Maint.", "Construction and Extraction" = "Construc./Extrac.", "Farming, Fishing, and Forestry" = "Farm/Fish/Fores.", "Food Preparation and Serving Related" = "Food Prep/Serv.", "Installation, Maintenance, and Repair" = "Install./Repair", "Management" = "Mgmt.", "Office and Administrative Support"="Admin. Supp.", "Production" = "Prod.", "Sales" = "Sales", "Transportation and Material Moving" = "Transpo."))
















#age?
a$AGE <- as.numeric(as.character(a$AGE))
a_age <- subset(a, AGE > 0)

### PLOTTING
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

#look at degree 2 = hospitalized injury:
a_hospinj <- subset(a, DEGREE == 2)
#remove all entries with 43 = "other"
a_hospinj <- a_hospinj[grep("43", aFac$CATEGORY, invert = TRUE),]
#for mode
getmode(a_hospinj$SOURCE)
plot(a_hospinj$SOURCE)

#look at degree 3 = non hospitalized injury:
a_nohosp <- subset(a, DEGREE == 3)
#remove all entries with 43 = "other"
a_nohosp <- a_nohosp[grep("43", aFac$CATEGORY, invert = TRUE),]
#for mode
getmode(a_nohosp$SOURCE)
plot(a_nohosp$SOURCE)

a_fatality$SOURCE <- as.numeric(as.character(a_fatality$SOURCE))
a_hospinj$SOURCE <- as.numeric(as.character(a_hospinj$SOURCE))
a_nohosp$SOURCE <- as.numeric(as.character(a_nohosp$SOURCE))


a_fatality$name <- "Fatalities"
a_hospinj$name <- "Hospital Injuries"
a_nohosp$name <- "Non-Hospital Injuries"


#bar plot
p <- ggplot(NULL, aes(SOURCE)) + 
  geom_histogram(binwidth=1,aes(fill = "Fatalities"), data = a_fatality, alpha = 0.5) +
  geom_histogram(binwidth=1,aes(fill = "Hospital Injuries"), data = a_hospinj, alpha = 0.5) +
  geom_histogram(binwidth=1,aes(fill = "Non - Hospital Injuries"), data = a_nohosp, alpha = 0.5)
p + theme(legend.position="top")


#side by side plots
d <- rbind(a_fatality, a_hospinj, a_nohosp)
p <- ggplot(d, aes(SOURCE, fill=name)) + geom_bar(position = "dodge")

#repeat w human factor
