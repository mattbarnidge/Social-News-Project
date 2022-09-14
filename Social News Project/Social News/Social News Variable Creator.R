#Libraries, Working Directory, and Data
library(dplyr)
library(tidyr)
setwd("~/Desktop")
d <- read.csv(file = "Social News Text Clean.csv", header = TRUE, sep = ",")

#Bookkeeping Variables
#gc, ResponseId, rid, StartDate, EndDate, Duration..in.seconds., state, region

#Section A: Demographics and Tech Use

#Age
d$age = as.numeric(d$A1)

#Gender
d$gender[d$A2 != "Male"] <- 1
d$gender[d$A2 == "Male"] <- 0

#Race
d$poc[d$A3 != "White"] <- 1
d$poc[d$A3 == "White"] <- 0
d$poc[d$A3_6_TEXT == "caucasian"] <- 0
d$poc[d$A3_6_TEXT == "Spain"] <- 0
d$poc[d$A3_6_TEXT == "Cherokee White and Fuck you"] <- 0
d$poc[d$A3_7_TEXT == "American "] <- 0
d$poc[d$A3_7_TEXT == "Caucasian"] <- 0
d$poc[d$A3_7_TEXT == "human"] <- 0
d$poc[d$A3_7_TEXT == "European-American"] <- 0

#Education
d$edu[d$A4 == "Some high school"] <- 1
d$edu[d$A4 == "High school diploma"] <- 2
d$edu[d$A4 == "Some college"] <- 3
d$edu[d$A4 == "Associate’s degree or trade school diploma"] <- 4
d$edu[d$A4 == "College degree"] <- 5
d$edu[d$A4 == "Some post-graduate education"] <- 6
d$edu[d$A4 == "Post-graduate degree"] <- 7

#Income
d$inc[d$A5 == "Less than $15,000"] <- 1
d$inc[d$A5 == "$15,001 to $30,000"] <- 2
d$inc[d$A5 == "$30,001 to $45,000"] <- 3
d$inc[d$A5 == "$45,001 to $60,000"] <- 4
d$inc[d$A5 == "$60,001 to $75,000"] <- 5
d$inc[d$A5 == "$75,001 to $100,000"] <- 6
d$inc[d$A5 == "$100,001 to $150,000"] <- 7
d$inc[d$A5 == "More than $150,000"] <- 8

#Facebook Account
d$fb.acct[d$A6 == "Yes"] <- 1
d$fb.acct[d$A6 == "No"] <- 0

#Frequency of Facebook Use
d$fb.freq[d$A7 == ""] <- 0
d$fb.freq[d$A7 == "Less than 10 minutes per day"] <- 1
d$fb.freq[d$A7 == "10-30 minutes per day"] <- 2
d$fb.freq[d$A7 == "31-60 minutes per day"] <- 3
d$fb.freq[d$A7 == "1-2 hours per day"] <- 4
d$fb.freq[d$A7 == "2-3 hours per day"] <- 5
d$fb.freq[d$A7 == "More than 3 hours per day"] <- 6

#Section B: Baseline Variables

#Social Connectedness
d <- d %>% mutate(across(
  .cols = all_of(c(22,23,25,26,29,30,31,33,35,37,38,39,40)), 
  ~(recode(., "Strongly disagree" = 1, "Somewhat disagree" = 2, "Neither agree nor disagree" = 3, "Somewhat agree" = 4, "Strongly agree" = 5, .default = NA_real_)), 
  .names = "{col}"))
d <- d %>% mutate(across(
  .cols = all_of(c(24,27,28,32,34,36,41)), 
  ~(recode(., "Strongly disagree" = 5, "Somewhat disagree" = 4, "Neither agree nor disagree" = 3, "Somewhat agree" = 2, "Strongly agree" = 1, .default = NA_real_)), 
  .names = "{col}"))
ltm::cronbach.alpha(d[,22:41], na.rm=TRUE) #alpha = .739
d$soccon = rowMeans(d[,22:41], na.rm=TRUE)

#Community Belongingness
d <- d %>% mutate(across(
  .cols = all_of(c(42:47)), 
  ~(recode(., "Not at all" = 1, "A little" = 2, "Some" = 3, "A lot" = 4, "A great deal" = 5, .default = NA_real_)), 
  .names = "{col}"))
ltm::cronbach.alpha(d[,42:47], na.rm=TRUE) #alpha = .901
d$combel = rowMeans(d[,42:47], na.rm=TRUE)

#Social Trust
d$B10_1 = as.numeric(d$B10_1)
d <- d %>% mutate(B11 = recode(B11, "None at all" = 1, "A little" = 2, "A moderate amount" = 3, "A lot" = 4, "A great deal" = 5, .default = NA_real_))
d <- d %>% mutate(B12 = recode(B12, "Never" = 1, "Sometimes" = 2, "About half the time" = 3, "Most of the time" = 4, "Always" = 5, .default = NA_real_))
ltm::cronbach.alpha(scale(d[,48:50]), na.rm=TRUE) #.846
d$soctrust = rowMeans(scale(d[,48:50]), na.rm=TRUE)

#Trust in Entities
d <- d %>% mutate(across(
  .cols = all_of(c(52:68)), 
  ~(recode(., "Distrust very much" = 1, "Somewhat distrust" = 2, "Neither trust nor distrust" = 3, "Somewhat trust" = 4, "Somewhat trust" = 5, .default = NA_real_)), 
  .names = "{col}"))

#Community
ltm::cronbach.alpha(cbind(d[,52], d[61:63], d[,67]), na.rm=TRUE) #alpha = .783
d$commtrust = rowMeans(cbind(d[,52], d[61:63], d[,67]), na.rm=TRUE)

#News
ltm::cronbach.alpha(d[,55:59], na.rm=TRUE) #alpha = .813
d$newstrust = rowMeans(d[,55:59], na.rm=TRUE)

#Government
ltm::cronbach.alpha(d[,53:54], na.rm=TRUE) #alpha = .834
d$govtrust = rowMeans(d[,53:54], na.rm=TRUE)

#Business
ltm::cronbach.alpha(d[,64:66], na.rm=TRUE) #alpha = .748
d$bustrust = rowMeans(d[,64:66], na.rm=TRUE)

#Science
ltm::cronbach.alpha(cbind(d[,60], d[,68]), na.rm=TRUE) #alpha = .686
d$scitrust = rowMeans(cbind(d[,60], d[,68]), na.rm=TRUE)

#Trust in News
d <- d %>% mutate(across(
  .cols = all_of(c(69:70)), 
  ~(recode(., "Strongly disagree" = 5, "Somewhat disagree" = 4, "Neither agree nor disagree" = 3, "Somewhat agree" = 2, "Strongly agree" = 1, .default = NA_real_)), 
  .names = "{col}"))
d <- d %>% mutate(B14_3 = recode(B14_3, "Strongly disagree" = 1, "Somewhat disagree" = 2, "Neither agree nor disagree" = 3, "Somewhat agree" = 4, "Strongly agree" = 5, .default = NA_real_))
ltm::cronbach.alpha(d[,69:71], na.rm=TRUE) #alpha = .591
ltm::cronbach.alpha(d[,69:70], na.rm=TRUE) #alpha = .746
d$newstrust2 = rowMeans(d[,69:70], na.rm=TRUE)

#Correlations
with(d, cor(cbind(soccon, combel, soctrust, 
                  commtrust, newstrust, govtrust, bustrust, scitrust, 
                  newstrust2), 
            use="complete.obs", method="pearson"))

#Section C: Interpersonal Evaluations

#Filter variable: 179 do not have FB acct (see $fbacct); 238 skipped this Q
d$filt <- NA
d$filt[d$CFilt=="Yes"] <- 1
d$filt[d$CFilt=="No"] <- 0
d$filt <- ifelse(d$fb.acct == 1 & is.na(d$filt)==TRUE, 0, d$filt)
#table(d$fb.acct, is.na(d$filt)) #cross-check with $fb.acct

#Name: Need to clean these
d$name1 <- 0
d$name1 = ifelse(d$C15!="", 1, d$name1)
d$name1 = ifelse(d$filt==0, 0, d$name1)

d$name2 <- 0
d$name2 = ifelse(d$C22!="", 1, d$name2)
d$name2 = ifelse(d$filt==0, 0, d$name2)

d$name3 <- 0
d$name3 = ifelse(d$C29!="", 1, d$name3)
d$name3 = ifelse(d$filt==0, 0, d$name3)

d$names = with(d, rowSums(cbind(name1, name2, name3), na.rm=TRUE))

#Category: Family
d$ng1.fam[d$C16=="Family member"] <- 1
d$ng1.fam[d$C16!="Family member"] <- 0
d$ng1.fam[d$C16==""] <- NA
d$ng1.fam[d$C16_6_TEXT=="Cousin" | 
            d$C16_6_TEXT=="Cousin " | 
            d$C16_6_TEXT=="Family member of a family member" | 
            d$C16_6_TEXT=="Fiance" | 
            d$C16_6_TEXT=="Husband " | 
            d$C16_6_TEXT=="sibling" | 
            d$C16_6_TEXT=="Sister "] <- 1
d$ng1.fam[d$C16_6_TEXT=="All people on Facebook" | 
            d$C16_6_TEXT=="Former US president " |
            d$C16_6_TEXT=="no one" | 
            d$C16_6_TEXT=="No one" | 
            d$C16_6_TEXT=="No person" | 
            d$C16_6_TEXT=="None" | 
            d$C16_6_TEXT=="nope" | 
            d$C16_6_TEXT=="Not giving any personal details for safety"] <- NA

d$ng2.fam[d$C23=="Family member"] <- 1
d$ng2.fam[d$C23!="Family member"] <- 0
d$ng2.fam[d$C23==""] <- NA
d$ng2.fam[d$C23_6_TEXT=="Cousin " | 
            d$C23_6_TEXT=="Spouse"] <- 1
d$ng2.fam[d$C23_6_TEXT=="Do not see political posts" | 
            d$C23_6_TEXT=="FB " | 
            d$C23_6_TEXT=="leader" | 
            d$C23_6_TEXT=="no one" | 
            d$C23_6_TEXT=="No one" | 
            d$C23_6_TEXT=="None" | 
            d$C23_6_TEXT=="Not applicable" | 
            d$C23_6_TEXT=="Not giving any personal details for safety" | 
            d$C23_6_TEXT=="President " | 
            d$C23_6_TEXT=="professional org" | 
            d$C23_6_TEXT=="Public Figure"] <- NA


d$ng3.fam[d$C30=="Family member"] <- 1
d$ng3.fam[d$C30!="Family member"] <- 0
d$ng3.fam[d$C30==""] <- NA
d$ng3.fam[d$C30_6_TEXT=="SPOUSE" | 
            d$C30_6_TEXT=="Uncle"] <- 1
d$ng3.fam[d$C30_6_TEXT=="Do not know of anyone" | 
            d$C30_6_TEXT=="Don't have a 3rd" | 
            d$C30_6_TEXT=="N/A" | 
            d$C30_6_TEXT=="no one" | 
            d$C30_6_TEXT=="No one" | 
            d$C30_6_TEXT=="nobody" | 
            d$C30_6_TEXT=="Nobody" | 
            d$C30_6_TEXT=="none" | 
            d$C30_6_TEXT=="None" | 
            d$C30_6_TEXT=="None " | 
            d$C30_6_TEXT=="not applicable" | 
            d$C30_6_TEXT=="Not giving any personal details for safety" | 
            d$C30_6_TEXT=="Nothing" | 
            d$C30_6_TEXT=="President" | 
            d$C30_6_TEXT=="Other people" | 
            d$C30_6_TEXT=="FD"] <- NA

d$ng.fam = with(d, rowSums(cbind(ng1.fam, ng2.fam, ng3.fam), na.rm=TRUE))
d$ng.fam[d$filt!=1] <- NA

#Category: Friend
d$ng1.fri[d$C16=="Friend"] <- 1
d$ng1.fri[d$C16!="Friend"] <- 0
d$ng1.fri[d$C16==""] <- NA
d$ng1.fri[d$C16_6_TEXT=="Boyfriend"] <- 1
d$ng1.fri[d$C16_6_TEXT=="All people on Facebook" | 
            d$C16_6_TEXT=="Former US president " |
            d$C16_6_TEXT=="no one" | 
            d$C16_6_TEXT=="No one" | 
            d$C16_6_TEXT=="No person" | 
            d$C16_6_TEXT=="None" | 
            d$C16_6_TEXT=="nope" | 
            d$C16_6_TEXT=="Not giving any personal details for safety"] <- NA

d$ng2.fri[d$C23=="Friend"] <- 1
d$ng2.fri[d$C23!="Friend"] <- 0
d$ng2.fri[d$C23==""] <- NA
d$ng2.fri[d$C23_6_TEXT=="Significant other"] <- 1
d$ng2.fri[d$C23_6_TEXT=="Do not see political posts" | 
            d$C23_6_TEXT=="FB " | 
            d$C23_6_TEXT=="leader" | 
            d$C23_6_TEXT=="no one" | 
            d$C23_6_TEXT=="No one" | 
            d$C23_6_TEXT=="None" | 
            d$C23_6_TEXT=="Not applicable" | 
            d$C23_6_TEXT=="Not giving any personal details for safety" | 
            d$C23_6_TEXT=="President " | 
            d$C23_6_TEXT=="professional org" | 
            d$C23_6_TEXT=="Public Figure"] <- NA

d$ng3.fri[d$C30=="Friend"] <- 1
d$ng3.fri[d$C30!="Friend"] <- 0
d$ng3.fri[d$C30==""] <- NA
d$ng3.fri[d$C30_6_TEXT=="Friend and love interest who lives in another country" | 
            d$C30_6_TEXT=="Significant other"] <- 1
d$ng3.fri[d$C30_6_TEXT=="Do not know of anyone" | 
            d$C30_6_TEXT=="Don't have a 3rd" | 
            d$C30_6_TEXT=="N/A" | 
            d$C30_6_TEXT=="no one" | 
            d$C30_6_TEXT=="No one" | 
            d$C30_6_TEXT=="nobody" | 
            d$C30_6_TEXT=="Nobody" | 
            d$C30_6_TEXT=="none" | 
            d$C30_6_TEXT=="None" | 
            d$C30_6_TEXT=="None " | 
            d$C30_6_TEXT=="not applicable" | 
            d$C30_6_TEXT=="Not giving any personal details for safety" | 
            d$C30_6_TEXT=="Nothing" | 
            d$C30_6_TEXT=="President" | 
            d$C30_6_TEXT=="Other people" | 
            d$C30_6_TEXT=="FD"] <- NA

d$ng.fri = with(d, rowSums(cbind(ng1.fri, ng2.fri, ng3.fri), na.rm=TRUE))
d$ng.fri[d$filt!=1] <- NA

#Category: Coworker or Classmate
d$ng1.cow[d$C16=="Coworker or classmate"] <- 1
d$ng1.cow[d$C16!="Coworker or classmate"] <- 0
d$ng1.cow[d$C16==""] <- NA
d$ng1.cow[d$C16_6_TEXT=="All people on Facebook" | 
            d$C16_6_TEXT=="Former US president " |
            d$C16_6_TEXT=="no one" | 
            d$C16_6_TEXT=="No one" | 
            d$C16_6_TEXT=="No person" | 
            d$C16_6_TEXT=="None" | 
            d$C16_6_TEXT=="nope" | 
            d$C16_6_TEXT=="Not giving any personal details for safety"] <- NA



d$ng2.cow[d$C23=="Coworker or classmate"] <- 1
d$ng2.cow[d$C23!="Coworker or classmate"] <- 0
d$ng2.cow[d$C23==""] <- NA
d$ng2.cow[d$C23_6_TEXT=="Former supervisor"] <- 1
d$ng2.cow[d$C23_6_TEXT=="Do not see political posts" | 
            d$C23_6_TEXT=="FB " | 
            d$C23_6_TEXT=="leader" | 
            d$C23_6_TEXT=="no one" | 
            d$C23_6_TEXT=="No one" | 
            d$C23_6_TEXT=="None" | 
            d$C23_6_TEXT=="Not applicable" | 
            d$C23_6_TEXT=="Not giving any personal details for safety" | 
            d$C23_6_TEXT=="President " | 
            d$C23_6_TEXT=="professional org" | 
            d$C23_6_TEXT=="Public Figure"] <- NA

d$ng3.cow[d$C30=="Coworker or classmate"] <- 1
d$ng3.cow[d$C30!="Coworker or classmate"] <- 0
d$ng3.cow[d$C30==""] <- NA
d$ng3.cow[d$C30_6_TEXT=="Old coworker"] <- 1
d$ng3.cow[d$C30_6_TEXT=="Do not know of anyone" | 
            d$C30_6_TEXT=="Don't have a 3rd" | 
            d$C30_6_TEXT=="N/A" | 
            d$C30_6_TEXT=="no one" | 
            d$C30_6_TEXT=="No one" | 
            d$C30_6_TEXT=="nobody" | 
            d$C30_6_TEXT=="Nobody" | 
            d$C30_6_TEXT=="none" | 
            d$C30_6_TEXT=="None" | 
            d$C30_6_TEXT=="None " | 
            d$C30_6_TEXT=="not applicable" | 
            d$C30_6_TEXT=="Not giving any personal details for safety" | 
            d$C30_6_TEXT=="Nothing" | 
            d$C30_6_TEXT=="President" | 
            d$C30_6_TEXT=="Other people" | 
            d$C30_6_TEXT=="FD"] <- NA

d$ng.cow = with(d, rowSums(cbind(ng1.cow, ng2.cow, ng3.cow), na.rm=TRUE))
d$ng.cow[d$filt!=1] <- NA

#Category: Acquaintance
d$ng1.acq[d$C16=="Acquaintance"] <- 1
d$ng1.acq[d$C16!="Acquaintance"] <- 0
d$ng1.acq[d$C16==""] <- NA
d$ng1.acq[d$C16_6_TEXT=="Boyfriend's stepmom" | 
            d$C16_6_TEXT=="Facebook contact" | 
            d$C16_6_TEXT=="Facebook friend" | 
            d$C16_6_TEXT=="FB friend" | 
            d$C16_6_TEXT=="friend of friend" | 
            d$C16_6_TEXT=="People I rarely know"] <- 1
d$ng1.acq[d$C16_6_TEXT=="All people on Facebook" | 
            d$C16_6_TEXT=="Former US president " |
            d$C16_6_TEXT=="no one" | 
            d$C16_6_TEXT=="No one" | 
            d$C16_6_TEXT=="No person" | 
            d$C16_6_TEXT=="None" | 
            d$C16_6_TEXT=="nope" | 
            d$C16_6_TEXT=="Not giving any personal details for safety"] <- NA

d$ng2.acq[d$C23=="Acquaintance"] <- 1
d$ng2.acq[d$C23!="Acquaintance"] <- 0
d$ng2.acq[d$C23==""] <- NA
d$ng2.acq[d$C23_6_TEXT=="facebook friend" | 
            d$C23_6_TEXT=="friend of friend" | 
            d$C23_6_TEXT=="Met on fb" | 
            d$C23_6_TEXT=="Roommate" | 
            d$C23_6_TEXT=="Significant other"] <- 1
d$ng2.acq[d$C23_6_TEXT=="Do not see political posts" | 
            d$C23_6_TEXT=="FB " | 
            d$C23_6_TEXT=="leader" | 
            d$C23_6_TEXT=="no one" | 
            d$C23_6_TEXT=="No one" | 
            d$C23_6_TEXT=="None" | 
            d$C23_6_TEXT=="Not applicable" | 
            d$C23_6_TEXT=="Not giving any personal details for safety" | 
            d$C23_6_TEXT=="President " | 
            d$C23_6_TEXT=="professional org" | 
            d$C23_6_TEXT=="Public Figure"] <- NA

d$ng3.acq[d$C30=="Acquaintance"] <- 1
d$ng3.acq[d$C30!="Acquaintance"] <- 0
d$ng3.acq[d$C30==""] <- NA
d$ng3.acq[d$C30_6_TEXT=="friend of friend"] <- 1
d$ng3.acq[d$C30_6_TEXT=="Do not know of anyone" | 
            d$C30_6_TEXT=="Don't have a 3rd" | 
            d$C30_6_TEXT=="N/A" | 
            d$C30_6_TEXT=="no one" | 
            d$C30_6_TEXT=="No one" | 
            d$C30_6_TEXT=="nobody" | 
            d$C30_6_TEXT=="Nobody" | 
            d$C30_6_TEXT=="none" | 
            d$C30_6_TEXT=="None" | 
            d$C30_6_TEXT=="None " | 
            d$C30_6_TEXT=="not applicable" | 
            d$C30_6_TEXT=="Not giving any personal details for safety" | 
            d$C30_6_TEXT=="Nothing" | 
            d$C30_6_TEXT=="President" | 
            d$C30_6_TEXT=="Other people" | 
            d$C30_6_TEXT=="FD"] <- NA

d$ng.acq = with(d, rowSums(cbind(ng1.acq, ng2.acq, ng3.acq), na.rm=TRUE))
d$ng.acq[d$filt!=1] <- NA

#Category: Neighbor
d$ng1.nei[d$C16=="Neighbor"] <- 1
d$ng1.nei[d$C16!="Neighbor"] <- 0
d$ng1.nei[d$C16==""] <- NA
d$ng1.nei[d$C16_6_TEXT=="All people on Facebook" | 
            d$C16_6_TEXT=="Former US president " |
            d$C16_6_TEXT=="no one" | 
            d$C16_6_TEXT=="No one" | 
            d$C16_6_TEXT=="No person" | 
            d$C16_6_TEXT=="None" | 
            d$C16_6_TEXT=="nope" | 
            d$C16_6_TEXT=="Not giving any personal details for safety"] <- NA

d$ng2.nei[d$C23=="Neighbor"] <- 1
d$ng2.nei[d$C23!="Neighbor"] <- 0
d$ng2.nei[d$C23==""] <- NA
d$ng2.acq[d$C23_6_TEXT=="Do not see political posts" | 
            d$C23_6_TEXT=="FB " | 
            d$C23_6_TEXT=="leader" | 
            d$C23_6_TEXT=="no one" | 
            d$C23_6_TEXT=="No one" | 
            d$C23_6_TEXT=="None" | 
            d$C23_6_TEXT=="Not applicable" | 
            d$C23_6_TEXT=="Not giving any personal details for safety" | 
            d$C23_6_TEXT=="President " | 
            d$C23_6_TEXT=="professional org" | 
            d$C23_6_TEXT=="Public Figure"] <- NA

d$ng3.nei[d$C30=="Neighbor"] <- 1
d$ng3.nei[d$C30!="Neighbor"] <- 0
d$ng3.nei[d$C30==""] <- NA
d$ng3.nei[d$C30_6_TEXT=="Do not know of anyone" | 
            d$C30_6_TEXT=="Don't have a 3rd" | 
            d$C30_6_TEXT=="N/A" | 
            d$C30_6_TEXT=="no one" | 
            d$C30_6_TEXT=="No one" | 
            d$C30_6_TEXT=="nobody" | 
            d$C30_6_TEXT=="Nobody" | 
            d$C30_6_TEXT=="none" | 
            d$C30_6_TEXT=="None" | 
            d$C30_6_TEXT=="None " | 
            d$C30_6_TEXT=="not applicable" | 
            d$C30_6_TEXT=="Not giving any personal details for safety" | 
            d$C30_6_TEXT=="Nothing" | 
            d$C30_6_TEXT=="President" | 
            d$C30_6_TEXT=="Other people" | 
            d$C30_6_TEXT=="FD"] <- NA

d$ng.nei = with(d, rowSums(cbind(ng1.nei, ng2.nei, ng3.nei), na.rm=TRUE))
d$ng.nei[d$filt!=1] <- NA

#Engage
d <- d %>% mutate(across(
  .cols = all_of(c(76:82, 90:96, 104:110)), 
  ~(recode(., "No, never" = 1, "Yes, occasionally" = 2, "Yes, often" = 3, .default = NA_real_)), 
  .names = "{col}"))
ltm::cronbach.alpha(d[,76:82], na.rm=TRUE) #alpha = .892
ltm::cronbach.alpha(d[,90:96], na.rm=TRUE) #alpha = .911
ltm::cronbach.alpha(d[,104:110], na.rm=TRUE) #alpha = .919
d$ng1.engage = rowMeans(d[,76:82], na.rm=TRUE)
d$ng1.engage[d$filt!=1] <- NA
d$ng2.engage = rowMeans(d[,90:96], na.rm=TRUE)
d$ng2.engage[d$filt!=1] <- NA
d$ng3.engage = rowMeans(d[,104:110], na.rm=TRUE)
d$ng3.engage[d$filt!=1] <- NA
ltm::cronbach.alpha(cbind(d[,76:82], d[,90:96], d[,104:110]), na.rm=TRUE) #alpha = .958
d$ng.engage = rowMeans(cbind(d[,76:82], d[,90:96], d[,104:110]), na.rm=TRUE)
d$ng.engage[d$filt!=1] <- NA

#Agreement
d <- d %>% mutate(across(
  .cols = all_of(c(83,97,111)), 
  ~(recode(., "Very different" = 1, "Somewhat different" = 2, "Neither similar nor different" = 3, "Somewhat similar" = 4, "Very similar" = 5, .default = NA_real_)), 
  .names = "{col}"))
d$ng1.agree = d$C18
d$ng1.agree[d$filt!=1] <- NA
d$ng2.agree = d$C25
d$ng2.agree[d$filt!=1] <- NA
d$ng3.agree = d$C32
d$ng3.agree[d$filt!=1] <- NA
ltm::cronbach.alpha(cbind(d[,83], d[,97], d[,111]), na.rm=TRUE) #alpha = .752
d$ng.agree = rowMeans(cbind(d[,83], d[,97], d[,111]), na.rm=TRUE)
d$ng.agree[d$filt!=1] <- NA

#Similarity
d <- d %>% mutate(across(
  .cols = all_of(c(84,98,112)), 
  ~(recode(., "None at all" = 1, "A little" = 2, "A moderate amount" = 3, "A lot" = 4, "A great deal" = 5, .default = NA_real_)), 
  .names = "{col}"))
d$ng1.sim = d$C19
d$ng1.sim[d$filt!=1] <- NA
d$ng2.sim = d$C26
d$ng2.sim[d$filt!=1] <- NA
d$ng3.sim = d$C33
d$ng3.sim[d$filt!=1] <- NA
ltm::cronbach.alpha(cbind(d[,84], d[,98], d[,112]), na.rm=TRUE) #alpha = .802
d$ng.sim = rowMeans(cbind(d[,84], d[,98], d[,112]), na.rm=TRUE)
d$ng.sim[d$filt!=1] <- NA

#Liking
d <- d %>% mutate(across(
  .cols = all_of(c(85,99,113)), 
  ~(recode(., "Dislike a great deal" = 1, "Dislike somewhat" = 2, "Neither like nor dislike" = 3, "Like somewhat" = 4, "Like a great deal" = 5, .default = NA_real_)), 
  .names = "{col}"))
d$ng1.lik = d$C20
d$ng1.lik[d$filt!=1] <- NA
d$ng2.lik = d$C27
d$ng2.lik[d$filt!=1] <- NA
d$ng3.lik = d$C34
d$ng3.lik[d$filt!=1] <- NA
ltm::cronbach.alpha(cbind(d[,85], d[,99], d[,113]), na.rm=TRUE) #alpha = .737
d$ng.lik = rowMeans(cbind(d[,85], d[,99], d[,113]), na.rm=TRUE)
d$ng.lik[d$filt!=1] <- NA

#Closeness
d <- d %>% mutate(across(
  .cols = all_of(c(86,100,114)), 
  ~(recode(., "A" = 1, "B" = 2, "C" = 3, "D" = 4, "E" = 5, "F" = 6, "G" = 7, .default = NA_real_)), 
  .names = "{col}"))
d$ng1.clo = d$C21
d$ng1.clo[d$filt!=1] <- NA
d$ng2.clo = d$C28
d$ng2.clo[d$filt!=1] <- NA
d$ng3.clo = d$C35
d$ng3.clo[d$filt!=1] <- NA
ltm::cronbach.alpha(cbind(d[,86], d[,100], d[,114]), na.rm=TRUE) #alpha = .792
d$ng.clo = rowMeans(cbind(d[,86], d[,100], d[,114]), na.rm=TRUE)
d$ng.clo[d$filt!=1] <- NA

#Correlations
with(d, cor(cbind(ng.fam, ng.fri, ng.cow, ng.acq, ng.nei, 
                  ng.engage, ng.agree, ng.sim, ng.lik, ng.clo), 
            use="complete.obs", method="pearson"))

#Section D: Social Networks

#Network Size
d <- d %>% mutate(D36 = recode(D36, "None" = 1, "50 or fewer" = 2, "Between 51 and 150" = 3, "Between 151 and 500" = 4, "Between 501 and 1,000" = 5, "Between 1,001 and 2,000" = 6, "More than 2,000" = 7, .default = NA_real_))
d$netsize = d$D36

#News Network Size
d <- d %>% mutate(D37 = recode(D37, "None" = 1, "5 or fewer" = 2, "Between 6 and 10" = 3, "Between 11 and 25" = 4, "Between 26 and 50" = 5, "Between 51 and 100" = 6, "More than 100" = 7, .default = NA_real_))
d$newssize = d$D37

#Network Diversity Ties
d <- d %>% mutate(across(
  .cols = all_of(c(118:125)), 
  ~(recode(., "None" = 1, "A few" = 2, "A lot" = 3, "Many" = 4, "Most" = 5, .default = NA_real_)), 
  .names = "{col}"))
ltm::cronbach.alpha(d[,118:125], na.rm=TRUE) #alpha = .886
d$netdiv1 = rowMeans(d[,118:125], na.rm=TRUE)

#Network Diversity Occupation
d <- d %>% mutate(across(
  .cols = all_of(c(126:147)), 
  ~(recode(., "No" = 0, "Yes" = 1, .default = NA_real_)), 
  .names = "{col}"))
d$netdiv2 = rowSums(d[, 126:147], na.rm=TRUE)

#News Network Diversity
d <- d %>% mutate(across(
  .cols = all_of(c(148:159)), 
  ~(recode(., "None at all" = 1, "A little" = 2, "Some" = 3, "A lot" = 4, "A great deal" = 5, .default = NA_real_)), 
  .names = "{col}"))
ltm::cronbach.alpha(d[,148:159], na.rm=TRUE) #alpha = .887
d$netdiv3 = rowMeans(d[,148:159], na.rm=TRUE)

#Correlations
with(d, cor(cbind(netsize, newssize, netdiv1, netdiv2, netdiv3), 
            use="complete.obs", method="pearson"))

#Section E: News

#FB News
d <- d %>% mutate(E41 = recode(E41, "Never" = 1, "Several times a month" = 2, "Several times a week" = 3, "Once or twice a day" = 4, "Several times a day" = 5, .default = NA_real_))
d$fbnews = d$E41

#FB News Agree/Disagree
d <- d %>% mutate(across(
  .cols = all_of(c(161:162)), 
  ~(recode(., "Never" = 1, "Several times a month" = 2, "Several times a week" = 3, "Once or twice a day" = 4, "Several times a day" = 5, .default = NA_real_)), 
  .names = "{col}"))
d$fbnews.dis = d$E42_1
d$fbnews.agr = d$E42_2

#Follows for News
d <- d %>% mutate(across(
  .cols = all_of(c(163:165)), 
  ~(recode(., "Never" = 1, "Rarely" = 2, "Sometimes" = 3, "Often" = 4, "Very often" = 5, .default = NA_real_)), 
  .names = "{col}"))
ltm::cronbach.alpha(d[,163:165], na.rm=TRUE) #alpha = .910
d$follow = rowMeans(d[,163:165], na.rm=TRUE)

#Non-FB News
d <- d %>% mutate(across(
  .cols = all_of(c(166:171)), 
  ~(recode(., "Never" = 1, "Several times a month" = 2, "Several times a week" = 3, "Once or twice a day" = 4, "Several times a day" = 5, .default = NA_real_)), 
  .names = "{col}"))
d$olnews.msm = d$E44_1
d$olnews.olo = d$E44_2
d$npnews = d$E44_3
d$tvnews = d$E44_4
d$radnews = d$E44_5
d$appnews = d$E44_6
ltm::cronbach.alpha(d[,166:171], na.rm=TRUE) #alpha = .866
d$nfbnews = rowMeans(d[,166:171], na.rm=TRUE)

#Online News Engagement
d <- d %>% mutate(across(
  .cols = all_of(c(172:175)), 
  ~(recode(., "Never" = 1, "Several times a month" = 2, "Several times a week" = 3, "Once or twice a day" = 4, "Several times a day" = 5, .default = NA_real_)), 
  .names = "{col}"))
d$click = d$E45_1
d$scan = d$E45_2
d$read = d$E45_3
d$seek = d$E45_4
ltm::cronbach.alpha(d[,172:175], na.rm=TRUE) #alpha = .908
d$engage = rowMeans(d[,172:175], na.rm=TRUE)

#News Avoidance
d <- d %>% mutate(across(
  .cols = all_of(c(176,178,179,180,182)), 
  ~(recode(., "Strongly disagree" = 1, "Somewhat disagree" = 2, "Neither agree nor disagree" = 3, "Somewhat agree" = 4, "Strongly agree" = 5, .default = NA_real_)), 
  .names = "{col}"))
d <- d %>% mutate(across(
  .cols = all_of(c(177,181)), 
  ~(recode(., "Strongly disagree" = 5, "Somewhat disagree" = 4, "Neither agree nor disagree" = 3, "Somewhat agree" = 2, "Strongly agree" = 1, .default = NA_real_)), 
  .names = "{col}"))
cor(d[,177:178], use="complete.obs") #r = -.08
cor(d[,179:180], use="complete.obs") #r = .48
cor(d[,181:182], use="complete.obs") #r = -.20
d$avoid.single = d$E46_1
d$avoid.norms = d$E46_3
d$avoid.time = d$E46_4
d$avoid.emo = d$E46_5
d$avoid.soc = d$E46_7
ltm::cronbach.alpha(cbind(d[,176], d[,178:180], d[,182]), na.rm=TRUE) #alpha = .745
d$avoid = rowMeans(cbind(d[,176], d[,178:180], d[,182]), na.rm=TRUE)

#Unfriend
d <- d %>% mutate(across(
  .cols = all_of(c(183:185)), 
  ~(recode(., "Yes" = 1, "No" = 0, .default = NA_real_)), 
  .names = "{col}"))
ltm::cronbach.alpha(d[,183:185], na.rm=TRUE) #alpha = .799
d$unfriend = rowMeans(d[,183:185], na.rm=TRUE)

#Correlations
with(d, cor(cbind(fbnews, fbnews.agr, fbnews.dis, follow, 
                  nfbnews, engage, avoid, unfriend), 
            use="complete.obs", method="pearson"))

#Section F: Talk

#Talk Frequency
d <- d %>% mutate(E48 = recode(E48, "Never" = 1, "Rarely" = 2, "Occassionally" = 3, "Frequently" = 4, "Very frequently" = 5, .default = NA_real_))
d$fbtalk.freq = d$E48

#Talk Network Size
d <- d %>% mutate(E49 = recode(E49, "None" = 1, "One or two" = 2, "Several" = 3, "Quite a few" = 4, "Many" = 5, .default = NA_real_))
d$fbtalk.size = d$E49

#FB Talk Agree/Disagree
d <- d %>% mutate(across(
  .cols = all_of(c(188:189)), 
  ~(recode(., "Never" = 1, "Rarely" = 2, "Occassionally" = 3, "Frequently" = 4, "Very frequently" = 5, .default = NA_real_)), 
  .names = "{col}"))
d$fbtalk.dis = d$E50_1
d$fbtalk.agr = d$E50_2

#FB Expression
d <- d %>% mutate(across(
  .cols = all_of(c(190:195)), 
  ~(recode(., "Never" = 1, "Rarely" = 2, "Occasionally" = 3, "Frequently" = 4, "Very frequently" = 5, .default = NA_real_)), 
  .names = "{col}"))
ltm::cronbach.alpha(d[,190:195], na.rm=TRUE) #alpha = .947
d$express = rowMeans(d[,190:195], na.rm=TRUE)

#Correlations
with(d, cor(cbind(fbtalk.freq, fbtalk.size, 
                  fbtalk.dis, fbtalk.agr), 
            use="complete.obs", method="pearson"))

#Section G: Political Antecedents

#Efficacy
d <- d %>% mutate(across(
  .cols = all_of(c(196:197)), 
  ~(recode(., "Strongly disagree" = 1, "Somewhat disagree" = 2, "Neither agree nor disagree" = 3, "Somewhat agree" = 4, "Strongly agree" = 5, .default = NA_real_)), 
  .names = "{col}"))
d <- d %>% mutate(across(
  .cols = all_of(c(198:199)), 
  ~(recode(., "Strongly disagree" = 5, "Somewhat disagree" = 4, "Neither agree nor disagree" = 3, "Somewhat agree" = 2, "Strongly agree" = 1, .default = NA_real_)), 
  .names = "{col}"))
ltm::cronbach.alpha(d[,196:197], na.rm=TRUE) #alpha = .773
ltm::cronbach.alpha(d[,198:199], na.rm=TRUE) #alpha = .747
d$eff.i = rowMeans(d[,196:197], na.rm=TRUE)
d$eff.e = rowMeans(d[,198:199], na.rm=TRUE)

#Interest
d <- d %>% mutate(across(
  .cols = all_of(c(200:202)), 
  ~(recode(., "Not at all interested" = 1, "A little bit interested" = 2, "Somewhat interested" = 3, "Interested" = 4, "Very interested" = 5, .default = NA_real_)), 
  .names = "{col}"))
ltm::cronbach.alpha(d[,200:202], na.rm=TRUE) #alpha = .847
d$int = rowMeans(d[,200:202], na.rm=TRUE)

#Ideology
d$ideo = d$G54_N

#Party ID
d$pid <- NA
d$pid = ifelse(d$G55 == "Democrat" & d$G56a == "Strong", -3, d$pid)
d$pid = ifelse(d$G55 == "Democrat" & d$G56a == "Not that strong", -2, d$pid)
d$pid = ifelse(d$G55 == "Independent" & d$G56b == "Closer to the Democratic Party", -1, d$pid)
d$pid = ifelse(d$G55 == "Other party (please specify:)" & d$G56b == "Closer to the Democratic Party", -1, d$pid)
d$pid = ifelse(d$G55 == "Independent" & d$G56b == "Neither", 0, d$pid)
d$pid = ifelse(d$G55 == "Other party (please specify:)" & d$G56b == "Neither", 0, d$pid)
d$pid = ifelse(d$G55 == "Independent" & d$G56b == "Closer to the Republican Party", 1, d$pid)
d$pid = ifelse(d$G55 == "Other party (please specify:)" & d$G56b == "Closer to the Republican Party", 1, d$pid)
d$pid = ifelse(d$G55 == "Republican" & d$G56a == "Not that strong", 2, d$pid)
d$pid = ifelse(d$G55 == "Republican" & d$G56a == "Strong", 3, d$pid)

#Knowledge
d$know1[d$G57 == "Kamala Harris"] <- 1 
d$know1[d$G57 != "Kamala Harris"] <- 0
d$know2[d$G58 == "Democrat"] <- 1 
d$know2[d$G58 != "Democrat"] <- 0
d$know3[d$G59 == "Judicial branch"] <- 1 
d$know3[d$G59 != "Judicial branch"] <- 0
d$know = with(d, rowSums(cbind(know1, know2, know3), na.rm=TRUE))

save(d, file="socnews.Rdata")






