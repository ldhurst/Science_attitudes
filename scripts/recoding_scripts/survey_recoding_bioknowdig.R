#install all packages

install.packages("pacman")

pacman::p_load(foreign, plyr,multcomp,tidyr)


library(foreign)
library(plyr)
library(multcomp)
library(tidyr)


data1 <- read.spss(file = "Public Voice - Genetics Society 150621.sav", to.data.frame=TRUE)

write.csv(data1, "survey_data.csv", row.names = FALSE)




#the confidence in understanding are scored from on ascending scale

data1$Stories <- as.numeric(as.character(revalue(data1$Stories, c("I usually understand what they are talking about"=3, "I sometimes understand what they are talking about"=2,"I usually do not understand what they are talking about"=1, "I don’t see or hear science news stories"=0))))
data1$Informed <- as.numeric(as.character(revalue(data1$Informed, c("Very well informed"=3, "Fairly well informed"=2,"Not very well informed"=1, "Not at all informed"=0))))

#each participant is given a score range 0 to +1 for self perceived scientific understanding

data1$sci_self_under <- rowSums( data1[,1:2] )/6.0


#self assessed understanding of genetics scored 0 to 4
data1$DNA <- as.numeric(as.character(revalue(data1$DNA, c("Very good"=4, "Good"=3,"Some understanding"=2, "Have heard the term but have little understanding of what it means"=1, "Have not heard the term" =0))))
data1$GM <- as.numeric(as.character(revalue(data1$GM, c("Very good"=4, "Good"=3,"Some understanding"=2, "Have heard the term but have little understanding of what it means"=1, "Have not heard the term" =0))))
data1$NaturalSelection <- as.numeric(as.character(revalue(data1$NaturalSelection, c("Very good"=4, "Good"=3,"Some understanding"=2, "Have heard the term but have little understanding of what it means"=1, "Have not heard the term" =0))))
data1$PCR <- as.numeric(as.character(revalue(data1$PCR, c("Very good"=4, "Good"=3,"Some understanding"=2, "Have heard the term but have little understanding of what it means"=1, "Have not heard the term" =0))))

#each participant is given a score range 0 to +1 for self perceived genetics understanding

data1$gen_self_under <- rowSums( data1[,3:6] )/16.0

#each participant is given a score range 0 to +1 for self perceived science/genetics understanding

#data1$scigen_self_under <- (data1$gen_self_under*4 + data1$sci_self_under*2)/6.0
data1$scigen_self_under <- data1$gen_self_under
#change in understanding is scored +1 to -1, NA coded as 0: NAs are because hadn't heard of x, so not asked about change in understanding, therefore no change.  Don't know also coded as 0 - could be NA?
data1$UndDNA <- as.numeric(as.character(revalue(data1$UndDNA, c("Stayed the same"=0, "Increased"=1,"Decreased"=-1, "Don't know"=0))))
data1$UndDNA <-replace_na(data1$UndDNA, 0)
data1$UndGM <- as.numeric(as.character(revalue(data1$UndGM, c("Stayed the same"=0, "Increased"=1,"Decreased"=-1, "Don't know"=0))))
data1$UndGM <-replace_na(data1$UndGM, 0)
data1$UndNaturalSelection <- as.numeric(as.character(revalue(data1$UndNaturalSelection, c("Stayed the same"=0, "Increased"=1,"Decreased"=-1, "Don't know"=0))))
data1$UndNaturalSelection <-replace_na(data1$UndNaturalSelection, 0)
data1$UndPCR <- as.numeric(as.character(revalue(data1$UndPCR, c("Stayed the same"=0, "Increased"=1,"Decreased"=-1, "Don't know"=0 ))))
data1$UndPCR <-replace_na(data1$UndPCR, 0)

#each participant is given a score range 0 to +1 for self perceived net change understanding

data1$self_change_under <- rowSums( data1[,7:10] )/4.0

#attitude to genetics before pandemic scored +2 to -2

data1$BeforePand_1 <- as.numeric(as.character(revalue(data1$BeforePand_1, c("Strongly agree"=-2, "Agree"=-1, "Disagree"=1,"Strongly disagree"=2, "Neither agree or disagree"=0 ))))
data1$BeforePand_2 <- as.numeric(as.character(revalue(data1$BeforePand_2, c("Strongly agree"=-2, "Agree"=-1, "Disagree"=1,"Strongly disagree"=2, "Neither agree or disagree"=0 ))))
data1$BeforePand_3 <- as.numeric(as.character(revalue(data1$BeforePand_3, c("Strongly agree"=2, "Agree"=1, "Disagree"=-1,"Strongly disagree"=-2, "Neither agree or disagree"=0 ))))


#each participant is given a score range -1 to +1 for attitude to genetics prior to pandemic

data1$attitude_prior <- rowSums( data1[,11:13] )/6.0

#change in attitude is scored -1 to +1
data1$ViewChange1 <- as.numeric(as.character(revalue(data1$ViewChange1, c("Become more likely to agree with this"=-1, "Become more likely to disagree with this"=1, "Not changed your opinion"=0, "Don't know"=NA))))
data1$ViewChange2 <- as.numeric(as.character(revalue(data1$ViewChange2, c("Become more likely to agree with this"=-1, "Become more likely to disagree with this"=1, "Not changed your opinion"=0, "Don't know"=NA))))
data1$ViewChange3 <- as.numeric(as.character(revalue(data1$ViewChange3, c("Become more likely to agree with this"=1, "Become more likely to disagree with this"=-1, "Not changed your opinion"=0, "Don't know"=NA))))

#each participant is given a score range -1 to +1 for change in attitude to genetics since to pandemic

data1$attitude_change <- rowSums( data1[,14:16] )/3.0


#the quiz statements are recoded from +2 definite and correct to -2 definite and wrong

data1$Statements_1 <- as.numeric(as.character(revalue(data1$Statements_1, c("Definitely true"=1, "Probably true"=1,"Probably false"=-1, "Definitely false"=-1, "Don't know"=0 ))))
data1$Statements_2 <- as.numeric(as.character(revalue(data1$Statements_2, c("Definitely true"=1, "Probably true"=1,"Probably false"=-1, "Definitely false"=-1, "Don't know"=0 ))))
data1$Statements_3 <- as.numeric(as.character(revalue(data1$Statements_3, c("Definitely true"=1, "Probably true"=1,"Probably false"=-1, "Definitely false"=-1, "Don't know"=0 ))))
data1$Statements_7 <- as.numeric(as.character(revalue(data1$Statements_7, c("Definitely true"=1, "Probably true"=1,"Probably false"=-1, "Definitely false"=-1, "Don't know"=0 ))))
data1$Statements_9 <- as.numeric(as.character(revalue(data1$Statements_9, c("Definitely true"=1, "Probably true"=1,"Probably false"=-1, "Definitely false"=-1, "Don't know"=0 ))))
data1$Statements_10 <- as.numeric(as.character(revalue(data1$Statements_10, c("Definitely true"=1, "Probably true"=1,"Probably false"=-1, "Definitely false"=-1, "Don't know"=0 ))))
data1$Statements_12 <- as.numeric(as.character(revalue(data1$Statements_12, c("Definitely true"=1, "Probably true"=1,"Probably false"=-1, "Definitely false"=-1, "Don't know"=0 ))))
data1$Statements_4 <- as.numeric(as.character(revalue(data1$Statements_4, c("Definitely true"=-1, "Probably true"=-1,"Probably false"=1, "Definitely false"=1, "Don't know"=0 ))))
data1$Statements_5 <- as.numeric(as.character(revalue(data1$Statements_5, c("Definitely true"=-1, "Probably true"=-1,"Probably false"=1, "Definitely false"=1, "Don't know"=0 ))))
data1$Statements_6 <- as.numeric(as.character(revalue(data1$Statements_6, c("Definitely true"=-1, "Probably true"=-1,"Probably false"=1, "Definitely false"=1, "Don't know"=0 ))))
data1$Statements_8 <- as.numeric(as.character(revalue(data1$Statements_8, c("Definitely true"=-1, "Probably true"=-1,"Probably false"=1, "Definitely false"=1, "Don't know"=0 ))))
data1$Statements_11 <- as.numeric(as.character(revalue(data1$Statements_11, c("Definitely true"=-1, "Probably true"=-1,"Probably false"=1, "Definitely false"=1, "Don't know"=0 ))))

#each participant is given a score range -1 to +1 for scientific knowledge

#data1$sci_know <- rowSums( data1[,17:28] )/12.0

#the bio questions are in cols 17-20, 22, 24-28
c
data1$sci_know <- rowSums( data1[,rows] )/10.0


#attitude to genetics is scored +1 to -1, or NA
data1$AttGenetics <- as.numeric(as.character(revalue(data1$AttGenetics, c("Very positive"=1, "Slightly positive"=0.5,"Slightly negative"=-0.5, "Very negative"=-1, "Undecided"=0, "I don’t know enough to form a judgement"=NA ))))


#optimism about genetics is scored +1 to -1

data1$Optimism <- as.numeric(as.character(revalue(data1$Optimism, c("Very optimistic"=1, "Somewhat optimistic"=0.5,"Not too optimistic"=-0.5, "Not at all optimistic"=-1 ))))

#scientific relationship is scored 0 to 1

data1$SciRelationship <- as.numeric(as.character(revalue(data1$SciRelationship, c("I feel connected with science – I actively seek out science news, events, activities or entertainment"=1, "I’m interested in science, but I don’t make a special effort to keep informed"=0.5,"Science is not for me"=0))))

#last three months exposure to scored as 1 yes, 0 no.  None of the above treated opposite and is score of heard about any 


data1$ThreeMonths1 <- as.numeric(as.character(revalue(data1$ThreeMonths1, c("Yes"=1, "No"=0))))
data1$ThreeMonths2 <- as.numeric(as.character(revalue(data1$ThreeMonths2, c("Yes"=1, "No"=0))))
data1$ThreeMonths3 <- as.numeric(as.character(revalue(data1$ThreeMonths3, c("Yes"=1, "No"=0))))
data1$ThreeMonths4 <- as.numeric(as.character(revalue(data1$ThreeMonths4, c("Yes"=1, "No"=0))))
data1$ThreeMonths5 <- as.numeric(as.character(revalue(data1$ThreeMonths5, c("Yes"=1, "No"=0))))
data1$ThreeMonths6 <- as.numeric(as.character(revalue(data1$ThreeMonths6, c("Yes"=0, "No"=1))))

#each participant is given a score range 0 to +1 for exposure. None of the above treated is ignored

data1$ThreeMonthExposure <- rowSums( data1[,32:36] )/5.0

#who  to trust is scored 1 yes, 0 no. None of the above treated opposite and is score of trust in any . don't know is treated as absence of trust

data1$WhoTrust01 <- as.numeric(as.character(revalue(data1$WhoTrust01, c("Yes"=1, "No"=0))))
data1$WhoTrust02 <- as.numeric(as.character(revalue(data1$WhoTrust02, c("Yes"=1, "No"=0))))
data1$WhoTrust03 <- as.numeric(as.character(revalue(data1$WhoTrust03, c("Yes"=1, "No"=0))))
data1$WhoTrust04 <- as.numeric(as.character(revalue(data1$WhoTrust04, c("Yes"=1, "No"=0))))
data1$WhoTrust05 <- as.numeric(as.character(revalue(data1$WhoTrust05, c("Yes"=1, "No"=0))))
data1$WhoTrust06 <- as.numeric(as.character(revalue(data1$WhoTrust06, c("Yes"=1, "No"=0))))
data1$WhoTrust07 <- as.numeric(as.character(revalue(data1$WhoTrust07, c("Yes"=1, "No"=0))))
data1$WhoTrust08 <- as.numeric(as.character(revalue(data1$WhoTrust08, c("Yes"=1, "No"=0))))
data1$WhoTrust09 <- as.numeric(as.character(revalue(data1$WhoTrust09, c("Yes"=0, "No"=1))))
data1$WhoTrust10 <- as.numeric(as.character(revalue(data1$WhoTrust10, c("Yes"=0, "No"=1))))


#each participant is given a score range 0 to +1 for who to trust. None of the above and don't know treated as ignored. 

data1$Who2Trust <- rowSums( data1[,38:45] )/8.0

#which sources  to trust is scored 1 yes, 0 no. None of the above treated opposite and is score of trust in any . don't know is treated as absence of trust
data1$WhichTrust01 <- as.numeric(as.character(revalue(data1$WhichTrust01, c("Yes"=1, "No"=0))))
data1$WhichTrust02 <- as.numeric(as.character(revalue(data1$WhichTrust02, c("Yes"=1, "No"=0))))
data1$WhichTrust03 <- as.numeric(as.character(revalue(data1$WhichTrust03, c("Yes"=1, "No"=0))))
data1$WhichTrust04 <- as.numeric(as.character(revalue(data1$WhichTrust04, c("Yes"=1, "No"=0))))
data1$WhichTrust05 <- as.numeric(as.character(revalue(data1$WhichTrust05, c("Yes"=1, "No"=0))))
data1$WhichTrust06 <- as.numeric(as.character(revalue(data1$WhichTrust06, c("Yes"=1, "No"=0))))
data1$WhichTrust07 <- as.numeric(as.character(revalue(data1$WhichTrust07, c("Yes"=1, "No"=0))))
data1$WhichTrust08 <- as.numeric(as.character(revalue(data1$WhichTrust08, c("Yes"=1, "No"=0))))
data1$WhichTrust09 <- as.numeric(as.character(revalue(data1$WhichTrust09, c("Yes"=1, "No"=0))))
data1$WhichTrust10 <- as.numeric(as.character(revalue(data1$WhichTrust10, c("Yes"=1, "No"=0))))
data1$WhichTrust11 <- as.numeric(as.character(revalue(data1$WhichTrust11, c("Yes"=1, "No"=0))))
data1$WhichTrust12 <- as.numeric(as.character(revalue(data1$WhichTrust12, c("Yes"=1, "No"=0))))
data1$WhichTrust13 <- as.numeric(as.character(revalue(data1$WhichTrust13, c("Yes"=1, "No"=0))))
data1$WhichTrust14 <- as.numeric(as.character(revalue(data1$WhichTrust14, c("Yes"=1, "No"=0))))
data1$WhichTrust15 <- as.numeric(as.character(revalue(data1$WhichTrust15, c("Yes"=1, "No"=0))))
data1$WhichTrust16 <- as.numeric(as.character(revalue(data1$WhichTrust16, c("Yes"=0, "No"=1))))
data1$WhichTrust17 <- as.numeric(as.character(revalue(data1$WhichTrust17, c("Yes"=0, "No"=1))))

#each participant is given a score range 0 to +1 for which outlets to trust. None of the above and don't know treated as ignored. 

data1$Who2Trust <- rowSums( data1[,48:62] )/15.0

#heard and though about genetics rescaled 0 to 5

data1$HeardGenes <- as.numeric(as.character(revalue(data1$HeardGenes, c("A great deal"=5, "Quite a lot"=4, "A small amount"=3,"Not very much"=1, "Not at all"=0 ))))
data1$ThoughtGenes <- as.numeric(as.character(revalue(data1$ThoughtGenes, c("A great deal"=5, "Quite a lot"=4, "A small amount"=3,"Not very much"=1, "Not at all"=0 ))))

#each participant is given a score range 0 to +1 for extent genetics conciousness

data1$GeneConciousness <- rowSums( data1[,65:66] )/10.0 

#amount of science information rescaled +2 (far too much) to -2 (far too little)

data1$TheseDays  <- as.numeric(as.character(revalue(data1$TheseDays, c("... far too much information about science"=2, "… too much information about science"=1, "… the right amount of information about science"=0,"… too little information about science"=-1, "… far too little information about science"=-2 ))))

#trust in various scientists is scaled +2 (full trust) to -2 (complete distrust)
data1$TrustSci  <- as.numeric(as.character(revalue(data1$TrustSci, c("Completely trust"=2, "Partially trust"=1, "Neither distrust nor trust"=0,"Partially distrust"=-1, "Completely distrust"=-2, "Not applicable / No Opinion" ="NA" ))))
data1$TrustGenet  <- as.numeric(as.character(revalue(data1$TrustGenet, c("Completely trust"=2, "Partially trust"=1, "Neither distrust nor trust"=0,"Partially distrust"=-1, "Completely distrust"=-2, "Not applicable / No Opinion" ="NA" ))))
data1$TrustGeol  <- as.numeric(as.character(revalue(data1$TrustGeol, c("Completely trust"=2, "Partially trust"=1, "Neither distrust nor trust"=0,"Partially distrust"=-1, "Completely distrust"=-2, "Not applicable / No Opinion" ="NA" ))))

#each participant is given a score range -1 to +1 for trust in scientists
data1$TrustScientists <- rowSums( data1[,68:70] )/6.0 

#change in trust in various scientists is scaled +2 (much more trust) to -2 (much more distrust)
data1$PandTrustSci  <- as.numeric(as.character(revalue(data1$PandTrustSci, c("Trust them much more"=2, "Trust them a little more"=1, "About the same"=0,"Trust them a little less"=-1, "Trust them much less"=-2, "Don't know" ="NA" ))))
data1$PandTrustGenet  <- as.numeric(as.character(revalue(data1$PandTrustGenet, c("Trust them much more"=2, "Trust them a little more"=1, "About the same"=0,"Trust them a little less"=-1, "Trust them much less"=-2, "Don't know" ="NA" ))))
data1$PandTrustGeol  <- as.numeric(as.character(revalue(data1$PandTrustGeol, c("Trust them much more"=2, "Trust them a little more"=1, "About the same"=0,"Trust them a little less"=-1, "Trust them much less"=-2, "Don't know" ="NA" ))))

#each participant is given a score range -1 to +1 for change in trust in scientists

data1$ChangeTrustScientists <- rowSums( data1[,71:73] )/6.0 

#change in trust in Pfizer scored -2 to +2

data1$Pfiz  <- as.numeric(as.character(revalue(data1$Pfiz, c("Trust them much more"=2, "Trust them a little more"=1, "About the same"=0,"Trust them a little less"=-1, "Trust them much less"=-2, "Don't know" ="NA" ))))
data1$Glaxo  <- as.numeric(as.character(revalue(data1$Glaxo, c("Trust them much more"=2, "Trust them a little more"=1, "About the same"=0,"Trust them a little less"=-1, "Trust them much less"=-2, "Don't know" ="NA" ))))

#had covid left as is but also recode dto new column to aggregate any yes = 1, no =0
data1$HadCov_recoded  <- as.numeric(as.character(revalue(data1$HadCov, c("Yes, confirmed by a positive test"=1, "Yes, suspected by a healthcare professional but not tested"=1, "Yes, my own suspicions"=1,"No"=0, "Prefer not to answer"="NA" ))))

#vaccine position left as is but also recoded to yes =1, no =0
data1$CovVac_recoded  <- as.numeric(as.character(revalue(data1$CovVac, c("Yes, and I have already been vaccinated"=1, "Yes, but I am yet to be vaccinated"=1, "No, I would not get vaccinated"=0, "Prefer not to answer"="NA" ))))


#recode age variable with better heading
data1$Age <- as.numeric(as.character(data1$RS_Age_Exact))

#recode education

data1$education_level <- as.numeric(as.character(revalue(data1$RS_EducationLevel, c("Degree level qualification(s)"=2, "Non-degree level qualifications"=1, "No academic or vocational qualifications"=0, "NA"=NA))))

#recode religion
data1$religion <- as.numeric(as.character(revalue(data1$RS_Religiosity, c("Not religious"=0, "Religious (not actively practising)"=1, "Religious (practising)"=2, "NA"=NA))))


write.csv(data1, "survey_data_recoded_biodig.csv", row.names = FALSE)

