#install all packages

install.packages("pacman")

pacman::p_load(vioplot,Hmisc,rstatix,foreign,plyr,multcomp,tidyr,data.table,dplyr,ppcor,ggplot2,forcats,gridExtra,diptest,stargazer,psych,ltm,tidyverse,xtable, VennDiagram)

#if for some reason that doesn't work, try the relevant line manuually - just remove the hash

#install.packages("vioplot")
#install.packages("Hmisc")
#install.packages("rstatix")
#install.packages("foreign")
#install.packages("plyr")
#install.packages("multcomp")
#install.packages("tidyr")
#install.packages("data.table")
#install.packages("dplyr")
#install.packages("ppcor")
#install.packages("ggplot2")
#install.packages("forcats")
#install.packages("gridExtra")
#install.packages("diptest")
#install.packages("stargazer")
#install.packages("psych")
#install.packages("ltm")
#install.packages("tidyverse")



#require all packages

library(vioplot)
library(Hmisc)
library(rstatix)
library(foreign)
library(plyr)
library(multcomp)
library(tidyr)
library(data.table)
library(dplyr)
library(ppcor)
library(ggplot2)
library(forcats)
library(gridExtra)
library(diptest)
library(stargazer)
library(psych)
library(ltm)
library(tidyverse)
library(xtable)
library(VennDiagram)

corstars <-function(x, method=c("pearson", "spearman"), removeTriangle=c("upper", "lower"),
                     result=c("none", "html", "latex")){
    #Compute correlation matrix
    require(Hmisc)
    x <- as.matrix(x)
    correlation_matrix<-rcorr(x, type=method[1])
    R <- correlation_matrix$r # Matrix of correlation coeficients
    p <- correlation_matrix$P # Matrix of p-value 
    
    ## Define notions for significance levels; spacing is important.
    mystars <- ifelse(p < .0001, "****", ifelse(p < .001, "*** ", ifelse(p < .01, "**  ", ifelse(p < .05, "*   ", "    "))))
    
    ## trunctuate the correlation matrix to two decimal
    R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1]
    
    ## build a new matrix that includes the correlations with their apropriate stars
    Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x))
    diag(Rnew) <- paste(diag(R), " ", sep="")
    rownames(Rnew) <- colnames(x)
    colnames(Rnew) <- paste(colnames(x), "", sep="")
    
    ## remove upper triangle of correlation matrix
    if(removeTriangle[1]=="upper"){
      Rnew <- as.matrix(Rnew)
      Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
      Rnew <- as.data.frame(Rnew)
    }
    
    ## remove lower triangle of correlation matrix
    else if(removeTriangle[1]=="lower"){
      Rnew <- as.matrix(Rnew)
      Rnew[lower.tri(Rnew, diag = TRUE)] <- ""
      Rnew <- as.data.frame(Rnew)
    }
    
    ## remove last column and return the correlation matrix
    Rnew <- cbind(Rnew[1:length(Rnew)])
    if (result[1]=="none") return(Rnew)
    else{
      if(result[1]=="html") print(xtable(Rnew), type="html")
      else print(xtable(Rnew), type="latex") 
    }
}

corstars_partial <-function(x, method=c("pearson", "spearman"), removeTriangle=c("upper", "lower"),
                     result=c("none", "html", "latex")){
    #Compute correlation matrix
    require(Hmisc)
    x <- as.matrix(x)
    correlation_matrix<-pcor(x, method=method[1])
    R <- correlation_matrix$estimate # Matrix of correlation coeficients
    p <- correlation_matrix$p.value # Matrix of p-value 
    
    ## Define notions for significance levels; spacing is important.
    mystars <- ifelse(p < .0001, "****", ifelse(p < .001, "*** ", ifelse(p < .01, "**  ", ifelse(p < .05, "*   ", "    "))))
    
    ## trunctuate the correlation matrix to two decimal
    R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1]
    
    ## build a new matrix that includes the correlations with their apropriate stars
    Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x))
    diag(Rnew) <- paste(diag(R), " ", sep="")
    rownames(Rnew) <- colnames(x)
    colnames(Rnew) <- paste(colnames(x), "", sep="")
    
    ## remove upper triangle of correlation matrix
    if(removeTriangle[1]=="upper"){
      Rnew <- as.matrix(Rnew)
      Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
      Rnew <- as.data.frame(Rnew)
    }
    
    ## remove lower triangle of correlation matrix
    else if(removeTriangle[1]=="lower"){
      Rnew <- as.matrix(Rnew)
      Rnew[lower.tri(Rnew, diag = TRUE)] <- ""
      Rnew <- as.data.frame(Rnew)
    }
    
    ## remove last column and return the correlation matrix
    Rnew <- cbind(Rnew[1:length(Rnew)])
    if (result[1]=="none") return(Rnew)
    else{
      if(result[1]=="html") print(xtable(Rnew), type="html")
      else print(xtable(Rnew), type="latex") 
    }
}





table_glht <- function(x) {
  pq <- summary(x)$test
  mtests <- cbind(pq$coefficients, pq$sigma, pq$tstat, pq$pvalues)
  error <- attr(pq$pvalues, "error")
  pname <- switch(x$alternativ, less = paste("Pr(<", ifelse(x$df ==0, "z", "t"), ")", sep = ""), 
  greater = paste("Pr(>", ifelse(x$df == 0, "z", "t"), ")", sep = ""), two.sided = paste("Pr(>|",ifelse(x$df == 0, "z", "t"), "|)", sep = ""))
  colnames(mtests) <- c("Estimate", "Std. Error", ifelse(x$df ==0, "z value", "t value"), pname)
  return(mtests)

}


#these variables need adjusting depending on which analysis is done.  Currently set to perform analysis as in the main text

nrands <- 1000
#rows <- c(17:20,22,24:28)
rows <- c(17:28)



data1 <- read.csv(file = "survey_data_recoded_dig.csv", header=TRUE)
#data1 <- read.csv(file = "survey_data_recoded_biodig.csv", header=TRUE)
self_all <- data1[,1:6]
#self_all <- data1[,3:6]


# program starts


colnames(data1)[11:13] <- c("Hype","Trust", "GM_benefits" )

data1$CovVac <- as.factor(data1$CovVac)


edu <- as.numeric(as.character(data1$education_level))

tr <- as.numeric(as.character(data1$Trust))
gm <- as.numeric(as.character(data1$GM_benefits))
hy <- as.numeric(as.character(data1$Hype))


x1 <- edu[tr==-2 ]
x2 <- edu[tr==-1 ]
x3 <- edu[tr==0 ]
x4 <- edu[tr==1 ]
x5 <- edu[tr==2 ]

slices.tr <- c(length(x1), length(x2), length(x3), length(x4), length(x5))

lbls.tr <- c("Trust--", "Trust-", "Neutral", "Trust+", "Trust++")
pct.tr <- round(slices.tr/sum(slices.tr)*100)
#lbls.tr <- paste(lbls, pct) # add percents to labels 
#lbls.tr <- paste(lbls,"%",sep="") # ad % to labels 

data.trfr <- data.frame(name=lbls.tr, value=pct.tr)



x1.0 <- length(x1[x1==0 & !is.na(x1)])/length(x1[!is.na(x1)])
x1.1 <- length(x1[x1==1 & !is.na(x1)])/length(x1[!is.na(x1)])
x1.2 <- length(x1[x1==2 & !is.na(x1)])/length(x1[!is.na(x1)])

x2.0 <- length(x2[x2==0 & !is.na(x2)])/length(x2[!is.na(x2)])
x2.1 <- length(x2[x2==1 & !is.na(x2)])/length(x2[!is.na(x2)])
x2.2 <- length(x2[x2==2 & !is.na(x2)])/length(x2[!is.na(x2)])

x3.0 <- length(x3[x3==0 & !is.na(x3)])/length(x3[!is.na(x3)])
x3.1 <- length(x3[x3==1 & !is.na(x3)])/length(x3[!is.na(x3)])
x3.2 <- length(x3[x3==2 & !is.na(x3)])/length(x3[!is.na(x3)])

x4.0 <- length(x4[x4==0 & !is.na(x4)])/length(x4[!is.na(x4)])
x4.1 <- length(x4[x4==1 & !is.na(x4)])/length(x4[!is.na(x4)])
x4.2 <- length(x4[x4==2 & !is.na(x4)])/length(x4[!is.na(x4)])

x5.0 <- length(x5[x5==0 & !is.na(x5)])/length(x5[!is.na(x5)])
x5.1 <- length(x5[x5==1 & !is.na(x5)])/length(x5[!is.na(x5)])
x5.2 <- length(x5[x5==2 & !is.na(x5)])/length(x5[!is.na(x5)])

prop.0 <- c(x1.0, x2.0, x3.0, x4.0, x5.0)
prop.1 <- c(x1.1, x2.1, x3.1, x4.1, x5.1)
prop.2 <- c(x1.2, x2.2, x3.2, x4.2, x5.2)

#plot(x, prop.0)
#plot(x, prop.1)
#plot(x, prop.2)

Trust_levels <- as.factor(c(rep("Trust--", 3), rep("Trust-", 3), rep("Neutral", 3), rep("Trust+", 3), rep("Trust++", 3)))

Edu_levels <- rep(c("Degree level", "Not degree level", "None"), 5)

value <- c(x1.2, x1.1, x1.0, x2.2, x2.1, x2.0, x3.2, x3.1, x3.0, x4.2, x4.1, x4.0, x5.2, x5.1, x5.0) 



data5a <- data.frame(Trust_levels, Edu_levels, value)
    
p <- ggplot(data5a, aes(fill=fct_inorder(Edu_levels), y=value, x=Trust_levels)) + 
    geom_bar(position="stack", stat="identity")
    
    p<- p+ aes(x=fct_inorder(Trust_levels))+ xlab("Trust level")+ ylab("Frequency") + guides(fill=guide_legend(title="Educational qual")) + theme(aspect.ratio=1) +ggtitle("A")
    
pdf("FigSR2a_Edu_Trust.pdf")


    print(p)
    dev.off()
    
#hype and educational level
    
#hy <- as.numeric(as.character(data1$Hype))
    
x1 <- edu[hy==-2 ]
x2 <- edu[hy==-1 ]
x3 <- edu[hy==0 ]
x4 <- edu[hy==1 ]
x5 <- edu[hy==2 ]

slices.hy <- c(length(x1), length(x2), length(x3), length(x4), length(x5))

lbls.hy <- c("Hype++", "Hype+", "Neutral", "Hype-", "Hype--")
pct.hy <- round(slices.hy/sum(slices.hy)*100)
#lbls.hy <- paste(lbls, pct) # add percents to labels 
#lbls.hy <- paste(lbls,"%",sep="") # ad % to labels 


data.hyfr <- data.frame(name=lbls.hy, value=pct.hy)

pdf("FigSR3_Freq.pdf", height = 8, width = 15)

par(mfrow=c(1:2))
barplot(pct.tr, names.arg=lbls.tr, ylab = "%", main = "A.")
barplot(pct.hy, names.arg=lbls.hy, ylab = "%", main = "B.")
dev.off()



x1.0 <- length(x1[x1==0 & !is.na(x1)])/length(x1[!is.na(x1)])
x1.1 <- length(x1[x1==1 & !is.na(x1)])/length(x1[!is.na(x1)])
x1.2 <- length(x1[x1==2 & !is.na(x1)])/length(x1[!is.na(x1)])

x2.0 <- length(x2[x2==0 & !is.na(x2)])/length(x2[!is.na(x2)])
x2.1 <- length(x2[x2==1 & !is.na(x2)])/length(x2[!is.na(x2)])
x2.2 <- length(x2[x2==2 & !is.na(x2)])/length(x2[!is.na(x2)])

x3.0 <- length(x3[x3==0 & !is.na(x3)])/length(x3[!is.na(x3)])
x3.1 <- length(x3[x3==1 & !is.na(x3)])/length(x3[!is.na(x3)])
x3.2 <- length(x3[x3==2 & !is.na(x3)])/length(x3[!is.na(x3)])

x4.0 <- length(x4[x4==0 & !is.na(x4)])/length(x4[!is.na(x4)])
x4.1 <- length(x4[x4==1 & !is.na(x4)])/length(x4[!is.na(x4)])
x4.2 <- length(x4[x4==2 & !is.na(x4)])/length(x4[!is.na(x4)])

x5.0 <- length(x5[x5==0 & !is.na(x5)])/length(x5[!is.na(x5)])
x5.1 <- length(x5[x5==1 & !is.na(x5)])/length(x5[!is.na(x5)])
x5.2 <- length(x5[x5==2 & !is.na(x5)])/length(x5[!is.na(x5)])

prop.0 <- c(x1.0, x2.0, x3.0, x4.0, x5.0)
prop.1 <- c(x1.1, x2.1, x3.1, x4.1, x5.1)
prop.2 <- c(x1.2, x2.2, x3.2, x4.2, x5.2)

#plot(x, prop.0)
#plot(x, prop.1)
#plot(x, prop.2)

Hype_levels <- as.factor(c(rep("Hype++", 3), rep("Hype+", 3), rep("Neutral", 3), rep("Hype-", 3), rep("Hype--", 3)))

Edu_levels <- rep(c("Degree level", "Not degree level", "None"), 5)

value <- c(x1.2, x1.1, x1.0, x2.2, x2.1, x2.0, x3.2, x3.1, x3.0, x4.2, x4.1, x4.0, x5.2, x5.1, x5.0) 



data5b <- data.frame(Hype_levels, Edu_levels, value)
    
p1 <- ggplot(data5b, aes(fill=fct_inorder(Edu_levels), y=value, x=Hype_levels)) + 
    geom_bar(position="stack", stat="identity")
    
    p1<- p1+ aes(x=fct_inorder(Hype_levels))+ xlab("Hype level")+ ylab("Frequency") + guides(fill=guide_legend(title="Educational qual")) + theme(aspect.ratio=1) +ggtitle("B")
   
   

pdf("FigSR2b_Edu_Hype.pdf", onefile =TRUE)
print(p1)
    dev.off()
    
    pdf("FigSR2_all.pdf", width =15, height = 8)
    print(grid.arrange(p, p1, nrow=1))
    dev.off()
    
    
    #scientific+genetic knowledge vs Trust

x1 <- data1$sci_know[data1$Trust==-2]
x2<- data1$sci_know[data1$Trust==-1]
x3<- data1$sci_know[data1$Trust==0]
x4 <- data1$sci_know[data1$Trust==1]
x5 <- data1$sci_know[data1$Trust==2]

pdf("FigSR1_SciK_Attitudes.pdf", height = 8, width = 15)
par(mfrow=c(1,2))
par(pty="s")
vioplot(x1, x2, x3, x4, x5, names= c("Trust--", " Trust-", "Neutral", "Trust+", "Trust++"), 
   col="gold", ylab ="Scientific knowledge", cex.axis=1.20)
title("A")

#prediction 2b preamble

cor.sk.tr <- cor.test(tr, data1$sci_know, method = "spearman")
cor.sk.hy <- cor.test(hy, data1$sci_know, method = "spearman")

#scientific+genetic knowledge vs hype

x1 <- data1$sci_know[data1$Hype==-2]
x2<- data1$sci_know[data1$Hype==-1]
x3<- data1$sci_know[data1$Hype==0]
x4 <- data1$sci_know[data1$Hype==1]
x5 <- data1$sci_know[data1$Hype==2]

vioplot(x1, x2, x3, x4, x5, names=c("hype ++", "hype", "neutral", "hype-", "hype--"), 
   col="gold", ylab ="Scientific knowledge", cex.axis=1.2 )
title("B")
dev.off()

#scientific knowldeg v self assessment

cor.k_sg <- cor.test(data1$scigen_self_under, data1$sci_know, method = "spearman")

sci_know <- data1$sci_know

know2 <- sci_know^2
know3 <- sci_know^3
linear.model <- lm(data1$scigen_self_under~sci_know)
quad.model <- lm(data1$scigen_self_under~sci_know + know2)
three.model <- lm(data1$scigen_self_under~sci_know + know2 + know3)

#results prediction 2a

fit.lin <- summary(linear.model)
fit.quad <- summary(quad.model)

#which is best

comp.linquad <- anova(linear.model, quad.model)

xvals <- seq(-0.3, 1, 0.01)
predictedcounts <- predict(quad.model, list(sci_know = xvals, know2 = xvals^2))

predictedcounts3 <- predict(three.model, list(sci_know = xvals, know2 = xvals^2, know3 = xvals^3))
pdf("Fig2_SciKnow_v_scigen_selfassess.pdf")
pty="s"
plot(data1$sci_know, data1$scigen_self_under, xlab="Scientific knowledge",  ylab = "Self assessed understanding" , pch=16, cex=0.5, col="blue")
lines(xvals, predictedcounts, col = "darkgreen", lwd = 2)
#lines(xvals, predictedcounts3, col="blue", lwd =2)
abline(lm(data1$scigen_self_under~data1$sci_know), col=c("red"), lwd=2)

abline (0,1, col="green")
dev.off()

    #scientific self asseed vs Trust

x1 <- data1$scigen_self_under[data1$Trust==-2]
x2<- data1$scigen_self_under[data1$Trust==-1]
x3<- data1$scigen_self_under[data1$Trust==0]
x4 <- data1$scigen_self_under[data1$Trust==1]
x5 <- data1$scigen_self_under[data1$Trust==2]

pdf("Fig1_Selfass_Attitudes.pdf", height = 8, width = 15)
par(mfrow=c(1,2))
par(pty="s")
vioplot(x1, x2, x3, x4, x5, names= c("Trust--", " Trust-", "Neutral", "Trust+", "Trust++"), 
   col="gold", ylab ="Self assessed understanding", cex.axis=1.20)
title("A")

#scientific+genetic knowledge vs hype

x1 <- data1$scigen_self_under[data1$Hype==-2]
x2<- data1$scigen_self_under[data1$Hype==-1]
x3<- data1$scigen_self_under[data1$Hype==0]
x4 <- data1$scigen_self_under[data1$Hype==1]
x5 <- data1$scigen_self_under[data1$Hype==2]

vioplot(x1, x2, x3, x4, x5, names=c("Hype ++", "Hype", "neutral", "Hype-", "Hype--"), 
   col="gold", ylab ="Self assessed understanding", cex.axis=1.2 )
title("B")
dev.off()

#data for prediction 1b
tr <- as.numeric(as.character(data1$Trust))

tr2 <- tr*tr
linear.model.tr <- lm(data1$scigen_self_under~tr)
quad.model.tr <- lm(data1$scigen_self_under~tr+ tr2)
lm.tr.sg <- summary(linear.model.tr)
qm.tr.sg <- summary(quad.model.tr)
tr.sg.fit <- anova(linear.model.tr,quad.model.tr)

hy <- as.numeric(as.character(data1$Hype))

hy2 <- hy*hy
linear.model.hy <- lm(data1$scigen_self_under~hy)
quad.model.hy <- lm(data1$scigen_self_under~hy+ hy2)

lm.hy.sg <- summary(linear.model.hy)
qm.hy.sg <- summary(quad.model.hy)
hy.sg.fit <- anova(linear.model.hy,quad.model.hy)

gm <- as.numeric(as.character(data1$GM_benefits))

gm2 <- gm*gm
linear.model.gm <- lm(data1$scigen_self_under~gm)
quad.model.gm <- lm(data1$scigen_self_under~gm+ gm2)
lm.gm.sg <- summary(linear.model.gm)
qm.gm.sg <- summary(quad.model.gm)
gm.sg.fit <- anova(linear.model.gm,quad.model.gm)

#analysis of modulus of attitude
#table 1a
data1$modtrust <- abs(as.numeric(as.character(data1$Trust)))

ct.modtr.sg <- cor.test(data1$modtrust, data1$scigen_self_under, method="spearman")

dataMODTR <- data.frame(edu = as.numeric(as.character(data1$education_level)), age = as.numeric(data1$Age), rel=as.numeric(as.character(data1$religion)),scigen_self_under=data1$scigen_self_under, modtrust =as.numeric(as.character(data1$modtrust)))
dataMODTR.narm <- dataMODTR[complete.cases(dataMODTR),]
multi_pcor_MODtrust<- pcor(dataMODTR.narm, method="spearman")

resMODTR <- rcorr(as.matrix(dataMODTR.narm), type = "spearman")
resMODTRP  <-replace_na(resMODTR$P, 0)


Table1a_top <- corstars(dataMODTR.narm, method="spearman",removeTriangle="lower", result="html")
#Table1a_top <- gsub("Statements_", "Q ", Table1a_top)

write.table(Table1a_top, file="Table1a_top.html")
Table1a_bot <- corstars_partial(dataMODTR.narm, method="spearman",removeTriangle="upper", result="html")
write.table(Table1a_bot, file="Table1a_bot.html")

#table 1b
data1$modhy <- abs(as.numeric(as.character(data1$Hype)))

ct.modhy.sg <- cor.test(data1$modhy, data1$scigen_self_under, method="spearman")

dataMODhy <- data.frame(edu = as.numeric(as.character(data1$education_level)), age = as.numeric(data1$Age), rel=as.numeric(as.character(data1$religion)),scigen_self_under=data1$scigen_self_under, modhy =as.numeric(as.character(data1$modhy)))
 dataMODhy.narm <- dataMODhy[complete.cases(dataMODhy),]
multi_pcor_MODhy<- pcor(dataMODhy.narm, method="spearman")

resMODhy <- rcorr(as.matrix(dataMODhy.narm), type = "spearman")
resMODThyP  <-replace_na(resMODTR$P, 0)

Table1b_top <- corstars(dataMODhy.narm, method="spearman",removeTriangle="lower", result="html")
#Table1a_top <- gsub("Statements_", "Q ", Table1a_top)

write.table(Table1b_top, file="Table1b_top.html")

Table1b_bot <- corstars_partial(dataMODhy.narm, method="spearman",removeTriangle="upper", result="html")
write.table(Table1b_bot, file="Table1b_bot.html")


#data for prediction 1c

cor.tr.sg.pos <- cor.test(tr[tr>=0], data1$scigen_self_under[tr>=0], method = "spearman")
cor.tr.sg.neg <- cor.test(tr[tr<=0], data1$scigen_self_under[tr<=0], method = "spearman")
cor.hy.sg.pos <- cor.test(hy[hy>=0], data1$scigen_self_under[hy>=0], method = "spearman")
cor.hy.sg.neg <- cor.test(hy[hy<=0], data1$scigen_self_under[hy<=0], method = "spearman")


#and replication 1c
cor.gm.sg.pos <- cor.test(gm[gm>=0], data1$scigen_self_under[gm>=0], method = "spearman")
cor.gm.sg.neg <- cor.test(gm[gm<=0], data1$scigen_self_under[gm<=0], method = "spearman")


#repeat for knowledge
linear.model.kt <- lm(data1$sci_know~data1$Trust)
quad.model.kt <- lm(data1$sci_know~tr+ tr2)


#pdf("TrHy vs SciKn quad.pdf", width=15, height=8)
#par(mfrow=c(1,2))

xvals <- seq(-2, 2, 1)
predictedcounts <- predict(quad.model.kt, list(tr = xvals, tr2 = xvals^2))

#plot(tr, data1$sci_know)
#lines(xvals, predictedcounts, col = "darkgreen", lwd = 2)

hy <- data1$Hype

hy2 <- hy*hy
linear.model.kh <- lm(data1$sci_know~data1$Hype)
quad.model.kh <- lm(data1$sci_know~hy+ hy2)

xvals <- seq(-2, 2, 1)
predictedcounts <- predict(quad.model.kh, list(hy = xvals, hy2 = xvals^2))

#plot(hy, data1$sci_know)
#lines(xvals, predictedcounts, col = "darkgreen", lwd = 2)

#dev.off()
#gender effects	
sex <- data1$RS_Sex

x1 <- as.character(sex[data1$Trust==2])
x2<- as.character(sex[data1$Trust==1])
x3<- as.character(sex[data1$Trust==0])
x4 <- as.character(sex[data1$Trust==-1])
x5 <- as.character(sex[data1$Trust==-2])

x1.m <- x1[x1=="Male" & !is.na(x1)]
x1.f <- x1[x1=="Female" & !is.na(x1)]	

x1.sr <- length(x1.m)/(length(x1.f) + length(x1.m))

x2.m <- x2[x2=="Male" & !is.na(x2)]
x2.f <- x2[x2=="Female" & !is.na(x2)]

x2.sr <- length(x2.m)/(length(x2.f) + length(x2.m))	

x3.m <- x3[x3=="Male" & !is.na(x3)]
x3.f <- x3[x3=="Female" & !is.na(x3)]

x3.sr <- length(x3.m)/(length(x3.f) + length(x3.m))	

x4.m <- x4[x4=="Male" & !is.na(x4)]
x4.f <- x4[x4=="Female" & !is.na(x4)]

x4.sr <- length(x4.m)/(length(x4.f) + length(x4.m))

x5.m <- x5[x5=="Male" & !is.na(x5)]
x5.f <- x5[x5=="Female" & !is.na(x5)]	

x5.sr <- length(x5.m)/(length(x5.f) + length(x5.m))

srs <- c(x1.sr, x2.sr, x3.sr, x4.sr, x5.sr)


lbls <- c("Trust--", "Trust-", "Neutral", "Trust+", "Trust++")

pdf("Fig6abc_SR.pdf", height = 8, width=15)
par(mfrow=c(1,3))
par(pty="s")

boxplot(x5.sr, x4.sr, x3.sr, x2.sr, x1.sr, names = lbls, ylab="Sex ratio (m/(m+f)", main ="A.")

males <- c(length(x1.m), length(x2.m), length(x3.m), length(x4.m), length(x5.m))
females <- c(length(x1.f), length(x2.f), length(x3.f), length(x4.f), length(x5.f))

 M.trust <- as.table(rbind(males, females))
 chi.trust <- chisq.test(M.trust)
 
 
 
x1 <- as.character(sex[data1$Hype==2])
x2<- as.character(sex[data1$Hype==1])
x3<- as.character(sex[data1$Hype==0])
x4 <- as.character(sex[data1$Hype==-1])
x5 <- as.character(sex[data1$Hype==-2])

x1.m <- x1[x1=="Male" & !is.na(x1)]
x1.f <- x1[x1=="Female" & !is.na(x1)]	

x1.sr <- length(x1.m)/(length(x1.f) + length(x1.m))

x2.m <- x2[x2=="Male" & !is.na(x2)]
x2.f <- x2[x2=="Female" & !is.na(x2)]

x2.sr <- length(x2.m)/(length(x2.f) + length(x2.m))	

x3.m <- x3[x3=="Male" & !is.na(x3)]
x3.f <- x3[x3=="Female" & !is.na(x3)]

x3.sr <- length(x3.m)/(length(x3.f) + length(x3.m))	

x4.m <- x4[x4=="Male" & !is.na(x4)]
x4.f <- x4[x4=="Female" & !is.na(x4)]

x4.sr <- length(x4.m)/(length(x4.f) + length(x4.m))

x5.m <- x5[x5=="Male" & !is.na(x5)]
x5.f <- x5[x5=="Female" & !is.na(x5)]	

x5.sr <- length(x5.m)/(length(x5.f) + length(x5.m))

srs <- c(x1.sr, x2.sr, x3.sr, x4.sr, x5.sr)


lbls <- c("Hype++", "Hype+", "Neutral", "Hype-", "Hype--")

boxplot(x5.sr, x4.sr, x3.sr, x2.sr, x1.sr, names = lbls, ylab="Sex ratio (m/(m+f)", main ="B.")
par(pty="s")


males <- c(length(x1.m), length(x2.m), length(x3.m), length(x4.m), length(x5.m))
females <- c(length(x1.f), length(x2.f), length(x3.f), length(x4.f), length(x5.f))

 M.hype <- as.table(rbind(males, females))
 chi.hype <- chisq.test(M.hype)
 
# dev.off()
 
 
#pdf("FigS1b.pdf")  


x1 <- as.character(sex[data1$GM_benefits==2])
x2<- as.character(sex[data1$GM_benefits==1])
x3<- as.character(sex[data1$GM_benefits==0])
x4 <- as.character(sex[data1$GM_benefits==-1])
x5 <- as.character(sex[data1$GM_benefits==-2])

x1.m <- x1[x1=="Male" & !is.na(x1)]
x1.f <- x1[x1=="Female" & !is.na(x1)]	

x1.sr <- length(x1.m)/(length(x1.f) + length(x1.m))

x2.m <- x2[x2=="Male" & !is.na(x2)]
x2.f <- x2[x2=="Female" & !is.na(x2)]

x2.sr <- length(x2.m)/(length(x2.f) + length(x2.m))	

x3.m <- x3[x3=="Male" & !is.na(x3)]
x3.f <- x3[x3=="Female" & !is.na(x3)]

x3.sr <- length(x3.m)/(length(x3.f) + length(x3.m))	

x4.m <- x4[x4=="Male" & !is.na(x4)]
x4.f <- x4[x4=="Female" & !is.na(x4)]

x4.sr <- length(x4.m)/(length(x4.f) + length(x4.m))

x5.m <- x5[x5=="Male" & !is.na(x5)]
x5.f <- x5[x5=="Female" & !is.na(x5)]	

x5.sr <- length(x5.m)/(length(x5.f) + length(x5.m))

srs <- c(x1.sr, x2.sr, x3.sr, x4.sr, x5.sr)


lbls <- c("GM--", "GM-", "Neutral", "GM+", "GM++")

boxplot(x5.sr, x4.sr, x3.sr, x2.sr, x1.sr, names = lbls, ylab="Sex ratio (m/(m+f)", main = "C.")
par(pty="s")


males <- c(length(x1.m), length(x2.m), length(x3.m), length(x4.m), length(x5.m))
females <- c(length(x1.f), length(x2.f), length(x3.f), length(x4.f), length(x5.f))

 M.GM <- as.table(rbind(males, females))
 chi.GM <- chisq.test(M.GM)
 
 dev.off()
 
 pdf("FigS2b.pdf")
 
x1 <- as.character(sex[data1$CovVac=="Yes, and I have already been vaccinated"])
x2<- as.character(sex[data1$CovVac=="Yes, but I am yet to be vaccinated"])
x3<- as.character(sex[data1$CovVac=="No, I would not get vaccinated"])
x4 <- as.character(sex[data1$CovVac=="Prefer not to answer"])

x1.m <- x1[x1=="Male" & !is.na(x1)]
x1.f <- x1[x1=="Female" & !is.na(x1)]	

x1.sr <- length(x1.m)/(length(x1.f) + length(x1.m))

x2.m <- x2[x2=="Male" & !is.na(x2)]
x2.f <- x2[x2=="Female" & !is.na(x2)]

x2.sr <- length(x2.m)/(length(x2.f) + length(x2.m))	

x3.m <- x3[x3=="Male" & !is.na(x3)]
x3.f <- x3[x3=="Female" & !is.na(x3)]

x3.sr <- length(x3.m)/(length(x3.f) + length(x3.m))	

x4.m <- x4[x4=="Male" & !is.na(x4)]
x4.f <- x4[x4=="Female" & !is.na(x4)]

x4.sr <- length(x4.m)/(length(x4.f) + length(x4.m))

srs <- c(x1.sr, x2.sr, x3.sr, x4.sr)


lbls <- c("Had it", "Will have it", "Reject", "no answer")

boxplot(x1.sr, x2.sr, x3.sr, x4.sr, names = lbls, ylab="Sex ratio (m/(m+f)")



males <- c(length(x1.m), length(x2.m), length(x3.m), length(x4.m))
females <- c(length(x1.f), length(x2.f), length(x3.f), length(x4.f))

 M.vacc <- as.table(rbind(males, females))
 chi.vacc <- chisq.test(M.vacc)
 

 
dev.off()


#gm and educational level
    
    gm <- as.numeric(as.character(data1$GM_benefits))
    
x1 <- edu[gm==-2 ]
x2 <- edu[gm==-1 ]
x3 <- edu[gm==0 ]
x4 <- edu[gm==1 ]
x5 <- edu[gm==2 ]

slices.gm <- c(length(x1), length(x2), length(x3), length(x4), length(x5))

lbls.gm <- c("GM--", "GM-", "Neutral", "GM+", "GM++")
pct.gm <- round(slices.gm/sum(slices.gm)*100)


data.gmfr <- data.frame(name=lbls.gm, value=pct.gm)

pdf("FigS1a_FreqGM.pdf")

barplot(pct.gm, names.arg=lbls.gm, ylab = "%")

dev.off()

#dev.off()



x1.0 <- length(x1[x1==0 & !is.na(x1)])/length(x1[!is.na(x1)])
x1.1 <- length(x1[x1==1 & !is.na(x1)])/length(x1[!is.na(x1)])
x1.2 <- length(x1[x1==2 & !is.na(x1)])/length(x1[!is.na(x1)])

x2.0 <- length(x2[x2==0 & !is.na(x2)])/length(x2[!is.na(x2)])
x2.1 <- length(x2[x2==1 & !is.na(x2)])/length(x2[!is.na(x2)])
x2.2 <- length(x2[x2==2 & !is.na(x2)])/length(x2[!is.na(x2)])

x3.0 <- length(x3[x3==0 & !is.na(x3)])/length(x3[!is.na(x3)])
x3.1 <- length(x3[x3==1 & !is.na(x3)])/length(x3[!is.na(x3)])
x3.2 <- length(x3[x3==2 & !is.na(x3)])/length(x3[!is.na(x3)])

x4.0 <- length(x4[x4==0 & !is.na(x4)])/length(x4[!is.na(x4)])
x4.1 <- length(x4[x4==1 & !is.na(x4)])/length(x4[!is.na(x4)])
x4.2 <- length(x4[x4==2 & !is.na(x4)])/length(x4[!is.na(x4)])

x5.0 <- length(x5[x5==0 & !is.na(x5)])/length(x5[!is.na(x5)])
x5.1 <- length(x5[x5==1 & !is.na(x5)])/length(x5[!is.na(x5)])
x5.2 <- length(x5[x5==2 & !is.na(x5)])/length(x5[!is.na(x5)])

prop.0 <- c(x1.0, x2.0, x3.0, x4.0, x5.0)
prop.1 <- c(x1.1, x2.1, x3.1, x4.1, x5.1)
prop.2 <- c(x1.2, x2.2, x3.2, x4.2, x5.2)

#plot(x, prop.0)
#plot(x, prop.1)
#plot(x, prop.2)

GM_levels <- as.factor(c(rep("GM--", 3), rep("GM-", 3), rep("Neutral", 3), rep("GM+", 3), rep("GM++", 3)))

Edu_levels <- rep(c("Degree level", "Not degree level", "None"), 5)

value <- c(x1.2, x1.1, x1.0, x2.2, x2.1, x2.0, x3.2, x3.1, x3.0, x4.2, x4.1, x4.0, x5.2, x5.1, x5.0) 



data5b <- data.frame(GM_levels, Edu_levels, value)
    
p1 <- ggplot(data5b, aes(fill=fct_inorder(Edu_levels), y=value, x=GM_levels)) + 
    geom_bar(position="stack", stat="identity")
    
    p1<- p1+ aes(x=fct_inorder(GM_levels))+ xlab("GM level")+ ylab("Frequency") + guides(fill=guide_legend(title="Educational qual")) + theme(aspect.ratio=1) 
   
   

pdf("FigS1b_Edu_GM.pdf", onefile =TRUE)
print(p1)
    dev.off()
    
    
    
    #scientific+genetic self assessment vs GM opinions

x1 <- data1$scigen_self_under[data1$GM_benefits==2]
x2<- data1$scigen_self_under[data1$GM_benefits==1]
x3<- data1$scigen_self_under[data1$GM_benefits==0]
x4 <- data1$scigen_self_under[data1$GM_benefits==-1]
x5 <- data1$scigen_self_under[data1$GM_benefits==-2]
x6 <- c(x1, x2)
x7<- c(x4, x5)

GMB <- as.factor(data1$GM_benefits)

res.aov.sg_gm <- aov(data1$scigen_self_under~GMB)



res.sg_gm <- summary(res.aov.sg_gm)

post_test.sg_gm <- glht(res.aov.sg_gm,
  linfct = mcp(GMB = "Tukey") )

tukey.res.sg_gm <- summary(post_test.sg_gm)




    
    pdf("Fig4ab_GM.pdf", width=15, height =8)
    


par(mfrow=c(1,2))
par(pty="s")
vioplot(x5, x4, x3, x2, x1, names=c("GM--", "GM-", "Neutral", "GM+", "GM++"), 
   col="gold", ylab ="Self-assessed understanding")
title("A")
#dev.off()


#scientific+genetic knowledge vs GM opinions

ct.gmsk <- cor.test(data1$GM_benefits,data1$sci_know, method="spearman")
ct.gmsa <- cor.test(sqrt(data1$GM_benefits*data1$GM_benefits),data1$scigen_self_under, method="spearman")
data1$modgm <- abs(as.numeric(as.character(data1$GM_benefits)))

ct.modgm.sg <- cor.test(data1$modgm, data1$scigen_self_under, method="spearman")

x1 <- data1$sci_know[data1$GM_benefits==2]
x2<- data1$sci_know[data1$GM_benefits==1]
x3<- data1$sci_know[data1$GM_benefits==0]
x4 <- data1$sci_know[data1$GM_benefits==-1]
x5 <- data1$sci_know[data1$GM_benefits==-2]
x6 <- c(x1, x2)
x7<- c(x4, x5)




res.aov.k_gm <- aov(data1$sci_know~GMB)



res.k_gm <- summary(res.aov.k_gm)

post_test.k_gm <- glht(res.aov.k_gm,
  linfct = mcp(GMB = "Tukey") )

tukey.res.k_gm <- summary(post_test.k_gm)



#pdf("SciKnowledge_v_GM.pdf")
par(pty="s")
vioplot(x5, x4, x3, x2, x1, names=c("GM--", "GM-", "Neutral", "GM+", "GM++"), 
   col="gold", ylab ="Scientific knowledge")
title("B")

dev.off()

gm <- as.numeric(as.character(data1$GM_benefits))

gm2 <- gm*gm
linear.model.kgm <- lm(data1$sci_know~gm)
quad.model.kgm <- lm(data1$sci_know~gm+ gm2)




xvals <- seq(-2, 2, 1)
predictedcounts <- predict(quad.model.kgm, list(gm = xvals, gm2 = xvals^2))

#plot(gm, data1$sci_know)
#lines(xvals, predictedcounts, col = "darkgreen", lwd = 2)

linear.model.sagm <- lm(data1$scigen_self_under~gm)
quad.model.sagm <- lm(data1$scigen_self_under~gm+ gm2)
#gap v trust and hype

 gap1 <-  sci_know - data1$scigen_self_under
pdf("Fig3Gap_v_attitude.pdf", height = 8, width=15)

par(mfrow=c(1,2))
x1 <- as.numeric(gap1[data1$Trust==2])
x2<- as.numeric(gap1[data1$Trust==1])
x3<- as.numeric(gap1[data1$Trust==0])
x4 <- as.numeric(gap1[data1$Trust==-1])
x5 <- as.numeric(gap1[data1$Trust==-2])



data1$Trust <- as.factor(data1$Trust)
res.aov.gapsd_tr <- aov(gap1~Trust, na.action=na.omit, data = data1)

res.gapsd_tr <- summary(res.aov.gapsd_tr)

post_test.gapsd_tr <- glht(res.aov.gapsd_tr,
  linfct = mcp(Trust = "Tukey") )

tukey.res.gapsd_tr <- summary(post_test.gapsd_tr)

ct.gapsd.tr <- cor.test(as.numeric(gap1), as.numeric(as.character(data1$Trust)), method="spearman")







vioplot(x5, x4, x3, x2, x1, names= c("Trust--", " Trust-", "Neutral", "Trust+", "Trust++"), 
   col="gold", ylab ="OSD", cex.axis=1.10, na.rm=T)
title("A.")


x1 <- as.numeric(gap1[data1$Hype==2])
x2<- as.numeric(gap1[data1$Hype==1])
x3<- as.numeric(gap1[data1$Hype==0])
x4 <- as.numeric(gap1[data1$Hype==-1])
x5 <- as.numeric(gap1[data1$Hype==-2])


data1$Hype <- as.factor(data1$Hype)
res.aov.gapsd_hy <- aov(gap1~Hype, na.action=na.omit, data = data1)

res.gapsd_hy <- summary(res.aov.gapsd_hy)

post_test.gapsd_hy <- glht(res.aov.gapsd_hy,
  linfct = mcp(Hype = "Tukey") )

tukey.res.gapsd_hy <- summary(post_test.gapsd_hy)

ct.gapsd.hy <- cor.test(as.numeric(gap1), as.numeric(as.character(data1$Hype)), method="spearman")
ct.gapsd.gm <- cor.test(as.numeric(gap1), as.numeric(as.character(data1$GM_benefits)), method="spearman")


vioplot(x5, x4, x3, x2, x1, names= c("hyped++", "hyped+", "neutral", "hyped-", "hyped--"), 
   col="gold", ylab ="OSD", cex.axis=1.10, na.rm=T)
title("B.")
dev.off()       

#scientific+genetic self assessment and vaccine uptake

x1 <- data1$scigen_self_under[data1$CovVac=="Yes, and I have already been vaccinated"]
x2<- data1$scigen_self_under[data1$CovVac=="Yes, but I am yet to be vaccinated"]
x3<- data1$scigen_self_under[data1$CovVac=="No, I would not get vaccinated"]
x4 <- data1$scigen_self_under[data1$CovVac=="Prefer not to answer"]

x5 <- c(x1, x2)

res.aov.sg_v <- aov(scigen_self_under~CovVac, data = data1)

res.sg_v <- summary(res.aov.sg_v)

post_test.sg_v <- glht(res.aov.sg_v,
  linfct = mcp(CovVac = "Tukey") )

tukey.res.sg_v <- summary(post_test.sg_v)

tsa_v <- table_glht(tukey.res.sg_v)
stargazer(tsa_v, type='html', out = "TableS2a_Tukey_sa_vac.html")


pdf("Fig5_vaccine.pdf", width=20, height =8)
par(mfrow=c(1,3))
par(pty="s")
vioplot(x1, x2, x3, x4, names=c("Had it", "Will have it", "Reject", "no answer"), 
   col="gold", ylab ="Self assessed understanding", cex.axis=1.5)
title("A")

x1 <- data1$sci_know[data1$CovVac=="Yes, and I have already been vaccinated"]
x2<- data1$sci_know[data1$CovVac=="Yes, but I am yet to be vaccinated"]
x3<- data1$sci_know[data1$CovVac=="No, I would not get vaccinated"]
x4 <- data1$sci_know[data1$CovVac=="Prefer not to answer"]

x5 <- c(x1, x2)

res.aov.k_v <- aov(sci_know~CovVac, data = data1)

res.k_v <- summary(res.aov.k_v)

post_test.k_v <- glht(res.aov.k_v,
linfct = mcp(CovVac = "Tukey")
 )

tukey.res.k_v <- summary(post_test.k_v)

tk_v <- table_glht(tukey.res.k_v)
stargazer(tk_v, type='html', out = "TableS2b_Tukey_know_vac.html")

par(pty="s")
vioplot(x1, x2, x3, x4, names=c("Had it", "Will have it", "Reject", "no answer"), 
   col="gold", ylab ="Scientific knowledge", cex.axis=1.5)
title("B")

x1 <- as.numeric(gap1[data1$CovVac=="Yes, and I have already been vaccinated"])
x2<- as.numeric(gap1[data1$CovVac=="Yes, but I am yet to be vaccinated"])
x3<- as.numeric(gap1[data1$CovVac=="No, I would not get vaccinated"])
x4 <- as.numeric(gap1[data1$CovVac=="Prefer not to answer"])

x5 <- c(x1, x2)

res.aov.g_v <- aov(as.numeric(gap1)~CovVac, data = data1)

res.gapvac <- summary(res.aov.g_v)

post_test.g_v <- glht(res.aov.g_v,
linfct = mcp(CovVac = "Tukey")
 )

tukey.res.g_v <- summary(post_test.g_v)

tgap_v <- table_glht(tukey.res.g_v)
stargazer(tgap_v, type='html', out = "TableS2c_Tukey_gap_vac.html")

wilcox.test(x5, x3)

mean(x5)
mean(x2)

par(pty="s")
vioplot(x1, x2, x3, x4, names=c("Had it", "Will have it", "Reject", "no answer"), 
   col="gold", ylab ="OSD", cex.axis=1.5, na.rm=T)
title("C")



dev.off()

#vaccine and educational level
edu <- as.numeric(as.character(data1$education_level))


x1 <- as.numeric(edu[data1$CovVac=="Yes, and I have already been vaccinated"])
x2<- as.numeric(edu[data1$CovVac=="Yes, but I am yet to be vaccinated"])
x3<- as.numeric(edu[data1$CovVac=="No, I would not get vaccinated"])
x4 <- as.numeric(edu[data1$CovVac=="Prefer not to answer"])




slices.v <- c(length(x1), length(x2), length(x3), length(x4))

lbls.v <- c("Had it", "Will have it", "Reject", "no answer")
pct.v <- round(slices.v/sum(slices.v)*100)
#lbls.tr <- paste(lbls, pct) # add percents to labels 
#lbls.tr <- paste(lbls,"%",sep="") # ad % to labels 

data.vfr <- data.frame(name=lbls.v, value=pct.v)



x1.0 <- length(x1[x1==0 & !is.na(x1)])/length(x1[!is.na(x1)])
x1.1 <- length(x1[x1==1 & !is.na(x1)])/length(x1[!is.na(x1)])
x1.2 <- length(x1[x1==2 & !is.na(x1)])/length(x1[!is.na(x1)])

x2.0 <- length(x2[x2==0 & !is.na(x2)])/length(x2[!is.na(x2)])
x2.1 <- length(x2[x2==1 & !is.na(x2)])/length(x2[!is.na(x2)])
x2.2 <- length(x2[x2==2 & !is.na(x2)])/length(x2[!is.na(x2)])

x3.0 <- length(x3[x3==0 & !is.na(x3)])/length(x3[!is.na(x3)])
x3.1 <- length(x3[x3==1 & !is.na(x3)])/length(x3[!is.na(x3)])
x3.2 <- length(x3[x3==2 & !is.na(x3)])/length(x3[!is.na(x3)])

x4.0 <- length(x4[x4==0 & !is.na(x4)])/length(x4[!is.na(x4)])
x4.1 <- length(x4[x4==1 & !is.na(x4)])/length(x4[!is.na(x4)])
x4.2 <- length(x4[x4==2 & !is.na(x4)])/length(x4[!is.na(x4)])



prop.0 <- c(x1.0, x2.0, x3.0, x4.0)
prop.1 <- c(x1.1, x2.1, x3.1, x4.1)
prop.2 <- c(x1.2, x2.2, x3.2, x4.2)

#plot(x, prop.0)
#plot(x, prop.1)
#plot(x, prop.2)

V_levels <- as.factor(c(rep("Had it", 3), rep("Will have it", 3), rep("Reject", 3), rep("No answer", 3)))

Edu_levels <- rep(c("Degree level", "Not degree level", "None"), 4)

value <- c(x1.2, x1.1, x1.0, x2.2, x2.1, x2.0, x3.2, x3.1, x3.0, x4.2, x4.1, x4.0) 



data5c <- data.frame(V_levels, Edu_levels, value)
    
p <- ggplot(data5c, aes(fill=fct_inorder(Edu_levels), y=value, x=V_levels)) + 
    geom_bar(position="stack", stat="identity")
    
    p<- p+ aes(x=fct_inorder(V_levels))+ xlab("Vaccine status")+ ylab("Frequency") + guides(fill=guide_legend(title="Educational qual")) + theme(aspect.ratio=1) 
    
pdf("FigS2c_Edu_v.pdf")


    print(p)
    dev.off()
    
    pdf("FigS2a_Freq.pdf")


barplot(pct.v, names.arg=lbls.v, ylab = "%")

dev.off()


#engagement and attitude

#pdf("Engagement_TrustHype.pdf", height =8, width =15)

#par(pty="s")
#par(mfrow=c(1,2))

x1 <- data1$SciRelations[data1$Trust==-2]
x2<- data1$SciRelations[data1$Trust==-1]
x3<- data1$SciRelations[data1$Trust==0]
x4 <- data1$SciRelations[data1$Trust==1]
x5 <- data1$SciRelations[data1$Trust==2]

#vioplot(x1, x2, x3, x4, x5, names=c("Trust --", "Trust-", "neutral", "Trust+", "Trust++"), 
#   col="gold", ylab ="Engagement", cex.axis=1.2 )
   
   res.aov.e_t <- aov(data1$SciRelations~Trust, data = data1)

res.e_t <- summary(res.aov.e_t)

post_test.e_t <- glht(res.aov.e_t,
linfct = mcp(Trust= "Tukey")
 )

tukey.res.e_t <- summary(post_test.e_t)

te_t <- table_glht(tukey.res.e_t)



        

x1 <- data1$SciRelations[data1$Hype==-2]
x2<- data1$SciRelations[data1$Hype==-1]
x3<- data1$SciRelations[data1$Hype==0]
x4 <- data1$SciRelations[data1$Hype==1]
x5 <- data1$SciRelations[data1$Hype==2]

#vioplot(x1, x2, x3, x4, x5, names=c("Hype ++", "Hype", "neutral", "Hype-", "Hype--"), 
 #  col="gold", ylab ="Engagement", cex.axis=1.2 )
   
#   dev.off()

  dataA <- read.csv(file = "survey_data.csv", header=TRUE)
  
  #the quiz statements are recoded from +1 definite and not true aor else zero

dataA$Statements_1 <- as.numeric(as.character(revalue(dataA$Statements_1, c("Definitely true"=2, "Probably true"=1,"Probably false"=-1, "Definitely false"=-2, "Don't know"=0 ))))
dataA$Statements_2 <- as.numeric(as.character(revalue(dataA$Statements_2, c("Definitely true"=2, "Probably true"=1,"Probably false"=-1, "Definitely false"=-2, "Don't know"=0 ))))
dataA$Statements_3 <- as.numeric(as.character(revalue(dataA$Statements_3, c("Definitely true"=2, "Probably true"=1,"Probably false"=-1, "Definitely false"=-2, "Don't know"=0 ))))
dataA$Statements_9 <- as.numeric(as.character(revalue(dataA$Statements_9, c("Definitely true"=2, "Probably true"=1,"Probably false"=-1, "Definitely false"=-2, "Don't know"=0 ))))
dataA$Statements_12 <- as.numeric(as.character(revalue(dataA$Statements_12, c("Definitely true"=2, "Probably true"=1,"Probably false"=-1, "Definitely false"=-2, "Don't know"=0 ))))
dataA$Statements_4 <- as.numeric(as.character(revalue(dataA$Statements_4, c("Definitely true"=-2, "Probably true"=-1,"Probably false"=1, "Definitely false"=2, "Don't know"=0 ))))
dataA$Statements_5 <- as.numeric(as.character(revalue(dataA$Statements_5, c("Definitely true"=-2, "Probably true"=-1,"Probably false"=1, "Definitely false"=2, "Don't know"=0 ))))
dataA$Statements_6 <- as.numeric(as.character(revalue(dataA$Statements_6, c("Definitely true"=-2, "Probably true"=-1,"Probably false"=1, "Definitely false"=2, "Don't know"=0 ))))
dataA$Statements_8 <- as.numeric(as.character(revalue(dataA$Statements_8, c("Definitely true"=-2, "Probably true"=-1,"Probably false"=1, "Definitely false"=2, "Don't know"=0 ))))
dataA$Statements_11 <- as.numeric(as.character(revalue(dataA$Statements_11, c("Definitely true"=-2, "Probably true"=-1,"Probably false"=1, "Definitely false"=2, "Don't know"=0 ))))
#each participant is given a score range -1 to +1 for scientific knowledge


#rows <- c(17:20,22,24:28)
rows <- c(17:28)
sciK_all <- data1[,rows]
sciK_all_anal <- dataA[,rows]

CronAlpha.sk <- cronbach.alpha(sciK_all, na.rm=TRUE, CI=TRUE)
CronAlpha.sk.anal <- cronbach.alpha(sciK_all_anal, na.rm=TRUE, CI=TRUE)



self_all <- data1[,1:6]
#self_all <- data1[,3:6]

CronAlpha.sa <- cronbach.alpha(self_all, na.rm=TRUE, CI=TRUE)

#sex and gap

gap.m <- as.numeric(gap1[data1$RS_Sex=="Male"])
gap.m <- gap.m[!is.na(gap.m)]

gap.f <- as.numeric(gap1[data1$RS_Sex=="Female"])
gap.f<- gap.f[!is.na(gap.f)]
#pdf("Gap_sex.pdf")
#vioplot(gap.m, gap.f, names=c("Male", "Female"), 
#   col="gold", ylab ="Gap", cex.axis=1.2 )

#dev.off()

wt.sr.gap <- wilcox.test(gap.m, gap.f)
m.gapm <- median(gap.m)
m.gapf <- median(gap.f)



for (i in rows) {
ct <-cor.test(data1[,i], tr, method="spearman")
q <- i -16
#print(paste("Trust", q, ct$estimate, ct$p.value))

if (q==1) {

df.tr <- tibble(Question = q,rho = ct$estimate, P = ct$p.value)

} else {
df.tr <-  add_row(df.tr, Question = q, rho = ct$estimate, P =ct$p.value)
}


}
write.csv(df.tr, "TableS1_1TrustV12Qs.csv")

for (i in rows) {
ct <-cor.test(data1[,i], hy, method="spearman")
q <- i -16
#print(paste("Hype", q, ct$estimate, ct$p.value))

if (q==1) {

df.hy <- tibble(x = q,y = ct$estimate, z = ct$p.value)

} else {
df.hy <-  add_row(df.hy, x = q, y = ct$estimate, z =ct$p.value)
}


}
write.csv(df.hy, "TableS1_2HypeV12Qs.csv")

for (i in rows) {
ct <-cor.test(data1[,i], gm, method="spearman")
q <- i -16
#print(paste("GM", q, ct$estimate, ct$p.value))

if (q==1) {

df.gm <- tibble(x = q,y = ct$estimate, z = ct$p.value)

} else {
df.gm <-  add_row(df.gm, x = q, y = ct$estimate, z =ct$p.value)
}


}

write.csv(df.gm, "TableS1_3GMV12Qs.csv")



#edu, age, rel vs Trust

data3 <- data.frame(edu = as.numeric(as.character(data1$education_level)), age = as.numeric(data1$Age), rel=as.numeric(as.character(data1$religion)), Trust =as.numeric(as.character(data1$Trust)))
 data3.narm <- data3[complete.cases(data3),]
multi_pcor_trust<- pcor(data3.narm, method="spearman")

res3 <- rcorr(as.matrix(data3.narm), type = "spearman")
res3P  <-replace_na(res3$P, 0)


#edu, age, rel vs hype

data4 <- data.frame(edu = as.numeric(as.character(data1$education_level)), age = as.numeric(data1$Age), rel=as.numeric(as.character(data1$religion)), Hype =as.numeric(as.character(data1$Hype)))
 data4.narm <- data4[complete.cases(data4),]
multi_pcor_hype<- pcor(data4.narm, method="spearman")

res4 <- rcorr(as.matrix(data4.narm), type = "spearman")
res4P  <-replace_na(res4$P, 0)


#edu, age, rel vs gm

data5 <- data.frame(edu = as.numeric(as.character(data1$education_level)), age = as.numeric(data1$Age), rel=as.numeric(as.character(data1$religion)), GM =as.numeric(as.character(data1$GM_benefits)))
 data5.narm <- data5[complete.cases(data5),]
multi_pcor_gm<- pcor(data5.narm, method="spearman")

res5 <- rcorr(as.matrix(data5.narm), type = "spearman")
res5P  <-replace_na(res5$P, 0)

#edu, age, rel, gap

data2 <- data.frame(edu = as.numeric(as.character(data1$education_level)), age = as.numeric(data1$Age), rel=as.numeric(as.character(data1$religion)), gap =as.numeric(gap1))
 data2.narm <- data2[complete.cases(data2),]
multi_pcor_gap<- pcor(data2.narm, method="spearman")
res2a <- rcorr(as.matrix(data2.narm), type = "spearman")
res2aP  <-replace_na(res2a$P, 0)

#edu, age, rel, gap, trust for table 2a

data6 <- data.frame(edu = as.numeric(as.character(data1$education_level)), age = as.numeric(data1$Age), rel=as.numeric(as.character(data1$religion)), gap =as.numeric(gap1), Trust =as.numeric(as.character(data1$Trust)))
data6.narm <- data6[complete.cases(data6),]
multi_pcor_gap.tr <- pcor(data6.narm, method="spearman")
res6a <- rcorr(as.matrix(data6.narm), type = "spearman")
res6aP  <-replace_na(res6a$P, 0)

Table2a_top <- corstars(data6.narm, method="spearman",removeTriangle="lower", result="html")
#Table1a_top <- gsub("Statements_", "Q ", Table1a_top)

write.table(Table2a_top, file="Table2a_top.html")
Table2a_bot <- corstars_partial(data6.narm, method="spearman",removeTriangle="upper", result="html")
write.table(Table2a_bot, file="Table2a_bot.html")


#edu, age, rel, gap, hype for table 2b

data7 <- data.frame(edu = as.numeric(as.character(data1$education_level)), age = as.numeric(data1$Age), rel=as.numeric(as.character(data1$religion)), gap =as.numeric(gap1), Hype =as.numeric(as.character(data1$Hype)))
 data7.narm <- data7[complete.cases(data7),]
multi_pcor_gap.hy <- pcor(data7.narm, method="spearman")
res7a <- rcorr(as.matrix(data7.narm), type = "spearman")
res7aP  <-replace_na(res6a$P, 0)


Table2b_top <- corstars(data7.narm, method="spearman",removeTriangle="lower", result="html")
#Table1a_top <- gsub("Statements_", "Q ", Table1a_top)

write.table(Table2b_top, file="Table2b_top.html")

Table2b_bot <- corstars_partial(data7.narm, method="spearman",removeTriangle="upper", result="html")
write.table(Table2b_bot, file="Table2b_bot.html")

#edu, age, rel vs gm, gap for table 5

data8 <- data.frame(edu = as.numeric(as.character(data1$education_level)), age = as.numeric(data1$Age), rel=as.numeric(as.character(data1$religion)), gap =as.numeric(gap1),GM =as.numeric(as.character(data1$GM_benefits)))
 data8.narm <- data8[complete.cases(data8),]
multi_pcor_gm.gap <- pcor(data8.narm, method="spearman")

res8 <- rcorr(as.matrix(data8.narm), type = "spearman")
res8P  <-replace_na(res8$P, 0)
Table5_top <- corstars(data8.narm, method="spearman",removeTriangle="lower", result="html")
#Table1a_top <- gsub("Statements_", "Q ", Table1a_top)

write.table(Table5_top, file="Table5_top.html")

Table5_bot <- corstars_partial(data8.narm, method="spearman",removeTriangle="upper", result="html")
write.table(Table5_bot, file="Table5_bot.html")


#dip test on most negative

x5.tr <- data1$sci_know[data1$Trust==-2]
x5.tr<- x5.tr[!is.na(x5.tr)]
x5.hy <- data1$sci_know[data1$Hype==-2]
x5.hy<- x5.hy[!is.na(x5.hy)]
x5.gm <- data1$sci_know[data1$GM_benefits==-2]
x5.gm<- x5.gm[!is.na(x5.gm)]
x5.va <-data1$sci_know[data1$CovVac=="No, I would not get vaccinated"]
x5.va<- x5.va[!is.na(x5.va)]

d.tr <- dip.test(x5.tr)
length(x5.tr[!is.na(x5.tr)])
d.hy <- dip.test(x5.hy)
length(x5.hy[!is.na(x5.hy)])
d.gm <- dip.test(x5.gm)
length(x5.gm[!is.na(x5.gm)])
d.va <- dip.test(x5.va)
length(x5.va[!is.na(x5.va)])

min.l <- length(x5.hy)

tr.sub.dips <-c()
hy.sub.dips <-c()
gm.sub.dips <-c()
va.sub.dips <-c()

for (i in c(1:nrands)) {
x5.tr.sub <- sample(x5.tr, min.l, replace = FALSE)
d.tr.sub <- dip.test(x5.tr.sub)
x5.hy.sub <- sample(x5.hy, min.l, replace = TRUE)
d.hy.sub <- dip.test(x5.hy.sub)
x5.gm.sub <- sample(x5.gm, min.l, replace = FALSE)
d.gm.sub <- dip.test(x5.gm.sub)
x5.va.sub <- sample(x5.va, min.l, replace = FALSE)
d.va.sub <- dip.test(x5.va.sub)

tr.sub.dips <-append(tr.sub.dips, d.tr.sub$statistic)
hy.sub.dips <-append(hy.sub.dips, d.hy.sub$statistic)
gm.sub.dips <-append(gm.sub.dips, d.gm.sub$statistic)
va.sub.dips <-append(va.sub.dips, d.va.sub$statistic)


}

tr.sub.dips.sig <- length(tr.sub.dips[tr.sub.dips>0.09])

propsig.tr <- tr.sub.dips.sig /length(tr.sub.dips)

gm.sub.dips.sig <- length(gm.sub.dips[gm.sub.dips>0.09])
propsig.gm <- gm.sub.dips.sig /length(gm.sub.dips)
va.sub.dips.sig <- length(va.sub.dips[va.sub.dips>0.09])

propsig.va <- va.sub.dips.sig /length(va.sub.dips)

hy.sub.dips.sig <- length(hy.sub.dips[hy.sub.dips>0.09])

propsig.hy <- hy.sub.dips.sig /length(hy.sub.dips)


vars <- c("Trust", "Hype", "GM", "Vaccine")
dip <- c(d.tr$statistic, d.hy$statistic, d.gm$statistic, d.va$statistic)
P <- c(d.tr$p.value, d.hy$p.value, d.gm$p.value, d.va$p.value)
N <- c(length(x5.tr[!is.na(x5.tr)]), length(x5.hy[!is.na(x5.hy)]),length(x5.gm[!is.na(x5.gm)]),length(x5.va[!is.na(x5.va)]))
means.sub <- c(median(tr.sub.dips), median(hy.sub.dips), median(gm.sub.dips), median(va.sub.dips))
sd.sub <- c(propsig.tr, propsig.hy, propsig.gm, propsig.va)


data6 <- data.frame(Variable = vars, Dip=signif(dip, 3), P=signif(P, 3), N=N, Median = signif(means.sub, 3), Prop_sig = signif(sd.sub, 3) )

pdf("TableS3.pdf")
grid.table(data6, rows=NULL)
dev.off()


#calc slopes and P values of diff

hy.kn <-summary(lm(data1$sci_know[hy <=0]~hy[hy <=0]))
slp.hy.kn <- hy.kn$coefficients[2,1]
sem.hy.kn <-hy.kn$coefficients[2,2]
hy.su <- summary(lm(data1$scigen_self_under[hy <=0]~hy[hy <=0]))
slp.hy.su <- hy.su$coefficients[2,1]
sem.hy.su <-hy.su$coefficients[2,2]


t.hy <- (slp.hy.su - slp.hy.kn )/sqrt(sem.hy.kn^2 + sem.hy.su^2)
df.hy <- length(data1$sci_know[hy <=0]) + length(data1$scigen_self_under[hy <=0]) -4
P.hy <- pt(t.hy, df.hy, lower.tail=TRUE)

tr.kn <-summary(lm(data1$sci_know[tr <=0]~tr[tr <=0]))
slp.tr.kn <- tr.kn$coefficients[2,1]
sem.tr.kn <-tr.kn$coefficients[2,2]
tr.su <- summary(lm(data1$scigen_self_under[tr <=0]~tr[tr <=0]))
slp.tr.su <- tr.su$coefficients[2,1]
sem.tr.su <-tr.su$coefficients[2,2]


t.tr <- (slp.tr.su - slp.tr.kn )/sqrt(sem.tr.kn^2 + sem.tr.su^2)
df.tr <- length(data1$sci_know[tr <=0]) + length(data1$scigen_self_under[tr <=0]) -4
P.tr <- pt(t.tr, df.tr, lower.tail=TRUE)

head.line <- c("slope_subjective","sem", "slope_obj", "sem", "t", "P", "df" )
tr.line <- c(slp.tr.su,sem.tr.su, slp.tr.kn,sem.tr.kn,t.tr,P.tr,df.tr )
hy.line <- c(slp.hy.su,sem.hy.su, slp.hy.kn,sem.hy.kn,t.hy,P.hy,df.hy )

gm.kn <-summary(lm(data1$sci_know[gm <=0]~gm[gm <=0]))
slp.gm.kn <- gm.kn$coefficients[2,1]
sem.gm.kn <-gm.kn$coefficients[2,2]
gm.su <- summary(lm(data1$scigen_self_under[gm <=0]~gm[gm <=0]))
slp.gm.su <- gm.su$coefficients[2,1]
sem.gm.su <-gm.su$coefficients[2,2]


t.gm <- (slp.gm.su - slp.gm.kn )/sqrt(sem.gm.kn^2 + sem.gm.su^2)
df.gm <- length(data1$sci_know[gm <=0]) + length(data1$scigen_self_under[gm <=0]) -4
P.gm <- pt(t.gm, df.gm, lower.tail=TRUE)
gm.line <- c(slp.gm.su,sem.gm.su, slp.gm.kn,sem.gm.kn,t.gm,P.gm,df.gm )


df <- data.frame(attitude=head.line,
				trust = tr.line,
				hype= hy.line,
				GM = gm.line)
				
is.num <- sapply(df, is.numeric)
df[is.num] <- lapply(df[is.num], round, 4)
				
write.table(t(df), "Table 4.csv", quote=FALSE, sep=",")
				

#analysis using Z scores


Zknow <- (data1$sci_know - mean(data1$sci_know))/sd(data1$sci_know)
Zunder <- (data1$scigen_self_under - mean(data1$scigen_self_under, na.rm=TRUE))/sd(data1$scigen_self_under,na.rm=TRUE)

List <-data.frame(x=Zknow, y=Zunder)
data1$gap <- as.numeric(ifelse(!is.na(Zknow) & !is.na(Zunder), Zknow - Zunder, "NA"))
gap <- ifelse(!is.na(Zknow) & !is.na(Zunder), Zknow - Zunder, "NA")

#table 3 b

data2.z <- data.frame(edu = as.numeric(as.character(data1$education_level)), age = as.numeric(data1$Age), rel=as.numeric(as.character(data1$religion)), gap =as.numeric(gap))
data2.z.narm <- data2.z[complete.cases(data2.z),]
multi_pcor_gap.z<- pcor(data2.z.narm, method="spearman")
res2a.z <- rcorr(as.matrix(data2.z.narm), type = "spearman")
res2aP.z  <-replace_na(res2a$P, 0)

Table3b_top <- corstars(data2.z.narm, method="spearman",removeTriangle="lower", result="html")
#Table1a_top <- gsub("Statements_", "Q ", Table1a_top)

write.table(Table3b_top, file="Table3b_top.html")

Table3b_bot <- corstars_partial(data2.z.narm, method="spearman",removeTriangle="upper", result="html")
write.table(Table3b_bot, file="Table3b_bot.html")


# table 3a

#edu, age, rel, gap

data2 <- data.frame(edu = as.numeric(as.character(data1$education_level)), age = as.numeric(data1$Age), rel=as.numeric(as.character(data1$religion)), gap =as.numeric(gap1))
data2.narm <- data2[complete.cases(data2),]
multi_pcor_gap<- pcor(data2.narm, method="spearman")
res2a <- rcorr(as.matrix(data2.narm), type = "spearman")
res2aP  <-replace_na(res2a$P, 0)

Table3a_top <- corstars(data2.narm, method="spearman",removeTriangle="lower", result="html")
#Table1a_top <- gsub("Statements_", "Q ", Table1a_top)

write.table(Table3a_top, file="Table3a_top.html")

Table3a_bot <- corstars_partial(data2.narm, method="spearman",removeTriangle="upper", result="html")
write.table(Table3a_bot, file="Table3a_bot.html")

#result 2b heterogeneity in gap scores for z score measure

x1 <- as.numeric(gap[data1$Trust==2])
x2<- as.numeric(gap[data1$Trust==1])
x3<- as.numeric(gap[data1$Trust==0])
x4 <- as.numeric(gap[data1$Trust==-1])
x5 <- as.numeric(gap[data1$Trust==-2])



data1$Trust <- as.factor(data1$Trust)
res.aov.gap_tr <- aov(gap~Trust, na.action=na.omit, data = data1)

res.gap_tr <- summary(res.aov.gap_tr)

post_test.gap_tr <- glht(res.aov.gap_tr,
  linfct = mcp(Trust = "Tukey") )

tukey.res.gap_tr <- summary(post_test.gap_tr)

x1 <- as.numeric(gap[data1$Hype==2])
x2<- as.numeric(gap[data1$Hype==1])
x3<- as.numeric(gap[data1$Hype==0])
x4 <- as.numeric(gap[data1$Hype==-1])
x5 <- as.numeric(gap[data1$Hype==-2])


data1$Hype <- as.factor(data1$Hype)
res.aov.gap_hy <- aov(gap~Hype, na.action=na.omit, data = data1)

res.gap_hy <- summary(res.aov.gap_hy)

post_test.gap_hy <- glht(res.aov.gap_hy,
  linfct = mcp(Hype = "Tukey") )

tukey.res.gap_hy <- summary(post_test.gap_hy)



#make csv of correlation matrixes for knowledge questions - for all 12 questions

df.k <- data1[,17:28]


#rcx=rcorr(as.matrix(df.k), type="spearman")



    
#df.rcx.r=data.frame(rcx$r)
#df.rcx.p=data.frame(rcx$P)
#write.csv(df.rcx.r,'TableS4_top.csv') 
#write.csv(df.rcx.p,'TableS4_bottom.csv') 
TableS4 <- corstars(df.k, method="spearman",removeTriangle="lower", result="html")
TableS4 <- gsub("Statements_", "Q ", TableS4)

write.table(TableS4, file="TableS4.html")


#set analysis

df.gmth <- data.frame( "gm"=gm, "hype" = hy, "trust"=tr)
gm.neg <- which(df.gmth$gm == -2)
tr.neg <- which(df.gmth$tr == -2)
hy.neg <- which(df.gmth$hy == -2)

all_pos <- c(1: length(tr))

all.neg <- list(gm.neg, tr.neg, hy.neg)

venn.diagram(
  x = all.neg,
  category.names = c("GM" , "Trust " , "Hype"),
  filename = 'Fig7.tiff',
  output=TRUE
)

#determine significance of each overlap

l.hy <- length(hy.neg)
l.tr <- length(tr.neg)
l.gm <- length(gm.neg)



int.hy.tr <- length(intersect(tr.neg, hy.neg))
int.hy.gm <- length(intersect(gm.neg, hy.neg))
int.gm.tr <- length(intersect(tr.neg, gm.neg))
int.three <- length(intersect(intersect(tr.neg, gm.neg), hy.neg))

num.int.rand.trgm <- c()
num.int.rand.trhy <- c()
num.int.rand.gmhy <- c()
num.int.rand.three <- c()



for (i in c(1:nrands))  {

rand.tr <- sample(all_pos, l.tr, replace=TRUE)
rand.hy <- sample(all_pos, l.hy, replace=TRUE)
rand.gm <- sample(all_pos, l.gm, replace=TRUE)

num.int.rand.trgm <- c(num.int.rand.trgm, length(intersect(rand.tr, rand.gm)))
num.int.rand.trhy <- c(num.int.rand.trhy, length(intersect(rand.tr, rand.hy)))
num.int.rand.gmhy <- c(num.int.rand.gmhy, length(intersect(rand.hy, rand.gm)))
num.int.rand.three <- c(num.int.rand.three, length(intersect(intersect(rand.hy, rand.gm), rand.tr)))



}

P.hygm <- length(num.int.rand.gmhy[num.int.rand.gmhy >= int.hy.gm])/nrands
P.trhy <- length(num.int.rand.trhy[num.int.rand.trhy >= int.hy.tr])/nrands
P.trgm <- length(num.int.rand.trgm[num.int.rand.trgm >= int.gm.tr])/nrands
P.three <- length(num.int.rand.three[num.int.rand.three >= int.three])/nrands

Z.hygm <- (int.hy.gm - mean(num.int.rand.gmhy))/sd(num.int.rand.gmhy)
Z.hytr <- (int.hy.tr - mean(num.int.rand.trhy))/sd(num.int.rand.trhy)
Z.trgm <- (int.gm.tr - mean(num.int.rand.trgm))/sd(num.int.rand.trgm)
Z.three <- (int.three - mean(num.int.rand.three))/sd(num.int.rand.three)


df.res <- data.frame("Comparison"=c("N1", "N2", "N3", "Intersect", "Mean_intersection_rand", "Sd_rand", "Z", "P"), "Hype v GM" = c(l.hy, l.gm, "NA", int.hy.gm, round(mean(num.int.rand.gmhy), 4), round(sd(num.int.rand.gmhy),4), round(Z.hygm, 4), P.hygm), "Hype v Trust" = c(l.hy, l.tr, "NA", int.hy.tr, round(mean(num.int.rand.trhy),4), round(sd(num.int.rand.trhy),4), round(Z.hytr,4), P.trhy), "Trust v GM" = c(l.tr, l.gm, "NA", int.gm.tr, round(mean(num.int.rand.trgm),4), round(sd(num.int.rand.trgm),4), round(Z.trgm,4), P.trgm), "Three way"= c(as.integer(l.gm), as.integer(l.tr), as.integer(l.hy), as.integer(int.three), round(mean(num.int.rand.three),4), round(sd(num.int.rand.three),4), round(Z.three,4), P.three))

df.res %>% mutate(across(where(is.numeric), ~ round(., 4)))
#is.num <- sapply(df.res, is.numeric)
#df.res[is.num] <- lapply(df.res[is.num], round, 4)
write.table(t(df.res), file="Table_6.csv", sep=",", col.names=FALSE)



      