### read data (summary of behavioural data, based on raw files downloaded from LIONESS Lab)
a<-read.table('Data summary_repo2.txt', sep='\t', header=TRUE)

### load required packages ###
install.packages('lme4')
install.packages('stargazer')
install.packages('lmerTest')
install.packages('psych')
install.packages('rptR')
library('lme4')
library('stargazer')
library('lmerTest')
library('psych')
library('rptR')

########## RULE COMPLIANCE: TRAFFIC LIGHTS TASK (A-B-A design based on Gaechter et al 2018) ######

# we will look at peers who choose to FOLLOW social information showing the opposite behaviour (that is, switch from their initial decision). 

a$following<-ifelse(a$comply1==1, 1-a$comply2, a$comply2)

# basic descriptives (switch rates for both conditions)
switchRates<-c()
for (co in 0:1){
	b<-subset(a, a$comply1==co)
	switchRates<-c(switchRates, mean(b$following))
}
switchRates


# fit logistic models to decisions to comply in iteration 1 and to follow social information (switch) in iteration 2
model0<-glm(comply1 ~ female + age, data=a, family='binomial')
model1<-glm(following ~ female + age + comply1, data=a, family='binomial')
model2<-glm(following ~ female + age * comply1, data=a, family='binomial')

### output HTML file: this is Table S2 #######
library('stargazer')
stargazer(model0, model1,model2,type="html" ,ci=TRUE, ci.level=0.95, title="", align=TRUE, star.cutoffs=c(0.05,0.01,0.001), single.row=TRUE,
 report=('vcsp'),
	out="Table S2.html")


# create a matrix to document behaviour in Iteration 3: frequencies of reverting to initial behaviour or sticking with the updated behaviour
behMat<-matrix(nrow=4, ncol=6)
cnt<-1;
for (comp1 in 0:1){
	b<-subset(a, a$comply1==comp1)
	for (comp2 in 0:1){
		d<-subset(b, b$comply2==comp2)
		behMat[cnt, 1]<-comp1
		behMat[cnt, 2]<-comp2
		behMat[cnt, 3]<-round(mean(d$comply3),2)
		behMat[cnt, 4]<-sum(d$comply3)
		behMat[cnt, 5]<-nrow(d) - sum(d$comply3)
		behMat[cnt, 6]<-nrow(d)
		cnt<-cnt+1;
	}
}
behMat<-data.frame(behMat)
names(behMat)<-c('comply1', 'comply2','compl_rate3', 'comply3', 'violate3', 'n' )

# inspect the matrix showing compliance rates for iteration 3, broken down by behaviour in iterations 1 and 2
behMat


### for Fig. S2, we plot the time distribution for the first response (in milliseconds) and initial compliance across ages. the former are not in the summary files ### 


########### PROSOCIALITY: Dictator Game ##########

### calculate donations (out of 10)
a$d1s<-10-a$DG_self_SELFISH
a$d2s<-10-a$DG_self_withSocInfo_SELFISH
a$d1f<-10-a$DG_self_FAIR
a$d2f<-10-a$DG_self_withSocInfo_FAIR

### what portion of participants had a different initial decision from the social information (and hence could meaningfully adjust their donation based on social information)?
length(which(a$d1s > 0)) / length(na.omit(a$d1s))
length(which(a$d1f < 5)) / length(na.omit(a$d1f))

### count how many participants had non-zero initial donations in the 'selfish peer' condition
NsD1s<-c()
for (ag in 10:20){
	b<-subset(a, a$age==ag)
	NsD1s<-c(NsD1s, length(which(b$d1s>0)))
}


# calculate adjustments in donations
a$deltaSelfish<-a$d2s - a$d1s
a$deltaFair<-a$d2f - a$d1f

# calculate relative shifts (fraction moved towards social information; for the 'selfish peer' condition, social information was 0, for the 'fair peer' condition, social information was 5)
a$S_selfish<- (a$d2s - a$d1s) / (0-a$d1s)
a$S_fair<- (a$d2f - a$d1f) / (5-a$d1f)

# if first estimate was the same as social information, OR if revised estimates were not a weighted estimate of the first estimate and social info, fill in NA for the adjustment
a$S_selfish<-ifelse(a$S_selfish=='NaN', NA, a$S_selfish)
a$S_selfish<-ifelse(a$S_selfish<0, NA, a$S_selfish)
a$S_selfish<-ifelse(a$S_selfish>1, NA, a$S_selfish)

a$S_fair<-ifelse(a$S_fair=='NaN', NA, a$S_fair)
a$S_fair<-ifelse(a$S_fair<0, NA, a$S_fair)
a$S_fair<-ifelse(a$S_fair>1, NA, a$S_fair)

# mean shift across two conditions (for the factor analysis below)
a$S_DG<-NA;
for (i in 1:nrow(a)){
	a$S_DG[i]<- mean(c(a$S_selfish[i], a$S_fair[i]), na.rm=T)
}
# inspect distribution of this mean shift; it's U-shaped
hist(a$S_DG)

# prepare matrix for fitting regressions to mean shifts, accounting for individual differences. Each individual has 2 rows, 1 for each condition. We use this to fit a model with 'participant' as random intercept
regMat<-matrix(nrow=0, ncol=6)
for (i in 1:nrow(a)){
	
	# create a row for each individiual with relevant summary stats, and add to the matrix
	r<-c(a$participantNr[i], a$age[i], a$female[i], a$d1s[i], a$d2s[i], 1)
	regMat<-rbind(regMat, r)

	r<-c(a$participantNr[i], a$age[i], a$female[i], a$d1f[i], a$d2f[i], 2)
	regMat<-rbind(regMat, r)
}

regMat<-data.frame(regMat)
names(regMat)<-c('participantNr', 'age', 'female', 'firstDec', 'secondDec', 'treatment')

# calculate the shifts
regMat$dif<-regMat$secondDec - regMat$firstDec

# fit regression models to these data
library('lme4')
model0<-glmer(sign(firstDec) ~ treatment + age + female + (1|participantNr), data=regMat, family='binomial')
summary(model0)
model1<-lmer(firstDec ~ treatment + age + female + (1|participantNr), data=regMat)
summary(model1)
model2<-glmer(sign(abs(dif)) ~ treatment + age + female + (1|participantNr), data=regMat, family='binomial')
summary(model2)
model3<-lmer(abs(dif) ~ treatment + age + female + (1|participantNr), data=regMat)
summary(model3)


class(model1) <- "lmerMod"
class(model2) <- "lmerMod"
class(model3) <- "lmerMod"

### output HTML file; this is Table S3 #######
stargazer(model1,model2, model3,type="html" ,ci=TRUE, ci.level=0.95, title="", align=TRUE, star.cutoffs=c(0.05,0.01,0.001), single.row=TRUE,
 report=('vcsp'),
	out="Table S3.html")



########### BELIEF FORMATION: BEAST ####

### read data set with round-wise data for the BEAST task (participant numbers are the same as befeore)

b<-read.table('BEAST_data.txt', sep='\t', header=TRUE)

### first, a sanity check (cf. Fig. S6)
# create a round-wise matrix to explore deviations from the true (correct) values
correct<-c(93,78,59,74,69);

rMat<- matrix(nrow=0, ncol=8)

# loop over individuals
for (i in 1:nrow(b)){
	# loop over rounds
	for (j in 1:5){
		e1<-b[i,(j-1)*3+4]  # first estimate
		X<-b[i,(j-1)*3+5]	# social information
		e2<-b[i,(j-1)*3+6]	# second estimate
		
		T<- correct[j] 		# true value
		
		s<-(e2-e1)/(X-e1)	# adjustment
		
		ind<-b$participantNr[i]
		age<-b$age[i]
		female<-b$female[i]
		
		newRow<-c(ind, age, female, e1, X, e2, T, s)
		rMat<-rbind(rMat, newRow)
	}
}
rMat<-data.frame(rMat)
names(rMat)<-c('participantNr', 'age', 'female', 'E1', 'X', 'E2', 'T', 's')
rownames(rMat)<-c()

### load custom script for creating 'nice' looking histograms
source('niceHistograms.r')

### launch a device for 3 plots (Fig. S6)
par(las=1, lend=1, cex.lab=1.5, cex.axis=1.5, mfrow=c(1,3), yaxs='i', xaxs='i')
hist1(rMat$E1/rMat$T)
hist2(rMat$s)
hist1(rMat$E2/rMat$T)


### is the second estimate closer to the true value than the first estimate?
rMat$dev1<- abs(rMat$E1 - rMat$T) / rMat$T
rMat$dev2<- abs(rMat$E2 - rMat$T) / rMat$T

t.test(rMat$dev1, rMat$dev2, paired=TRUE)


#### check: does underestimation vary with age? 

rMat$relativeE1<-rMat$E1/rMat$T
### plot all E1/T values as a function of age. it varies somewhat with age

### summarize underestimation (E1 / T) for each individual, and store in new matrix
E1accuracyMat<-matrix(nrow=0, ncol=3)
for (ind in unique(rMat$participantNr)){
	d<-subset(rMat, rMat$participantNr==ind)
	meanUnderestimation<-mean(d$relativeE1)
	newRow<-c(ind, d$age[1], meanUnderestimation)
	E1accuracyMat<-rbind(E1accuracyMat, newRow)	
}
E1accuracyMat<-data.frame(E1accuracyMat)
names(E1accuracyMat)<-c('participantNr', 'age', 'meanRelativeE1')
rownames(E1accuracyMat)<-c()

# does underestimation vary with age? yes, somewhat
m1<-lm(meanRelativeE1 ~ age, data=E1accuracyMat)
summary(m1)
visreg(m1)

# and significantly so...
m1<-lmer(dev1 ~ age + female + (1|participantNr), data=rMat)
summary(m1)

# but does this change the age effect? no
m1<-lmer(s~ dev1 + age + female + (1|participantNr), data=rMat)
summary(m1)

### calculate mean adjustment per individiual from round-wise data
d<-matrix(nrow=0, ncol=5)
for (i in 1:nrow(b)){
	for (j in 1:5){
		
		E1<-b[i,3+(j-1)*3+1]  	# first estimates
		X<- b[i,3+(j-1)*3+2]	# social information
		E2<-b[i,3+(j-1)*3+3]	# second estimates
		s<-(E2-E1)/(X-E1)		# adjustments
		
		# exclude qualitatively different cases where second estimates were no weighted average of first estimates and social information
		s<-ifelse(s<0, NA, s)	# moving away from social information
		s<-ifelse(s>1, NA, s)	# moving beyond social information
		
		# add row to the matrix
		r<-c(b$participantNr[i], b$age[i], b$female[i], j, s)
		d<-rbind(d, r)
		
	}
}
d<-data.frame(d)
names(d)<-c('participantNr', 'age', 'female', 'period', 'adjustment')


### linear model fitted to individual summary data
model1<-lm(BEAST_S ~ age + female, data=a)
summary(model1)
### linear model fitted to round-wise data(Table S4)
model2<-lmer(adjustment ~ age + female + (1 | participantNr), data=d)
summary(model2)
### logistic model fitted to round-wise data (Table S4)
model3<-glmer(adjustment ~ age + female + (1 | participantNr), data=d, family='binomial')
summary(model3)

class(model2) <- "lmerMod"
class(model3) <- "lmerMod"

## calculate repeatability for the linear model
rpt(adjustment ~ age + female + (1 | participantNr), grname="participantNr", data=d)


### OUTPUT HTML FILE; this is Table S4 ###
stargazer(model1, model2, model3, type="html" ,ci=TRUE, ci.level=0.95, title="", align=TRUE, star.cutoffs=c(0.05,0.01,0.001), single.row=TRUE,
 report=('vcsp'),
	out="Table S4.html")



### Cross-domain generality of social information use: is there a latent factor/construct underlying social information use across tasks?

fa1 <- fa(cbind(a$following, a$S_DG, a$BEAST_S), nfactors=1)#, rotate='varimax')
a$fa1<-fa1$scores
fa1$loadings
boxplot(fa1 ~ age, data=a)

### more complex versions
### rule compliance vs BEAST

d<-subset(a, a$following==0)
x1<-d$BEAST_S
d<-subset(a, a$following==1)
x2<-d$BEAST_S
t.test(x1,x2)

m1<-lm(BEAST_S ~ following + comply1, data=a)
summary(m1)


### rule compliance vs BEAST

d<-subset(a, a$following==0)
x1<-d$BEAST_S
d<-subset(a, a$following==1)
x2<-d$BEAST_S
t.test(x1,x2)

m1<-lm(BEAST_S ~ following + comply1, data=a)
summary(m1)

### rule compliance vs altruistic giving

d<-subset(a, a$following==0)
x1<-d$S_DG
d<-subset(a, a$following==1)
x2<-d$S_DG
t.test(x1,x2)



### version 2

mat<-matrix(nrow=0, ncol=6)


### selfish example
### only consider those who donated at least 1 point
d<-subset(a, a$d1s>0)

### code those who REDUCED their donation as a 1, those who did not as a 0
d$movedS<-ifelse(d$deltaSelfish<0,1,0)
e<-subset(d, d$movedS==0)
x1<-e$BEAST_S
e<-subset(d, d$movedS==1)
x2<-e$BEAST_S

mat<-rbind(mat, cbind(d$participantNr, d$BEAST_S, d$movedS, 0, d$following, d$comply1))


# compare their mean S values in a t-test
t.test(x1,x2)


### fair example
### only consider those who donated fewer than 5 points
d<-subset(a, a$d1f<5)

### code those who INCREASED their donation as a 1, those who did not as a 0
d$movedF<-ifelse(d$deltaFair>0,1,0)
e<-subset(d, d$movedF==0)
x1<-e$BEAST_S
e<-subset(d, d$movedF==1)
x2<-e$BEAST_S
t.test(x1,x2)

mat<-rbind(mat, cbind(d$participantNr, d$BEAST_S, d$movedF, 1, d$following, d$comply1))
mat<-data.frame(mat)

names(mat)<-c('participantNr', 'BEAST_S', 'moved', 'fairPeer', 'following', 'comply1')
m1<-glmer(moved~S+fairPeer + (1|participantNr), data=mat, family='binomial')
summary(m1)

model0<-glm(S_DG~following+BEAST_S, data=a, family='gaussian')
summary(model0)


model1<-glm(following~BEAST_S + comply1, data=a, family='binomial')
model2<-glmer(moved~BEAST_S+fairPeer + (1|participantNr), data=mat, family='binomial')
model3<-glmer(moved~following+comply1+fairPeer + (1|participantNr), data=mat, family='binomial')

### model output needs to be get a new class to make it compatible with stargazer
class(model2) <- "lmerMod"
class(model3) <- "lmerMod"

### output HTML file; this is Table S5 #######
stargazer(model1,model2, model3,type="html" ,ci=TRUE, ci.level=0.95, title="", align=TRUE, star.cutoffs=c(0.05,0.01,0.001), single.row=TRUE,
 report=('vcsp'),
	out="Table S5.html")




### produce descriptive summary stats for age and gender composition of our sample, as well as the number of observations underlying the graphs in Figure 1 (this is Table S1; put in last bc some variables have been calculated for the other Tables)

statMat<-matrix(nrow=11,ncol=10)
cnt<-1;
for (ag in 10:20) {
	b<-subset(a, a$age==ag)
	

	statMat[cnt, 1]<-ag
	statMat[cnt, 2]<-nrow(b)
	d<-subset(b, b$female==0)
	statMat[cnt, 3]<-nrow(d)
	d<-subset(b, b$female==1)
	statMat[cnt, 4]<-nrow(d)
	statMat[cnt, 5]<-length(which(b$comply1==1))
	statMat[cnt, 6]<-length(which(b$comply1==0))
	statMat[cnt, 7]<-length(which(!is.na(b$S_selfish)))
	statMat[cnt, 8]<-length(which(!is.na(b$S_fair)))
	statMat[cnt, 9]<-length(which(!is.na(b$BEAST_S)))
	statMat[cnt, 10]<-length(which(!is.na(b$fa1)))

	
	cnt<-cnt+1;
}
statMat<-data.frame(statMat)
names(statMat)<-c('age', 'total', 'males', 'females', 'bad example (Fig. 1A)', 'good example (Fig. 1B)', 'selfish peer (Fig. 1C)', 'fair peer (Fig. 1D)', 'belief formation (Fig. 1E)', 'factor S (Fig. 1F)')


statMat
write.table(statMat, file='Table S1.txt', sep='\t', row.names=FALSE)



