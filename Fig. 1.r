a<-read.table('Data summary.txt', sep='\t', header=TRUE)

a[1,]

#dev.new(height=19.05, width=11.56)

######## Rule compliance: TRAFFIC LIGHTS

cols<-c( '#72a555', '#ca5670')
par(cex.lab=1.5, cex.axis=1.5, las=1, lend=1, mfrow=c(3,2), mar=c(5,3,1,1), xaxs='r', yaxs='r')

# we will look at FOLLOWING peers, which is reversed with 'compliance' for the condition in which a participant complies first (in that case, 'following peers' means violation)
a$following<-ifelse(a$comply1==1, 1-a$comply2, a$comply2)


for (firstC in 1:0){
	# make empty plots
	plot(0, type='n', xlim=c(9.5,20.5), ylim=c(0,1), xlab='', ylab='', axes=FALSE)
	axis(1, at=9:21, labels=FALSE)
	axis(1, at=5:10*2)
	axis(2, at=0:10/10, labels=FALSE)
#	if (firstC==1) 
	axis(2, at=0:5/5)
	
	# only look at those who complied OR violated first (depending on iterator 'firstC')
	b<-subset(a, a$comply1==firstC)
	

	b$comply2r<-b$following
	
	# run the regression and calculate predicted line across ages
	m1<-glm(comply2r ~ age, family='binomial', data=b)
#	abline(m1)
	x<-900:2100/100	
	a1<-coef(m1)[1]; b1<-coef(m1)[2]
	Z<-a1+b1*x
	y<-1/(1+exp(-Z))
#	visreg(m1)

	s<-summary(m1)$coefficients
	uppZ<- s[1] + (s[2]+s[4])*x
	y_upp<- 1/(1+exp(-uppZ));
	lwrZ<- s[1] + (s[2]-s[4])*x
	y_lwr<- 1/(1+exp(-lwrZ));
	
	polygon(c(x,rev(x)),c(y_upp, rev(y_lwr)), col=adjustcolor(cols[firstC+1], alpha=0.2), border=FALSE)
	
	lines(x,y, col=cols[firstC+1], lwd=5)	

	# add data points per age cohort
	for (ag in 10:20){
		d<-subset(b, b$age==ag)
		m<-mean(d$comply2)
		s<-sd(d$comply2)/sqrt(nrow(d))
#		arrows(ag,m-s,ag,m+s,code=0,col=cols[firstC+1])
		y<-m
		if (firstC==1)y<-1-m
		points(ag,y, pch=22+(1-firstC)*2, bg=adjustcolor(cols[firstC+1], alpha=0.2+0.8*nrow(d)/11), cex=2, lwd=2)
		
#		text(ag, 1.08, nrow(d), col='black', cex=1.5)
		
	}
}


######## Prosociality: Dictator Game

# calculate donations (rather than amount kept)
a$d1s<-10-a$DG_self_SELFISH
a$d2s<-10-a$DG_self_withSocInfo_SELFISH
a$d1f<-10-a$DG_self_FAIR
a$d2f<-10-a$DG_self_withSocInfo_FAIR


# calculate switching
a$switchedFair<-ifelse(a$d1f<a$d2f,1,0)
a$switchedSelfish<-ifelse(a$d1s>a$d2s,1,0)


for (otherSelfish in 1:0){
	# create empty plot
	plot(0, type='n', xlim=c(9.5, 20.5), ylim=c(0,1), xlab='', ylab='', axes=FALSE)
	axis(1, at=9:21, labels=FALSE)
	axis(1, at=5:10*2)
	axis(2, at=0:10/10, labels=FALSE)
#	if (otherSelfish==1) 
	axis(2, at=0:5/5)


	# run regression, exclude those that matched the social information (switching would have a different meaning in those cases)
	m1<-glm(switchedFair~age, data=subset(a, a$d1f<5), family='binomial')
	if (otherSelfish==1) m1<-glm(switchedSelfish~age, data=subset(a, a$d1s>0), family='binomial')

	# add regression line to the chart	
	x<-900:2100/100	
	a1<-coef(m1)[1]; b1<-coef(m1)[2]
	Z<-a1+b1*x
	y<-1/(1+exp(-Z))
#	visreg(m1)

	s<-summary(m1)$coefficients
	uppZ<- s[1] + (s[2]+s[4])*x
	y_upp<- 1/(1+exp(-uppZ));
	lwrZ<- s[1] + (s[2]-s[4])*x
	y_lwr<- 1/(1+exp(-lwrZ));
	
	polygon(c(x,rev(x)),c(y_upp, rev(y_lwr)), col=adjustcolor(cols[1+otherSelfish], alpha=0.2), border=FALSE)
	
	lines(x,y, col=cols[1+otherSelfish], lwd=5)	
	
		# loop over ages
	for (ag in 10:20){
		b<-subset(a, a$age==ag)
		
		#count instances in which switching took place out of all *valid* instances (in which switching was possible)
		nS<-0; kS<-0
		nF<-0; kF<-0;
		for (i in 1:nrow(b)){
			# when observing a SELFISH peer, only include those who donated more than 0 in the solo condition
			if (!is.na(b$switchedSelfish[i]) && b$d1s[i]>0){
				nS<-nS+1;
				kS<-kS+b$switchedSelfish[i];
			}
			# when observing a FAIR peer, only include those who donated less than 5 in the solo condition			
			if (!is.na(b$switchedFair[i]) && b$d1f[i]<5){
				nF<-nF+1;
				kF<-kF+b$switchedFair[i];
			}
		}
		
		# add points to the chart
		if (otherSelfish==1){
			points(ag, kS/nS, pch=22, bg=adjustcolor(cols[2], alpha=0.2+0.8*nS/20), cex=2, lwd=2)
			text(ag, 1.18, nS, col=cols[2], cex=1.2)
		}
		if (otherSelfish==0){
			points(ag, kF/nF, pch=24, bg=adjustcolor(cols[1], alpha=0.2+0.8*nF/20), cex=2, lwd=2)
		
#			text(ag, 1.18, nF, col=cols[1], cex=1.2)
		}
		
	}
	
}

###### Belief formation: BEAST ######

cols<-c("#8176cc","#5ba966", "#c75a93")

plot(0, type='n', xlim=c(9.8,20.2), ylim=c(0,0.6), axes=FALSE, ylab='', xlab='')
axis(1, at=9:21)
axis(2, at=0:10/10, labels=FALSE)
axis(2, at=0:10/10)
m1<-lm(BEAST_S~age + female, data=a)
m1b<-lm(BEAST_S~age, data=a)
summ<-summary(m1b)$coefficients

x<-90:210/10
xx<-c(x,rev(x))
a1<-summ[1]
b1<-summ[2]

b_si<-summ[4]

y<-a1+b1*x

y_lo<-a1+(b1-b_si)*x
y_hi<-a1+(b1+b_si)*x
yy<-c(y_lo, rev(y_hi))

polygon(xx, yy, col=adjustcolor(cols[1], alpha=0.5), border=FALSE)
lines(x,y, lwd=3, col=cols[1])

for (ag in 10:20){
	b<-subset(a, a$age==ag)
	
	x<-b$BEAST_S
	

	
	# summary stats per treatment
	y<-mean(x, na.rm=T)
	
	w<-summary(x)
	IQR<-w[5]-w[2]
	se<- sd(x)/sqrt(length(x))	
	#interquartile
#	rect(ag-0.3,w[2],ag+0.3,w[5],col=adjustcolor(cols[1], alpha=0.3),lwd=2)
#	arrows(ag, w[2]-1.5*IQR, ag,w[2], code=0)
#	arrows(ag,w[5],ag,w[5]+1.5*IQR, code=0)

#	arrows(ag, y-sd(x), ag,w[2], code=0, lwd=1.5)
#	arrows(ag,w[5],ag,y+sd(x), code=0, lwd=1.5)

#	arrows(ag,y-se,ag,y+se, code=0, lwd=2)
#	points(ag,y, pch=21, bg=cols[1], cex=2, lwd=2)
	
	#median
#	arrows(ag-0.3, w[3], ag+0.3, w[3], lwd=4, code=0, col='black')
	
	#mean
#	arrows(ag-0.3, y, ag+0.3, y, lwd=2, code=0, col=cols[1])
	points(ag, y, pch=21, cex=2.2,bg=adjustcolor(cols[1], alpha=0.2+0.8*length(x)/20), lwd=2)
#	points(ag, kF/nF, pch=24, bg=adjustcolor(cols[1], alpha=0.2+0.8*nF/20), cex=2, lwd=2)
	
	#individual data points
	for (j in 1:nrow(b)){
		x1<-ag-0.2+runif(1)*0.4
#		points(x1, b$BEAST_S[j], pch=16, col='grey40', cex=0.6)
	}
#	text(ag, 1.09, nrow(b), cex=1.5)
}


######## Factor Analysis #########

library('psych')
fa1 <- fa(cbind(a$following, a$S_DG, a$BEAST_S), nfactors=1)
a$fa1<-fa1$scores;

plot(0, type='n', xlim=c(9.8,20.2), ylim=c(-0.8,0.6), axes=FALSE, ylab='', xlab='')
axis(1, at=9:21)
axis(2, at=-5:5/5, labels=FALSE)
axis(2, at=-5:5/5)

cols<-c('#9750a1')

for (ag in 10:20){
	b<-subset(a, a$age==ag)
	
	x<-b$fa1
	

	
	# summary stats per treatment
	y<-mean(x, na.rm=T)
	
	w<-summary(as.numeric(x))
	IQR<-w[5]-w[2]
	se<- sd(x)/sqrt(length(x))	
	#interquartile
#	rect(ag-0.3,w[2],ag+0.3,w[5],col=adjustcolor("#9750a1", alpha=0.3),lwd=2)
#	arrows(ag, w[2]-1.5*IQR, ag,w[2], code=0)
#	arrows(ag,w[5],ag,w[5]+1.5*IQR, code=0)
#	arrows(ag, y-sd(x), ag,w[2], code=0, lwd=1.5)
#	arrows(ag,w[5],ag,y+sd(x), code=0, lwd=1.5)
	
	#median
#	arrows(ag-0.3, w[3], ag+0.3, w[3], lwd=4, code=0, col='black')
	
	#mean
#	arrows(ag-0.3, y, ag+0.3, y, lwd=2, code=0, col=cols[1])
	
	points(ag, y, pch=23, cex=2,bg=adjustcolor(cols[1], alpha=0.2+0.8*length(x)/20), lwd=2)
	
	#individual data points
	for (j in 1:nrow(b)){
		x1<-ag-0.2+runif(1)*0.4
#		points(x1, b$fa1[j], pch=16, col='grey40', cex=0.6)
	}
#	text(ag, 1.09, nrow(b), cex=1.5)
}

m1b<-lm(fa1~age, data=a)
summ<-summary(m1b)$coefficients

x<-90:210/10
xx<-c(x,rev(x))
a1<-summ[1]
b1<-summ[2]

b_si<-summ[4]

y<-a1+b1*x

y_lo<-a1+(b1-b_si)*x
y_hi<-a1+(b1+b_si)*x
yy<-c(y_lo, rev(y_hi))

polygon(xx, yy, col=adjustcolor(cols[1], alpha=0.5), border=FALSE)
lines(x,y, lwd=3, col=cols[1])

