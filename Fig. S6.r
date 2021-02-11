### Figure S6: belief formation

### we need the round-wise data to make the graphs
b<-read.table('BEAST_data.txt', sep='\t', header=TRUE)

# create a round-wise matrix to explore deviations from the true (correct) values
# store correct values
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
