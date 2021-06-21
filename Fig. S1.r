a<-read.table('Data summary.txt', sep='\t', header=TRUE)

x<-a$move1
cols<-c( '#72a555', '#ca5670')

f<-rep(0,21)
for (i in 0:20){
	j<-length(which(floor(x/1000) == i))
	f[i]<-j
}
f

f<-f/sum(f)

par(las=1, lend=1, cex.lab=1.5, cex.axis=1.5, yaxs='i', mar=c(5,4,3,2), mfrow=c(1,2))
plot(0, type='n', xlim=c(0,20), ylim=c(0,0.4), xlab='', ylab='', axes=FALSE)
axis(1, at=0:4*5)
axis(2)
axis(3, labels=c(20,15,10,5,0), at=0:4*5)

for (i in 1:21){
	col1<-cols[1]
	if (i<12) col1<-cols[2]
	rect(i, 0, i+1, f[i], col=col1)
}

a$compliance1<-ifelse(a$move1<12000,0,1)
for (i in 1:nrow(a)){
	if (is.na(a$move1[i])) a$compliance1[i]<-NA
}

plot(0, type='n', xlim=c(9,21), ylim=c(0,1), xlab='', ylab='', axes=FALSE)
axis(1)
axis(2)
for (ag in 10:20){
	b<-subset(a, a$age==ag)
	y<-mean(b$compliance1, na.rm=TRUE)
	rect(ag-0.5, 0, ag+0.5, y, col=cols[1])
	rect(ag-0.5, y, ag+0.5, 1, col=cols[2])
#	text(ag, 0.1, length(which(!is.na(b$compliance1))))
}
