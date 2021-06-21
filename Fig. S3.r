### read data (summary of behavioural data, based on raw files downloaded from LIONESS Lab)
a<-read.table('Data summary.txt', sep='\t', header=TRUE)

### calculate donations (not keep amounts)
a$d1s<-10-a$DG_self_SELFISH
a$d2s<-10-a$DG_self_withSocInfo_SELFISH
a$d1f<-10-a$DG_self_FAIR
a$d2f<-10-a$DG_self_withSocInfo_FAIR

## print summary stats
mean(c(a$d1s, a$d1f), na.rm=T)
sd(c(a$d1s, a$d1f), na.rm=T)

cols<-c('#72a555','#ca5670')
par(cex.lab=1.5, cex.axis=1.5, las=1, lend=1, mfrow=c(1,2))


fairMat<-matrix(0, nrow=11, ncol=11)
selfishMat<-matrix(0, nrow=11, ncol=11)

###### FAIR PEER

plot(0, type='n', xlim=c(-0.5,10.5), ylim=c(-0.5,10.5), xlab='First donation', ylab='Second donation', main='Fair peer')
arrows(-1,-1,11,11,lwd=40,col='grey90', code=0)
box()
for (i in 1:nrow(a)){

	fairMat[1+a$d1f[i], 1+a$d2f[i] ] <- fairMat[1+a$d1f[i], 1+a$d2f[i] ] + 1;

	x2<-a$d1f[i] -0.25 + 0.5*runif(1);	
	y2<-a$d2f[i] -0.25 + 0.5*runif(1)
	points(x2, y2, col=adjustcolor(cols[1], alpha=0.6), pch=17, cex=1)
}
arrows(-1,-1,11,11, lty=2, code=0)



############ SELFISH PEER



plot(0, type='n', xlim=c(-0.5,10.5), ylim=c(-0.5,10.5), xlab='First donation', ylab='Second donation', main='Selfish peer')
arrows(-1,-1,11,11,lwd=40,col='grey90', code=0)
box()
for (i in 1:nrow(a)){		

	selfishMat[1+a$d1s[i], 1+a$d2s[i] ] <- selfishMat[1+a$d1s[i], 1+a$d2s[i] ] + 1;


	x1<-a$d1s[i] -0.25 + 0.5*runif(1);	
	y1<-a$d2s[i] -0.25 + 0.5*runif(1)
	points(x1, y1, col=adjustcolor(cols[2], alpha=0.6), pch=16, cex=1)

#	arrows(x1,y1,x2,y2, code=0)
}

arrows(-1,-1,11,11, lty=2, code=0)

### print the 'transition matrix' for inspection
fairMat<-round(fairMat/sum(fairMat)*100)
fairMat

selfishMat<-round(selfishMat/sum(selfishMat)*100)
selfishMat


