
### define functions for making 'nice'-looking histograms

hist1<- function(x){
	f<-rep(0,41)
	
	for (i in x){
		x1<-1+round(i*10)
		f[x1]<-f[x1]+1
	}
	f<-f/sum(f)
	
	plot(0, type='n', xlim=c(0,2), ylim=c(0,0.3), xlab='', ylab='')
	for (k in 1:length(f)){
		x0<- (k-1) / 10 - 0.05
		x1<- (k-1) / 10 + 0.05
		y0<-0;
		y1<- f[k]
		
		col1<-'steelblue';
		if (k==11) col1<-'forestgreen'
		if (k>11) col1<-'firebrick'
		rect(x0,y0,x1,y1, col=col1)
	}
	
	arrows(1, 0, 1, 10, code=0, lty=2, col='black')
}

hist2<- function(x){
	f<-rep(0,41)
	
	for (i in x){
		x1<- 11+round(i*10)
		f[x1]<-f[x1]+1
	}
	
	f<-f/sum(f)
	
	plot(0, type='n', xlim=c(-0.5,1.5), ylim=c(0,0.3), xlab='', ylab='')
	rect(-2,-2,0,2, col='grey90', border=FALSE)
	rect(1,-2,2,2, col='grey90', border=FALSE)
	
	for (k in 1:length(f)){
		x0<- (-10 + (k-1)) / 10 - 0.05
		x1<- (-10 + (k-1)) / 10 + 0.05
		y0<-0;
		y1<- f[k]
		
		col1<-'violet';
		rect(x0,y0,x1,y1, col=col1)
	}
	box()
}
