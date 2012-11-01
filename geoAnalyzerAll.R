library('RMySQL')
library('fpc')
library('mclust')
library('edci')
#require("geneplotter")
require("RColorBrewer")
require("class")
library('kernlab')
library('e1071')
require("hdrcde")

#if (!"package:misc3d" %in% search()) {
#  Sys.sleep(15)
#}

# Setup graphs
lay <- layout(matrix(1:8, ncol=2, byrow=TRUE))
#op <- par(mar=rep(1,4), ask = FALSE)

K <- 3
statements <- {}

# Week days 14-20
statements <- append(statements, paste(
	"SELECT longitude, latitude",# timestamp",
	"FROM reports",
	"WHERE uid LIKE '5eedd0514bbc4c2c7b77903f13dbf95f4693638f'",
	#"AND EXTRACT(HOUR FROM timeofday) >= 14",
	#"AND EXTRACT(HOUR FROM timeofday) < 20",
	"AND dayofweek >= 1",
	"AND dayofweek <= 5",
	"GROUP BY longitude, latitude",
	"ORDER BY timestamp ASC"))

conn <- {}
for (i in 1:length(statements)) {
		# Setup graphs

		conn <- dbConnect(MySQL(), user="skillup", password="skillup", host="192.168.13.44", dbname="qori_analyzer")
		rso <- dbSendQuery(conn, statements[i])
		x <- fetch(rso,n=-1)
		count <- nrow(x)

		rowCount <- 0
		datax <- {}
		datay <- {}
		rso <- dbSendQuery(conn, statements[i])
		while(rowCount < count) {
				stopifnot(rowCount < count)
		        y <- fetch(rso,n=1)
		        rowCount <- rowCount + nrow(y)

		        datax <- append(datax, y[1,2])
		        datay <- append(datay, y[1,1])
		}
		dbClearResult(rso)
		dbDisconnect(conn)

	x <- cbind(datax,datay)

	if(is.null(x)){
		plot(c(0,0), c(0,0), main=timing[i], ylim = c(-33.50, -33.32), xlim = c(-70.65, -70.50))
	}
	else{
		###
		# Scatterplots with smoothed densities color representation
		###
		#smoothScatter(x, nrpoints=0)
		#smoothScatter(x, main="Smooth Scatter", ylim = c(-33.50, -33.32), xlim = c(-70.65, -70.50))
		#smoothScatter(x, nrpoints=Inf, colramp=colorRampPalette(brewer.pal(9,"YlOrRd")), bandwidth=0.025)
		###
		# Normal points
		###
		#colors <- densCols(x)
		#plot(x, col=colors, pch=20, main="a) density colored points", ylab = "Latitude" , xlab = "Longitude", ylim = c(-33.55, -33.3), xlim = c(-70.70, -70.50))
		###
		# Bivariate Highest density regions
		###
		#plot(datax,datay, pch="+", cex=.5)
		#hdr.boxplot.2d(datax,datay, main="Bivariate density regions", ylim = c(-33.50, -33.32), xlim = c(-70.65, -70.50))
		###
		# Density-based clustering (DBSCAN)
		###

		# Best for Caffati (2215)
		# safe
		#d <- dbscan(x,eps=0.1, MinPts=4, scale=1);
		d <- dbscan(x,eps=0.1, MinPts=10, scale=1, method="raw");
		plot(d,x, main="a) density-based clustering (Eps=0.1, MinPts=10)", ylab = "Latitude" , xlab = "Longitude", ylim = c(-33.55, -33.3), xlim = c(-70.70, -70.50))
		legend("bottomright", inset=.05, title="Clusters", c("1","2","3"), fill=tail(palette(), n=-1), horiz=TRUE)
		#plot(x[d$cluster %in% 1:10,], main=timing[i], ylim = c(-33.50, -33.32), xlim = c(-70.65, -70.50))
		# Santiago + Vina del mar
		#plot(d, x, main="Density-based", ylim = c(-33.55, -32.9), xlim = c(-71.6, -70.5))
		# Best for Nico (2754)
		#d <- dbscan(x,eps=0.025, MinPts=4, scale=1);
		#plot(d, x, main=timing[i], ylim = c(-33.8, -33.3), xlim = c(-71, -70.45))
		# best for Luis (915)
		#d <- dbscan(x,eps=0.8, MinPts=4, scale=1);
		#plot(d, x, main=timing[i], xlim = c(139.7, 140.2), ylim = c(35.6, 36.1));

		###
		# Spectral clustering
		###
		#s <- specc(x,length(unique(d$cluster)))
		s <- specc(x,K)
		plot(x,col=s,main="b) spectral clustering", ylab = "Latitude" , xlab = "Longitude", ylim = c(-33.55, -33.3), xlim = c(-70.70, -70.50))
		legend("bottomright", inset=.05, title="Clusters", c("0","1","2"), fill=palette(), horiz=TRUE)
		
		###
		# K-means (k=K)
		###
		kclus <- kmeans(x, K)
		plot(x, col = kclus$cluster, main="c) k-means clustering (k=3)", ylab = "Latitude" , xlab = "Longitude", ylim = c(-33.55, -33.3), xlim = c(-70.70, -70.50))
		#points(kclus$centers, col = 1:2, pch = 8, cex=2, ylab = "Latitude" , xlab = "Longitude", ylim = c(-33.55, -33.3), xlim = c(-70.70, -70.50))
		legend("bottomright", inset=.05, title="Clusters", c("1","2","3"), fill=palette(), horiz=TRUE)

		###
		# Hierachical cluster
		###

	    #def.par <- par(no.readonly = TRUE)
    	#on.exit(par(def.par))

    	#par(mar = c(3, 0.3, 1, 0.5))
    	#par(oma = rep(1,2))

		hcut <- 0.02
		hc <- hclust(dist(x), "ave")
		#hc <- hclust(dist(x)^2, "ave")
		#plot(hc, main="Hierarchical cluster dendrogram")
		#plot(cutree(hc, k=7)) # What's a good K?! ;)

		newLabels <- paste("Cluster", 1:12, sep=" ")
		local({
			newLab <<- function(n) {
		        if(is.leaf(n)) {
		          	a <- attributes(n)
		          	i <<- i+1
		          	attr(n, "label") <- newLabels[i]
		        }
		        n
			}
			i <- 0
		})
		chc <- cut(dend, h=hcut)$upper
		nhc <- dendrapply(chc, newLab)
		plot(nhc, main="d) hierarchical cluster (cut at 0.02)",horiz=TRUE)

		dend <- as.dendrogram(hc)
		ngrp <- length(cut(dend,hcut)$lower)
		groupes <- cutree(hc,h=hcut)
		ttab <- table(groupes)
		colorsnames <- brewer.pal(ngrp,"Dark2")

		abline(v=hcut,col="red")
	    text(x=hcut,y=length(hc$height),labels=as.character(round(hcut,3)),col="red",pos=4)

		bplot <- barplot(as.vector(rev(ttab)),horiz=TRUE,space=0,col=rev(colorsnames),xlim=c(0,max(ttab)+10),axes=FALSE,main="Groups",axisnames=FALSE)
		text(rev(ttab),bplot,as.character(rev(ttab)),col=rev(colorsnames),cex=1.2,pos=4)

		#chc <- cut(as.dendrogram(hc), h=0.02)$upper
		#newLabels <- paste("Cluster", 1:22, sep=" ")
		#local({
		#	newLab <<- function(n) {
		#        if(is.leaf(n)) {
		#          	a <- attributes(n)
		#          	i <<- i+1
		#          	attr(n, "label") <- newLabels[i]
		#        }
		#        n
		#	}
		#	i <- 0
		#})
		#nhc <- dendrapply(chc, newLab)
		#plot(nhc, main="d) hierarchical cluster (cut at 0.02)")

		###
		# Fanny (fuzzy logic, k=K)
		###
		fanny <- fanny(x,K)
		plot(x, col=fanny$clustering, main="e) fuzzy-logic clustering (FANNY) (k=3)", ylab = "Latitude" , xlab = "Longitude", ylim = c(-33.55, -33.3), xlim = c(-70.70, -70.50))
		#points(fanny$centers, col = 1:2, pch = 8, cex=2, ylim = c(-33.55, -33.3), xlim = c(-70.70, -70.50))
		legend("bottomright", inset=.05, title="Clusters", c("1","2","3"), fill=palette(), horiz=TRUE)
		#plot(fanny, which = 1)
		###
		# Fuzzy Cmeans (k=K)
		###
		#library(e1071)
		#cm <- cmeans(x,K)
		#plot(x, col=cm$cluster, main="f) fuzzy-logic clustering (C-means) (k=3)", ylab = "Latitude" , xlab = "Longitude", ylim = c(-33.55, -33.3), xlim = c(-70.70, -70.50))
		#points(cm$centers, col = 1:2, pch = 8, cex=2, ylim = c(-33.55, -33.3), xlim = c(-70.70, -70.50))
		#legend("bottomright", inset=.05, title="Clusters", c("1","2","3"), fill=palette(), horiz=TRUE)
		###
		# Fuzzy Cshell (k=K)
		###
		cs <- cshell(x,K)
		plot(x, col=cs$cluster, main="f) fuzzy-logic clustering (C-shell) (k=3)", ylab = "Latitude" , xlab = "Longitude", ylim = c(-33.55, -33.3), xlim = c(-70.70, -70.50))
		#points(cs$centers, col = 1:2, pch = 8, cex=2, ylim = c(-33.55, -33.3), xlim = c(-70.70, -70.50))
		legend("bottomright", inset=.05, title="Clusters", c("1","2","3"), fill=palette(), horiz=TRUE)
		###
		# Model-Based Clustering
		###
		#op <- par()
		#f <- cbind(datax,datay)
		#emclust <- Mclust(f)
		#plot(emclust, f)#, main="g) model-based clustering", ylab = "Latitude" , xlab = "Longitude", ylim = c(-33.55, -33.3), xlim = c(-70.70, -70.50))#, main="Model-Based Clustering")
		#op$new <- TRUE
		#par(op)
		###
		# Unstable sensitive clustering algos
		###
		# change to 0.28 or 0.3 and it finds no cluster!
		#b <- circMclust(datax,datay,0.29)
		#plot(b,datax,datay, main="Circular Clustering (Bandwidth=0.29)")
		# changing between 0.35 and 0.39 gives too many different results!
		b <- oregMclust(datax,datay,0.38)
		plot(b,datax,datay, main="h) orthogonal regression clustering (Bandwidth=0.38)", ylab = "Latitude" , xlab = "Longitude", ylim = c(-33.55, -33.3), xlim = c(-70.70, -70.50))
	}
}
