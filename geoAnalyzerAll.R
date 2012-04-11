library('RMySQL')
library('fpc')
library('mclust')
library('edci')
require("geneplotter")
require("RColorBrewer")
require("class")
require("hdrcde")

#if (!"package:misc3d" %in% search()) {
#  Sys.sleep(15)
#}

# Setup graphs
lay <- layout(matrix(1:10, ncol=5, byrow=TRUE))
#op <- par(mar=rep(1,4), ask = FALSE)

K <- 2
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
	"group by longitude, latitude",
	"ORDER BY timestamp ASC"))

conn <- {}
for (i in 1:length(statements)) {
		# Setup graphs

		conn <- dbConnect(MySQL(), user="skillup", password="skillup", host="192.168.12.37", dbname="qori_analyzer")
		rso <- dbSendQuery(conn, statements[i])
		x <- fetch(rso)
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
		smoothScatter(x, main="Smooth Scatter", ylim = c(-33.50, -33.32), xlim = c(-70.65, -70.50))
		#smoothScatter(x, nrpoints=Inf, colramp=colorRampPalette(brewer.pal(9,"YlOrRd")), bandwidth=0.025)
		###
		# Normal points
		###
		colors <- densCols(x)
		plot(x, col=colors, pch=20, main="Density colored points", ylim = c(-33.50, -33.32), xlim = c(-70.65, -70.50))
		###
		# Bivariate Highest density regions
		###
		#plot(datax,datay, pch="+", cex=.5)
		hdr.boxplot.2d(datax,datay, main="Bivariate density regions", ylim = c(-33.50, -33.32), xlim = c(-70.65, -70.50))
		###
		# Density-based clustering (DBSCAN)
		###

		# Best for Caffati (2215)
		# safe
		#d <- dbscan(x,eps=0.1, MinPts=5, scale=1);
		d <- dbscan(x,eps=0.1, MinPts=5, scale=1, method="raw");
		plot(d,x, main="Density-based", ylim = c(-33.50, -33.32), xlim = c(-70.65, -70.50))
		#plot(x[d$cluster %in% 1:10,], main=timing[i], ylim = c(-33.50, -33.32), xlim = c(-70.65, -70.50))
		# Santiago + Vina del mar
		#plot(d, x, main="Density-based", ylim = c(-33.55, -32.9), xlim = c(-71.6, -70.5))
		# Best for Nico (2754)
		#d <- dbscan(x,eps=0.025, MinPts=5, scale=1);
		#plot(d, x, main=timing[i], ylim = c(-33.8, -33.3), xlim = c(-71, -70.45))
		# best for Luis (915)
		#d <- dbscan(x,eps=0.8, MinPts=5, scale=1);
		#plot(d, x, main=timing[i], xlim = c(139.7, 140.2), ylim = c(35.6, 36.1));

		###
		# K-means (k=K)
		###
		kclus <- kmeans(x, K)
		plot(x, col = kclus$cluster, main="K-Means (k=K)", ylim = c(-33.50, -33.32), xlim = c(-70.65, -70.50))
		points(kclus$centers, col = 1:2, pch = 8, cex=2, ylim = c(-33.50, -33.32), xlim = c(-70.65, -70.50))
		###
		# Hierachical cluster
		###
		hc <- hclust(dist(x), "ave")
		#hc <- hclust(dist(x)^2, "ave")
		plot(hc, main="Hierachical cluster dendrogram")
		###
		# Fanny (fuzzy logic, k=K)
		###
		fanny <- fanny(x,K)
		plot(x, col=fanny$clustering, main="FANNY (Fuzzy-logic based (k=K))", ylim = c(-33.50, -33.32), xlim = c(-70.65, -70.50))
		points(fanny$centers, col = 1:2, pch = 8, cex=2, ylim = c(-33.50, -33.32), xlim = c(-70.65, -70.50))
		#plot(fanny, which = 1)
		###
		# Fuzzy Cmeans (k=K)
		###
		library(e1071)
		cm <- cmeans(x,K)
		plot(x, col=cm$cluster, main="Cmeans (Fuzzy-logic based (k=K))", ylim = c(-33.50, -33.32), xlim = c(-70.65, -70.50))
		points(cm$centers, col = 1:2, pch = 8, cex=2, ylim = c(-33.50, -33.32), xlim = c(-70.65, -70.50))
		###
		# Fuzzy Cshell (k=K)
		###
		cs <- cshell(x,K)
		plot(x, col=cs$cluster, main="Cshell (Fuzzy-logic based (k=K))", ylim = c(-33.50, -33.32), xlim = c(-70.65, -70.50))
		points(cs$centers, col = 1:2, pch = 8, cex=2, ylim = c(-33.50, -33.32), xlim = c(-70.65, -70.50))
		###
		# Model-Based Clustering
		###
		#op <- par()
		#f <- cbind(datax,datay)
		#emclust <- Mclust(f)
		#plot(emclust, f, what = "classification")#, main="Model-Based Clustering")
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
		plot(b,datax,datay, main="Orthogonal Regression Clustering (Bandwidth=0.38)", ylim = c(-33.50, -33.32), xlim = c(-70.65, -70.50))
	}
}
