library('RMySQL')
library('fpc')
library('mclust')
library('edci')
require("geneplotter")
require("RColorBrewer")
require("class")
require("hdrcde")
library("RCurl")
library("rjson")
library("gtools")

# Setup graphs
lay <- layout(matrix(1:10, ncol=2, byrow=FALSE))
#op <- par(mar=rep(1,4), ask = FALSE)

uid <- "'5eedd0514bbc4c2c7b77903f13dbf95f4693638f'"
density <- c(0.025)
timingWD <- c('WD 6-8','WD 8-10','WD 10-12','WD 12-14','WD 14-16','WD 16-18','WD 18-20','WD 20-22','WD 22-24','WD 0-2','WD 2-6')
timingWE <- c('WE 6-8','WE 8-10','WE 10-12','WE 12-14','WE 14-16','WE 16-18','WE 18-20','WE 20-22','WE 22-24','WE 0-2','WE 2-6')
tod <- array(c(6,8,8,10,10,12,12,14,14,16,16,18,18,20,20,22,22,24,0,2),c(2,10))
statements <- {}

for (t in 1:(length(tod)/2)) {
	statements <- append(statements, paste(
	"SELECT longitude, latitude",
	"FROM reports",
	"WHERE uid LIKE", uid,
	"AND EXTRACT(HOUR FROM timeofday) >=", tod[1,t],
	"AND EXTRACT(HOUR FROM timeofday) <", tod[2,t],
	"AND dayofweek >= 1",
	"AND dayofweek <= 5",
	"group by longitude, latitude",
	"ORDER BY timestamp ASC"))
}

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
		for (den in 1:length(density)) { 
			plot(c(0,0), c(0,0), main=paste(timingWD[i],"density",density[den]), ylim = c(-33.50, -33.32), xlim = c(-70.85, -70.50))
		}
	}
	else{
		for (den in 1:length(density)) {
			d <- dbscan(x,eps=density[den], MinPts=5, scale=1, method="raw");
			# Caffati (2215)
			plot(d,x, main=paste(timingWD[i],"density",density[den]), ylim = c(-33.50, -33.32), xlim = c(-70.65, -70.50))
			#plot(x[d$cluster %in% 1:10,], main=timingWD[i], ylim = c(-33.50, -33.32), xlim = c(-70.65, -70.50))
			# Santiago + Vina del mar
			#plot(d, x, main=timingWD[i], ylim = c(-33.55, -32.9), xlim = c(-71.6, -70.5))
			# Best for Nico (2754)
			#plot(d, x, main=timingWD[i], ylim = c(-33.8, -33.3), xlim = c(-71, -70.45))
			# best for Luis (915)
			# Kantou
			#plot(d, x, main=paste(timingWD[i],"density",density[den]), xlim = c(139.7, 140.2), ylim = c(35.6, 36.1))
			# Tokyo
			#plot(d, x, main=paste(timingWD[i],"density",density[den]), xlim = c(139.7, 139.85), ylim = c(35.6, 35.8))

			if (max(d$cluster)) {
				for (c in 1:max(d$cluster)) {
					# Cluster center
					clusCenter <- colMeans(x[d$cluster==c, ])

					# Fetch context
					# Foursquare
					Uctx <- getURL(paste("https://api.foursquare.com/v2/venues/search?ll=",clusCenter[2],",",clusCenter[1],"&oauth_token=ADB02WREAK4W4R5BDYBVEXHWB14VZM4TQOIWZCYAD1GY22EK&v=20120410&radius=80&intent=browse", sep=""))
					# Twitter
					#Uctx <- getURL(paste("http://search.twitter.com/search.json?q=@foursquare&geocode=",clusCenter[2],",",clusCenter[1],"0.50km&rpp=100&result_type=recent", sep=""))
					# Facebook
					#Uctx <- getURL(paste("https://graph.facebook.com/search?q=%20&type=place&center=",clusCenter[2],",",clusCenter[1],"&distance=1000&access_token=AAAAAAITEghMBAC1t4hU54qR4lTKVTdTXQ7OlZAGfWlRRK5FdZCrAlhrNYhksK0Fr71IlDtP9xMLZCLXRf0aPWHNGJl3EKBpNgFsPZBNszAZDZD", sep=""))
					
					# Convert JSON to R-object
					Rctx <- fromJSON(Uctx,method = "C")

					# Parse context
					ctx <- {}
					if (length(Rctx$response$venues)) {
						for (v in 1:length(Rctx$response$venues)) {
							if(length(Rctx$response$venues[[v]]$categories)) {
								ctx <- append( ctx, paste(Rctx$response$venues[[v]]$location$distance,Rctx$response$venues[[v]]$categories[[1]]$shortName,Rctx$response$venues[[v]]$name) )
							}
						}

						# Infer context
						print(paste(timingWD[i], "density", density[den], "cluster", c, "with", length(x[d$cluster==1])/2, "elements"))
						sortedctx <- mixedsort(ctx)
						print(sortedctx)
					}
				}
			}
		}
	}
}
