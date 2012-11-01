library('RMySQL')
library('fpc')
plot(library('mclust')
library('edci')
require("geneplotter")
require("RColorBrewer")
require("class")
require("hdrcde")
library("RCurl")
library("rjson")
library("gtools")
library("rmongodb")

crawler <- function(tree, name, d) {
	treeArray[tree$shortName] <<- name
	c <- length(tree$categories)
	if (c) {
		for (i in 1:c) {
			tree$categories[[i]]$shortName
			crawler(tree$categories[[i]], name)
		}
	}
}

# Setup graphs
lay <- layout(matrix(1:10, ncol=2, byrow=FALSE))
#op <- par(mar=rep(1,4), ask = FALSE)

radius <- 750
uid <- "'5eedd0514bbc4c2c7b77903f13dbf95f4693638f'"
density <- c(0.025)
timingWD <- c('WD 6-8','WD 8-10','WD 10-12','WD 12-14','WD 14-16','WD 16-18','WD 18-20','WD 20-22','WD 22-24','WD 0-2','WD 2-6')
timingWE <- c('WE 6-8','WE 8-10','WE 10-12','WE 12-14','WE 14-16','WE 16-18','WE 18-20','WE 20-22','WE 22-24','WE 0-2','WE 2-6')
tod <- array(c(6,8,8,10,10,12,12,14,14,16,16,18,18,20,20,22,22,24,0,2),c(2,10))
#tod <- array(c(6,8,8,10,10,12,12,14,14,16,16,18,18,20,20,22),c(2,8))
statements <- {}

# Build Foursquare tree
FSCats <- getURL("https://api.foursquare.com/v2/venues/categories?oauth_token=ADB02WREAK4W4R5BDYBVEXHWB14VZM4TQOIWZCYAD1GY22EK")
FSTree <- fromJSON(FSCats, method = "C")
treeArray <- {}

for (sc in 1:length(FSTree$response$categories)) {
	crawler(FSTree$response$categories[[sc]], FSTree$response$categories[[sc]]$shortName)
}

# Connect to MongoDB
mongo <- mongo.create()

for (t in 1:(length(tod)/2)) {
	statements <- append(statements, paste(
	"SELECT longitude, latitude",
	"FROM reports",
	"WHERE uid LIKE", uid,
	"AND EXTRACT(HOUR FROM timeofday) >=", tod[1,t],
	"AND EXTRACT(HOUR FROM timeofday) <", tod[2,t],
	"AND (dayofweek < 1",
	"OR dayofweek > 5)",
	"ORDER BY timestamp ASC"))
}

conn <- {}
for (i in 1:length(statements)) {
		# Setup graphs

		conn <- dbConnect(MySQL(), user="skillup", password="skillup", host="192.168.13.44", dbname="qori_analyzer")
		rso <- dbSendQuery(conn, statements[i])
		x <- fetch(rso, n=-1)
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
			plot(c(0,0), c(0,0), main=paste(timingWE[i],"density",density[den]), ylim = c(-33.50, -33.32), xlim = c(-70.85, -70.50))
		}
	}
	else{
		for (den in 1:length(density)) {
			d <- dbscan(x,eps=density[den], MinPts=20, scale=1, method="raw");
			# Caffati (2215)
			plot(d,x, main=paste(timingWE[i],"density",density[den]), ylim = c(-33.50, -33.32), xlim = c(-70.65, -70.50))
			#plot(x[d$cluster %in% 1:10,], main=timingWE[i], ylim = c(-33.50, -33.32), xlim = c(-70.65, -70.50))
			# Santiago + Vina del mar
			#plot(d, x, main=timingWE[i], ylim = c(-33.55, -32.9), xlim = c(-71.6, -70.5))
			# Best for Nico (2754)
			#plot(d, x, main=timingWE[i], ylim = c(-33.8, -33.3), xlim = c(-71, -70.45))
			# best for Luis (915)
			# Kanto
			#plot(d, x, main=paste(timingWE[i],"density",density[den]), xlim = c(139.7, 140.2), ylim = c(35.6, 36.1))
			# Tokyo
			#plot(d, x, main=paste(timingWE[i],"density",density[den]), xlim = c(139.7, 139.85), ylim = c(35.6, 35.8))

			if (max(d$cluster)) {
				for (c in 1:max(d$cluster)) {
					# Cluster center
					clusCenter <- colMeans(x[d$cluster==c, ])

					# Fetch context
					# Foursquare
					Uctx <- getURL(paste("https://api.foursquare.com/v2/venues/search?ll=",clusCenter[2],",",clusCenter[1],"&oauth_token=ADB02WREAK4W4R5BDYBVEXHWB14VZM4TQOIWZCYAD1GY22EK&v=20120410&radius=",radius,"&intent=browse", sep=""))
					# Twitter
					#Uctx <- getURL(paste("http://search.twitter.com/search.json?q=@foursquare&geocode=",clusCenter[2],",",clusCenter[1],"0.50km&rpp=100&result_type=recent", sep=""))
					# Facebook + NLP?
					#https://graph.facebook.com/search?q=ofi&type=place&center=-33.39,-70.5466333333333&distance=250&access_token=AAAAAAITEghMBANyEQtKaNcp4BIJBPTixFR3dtNySpOvlFaYRZCnlL9ZBtn7ILYszpZB4OJGGhLYsqgxVaapFlF3uvZBzFxV7cXGbdkej0wZDZD
					#https://graph.facebook.com/search?q=rest&type=place&center=-33.39,-70.5466333333333&distance=250&access_token=AAAAAAITEghMBANyEQtKaNcp4BIJBPTixFR3dtNySpOvlFaYRZCnlL9ZBtn7ILYszpZB4OJGGhLYsqgxVaapFlF3uvZBzFxV7cXGbdkej0wZDZD
					#https://graph.facebook.com/search?q=casa&type=place&center=-33.39,-70.5466333333333&distance=250&access_token=AAAAAAITEghMBANyEQtKaNcp4BIJBPTixFR3dtNySpOvlFaYRZCnlL9ZBtn7ILYszpZB4OJGGhLYsqgxVaapFlF3uvZBzFxV7cXGbdkej0wZDZD
					# Google Places
					#https://maps.googleapis.com/maps/api/place/search/json?location=-33.39,-70.5466333333333&radius=250&sensor=false&key=AIzaSyCSQCjmllk7W-e2WaVbBHADeBmunOMj66w

					# Convert JSON to R-object
					Rctx <- fromJSON(Uctx,method = "C")

					# Parse context
					ctx <- {}
					venue <- {}
					finalvenue <- {}
					venue[""] = 0
					if (length(Rctx$response$venues)) {
						for (v in 1:length(Rctx$response$venues)) {
							if(length(Rctx$response$venues[[v]]$categories)) {
								cat <- treeArray[Rctx$response$venues[[v]]$categories[[1]]$shortName]
								dist <- Rctx$response$venues[[v]]$location$distance
								if (dist < radius) {
									ctx <- append( ctx, paste(dist,cat,Rctx$response$venues[[v]]$name) )
									if(is.na(venue[cat])){
										venue[cat] <- 1
									}
									else {
										venue[cat] <- venue[cat] + 1
									}
								}
							}
						}

						for (fv in 1:length(venue)){
							if(venue[fv] >= 1){
								finalvenue <- append(finalvenue, venue[fv])
							}
						}

						# Infer context
						print(paste(timingWE[i], " density: ", density[den], " cluster: ", c, " of ", length(finalvenue), " with ", sum(d$cluster == c) ," elements @ ", clusCenter[2], "," ,clusCenter[1], sep=""))
						#sortedctx <- mixedsort(ctx)
						#print(sortedctx)
						sortedvenues<- mixedsort(finalvenue)
						for (sv in 1:length(sortedvenues)) {
							#mongo.insert(mongo, "test.people", list(name=uid, category=names(sortedvenues[sv]), qty=matrix(sortedvenues)[sv], timeslot=timingWE[i], clusterpoints=sum(d$cluster == c), loc=c(clusCenter[2], clusCenter[1])))
						}
						print(tail(sortedvenues,2))
					}
				}
			}
		}
	}
}
