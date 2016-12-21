##### Section II Data weights
################################################

#THIS SECTION ONLY NEEDS TO BE RUN THE FIRST TIME
#IF IT GETS RUN AGAIN DELETE THE FILE FIRST!!!!!!!!!!!!!!!!
#_______________________________________________________________________

#Create weights
fileNames <- c('spatial3.csv','spatial4.csv','spatial5.csv','spatial6.csv','spatial7.csv','spatial8.csv','spatial9.csv','spatial10.csv','spatial12.csv','spatial13.csv')

for (fileName in fileNames){
	#read data
	sample <- read.csv(fileName,
							 header=TRUE,
							 sep=',')
	#compute data
	#create dataframe
	xy <- data.frame(longitude = sample$start_longitude, latitude = sample$start_latitude)
	#rotate the data
	rotate <- rotate.axis(xy, 30)
	#bind the rotated data to the original data
	new <- cbind (sample, rotate)
	#add in regions these are taken from the CART model: info is further down
	new$region=NA
	new <- within(new, region[newx > -105.229 & newy > 127.354] <- 'A')
	new <- within(new, region[newx < -105.229 & newy > 127.354] <- 'B')
	new <- within(new, region[newy < 127.354] <- 'C')
	
	#weights need adjusted by the number of samples taken in each region
	a <- subset(new, region=='A')
	a$points <- length(levels(as.factor(a$start_longitude)))
	b <- subset(new, region=='B')
	b$points <- length(levels(as.factor(b$start_longitude)))
	c <- subset(new, region=='C')
	c$points <- length(levels(as.factor(c$start_longitude)))
	
	new <- rbind(a, b, c)
	
	#aggregate and merge the weight data files
	input <- aggregate(points~region, data=new, FUN=mean)
	names(input) <- c('region', 'points')
	output <- aggregate(biomass_kg~region+length, data=new, FUN=sum)
	output <- merge(input, output, by='region')
	output <- cbind(output, fileName)
	
	#write new data to seperate file
	write.table(output,
					'allSamples.csv',
					append=TRUE,
					sep=',',
					row.names=FALSE,
					col.names = FALSE)
}
