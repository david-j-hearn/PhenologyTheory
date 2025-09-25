getSpecimenData = function(getMetadata=F,speciesListFile) {
library(ridigbio)
library(magick)
library(raster)
library(sp)
library(elevatr)
library(rgdal)

#setwd("D:\\Projects\\HerbariumConfounding\\Attributes\\")
#setwd("/home/david/Documents/Projects/HerbariumConfounding/Attributes/")
#path = paste("/home/david/Documents/Projects/HerbariumConfounding/Attributes/", speciesListFile,sep="");
path = paste("home/david/Documents/Projects/HerbariumConfounding/FinalAttempt/Analyses/A7_Empirical/HerbariumDNN/HerbariumDNNFolders/Attributes/", speciesListFile,sep="")
species = read.table(path, header=F, sep='\t')
species = species$V2

#fileConn<-file("SpecimenData.txt")

data <- data.frame(matrix(ncol = 3, nrow = 0))
head <- c("Species", "UUID", "URI")
colnames(data) <- head 

badNames = vector()

tot=0
for(sp in species)
	{
	sp <- sub("_", " ", sp)
	print(sp)

	#UUID in idig_search_media changes from one session to another!! use "records" field. INCONSISTENT FIELD NAMEs, super annoying
	records <- idig_search_media(rq=list(scientificname=sp, geopoint=list(type="exists"), hasMedia=TRUE), mq=list("data.ac:accessURI"=list("type"="exists")), fields=c("uuid","data.ac:accessURI", "records"), limit=50000)
	#records <- idig_search_media(rq=list(scientificname=sp, geopoint=list(type="exists")), mq=list("data.ac:accessURI"=list("type"="exists")), limit=50000) #all fields
	print(nrow(records))

	#fields found at: https://github.com/iDigBio/idigbio-search-api/wiki/Index-Fields#media-query-fields
	#get specimen data (UUID here = records above)
	if(getMetadata) {
	records1 <- idig_search_records(rq=list(scientificname=sp, geopoint=list(type="exists")), fields=c("uuid", "geopoint","eventdate"), limit=50000) 
	#records1 <- idig_search_records(rq=list(scientificname=sp, geopoint=list(type="exists")), fields=c("uuid", "geopoint","eventdate","occurrenceid"), limit=50000) 
	#records1 <- idig_search_records(rq=list(scientificname=sp, geopoint=list(type="exists")), fields="all", limit=50000)  # all fields


	#get climate data
	#extract and write the elevation data based on library(elevatr)
	lats = records1[c("geopoint.lat")]
	lons = records1[c("geopoint.lon")]
	coords = data.frame(x=lons,y=lats)
	df = data.frame(get_elev_point(coords, prj="EPSG:4326"))
	df <- cbind.data.frame(data.frame(records1[c("uuid", "geopoint.lon", "geopoint.lat","eventdate")]),df)
	sp <- sub(" ", "_", sp)
	#path = paste("/home/david/Documents/Projects/HerbariumConfounding/Data1/", sp,".csv",sep="")
	path = paste("/home/david/Documents/Projects/HerbariumConfounding/FinalAttempt/Analyses/A7_Empirical/Data", sp,".csv",sep="")
	#records1 = data.frame(lapply(records1, as.character), stringsAsFactors=FALSE)
	write.csv(df, file=path, row.names=FALSE);
	#write.csv(records1, file=path, row.names=FALSE);
	}

	if(nrow(records)==0) 
		{
		badNames = append(badNames,sp)
		}

	else {
		cnt = 0
		for(i in 1:nrow(records)) {
			uuid = records[["records"]][i] #get "records" field!
			uri = records[["data.ac:accessURI"]][i]
			data[i+tot,]=NA
			data$Species[i+tot] = sp
			data$UUID[i+tot] = uuid
			data$URI[i+tot] = uri
			print(c(uuid,uri,sp))
			}
		tot = tot+nrow(records)
		}
	}

for( i in 1:nrow(data)) 
	{
	#path = "/home/david/Documents/Projects/HerbariumConfounding/Images1/"
	path = "/home/david/Documents/Projects/HerbariumConfounding/FinalAttempt/Analyses/A7_Empirical/HerbariumDNN/HerbariumDNNFolders/Images"
	path = paste(path, sub(" ", "_", data$Species[i]), "_", data$UUID[i], ".jpg", sep="")

	print("New record")
	print(data$UUID[i])
	print(path)

	if(!is.na(data$UUID[i])) 
		{
		if(!file.exists(path))
			{
			#print(path)
			#print("File does not exist")
			out <- tryCatch(
				{
				download.file(data$URI[i], path, mode='wb')
				img = image_read(path)
				img = image_scale(img, "700")
				file.remove(path)
				image_write(img, path=path, format="jpg")
				image_destroy(img)
				gc()
		 		},
		 	error=function(cond) 
		 		{
		 		message(paste("URL does not seem to exist:", data$URI[i]))
				message("Here's the original error message:")
				message(cond)
				message("\n");
			 	},
			warning=function(cond) 
				{
				message(paste("URL caused a warning:", data$URI[i]))
				message("Here's the original warning message:")
				message(cond)
				message("\n");
				},
			finally={
				message(paste("Processed URL:", data$URI[i], "\n\n"))
				}
				) 
			}
		else 
			{
				print(path)
				print("File already exists. Continuing")
			}
		}
	}


#close(fileConn)
}
