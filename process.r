library(gdata)
Sys.setlocale("LC_TIME","es_ES");
#Sys.setlocale("LC_ALL", "es_ES.ISO8859-15");
Sys.setlocale("LC_ALL", "en_US.UTF-8");

urlbase <- "csv/"
provinces <- read.table("provinces.csv", colClasses=c("character", "character"), sep=",", header=TRUE)

minimumDate <- ISOdate(2005, 5, 1)
#minimumDate <- ISOdate(2009, 10, 1)
maximumDate <- ISOdate(2009, 10, 1)

urls <- data.frame()

for (year in 2005:2005)
{
	for (month in 1:12)
	{
		d <- ISOdate(year,month,1)
		if (d>=minimumDate && d<=maximumDate)
		{
			for (field in provinces$Field)
			{
				url <- format(d,"MUNI_field_%m%y.csv")
				url	<- paste(urlbase,sub("field", field, url), sep = "")
				urls <- rbind(urls, data.frame(month, year, I(url), province=field))
			}
		}
	}
}

dataProvinces <- data.frame()
dataCounties <- data.frame()
allData <- list()

processUrl <- function(entry)
{
	print(entry)
	#, colClasses=c("factor", rep("numeric", 12))
	dataTemp <- read.table(entry$url, sep=",", colClasses=c("factor", rep("numeric", 12)), stringsAsFactors=FALSE )
	names(dataTemp) <- c("MUNICIPIOS","TOTAL","HOMBRES_MENOS_25","HOMBRES_25_44","HOMBRES_MAS_45","MUJERES_MENOS_25","MUJERES_25_44","MUJERES_MAS_45","AGRICULTURA","INDUSTRIA","CONSTRUCCION","SERVICIOS","SIN_EMPLEO_ANTERIOR")	
	

	#Remove last line as it is useless
	size <- nrow(dataTemp)
	#if(paste(dataTemp[size,1],"."
	#dataTemp <- dataTemp[1:size,]

			
	#Cleanup the format of the numbers
	#dataTemp2 <- as.data.frame(sapply(dataTemp[2:13], cleanupNumbers))
	#dataTemp2[is.na(dataTemp2)] <- 0
	#dataTemp2 <- cbind(sapply(dataTemp$MUNICIPIOS, cleanupText), dataTemp2)
	#dataTemp<-dataTemp2
	
	#Add column with month and year
	timestamp <- rbind(data.frame(rep(entry$month, size), rep(entry$year, size)))
	dataTemp <- cbind(dataTemp, timestamp)
	dataTemp <- cbind(dataTemp, rep(as.factor(entry$province), size))
	names(dataTemp) <- c("MUNICIPIOS","TOTAL","HOMBRES_MENOS_25","HOMBRES_25_44","HOMBRES_MAS_45","MUJERES_MENOS_25","MUJERES_25_44","MUJERES_MAS_45","AGRICULTURA","INDUSTRIA","CONSTRUCCION","SERVICIOS","SIN_EMPLEO_ANTERIOR", "MONTH", "YEAR", "PROVINCE")	

	#Extract last line for the general analysis and remove it from this data.frame
	dataProvinces <- dataTemp[size,2:16]
	dataTemp <- dataTemp[1:size-1,]
	
	#return everything
	list(dataProvinces=dataProvinces, dataCounties=dataTemp)
}

allData <- by(urls, urls$url, processUrl)
for (entry in allData)
{
	dataProvinces <- rbind(dataProvinces, entry$dataProvinces)
	dataCounties <- rbind(dataCounties, entry$dataCounties)	
}

