library(gdata)
library(RColorBrewer)
library(ggplot2)
Sys.setlocale("LC_TIME","es_ES");
#Sys.setlocale("LC_ALL", "es_ES.ISO8859-15");
Sys.setlocale("LC_ALL", "en_US.UTF-8");

# Original data from http://www.sepe.es/contenidos/cifras/datos_estadisticos/municipios/index.html

urlbase <- "csv/"
provinces <- read.table("provinces.csv", colClasses=c("character", "character"), sep=",", header=TRUE)

minimumDate <- ISOdate(2005, 5, 1)
maximumDate <- ISOdate(2009, 10, 1)

urls <- data.frame()

for (year in 2005:2009)
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
	names(dataTemp) <- c("PROVINCIA","TOTAL","HOMBRES_MENOS_25","HOMBRES_25_44","HOMBRES_MAS_45","MUJERES_MENOS_25","MUJERES_25_44","MUJERES_MAS_45","AGRICULTURA","INDUSTRIA","CONSTRUCCION","SERVICIOS","SIN_EMPLEO_ANTERIOR")	
	
	#Add column with month and year
	timestamp <- rbind(data.frame(entry$month, entry$year))
	dataTemp <- cbind(dataTemp, timestamp)
	#dataTemp <- cbind(dataTemp, as.factor(entry$province))
	names(dataTemp) <- c("PROVINCIA","TOTAL","HOMBRES_MENOS_25","HOMBRES_25_44","HOMBRES_MAS_45","MUJERES_MENOS_25","MUJERES_25_44","MUJERES_MAS_45","AGRICULTURA","INDUSTRIA","CONSTRUCCION","SERVICIOS","SIN_EMPLEO_ANTERIOR", "MONTH", "YEAR")


	dataTemp
}

allData <- by(urls, urls$url, processUrl)
for (entry in allData)
{
	dataProvinces <- rbind(dataProvinces, entry)
}

dataProvinces$MENOS_25 <- dataProvinces$HOMBRES_MENOS_25 + dataProvinces$MUJERES_MENOS_25
dataProvinces$ENTRE_25_44 <- dataProvinces$HOMBRES_25_44 + dataProvinces$MUJERES_25_44
dataProvinces$MAS_45 <- dataProvinces$HOMBRES_MAS_45 + dataProvinces$MUJERES_MAS_45
dataProvinces$QUARTER <- floor((dataProvinces$MONTH-1)/3)+1

# Renaming some levels to make it more compatible with other sources
i <- which(levels(dataProvinces$PROVINCIA)=="STA C TENERIFE")
levels(dataProvinces$PROVINCIA)[i]<-"SANTA CRUZ TENERIFE"

i <- which(levels(dataProvinces$PROVINCIA)=="ALICANTE/ALACANT")
levels(dataProvinces$PROVINCIA)[i]<-"ALICANTE"

i <- which(levels(dataProvinces$PROVINCIA)=="ILLES BALEARS")
levels(dataProvinces$PROVINCIA)[i]<-"BALEARS (ILLES)"

i <- which(levels(dataProvinces$PROVINCIA)=="CASTELLON/CASTELLO")
levels(dataProvinces$PROVINCIA)[i]<-"CASTELLÓN DE LA PLANA"

i <- which(levels(dataProvinces$PROVINCIA)=="JAEN")
levels(dataProvinces$PROVINCIA)[i]<-"JAÉN"

i <- which(levels(dataProvinces$PROVINCIA)=="LEON")
levels(dataProvinces$PROVINCIA)[i]<-"LEÓN"

i <- which(levels(dataProvinces$PROVINCIA)=="MALAGA")
levels(dataProvinces$PROVINCIA)[i]<-"MÁLAGA"

i <- which(levels(dataProvinces$PROVINCIA)=="CADIZ")
levels(dataProvinces$PROVINCIA)[i]<-"CÁDIZ"

i <- which(levels(dataProvinces$PROVINCIA)=="ALMERIA")
levels(dataProvinces$PROVINCIA)[i]<-"ALMERÍA"

i <- which(levels(dataProvinces$PROVINCIA)=="GUIPUZCOA")
levels(dataProvinces$PROVINCIA)[i]<-"GUIPÚZCOA"

i <- which(levels(dataProvinces$PROVINCIA)=="CACERES")
levels(dataProvinces$PROVINCIA)[i]<-"CÁCERES"

i <- which(levels(dataProvinces$PROVINCIA)=="CORDOBA")
levels(dataProvinces$PROVINCIA)[i]<-"CÓRDOBA"


calculateYearMonth <- function (x)
{
	x<-x[1]+(x[2]-1)/12
	x
}

estudioEdad <- function(provincia)
{
temp2 <- subset(dataProvinces, dataProvinces$PROVINCIA==provincia)
temp3 <- cbind(temp2$MENOS_25, temp2$ENTRE_25_44, temp2$MAS_45)
temp4 <- as.data.frame(cbind(aperm(apply(temp3, 1, cumsum)), temp2$YEAR, temp2$MONTH))
temp3 <- cbind(temp2$YEAR, temp2$MONTH)
temp4<- cbind(temp4, apply(temp3, 1, calculateYearMonth))
names(temp4)<-c("MENOS_25", "ENTRE_25_44", "MAS_45", "YEAR", "MONTH", "TIMESTAMP")
catColor <- brewer.pal(3,"RdYlBu")
d<-ggplot(temp4 ,aes(x=TIMESTAMP))+
 geom_area(aes(y=MAS_45, fill="Mayores de 44 años"))+
 geom_area( aes(y=ENTRE_25_44, fill="Entre 25 y 44 años"))+
 geom_area( aes(y=MENOS_25, fill="Menores de 25 años"))+
 scale_fill_manual(name="Edad", 
		values=c("Mayores de 44 años"=catColor[1], 
				 "Entre 25 y 44 años"=catColor[2],
				 "Menores de 25 años"=catColor[3]))

d + opts(title =paste("Personas paradas (",provincia,")")) + ylab(NULL) + xlab(NULL)
}

estudioSectores <- function(provincia)
{
temp2 <- subset(dataProvinces, dataProvinces$PROVINCIA==provincia)
temp3 <- cbind(temp2$AGRICULTURA, temp2$INDUSTRIA, temp2$CONSTRUCCION, temp2$SERVICIOS, temp2$SIN_EMPLEO_ANTERIOR)
temp4 <- as.data.frame(cbind(aperm(apply(temp3, 1, cumsum)), temp2$YEAR, temp2$MONTH))
temp3 <- cbind(temp2$YEAR, temp2$MONTH)
temp4<- cbind(temp4, apply(temp3, 1, calculateYearMonth))
names(temp4)<-c("AGRICULTURA", "INDUSTRIA", "CONSTRUCCION", "SERVICIOS", "SIN_EMPLEO_ANTERIOR", "YEAR", "MONTH", "TIMESTAMP")
catColor <- brewer.pal(5,"RdYlBu")
d<-ggplot(temp4 ,aes(x=TIMESTAMP)) +
 geom_area( aes(y=SIN_EMPLEO_ANTERIOR, fill="Sin Empleo Anterior"))+
 geom_area( aes(y=SERVICIOS, fill="Servicios"))+
 geom_area( aes(y=CONSTRUCCION, fill="Construccion"))+
 geom_area( aes(y=INDUSTRIA, fill="Industria"))+ 
 geom_area(aes(y=AGRICULTURA, fill="Agricultura"))+
 scale_fill_manual(name="Sector", 
		values=c("Sin Empleo Anterior"=catColor[5], 
				 "Servicios"=catColor[4],
				 "Construccion"=catColor[3],
				 "Industria"=catColor[2],
				 "Agricultura"=catColor[1])) 

d + opts(title =paste("Personas paradas (",provincia,")")) + ylab(NULL) + xlab(NULL)
}

# Compare with the quarterly figures from the INE

require(rgdal)
require(RColorBrewer)


colorSelector <- function(x)
{
col_no <-cut(as.double(as.character(x)), c( 0.00 , 1.25 , 2.50 , 3.75 , 5.00 , 6.25 , 7.50 , 100.00))
levels(col_no) <- c( "< 1.25%", "1.25 - 2.50%", "2.50 - 3.75%", "3.75 - 5.00%", "5.00 - 6.25%", "6.25 - 7.50%", "> 7.50%")
col_no
}

cols <- c(	"TOTAL",
			"HOMBRES_MENOS_25",
		  	"HOMBRES_25_44",
			"HOMBRES_MAS_45" ,
			"MUJERES_MENOS_25",
			"MUJERES_25_44",
			"MUJERES_MAS_45",
			"AGRICULTURA",
			"INDUSTRIA",
			"CONSTRUCCION",
			"SERVICIOS",
			"SIN_EMPLEO_ANTERIOR",
			"MENOS_25",
			"ENTRE_25_44",
			"MAS_45")
			
individualToPercentage <- function(v)
{
		v<-as.array(v)
		m <- match(as.numeric(as.character(v["ProvinciaID"])), as.numeric(as.character(info$ProvinciaID)))
		year <- v["YEAR"][1]
		quarter <- v["QUARTER"][1]
		percentage <- as.double(info[m, paste("Y", year, "Q", quarter, sep="")])
		percentage
}


info <- read.table("tasa_paro_provincias.csv", sep=",", colClasses=c("factor","factor", rep("double", 15)), stringsAsFactors=FALSE, header=TRUE )
periods <- sapply(sapply(2009:2005, function(x) paste("Y",x,"Q",4:1, sep="")), rbind)[-1]
names(info) <- c("ProvinciaID","Provincia", periods)
info$Y2005Q4 <- as.double(as.character(info$Y2005Q4))
info$Y2005Q3 <- as.double(as.character(info$Y2005Q3))

m <- match(toupper(as.character(dataProvinces$PROVINCIA)), toupper(as.character(info$Provincia)))
dataProvinces$ProvinciaID <- as.numeric(as.character(info[m, "ProvinciaID"]))

aggregatedTemp <- aggregate(dataProvinces[cols], list(dataProvinces$YEAR, dataProvinces$QUARTER, dataProvinces$ProvinciaID), sum)
names(aggregatedTemp) <- c("YEAR", "QUARTER", "ProvinciaID", cols)
percentage <- as.double(apply(aggregatedTemp, 1, individualToPercentage))

dataAggregated <- cbind(aggregatedTemp, percentage)
dataAggregated <- dataAggregated[is.na(percentage)==FALSE,]
dataAggregated[cols] <- dataAggregated[cols] * dataAggregated$percentage / dataAggregated$TOTAL

provincias <- readOGR("maps/","spain_provinces_ag_2")

estudioPeriodoMap <- function(year, quarter)
{
	dataSelected <- subset(dataAggregated, dataAggregated$YEAR == year & dataAggregated$QUARTER == quarter)
	m <- match(as.numeric(as.character(provincias@data$PROV)), as.numeric(as.character(dataSelected$ProvinciaID)))
	provincias@data <- cbind(provincias@data, lapply(dataSelected[m,cols], colorSelector))
	#provincias@data <- cbind(provincias@data, dataSelected[m,cols])
	provincias
}


estudioSectorMap <- function(year, quarter, sector)
{
	colours = brewer.pal(7,"OrRd")
	dataPeriod <- estudioPeriodoMap(year, quarter)
	d<-spplot(dataPeriod, sector, col.regions=colours, main=paste("Tasa de paro - ", sector, " - ", year ,"Q", quarter, sep=""))
	d
	#spplot(dataPeriod, sector, main=paste("Tasa de paro - ", sector, " - ", year ,"Q", quarter, sep=""))
}

dir.create("images")
dir.create("images/png")


for (year in 2005:2009)
{
	for (quarter in 1:4)
	{
		if (year==2009 && quarter == 4)
		{
			print ("Not available")
		} else
		{
			for (sector in cols)
			{
				basedir <- paste("images/png/", gsub("(^ +)|( +$)", "", sector), "/", sep="")				
				dir.create(basedir)
				tablename<-paste("Y",year,"Q",quarter, sep="")
				png(file=paste(basedir,"paroSectores_", tablename,"_",sector,".png", sep=""), width=800, bg="grey")
				print(estudioSectorMap(year, quarter, sector))
				dev.off()
			}
		}
	}
}
