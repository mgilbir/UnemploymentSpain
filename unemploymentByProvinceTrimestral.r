Sys.setlocale("LC_ALL", "en_US.UTF-8");

require(rgdal)
require(RColorBrewer)

info <- read.table("tasa_paro_provincias.csv", sep=",", colClasses=c("factor","factor", rep("double", 15)), stringsAsFactors=FALSE, header=TRUE )
periods <- sapply(sapply(2009:2005, function(x) paste("Y",x,"Q",4:1, sep="")), rbind)[-1]
names(info) <- c("ProvinciaID","Provincia", periods)
info$Y2005Q4 <- as.double(as.character(info$Y2005Q4))
info$Y2005Q3 <- as.double(as.character(info$Y2005Q3))

provincias <- readOGR("maps/","spain_provinces_ag_2")

colorSelector <- function(x)
{
col_no <-cut(as.double(as.character(x)), c(0.00, 3.75,  7.50, 11.25, 15.00, 18.75, 22.50, 26.25, 100))
levels(col_no) <- c("< 3.75%","3.75 - 7.50%", "7.50 - 11.25%", "11.25 - 15.00%", "15.00 - 18.75%", "18.75 - 22.50%", "22.50 - 26.25%", "> 26.25%")
col_no
}

#periods <- sapply(sapply(2009:2005, function(x) x+(-0.5 + 4:1)/4), rbind)


m <- match(as.numeric(as.character(provincias@data$PROV)), as.numeric(as.character(info$ProvinciaID)))
provincias@data <- cbind(provincias@data, lapply(info[m,-(1:2)], colorSelector))

colours = brewer.pal(8,"OrRd")
#colours = brewer.pal(8, "Purples")
#spplot(provincias, periods, col.regions=colours, main="Tasa de Paro")

dir.create("images")
dir.create("images/png")
dir.creage("images/png/general")

for (year in 2005:2009)
{
	for (quarter in 1:4)
	{
		if (year==2009 && quarter == 4)
		{
			print ("Not available")
		} else
		{
			tablename<-paste("Y",year,"Q",quarter, sep="")
			png(file=paste("images/png/general/paro", tablename,".png", sep=""), width=800, bg="grey")
			print(spplot(provincias, tablename, col.regions=colours, main=paste("Tasa de paro ",year,"Q",quarter, sep="")))
			dev.off()
		}
	}
}

rangeFunc <- function(x)
{
x<-as.double(as.character(x))
range(x)
}

rangos<-as.data.frame(aperm(sapply(by(info[,-(1:2)], info$ProvinciaID, rangeFunc), cbind)))
names(rangos)<-c("MINIMOS", "MAXIMOS")
m<-match(as.numeric(provincias$PROV), 1:52)
provincias@data <- cbind(provincias@data, rangos[m,])
spplot(provincias, c("MINIMOS","MAXIMOS"), col.regions=rev(heat.colors(256)))

aperm(info[,-(1:2)])



periodsData <- as.double(sapply(sapply(2009:2005, function(x) x+(-0.5 + 4:1)/4), rbind))[-1]
approxParo <- function(periodsData, input)
{
	
	approxfun(x=periodsData, y=input)
}

d<-ggplot()
monthly<- sapply(sapply(2005:2009, function (x) x + (-1 + 1:12)/12), cbind)
monthly<- monthly[monthly<=max(periodsData)]
monthly<- monthly[monthly>=min(periodsData)]

approxParo(periodsData, as.double(as.character(subset(info, info$Provincia=="Valladolid")[-(1:2)])))(monthly)


#d + geom_point(aes(x=periodstemp2, y=temp2monthly, colour="Interpolated"))+geom_point(aes(x=as.double(colnames(temp2)), y=temp2, colour="Original"))
 