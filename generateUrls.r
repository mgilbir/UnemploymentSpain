Sys.setlocale("LC_TIME","es_ES");
Sys.setlocale("LC_ALL", "es_ES.ISO8859-1");

urlbase <- "http://www.sepe.es/contenidos/cifras/datos_estadisticos/municipios/"
provinces <- read.csv("provinces.csv", colClasses=c("character", "character"))

minimumDate <- ISOdate(2005, 5, 1)
#minimumDate <- ISOdate(2009, 10, 1)
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
				url <- format(d,"%B_%Y/MUNI_field_%m%y.xls")
				url	<- paste(urlbase,sub("field", field, url), sep = "")
	
					urls <- rbind(urls, data.frame(month, year, I(url), province=field))
			}
		}
	}
}

write.table(urls, "urls.csv", sep=",", quote=FALSE, row.name=FALSE)