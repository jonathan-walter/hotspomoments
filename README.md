# hotspomoments

Statistical approaches to detecting ecosystem hot spots and hot moments

'hotspomoments' uses the skewness and kurtosis of distributions of ecosystem measurements to where/when hot spots/hot moments (HSHM) are present, and to identify which observations constitute HSHM. We test whether skewness and/or kurtosis are significantly greater than expected if HSHM were not present by comparing against a reference distribution using a parametric bootstrapping procedure. Observations constituting HSHM can be identified by comparison to a reference distribution or other methods of identifying extreme values.

A manuscript describing the methodology in detail with applications to empirical case study examples is forthcoming.

## Example

```{r example}

# Get a dataset -----------------------------------------------------------------------------------

# Package ID: edi.420.1 Cataloging System:https://pasta.edirepository.org.
# Data set title: Hypereutrophic lake spatial sensor data during summer bloom, Swan Lake, Iowa, USA 2018.
# Data set creator:  David Ortiz - Iowa State University 
# Data set creator:  Grace Wilkinson - Iowa State University 
# Contact:  David Ortiz -  Iowa State University  - daortiz@iastate.edu
# Contact:  Grace Wilkinson -  Iowa State University  - wilkinso@iastate.edu
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/420/1/5419cbb0b2174c90aae9703acac7d4ff" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")

dt1 <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "Lake",     
                 "Site",     
                 "DOY",     
                 "TimeFraction",     
                 "DOY_TimeFraction",     
                 "Year",     
                 "Latitude",     
                 "Longitude",     
                 "Chlorophyll",     
                 "Phycocyanin",     
                 "pH",     
                 "DissolvedOxygen_Saturation",     
                 "DO_mg",     
                 "Temperature",     
                 "Specific_Conductivity",     
                 "TotalDissolved_Soilds",     
                 "Macrophyte"    ), check.names=TRUE)

unlink(infile1)

dt1 <- dt1[, colnames(dt1) %in% c("Site","DOY","DissolvedOxygen_Saturation")]
dt1 <- dt1[complete.cases(dt1),]


# Do some analyses --------------------------------------------------------------------------------

#Check histogram 
hist(dt1$DissolvedOxygen_Saturation)
sample_skewness(dt1$DissolvedOxygen_Saturation) #right-skewed
sample_kurtosis(dt1$DissolvedOxygen_Saturation)

#Test for HSHM
hshmtest_swanDO <- hshmtest(dt1$DissolvedOxygen_Saturation, stat="skewness")
#Note: using skewness because the histogram shows right skew and the focal variable doesn't change sign
#indicating, e.g., source vs. sink.

#Identify HSHM observations
hshmid_swanDO <- hshmid(dt1$DissolvedOxygen_Saturation, criteria = "ref.normal", side="upper", thresh=0.95)

#Plot where in the distribution the HSHM are identified
hist(dt1$DissolvedOxygen_Saturation)
points(dt1$DissolvedOxygen_Saturation[hshmid_swanDO], rep(0,sum(hshmid_swanDO)), pch=19, col="red")

#Inspect HSHM observations
hshm <- dt1[hshmid_swanDO,]
print(table(dt1$Site)) #number of times each site appears in the dataset
print(table(hshm$Site)) #number of times each site appears as a HSHM
#Most sites are HSHM once or twice --  little evidence for hot spots

print(table(dt1$DOY))
print(table(hshm$DOY))
#Only a few sampling dates have HSHM -- better evidence for hot moments

```
