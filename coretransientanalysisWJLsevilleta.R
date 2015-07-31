##########################################################
# Dataset 236 Sevilletta LTER Core-transient analysis
# Metadata for species occupancy: http://sev.lternet.edu/data/sev-8
# Metadata for dryad dataset: http://datadryad.org/resource/doi:10.5061/dryad.sb175.2
# Additional metadata for dryad: http://onlinelibrary.wiley.com/doi/10.1111/mec.12572/full
# Metadata for Ernest life history dataset: http://www.esapubs.org/archive/ecol/E084/093/default.htm#data
# Metadata for pantheria dataset: http://esapubs.org/Archive/ecol/E090/184/metadata.htm
##########################################################

# setwd('C:/Users/wlarsen/git/coretransientsummer')

# Set up species code - species conversion

allcodes = c('amin','amle','chin','dime','dior','disp','eudo','euqu','neal','nemi','onar','onle','pebo','pedi','peer','pele','pema','petr','pgfl','pgfv','remg','remn','sihi','spsp')

allspecies =  c('Ammospermophilus interpres','Ammospermophilus leucurus','Chaetodipus intermedius','Dipodomys merriami','Dipodomys ordii','Dipodomys spectabilis','Eutamias dorsalis','Eutamias quadrivittatus','Neotoma albigula','Neotoma micropus','Onychomys arenicola','Onychomys leucogaster','Peromyscus boylii','Peromyscus difficilis','Peromyscus eremicus','Peromyscus leucopus','Peromyscus maniculatus','Peromyscus truei','Perognathus flavescens','Perognathus flavus','Reithrodontomys megalotis','Reithrodontomys montanus','Sigmodon hispidus','Spermophilus spilosoma')

allspeccodes = data.frame(allspecies, allcodes)

# Get propOcc dataset:

dataset = read.csv('propOcc_232WJL.csv')

dataset1 = dataset[word(dataset$site,1,sep="_") == '5plarrea',]

# Found meaning of species codes from metadata, switching to full species names

currcodes = levels(factor(dataset1$species))

currspecies = allspeccodes$allspecies[allspeccodes$allcodes %in% currcodes]

# Make a column with the average propOcc for each species
avgpropOcc = c()

for (i in 1:length(currcodes)){
  
  avgpropOcc[i] = sum(dataset1[dataset1$species == currcodes[i],]$propOcc)/length((dataset1[dataset1$species == currcodes[i],]$propOcc))
  
}

# Make dataset with average propOcc and species, remove NAs
avgdataset = data.frame(currspecies,currcodes, avgpropOcc )
avgdataset = avgdataset[order(avgpropOcc),]
avgdataset = na.omit(avgdataset)


# List of transient and core species for reference:

tranlist = data.frame(table(dataset1$species[dataset1$propOcc<0.5]))$Var1[as.integer(data.frame(table(dataset1$species[dataset1$propOcc<0.5]))$Freq)>0]

corelist = data.frame(table(dataset1$species[dataset1$propOcc>0.5]))$Var1[as.integer(data.frame(table(dataset1$species[dataset1$propOcc>0.5]))$Freq)>0]

# Create core column, logical TRUE if core, FALSE if transient
avgdataset$core = rep(FALSE,length(currcodes))

for (i in 1:length(currcodes)){
  
  if (avgdataset$avgpropOcc[i] >0.5) {
    avgdataset$core[i] = TRUE
  }
  
  
}


# Plot
barplot(avgdataset$avgpropOcc, names.arg = avgdataset$speccodes)

### Dryad dataset
dryad = read.csv('Data4Dryad.csv')
dryadsp = dryad[dryad$TAXON %in% 'mammals',]
dryadsp$SPECIES = gsub("_"," ",dryadsp$SPECIES)
# Extract needed species
dryadsp = dryadsp[dryadsp$SPECIES %in% currspecies,]
# Remove unneccessary columns
dryadsp = dryadsp[-1]
head(dryadsp)
# Does not have an entry for Octodon degus



### Ernest life history dataset
lifehist = read.csv('Mammal_lifehistories_v2.csv')
lifehist$scientificname = paste(lifehist$Genus,lifehist$species,sep=" ")
lifehist = lifehist[c(15,5:14)]

# This dataset has 'Abrocoma bennetti', it should be 'Abrocoma bennettii'
lifehist$scientificname[lifehist$scientificname == 'Abrocoma bennetti'] = 'Abrocoma bennettii'

# Extract needed species
lifehistsp = lifehist[lifehist$scientificname %in% currspecies,]
head(lifehistsp)

# In this dataset, if a value is not available, a -999 is put in its place, replacing with NA's
lifehistsp[lifehistsp == -999] = NA_character_

# Remove unneccessary column:
lifehistsp = lifehistsp[-11]

head(lifehistsp)
# Does not have an entry for Abrothrix genus, Thylamys elegans, or Chelemys megalonyx


### Pantheria dataset
pantheria = read.csv('PanTHERIA_1-0_WR05_Aug2008.csv')

# Extract needed species
pantheria.sp = pantheria[pantheria$MSW05_Binomial %in% currspecies,]

# In this dataset, if a value is not available, a -999 is put in its place, replacing with NA's
pantheria.sp[pantheria.sp == -999] = NA_character_

# Many columns unnecessary:
unusedfields = c('MSW05_Order','MSW05_Family','MSW05_Genus','MSW05_Species','References')

# Also removing columns with all NA's (no usable data):
for (i in 1:dim(pantheria.sp)[2]){
  
  if(sum(is.na(pantheria.sp[,i]))==dim(pantheria.sp)[1]){
    
    unusedfields = c(unusedfields, names(pantheria.sp)[i])
    
  }
  
}
unusedfieldsind = which(names(pantheria.sp) %in% unusedfields)
pantheria.sp = pantheria.sp[-unusedfieldsind]

head(pantheria.sp)

### Merging the three datasets
# Merging dryad and life history datasets first
merge1= merge(dryadsp,lifehistsp, by.x = "SPECIES", by.y = "scientificname", all=TRUE)
# Check:
dim(dryadsp)
dim(lifehistsp)
dim(merge1)
# Merging first merge dataset with pantheria.sp
merge2 = merge(merge1,pantheria.sp, by.x = "SPECIES", by.y = "MSW05_Binomial", all=TRUE)
dim(merge1)
dim(pantheria.sp)
dim(merge2)
# Merge with avgdataset
datasetm = merge(avgdataset,merge2,by.x = "currspecies",by.y="SPECIES",all=TRUE)
# Check:
dim(avgdataset)
dim(merge2)
dim(datasetm)

plot(datasetm$X6.1_DietBreadth,datasetm$avgpropOcc, main = 'Diet Breadth vs Occupancy',xlab = 'Diet Breadth', ylab = 'Occupancy')
abline(lm(datasetm$avgpropOcc~as.numeric(datasetm$X6.1_DietBreadth)))


plot(datasetm$X12.1_HabitatBreadth,datasetm$avgpropOcc)
#abline(lm(datasetm$avgpropOcc~datasetm$X12.1_HabitatBreadth))
abline(0.5,0)

plot(log10(as.numeric(datasetm$X26.1_GR_Area_km2)),datasetm$avgpropOcc,
main = 'Geographic Range vs Occupancy', xlab = 'Geographic Range (log 10 km2)', ylab = 'Occupancy')
abline(lm(datasetm$avgpropOcc~log10(as.numeric(datasetm$X26.1_GR_Area_km2))))



plot(datasetm$X15.1_LitterSize,datasetm$avgpropOcc,
main ='Life History: Litter Size vs. Occupancy', xlab = 'Litter Size', ylab = 'Occupancy')
abline(lm(datasetm$avgpropOcc~as.numeric(datasetm$X15.1_LitterSize)))

plot(datasetm$X16.1_LittersPerYear,datasetm$avgpropOcc,
main = 'Life History: Litters per Year vs Occupancy',xlab = 'Litters per Year', ylab = 'Occupancy')
abline(lm(datasetm$avgpropOcc~as.numeric(datasetm$X16.1_LittersPerYear)))

plot(datasetm$X17.1_MaxLongevity_m,datasetm$avgpropOcc,
main = 'Life History: Max Lifespan vs. Occupancy' ,xlab = 'Max Lifespan (months)', ylab = 'Occupancy')
abline(lm(datasetm$avgpropOcc~as.numeric(datasetm$X17.1_MaxLongevity_m)))

