

library(sp)
library(raster)
library(fields)
library(rgdal)
#-----------------------------
setwd("/Users/nicomartin/Documents/Safran/")
SafranCoord<-read.table("coord-grille-sim2-fev2018.csv", sep=";", h=T)
head(SafranCoord)

#-----------------------------------------------------------------------------------
#Charger les systemes de projection utilise & Attribuer les systemes de projection au DEM final
EPSG <- make_EPSG() #Recherche du bon code, exemple pour les projections Lambert xx
#Charge la projection LIIE dont le Code numerique est : 27572
numLIIE=match(27572,EPSG[EPSG[,"code"]==27572,1]) 
PStLIIE<-(EPSG[EPSG[,"code"]==27572,3])[numLIIE]
PStLIIECRS = CRS(PStLIIE)
#Charge la projection L93 dont le Code numerique est : 2154
PStL93num = match(2154,EPSG[EPSG[,"code"]==2154,1])  #L93
PStL93CRS=(EPSG[EPSG[,"code"]==2154,3])[PStL93num]
PStL93CRS = CRS(PStL93CRS)
PStWGS84num = match(4326,EPSG[EPSG[,"code"]==4326,1])
PStWGS84CRS=(EPSG[EPSG[,"code"]==4326,3])[PStWGS84num]
PStWGS84CRS = CRS(PStWGS84CRS)
#-----------------------------

#
library(raster)




#Data France GCM-RCM
pathMPFile <- "/Volumes/paca-urfm/URFM/Climat/Simulations/Downscaled_bias_corrected_France/Hadgem_RCA4/hist/"
MPFile = read.table(paste(pathMPFile, "MP.txt",sep=""))
#Géographique WGS84:


tableT = rbind.data.frame(
data.frame(Name="Puechabon", Long=3.5957, Lat=43.7413),
data.frame(Name="Font-Blanche", Long=5.67865, Lat=43.24079),
data.frame(Name="O3HP", Long=05+42/60+642/3600, Lat=43+56/60+115/3600))

pp.wgs <- SpatialPoints(as.matrix(tableT[,c(2,3)]),PStWGS84CRS)
row.names(pp.wgs)<-tableT[,1]
ppL2 <- spTransform(pp.wgs,PStLIIECRS)
sf = SpatialPoints(MPFile[,c(4,5)], PStLIIECRS)

rr = rasterFromXYZ(cbind(MPFile[,c(4,5)],as.numeric(row.names(MPFile))),crs=PStLIIECRS)

SafranISBA = rasterFromXYZ(cbind(SafranCoord[,c(4)]*100, SafranCoord[,c(5)]*100, SafranCoord[,c(1)]))


quartz()
plot(SafranISBA)
points(ppL2, pch=21, bg=4)
extract(SafranISBA,ppL2)

fil=NULL

vecnn <- c("CNRM_RCA4/","HadGem_RCA4/","MPI_ESM_RCA4/", 'MPI_ESM_REMO2009/r1i1p1/',"MPI_ESM_REMO2009/r2i1p1/")
sc <- c("hist", 'rcp4.5', 'rcp8.5')

quartz()
par(mfrow=c(3,3))
#plot(as.numeric(rownames(tapply(temp[vec,"MeanTemperature"],vecY[vec],mean))), tapply(temp[vec,"MeanTemperature"],vecY[vec],mean), type='l',xlim=c(1950, 2100), ylim=c(0,30), col=j, lty=i)
for(i in 1:nrow(tableT)){

dist=spDistsN1(sf,ppL2[i,])
which(dist==min(dist))
MPFile[which(dist==min(dist)),]

nameFil=row.names(ppL2)[i]
fil=as.character(MPFile[which(dist==min(dist)),"filename"])


for(nn in 1:length(vecnn)){
 
   for(j in 1:3)
  {
    
    
    pathtemp<-paste(vecnn[nn], sc[j], "/", sep="")
    pathI<-"/Volumes/paca-urfm/URFM/Climat/Simulations/Downscaled_bias_corrected_France/"
    #"/Volumes/vol_pef/pef/home/hfargeon/Documents/Workspace R/Projections/Data/Data LN/Bias corrected Safran/"
    pathO<-"/Users/nicomartin/Dropbox/WBM_ sous_R/OneSoilLayerModel/DATA/Climat_Scenario/"
    
    nameI<-paste(pathI, pathtemp, fil, sep="")
    nameO<-paste(pathO, pathtemp, nameFil, ".txt", sep="")
    
    temp=readRDS(nameI)
    vec=strftime(as.Date(row.names(temp)), format="%m") %in% c("06","07","08","09")
    vecY=strftime(as.Date(row.names(temp)), format="%Y")
   
    if(j==1 & nn==1){
      plot(as.numeric(rownames(tapply(temp[vec,"MeanTemperature"],vecY[vec],mean))), 
           tapply(temp[vec,"MeanTemperature"],vecY[vec],mean), type='l',
           xlim=c(1950, 2100), ylim=c(5,27), col=j, lty=i, main=tableT$Name[i], ylab="Temperature Mensuelle", xlab="Temps")
    } else{
      lines(as.numeric(rownames(tapply(temp[vec,"MeanTemperature"],vecY[vec],mean))), tapply(temp[vec,"MeanTemperature"],vecY[vec],mean), type='l',xlim=c(1950, 2100), ylim=c(10,30), col=j, lty=i)
    }
    if(i==9) {write.table(temp, nameO)}
    
  }
  
}
}


