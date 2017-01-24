# Load the Data --------------------------------------------------------------------------------------------------
setwd("~/Desktop/Telecom/Traiettorie/user_tracking_export/final_trajectories")

library(rgdal)
library(leaflet)

# join the data
load(file = '06112016.Rda')
uno <- prova

load(file = '07112016.Rda')
due <- prova

load(file = '08112016.Rda')
tre <- prova

load(file = '15112016.Rda')
quattro <- prova

load(file = '17112016.Rda')
cinque <- prova

load(file = '21112016.Rda')
sei <- prova

load(file = '22112016.Rda')
sette <- prova

load(file = '23112016.Rda')
otto <- prova

load(file = '24112016.Rda')
nove <- prova

df <- rbind.data.frame(uno, due, tre, quattro, cinque, sei, sette, otto, nove)
save(df,file='ninepiecedf.Rda')


# Creating Matrix Skelethon --------------------------------------------------------------------------------------

load(file='ninepiecedf.Rda')

# elimino doppie informazioni per lo stesso secondo (!!!)
df <- df[!(duplicated(df[,c('id','connected_seconds')])),]

# Creo la struttura della matrice
mat <- as.data.frame(matrix(NA,nrow=length(unique(df$id)),ncol=24*60*60))
colnames(mat) <- as.character(c(1:(24*60*60)))

bioinfo <- as.data.frame(matrix(NA,nrow=length(unique(df$id)), ncol=5))
colnames(bioinfo) <- c('id','type','gender','age','flagcc')
bioinfo$id <- unique(df$id)

matmove <- as.data.frame(cbind(bioinfo,mat))


# Matrice Latitudine ---------------------------------------------------------------------------------------------

for ( i in 1:dim(matmove)[1]){
  print(i)
  #riempio con le informazioni demografice
  matmove[i,c('type','gender','age','flagcc')] <- df[which(
    df$id==matmove[i,'id']),c('type','gender','age','flagcc')]
  
  #riempio con gli spostamenti
  replace <- as.character(as.numeric(df[which(df$id==matmove[i,'id']),'connected_seconds']))
  interested_user <- df[which((df$id==matmove[i,'id'] )),'lat_most']
  matmove[i,replace] <- as.numeric(df[which((df$id==matmove[i,'id'])),'lat_most'])
}

matmove_lat <- matmove

# Approx

for( i in 1:(dim(matmove_lat)[1])){
  print(i)
  #trovo il primo NA
  NonNAindex <- which(!is.na(matmove_lat[i,c(6:length(matmove_lat[i,]))]))
  firstNonNA <- min(NonNAindex)
  lastNonNA <- max(NonNAindex)
  
  # riempio il vuoto iniziale
  if(firstNonNA>1)
    matmove_lat[i,c(6:(firstNonNA+5))] <- matmove_lat[i,as.character(firstNonNA)]
  
  #riempio il vuoto finale
  if(lastNonNA>1)
    matmove_lat[i,c((lastNonNA+5):(24*60*60+5))] <- matmove_lat[i,as.character(lastNonNA)]
  
  # riempio il mezzo!
  NAindex <- which(is.na(matmove_lat[i,c(6:length(matmove_lat[i,]))]))
  if(length(NAindex)>0)
    matmove_lat[i,(NAindex+5)] <- approx(t(matmove_lat[i,c(6:(dim(matmove_lat)[2]))]), xout=NAindex)$y
}

save(matmove_lat, file='latitude_matrix.Rda')

# Matrice longitudine -----------------------------------------------------------------------------

#correzione spuria che dovrei togliere rivedendo il codice Designer
firstsecondsTraj[which(firstsecondsTraj$connected_seconds==0),'connected_seconds']<-33
# elimino doppie informazioni per lo stesso secondo (!!!)
firstsecondsTraj <- firstsecondsTraj[!(duplicated(firstsecondsTraj[,c('id','connected_seconds')])),]

# Creo la struttura della matrice
length(unique(firstsecondsTraj$id))

mat <- as.data.frame(matrix(NA,nrow=length(unique(firstsecondsTraj$id)),ncol=24*60*60))
colnames(mat) <- as.character(c(1:(24*60*60)))

bioinfo <- as.data.frame(matrix(NA,nrow=length(unique(firstsecondsTraj$id)), ncol=5))
colnames(bioinfo) <- c('id','type','gender','age','flagcc')
bioinfo$id <- unique(firstsecondsTraj$id)

matmove <- as.data.frame(cbind(bioinfo,mat))

matmove_lon <- matmove

for ( i in 1:dim(matmove_lon)[1]){
  
  print(i)
  #riempio con le informazioni demografice
  matmove_lon[i,c('type','gender','age','flagcc')] <- firstsecondsTraj[which(
    firstsecondsTraj$id==matmove_lon[i,'id']),c('type','gender','age','flagcc')]
  
  #riempio con gli spostamenti
  replace <- as.character(as.numeric(firstsecondsTraj[which(firstsecondsTraj$id==matmove_lon[i,'id']),'connected_seconds']))
  interested_user <- firstsecondsTraj[which((firstsecondsTraj$id==matmove_lon[i,'id'] )),'lon_most']
  matmove_lon[i,replace] <- as.numeric(firstsecondsTraj[which((firstsecondsTraj$id==matmove_lon[i,'id'] )),'lon_most'])
  
  #trovo il primo NA
  NonNAindex <- which(!is.na(matmove_lon[i,c(6:length(matmove_lon[i,]))]))
  firstNonNA <- min(NonNAindex)
  lastNonNA <- max(NonNAindex)
  
  # riempio il vuoto iniziale
  if(firstNonNA>1)
    matmove_lon[i,c(6:(firstNonNA+5))] <- matmove_lon[i,as.character(firstNonNA)]
  
  #riempio il vuoto finale
  if(lastNonNA>1)
    matmove_lon[i,c((lastNonNA+5):(24*60*60+5))] <- matmove_lon[i,as.character(lastNonNA)]
  
  # riempio il mezzo!
  NAindex <- which(is.na(matmove_lon[i,c(6:length(matmove_lon[i,]))]))
  if(length(NAindex)>0)
    matmove_lon[i,(NAindex+5)] <- approx(t(matmove_lon[i,c(6:(dim(matmove_lon)[2]))]), xout=NAindex)$y
}

save(matmove_lon, file= 'matrice_longitudine_prova.Rda')
