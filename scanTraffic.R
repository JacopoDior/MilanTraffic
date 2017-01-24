
# Prepare Data -------------------------------------------------------------------------------------------------
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

# load data
load(file='ninepiecedf.Rda')

# elimino le righe identiche
df <- df[!duplicated(df[,c('id', 'connected_seconds')]),]

# next to Caiazzo case 1 IT WORKS
firstLat1 <- 45.486587
firstLon1 <- 9.208110

secLat1 <- 45.486076
secLon1 <- 9.207509

# Viale Corsica case 2 IT WORKS
firstLat2 <- 45.462794
firstLon2 <- 9.233193

secLat2 <- 45.461229
secLon2 <- 9.234567

# Viale Corsica case 3 IT WORKS

firstLat <- 45.462794
firstLon <- 9.233193

secLat <- 45.461229 
secLon <- 9.233193

# Statale
firstLat3 <- 45.487104
firstLon3 <- 9.252762
  
secLat3 <- 45.487104
secLon3 <- 9.250251

# Porta Venezia
firstLat4 <- 45.472189
firstLon4 <- 9.204375

secLat4 <- 45.472460
secLon4 <-  9.206135

# Per Designer 
LinateLat1 <- 45.462578 
LinateLon1 <- 9.263542
  
LinateLat2 <- 45.461374 
LinateLon2 <- 9.263542

#Pietro Borsieri - Isola
IsolaLat1 <- 45.487486
IsolaLon1 <- 9.188715
  
IsolaLat2 <- 45.487592
IsolaLon2 <- 9.189187

# istanti
firstinstant <- 17*60*60
firstinstant <- 1
secondinstant <- max(df$connected_seconds)
secondinstant <- 19*60*60

m = leaflet() %>% addTiles()
m = m %>% addPolylines(c(firstLon1,secLon1), c(firstLat1,secLat1), fill = FALSE, col='red')
m = m %>% addPolylines(c(firstLon2,secLon2), c(firstLat2,secLat2), fill = FALSE, col='red')
m = m %>% addPolylines(c(firstLon3,secLon3), c(firstLat3,secLat3), fill = FALSE, col='red')
m = m %>% addPolylines(c(firstLon4,secLon4), c(firstLat4,secLat4), fill = FALSE, col='red')
m

# FUNZIONE scanTraffic ----------------------------------------------------------------------------------------
scanTraffic <- function(df,firstLat,firstLon,secLat,secLon,firstinstant,secondinstant){
solution <- c()
done <- 0

# UTM coordinates for df
cord.dec <- SpatialPoints(cbind(as.numeric(df$lon_most),as.numeric(df$lat_most)), proj4string = CRS("+proj=longlat"))
cord.UTM <- spTransform(cord.dec, CRS("+init=epsg:32632"))
df.UTM <- as.data.frame(cord.UTM)
colnames(df.UTM) <- c('x','y')
df <- cbind(df, df.UTM)

# UTM coordinates for the user selections
cord.dec <- SpatialPoints(cbind(c(firstLon, secLon), c(firstLat,secLat)), proj4string = CRS("+proj=longlat"))
cord.UTM <- spTransform(cord.dec, CRS("+init=epsg:32632"))
users.UTM <- as.data.frame(cord.UTM)
colnames(users.UTM) <- c('x','y')

# find angle of rotation
delta <- users.UTM[1,] - users.UTM[2,]
angle <- atan2(as.numeric(delta[2]), as.numeric(delta[1]))

# rotate df
new.x <- df$x*cos(angle) + df$y*sin(angle)
new.y <- df$x*sin(angle) - df$y*cos(angle)
df <- cbind(df, new.x, new.y)

#rotate users coordinate 
x <- users.UTM$x*cos(angle) + users.UTM$y*sin(angle)
y <- users.UTM$x*sin(angle) - users.UTM$y*cos(angle)
new.users <- as.data.frame(cbind(x, y))

# cerco utenti che almeno una volta erano nella banda
passantiOnRange <- unique(df[which((df$new.x<max(new.users$x) & df$new.x>min(new.users$x))),'id'])
passanti <- df[which(df$id %in% passantiOnRange),]

#controllo funzionamento trasformazione
#m = m %>% addCircleMarkers(passanti$lon_most, passanti$lat_most, fill = FALSE)

passanti_id <- passantiOnRange

cut <- new.users$y[1]

for( i in passanti_id)
{
  followed <- df[which(df$id==i),]
  time_to_check <- as.numeric(followed[which((followed$new.x<max(new.users$x) & followed$new.x>min(new.users$x))),'connected_seconds'])
  # aggiungo il controllo sul tempo: devo avere almeno due utenti che sono nel Range in quei tempi
  if(length(time_to_check[which(time_to_check<secondinstant & time_to_check>firstinstant)])>1){
    
    # create vec to check with a dimension corresponding to first presence : last presence
    first_presence <- min(time_to_check[which(time_to_check<secondinstant & time_to_check>firstinstant)])
    last_presence <- max(time_to_check[which(time_to_check<secondinstant & time_to_check>firstinstant)])
    istants_to_check <- followed[which(followed$connected_seconds %in% c(first_presence:last_presence)), 'connected_seconds']
    vec_to_check <- as.data.frame(rep(0,length(istants_to_check)))
    colnames(vec_to_check) <- 'Presence'
    rownames(vec_to_check) <- as.character(istants_to_check)
    
    # find moment up and down the line
    interesting_moment <- time_to_check[which(time_to_check %in% c(first_presence:last_presence))]
    down_moments <- followed[which(followed$connected_seconds %in% interesting_moment & followed$new.y>cut),'connected_seconds']
    up_moments <- followed[which(followed$connected_seconds %in% interesting_moment & followed$new.y<cut),'connected_seconds']
    
    # only up or only down means no so skip to next iterations
    if(length(down_moments)==0  | length(up_moments)==0) next
      cat(i)
    
    
    vec_to_check[as.character(down_moments),] <- -1
    vec_to_check[as.character(up_moments),] <- 1
    
    # control
    control <- as.data.frame(vec_to_check[c(1:(dim(vec_to_check)[1]-1)),] - vec_to_check[c(2:(dim(vec_to_check)[1])),])
    since <- rownames(vec_to_check)[c(1:(dim(vec_to_check)[1]-1))]
    to <- rownames(vec_to_check)[c(2:(dim(vec_to_check)[1]))]
    control <- cbind(control, since, to)
    colnames(control) <- c('control', 'since','to')
    rownames(control) <- paste(as.character(istants_to_check[1:(length(istants_to_check)-1)]),
                               '--',as.character(istants_to_check[2:(length(istants_to_check))]))
    
    # find 2 : from up to down
    if(length(which(control$control==2))!=0){
      done <- 1
      id <- rep(i, length(which(control$control==2)))
      gender <- rep(followed$gender[1], length(which(control$control==2)))
      age <- rep(followed$age[1], length(which(control$control==2)))
      how <- rep('down',length(which(control$control==2)))
      since <- as.character(control[which(control$control==2),'since'])
      to <- as.character(control[which(control$control==2),'to'])
      solution <- rbind(solution,cbind(id, gender, age, how, since, to))
    }
    
    # find -2 : from down to up
    if(length(which(control$control==-2))!=0){
      done <- 1
      id <- rep(i, length(which(control$control==-2)))
      gender <- rep(followed$gender[1], length(which(control$control==-2)))
      age <- rep(followed$age[1], length(which(control$control==-2)))
      how <- rep('up',length(which(control$control==-2)))
      since <- as.character(control[which(control$control==-2),'since'])
      to <- as.character(control[which(control$control==-2),'to'])
      solution <- rbind(solution,cbind(id, gender, age, how, since, to))
    }
  }
}

if(done==0){
  return('NO Passangers found')
}else{
  return(solution)
}

}


# PLOT di CONTROLLO -----------------------------------------------------------------------------------------
m = leaflet() %>% addTiles()
m = m %>% addPolylines(c(firstLonsuper,secLonsuper), c(firstLatsuper,secLatsuper), fill = FALSE, col='red')

repeated <- names(sort(table(solution[,'id']))[sort(table(solution[,'id']))!=1])
if( length(repeated)!=0){
singlesolution <- solution[which(solution[,'id']!=repeated),]
up_solution <- singlesolution[which(singlesolution[,'how']=='up'),]
down_solution <- singlesolution[which(singlesolution[,'how']=='down'),]

# plotting the up
for( i in up_solution[,'id'])
{
   plotnow <- df[which(df$id==i),]
   m = m %>% addPolylines(as.numeric(plotnow$lon_most), as.numeric(plotnow$lat_most), fill = FALSE,col='red')
}

# plotting the down
for( i in down_solution[,'id'])
{
  plotnow <- df[which(df$id==i),]
  m = m %>% addPolylines(as.numeric(plotnow$lon_most), as.numeric(plotnow$lat_most), fill = FALSE,col='blue')
}

# plotting the both

for( i in repeated)
{
  plotnow <- df[which(df$id==i),]
  m = m %>% addPolylines(as.numeric(plotnow$lon_most), as.numeric(plotnow$lat_most), fill = FALSE,col='green')
}
}else{
  up_solution <- solution[which(solution[,'how']=='up'),]
  down_solution <- solution[which(solution[,'how']=='down'),]
  # plotting the up
  for( i in up_solution[,'id'])
  {
    plotnow <- df[which(df$id==i),]
    m = m %>% addPolylines(as.numeric(plotnow$lon_most), as.numeric(plotnow$lat_most), fill = FALSE,col='red')
  }
  
  # plotting the down
  for( i in down_solution[,'id'])
  {
    plotnow <- df[which(df$id==i),]
    m = m %>% addPolylines(as.numeric(plotnow$lon_most), as.numeric(plotnow$lat_most), fill = FALSE,col='blue')
  }
}
m



 pie(c(2,1,0),c('up','down','both'), col=c('red','blue','green'))
 pie(c(1,1,1),c('[18-30]','[51-60]','unknown'), col=c('yellow','orange','white'))
 pie(c(1,1,1),c('M','F','unknown'), col=c('lightblue','pink','white'))
 
 
 m = leaflet() %>% addTiles()
 m = m %>% addPolylines(c(nuovaLon1,nuovaLon2), c(nuovaLat1,nuovaLat2), fill = FALSE, col='red')
 m = m %>% addPolylines(c(comasinaLon1,comasinaLon2), c(comasinaLat1,comasinaLat2), fill = FALSE, col='red')
 m = m %>% addPolylines(c(vercellinaLon1,vercellinaLon2), c(vercellinaLat1,vercellinaLat2), fill = FALSE, col='red')
 m = m %>% addPolylines(c(ticineseLon1,ticineseLon2), c(ticineseLat1,ticineseLat2), fill = FALSE, col='red')
 m = m %>% addPolylines(c(romanaLon1,romanaLon2), c(romanaLat1,romanaLat2), fill = FALSE, col='red')
 m = m %>% addPolylines(c(ticineseLon1,ticineseLon2), c(ticineseLat1,ticineseLat2), fill = FALSE, col='red')
 m = m %>% addPolylines(c(orientaleLon1,orientaleLon2), c(orientaleLat1,orientaleLat2), fill = FALSE, col='red')
 m
 
 # Porte per Designers ----------------------------------------------------------------------------------------

 romanatravellers <- scanTraffic(df,firstLat = romanaLat1, firstLon = romanaLon1,secLat = romanaLat2, secLon = romanaLon2,firstinstant = firstinstant, secondinstant = secondinstant) 
 
 m = leaflet() %>% addTiles()
 for( i in solution[,'id'])
 {
   plotnow <- df[which(df$id==i),]
   m = m %>% addPolylines(as.numeric(plotnow$lon_most), as.numeric(plotnow$lat_most), fill = FALSE,col='blue')
 }
m
