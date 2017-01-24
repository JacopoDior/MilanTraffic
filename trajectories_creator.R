

# New version for create trajectories
SolvedSecondsTrajectory <- function(df, user_id){
  
  utente <- df[which(df$id==user_id),]
  approx_time <- utente$datetime

  # approssimo al secondo
  seconds <- format(approx_time, format='%H:%M:%S')
  seconds <-( (as.numeric(substr(seconds, 1, 2))*60*60) + ((as.numeric(substr(seconds, 4, 5))))*60 
              +  ((as.numeric(substr(seconds, 7, 8)))) )
  utente <- cbind(utente, seconds)
  
  # Secondi di connessione
  connected_seconds <- unique(utente$seconds)
  
  # scheletro movimenti
  lat_most <- rep(NA,length(connected_seconds))
  lon_most <- rep(NA,length(connected_seconds))
  most_frequent <- as.data.frame(cbind(connected_seconds,lat_most, lon_most))
  colnames(most_frequent) <- c('connected_seconds', 'lat_most','lon_most')
  
  for (i in connected_seconds){
    most_frequent[which(most_frequent$connected_seconds==i),c('lat_most','lon_most')] <- post.mode(utente[which(utente$seconds==i),c('lat','lon')])
  }
  
  # creo la prima riga di solution
  solution <- most_frequent[1,]
  
  # se ho più di una connessione:
  if (dim(most_frequent)[1]!=1){
    for (i in 1:(length(connected_seconds)-1)){
      # se due successive connessione sono uguali
      if(most_frequent$lon_most[i]==most_frequent$lon_most[i+1] && most_frequent$lat_most[i]==most_frequent$lat_most[i+1])
      {
        #allora riempio da 3600 (60min) a 4500 (75min) con delle ripetizioni
        numb = (most_frequent[i+1, 'connected_seconds'] - most_frequent[i, 'connected_seconds'] - 1)
        
        # se i momenti di connessione non sono immediatamente successivi
        if (numb > 0){
          insert <- cbind(
            c((most_frequent$connected_seconds[i]+1):(most_frequent$connected_seconds[i+1]-1)),
            rep(most_frequent$lat_most[i],numb),
            rep(most_frequent$lon_most[i],numb)
          )
          
          colnames(insert) <- colnames(solution)
          
          solution <- rbind(solution, insert, most_frequent[i+1,])
        }else{
          solution <- rbind(solution,most_frequent[i+1,])
        }
      # al contrario se sono c'è stato uno spostamento tra due connessioni  
      }else{
        # time between two connections
        numb = (most_frequent[i+1, 'connected_seconds'] - most_frequent[i, 'connected_seconds'])
        
        # ask google the best route
        path <- route(c(most_frequent$lon_most[i],most_frequent$lat_most[i]),
                      c(most_frequent$lon_most[i+1],most_frequent$lat_most[i+1]),
                      structure = "route", output = "all")
        
        # calcolo posizioni del percorso
        route_df <- as.data.frame((decodeLine(path$routes[[1]]$overview_polyline$points)))
        
        # do a route_df la struttura di solution
        route_df <- cbind(rep(NA,dim(route_df)[1]),route_df)
        colnames(route_df) <- c('connected_seconds','lat_most','lon_most')
        
        # trova gli le posizioni di cui abbiamo i tempi 
        chosen <- c()
        googletimes <- c()
        
        for(k in 1:length(path$routes[[1]]$legs[[1]]$steps)){
          
          # primo elemento di chosen
          if(length(chosen)==0){
            # trovo le lat più simili
            chosen_lat <- which(sprintf("%.5f", round(route_df$lat_most,5))==sprintf("%.5f",round(path$routes[[1]]$legs[[1]]$steps[[k]]$end_location$lat,5)))
            chosen_lat
            # trovo le lon più simili
            chosen_lon <- which(sprintf("%.5f", round(route_df$lon_most,5))==sprintf("%.5f",round(path$routes[[1]]$legs[[1]]$steps[[k]]$end_location$lng,5)))
            chosen_lon
            # interseziono e controllo che funzioni
            if(length(intersect(chosen_lat, chosen_lon))!=0){
              chosen <- c(chosen,intersect(chosen_lat, chosen_lon)[1])
            }else{
              chosen_lat <- which(sprintf("%.4f", round(route_df$lat_most,4))==sprintf("%.4f",round(path$routes[[1]]$legs[[1]]$steps[[k]]$end_location$lat,4)))
              chosen_lat
              # trovo le lon più simili
              chosen_lon <- which(sprintf("%.4f", round(route_df$lon_most,4))==sprintf("%.4f",round(path$routes[[1]]$legs[[1]]$steps[[k]]$end_location$lng,4)))
              chosen_lon
              if(length(intersect(chosen_lat, chosen_lon))!=0){
                chosen <- c(chosen,intersect(chosen_lat, chosen_lon)[1])
              }else{
                chosen_lat <- which(sprintf("%.3f", round(route_df$lat_most,3))==sprintf("%.3f",round(path$routes[[1]]$legs[[1]]$steps[[k]]$end_location$lat,3)))
                chosen_lat
                # trovo le lon più simili
                chosen_lon <- which(sprintf("%.3f", round(route_df$lon_most,3))==sprintf("%.3f",round(path$routes[[1]]$legs[[1]]$steps[[k]]$end_location$lng,3)))
                chosen_lon
                if(length(intersect(chosen_lat, chosen_lon))!=0){
                  chosen <- c(chosen,intersect(chosen_lat, chosen_lon)[1])
                }else{
                  chosen_lat <- which(sprintf("%.2f", round(route_df$lat_most,2))==sprintf("%.2f",round(path$routes[[1]]$legs[[1]]$steps[[k]]$end_location$lat,2)))
                  chosen_lat
                  # trovo le lon più simili
                  chosen_lon <- which(sprintf("%.2f", round(route_df$lon_most,2))==sprintf("%.2f",round(path$routes[[1]]$legs[[1]]$steps[[k]]$end_location$lng,2)))
                  chosen_lon
                  if(length(intersect(chosen_lat, chosen_lon))!=0)
                    chosen <- c(chosen,intersect(chosen_lat, chosen_lon)[1])# chiude if
                }# chiude else terzo controllo
              }# chiude else secondo controllo
            }#chiude else del primo controllo
            # chosen already has at least one element
          }else{
            
            #biggest and latest element of chosen:
            lastchosen <- chosen[length(chosen)]
            #variabile controllo
            done <- 0
            
            chosen_lat <- which(sprintf("%.5f", round(route_df$lat_most,5))==sprintf("%.5f",round(path$routes[[1]]$legs[[1]]$steps[[k]]$end_location$lat,5)))
            chosen_lat
            # trovo le lon più simili
            chosen_lon <- which(sprintf("%.5f", round(route_df$lon_most,5))==sprintf("%.5f",round(path$routes[[1]]$legs[[1]]$steps[[k]]$end_location$lng,5)))
            chosen_lon
            if(length(intersect(chosen_lat, chosen_lon))!=0){
              nextchoose <- (intersect(chosen_lat, chosen_lon))
              if(!is.na(nextchoose[which(nextchoose>chosen[length(chosen)])][1])){
                chosen <- c(chosen,nextchoose[which(nextchoose>chosen[length(chosen)])][1])
                done <- 1
              }# exit if after updating chosen  
            }# exit if
            
            if(done==0){
              chosen_lat <- which(sprintf("%.4f", round(route_df$lat_most,4))==sprintf("%.4f",round(path$routes[[1]]$legs[[1]]$steps[[k]]$end_location$lat,4)))
              chosen_lat
              # trovo le lon più simili
              chosen_lon <- which(sprintf("%.4f", round(route_df$lon_most,4))==sprintf("%.4f",round(path$routes[[1]]$legs[[1]]$steps[[k]]$end_location$lng,4)))
              chosen_lon
              if(length(intersect(chosen_lat, chosen_lon))!=0){
                nextchoose <- (intersect(chosen_lat, chosen_lon))
                if(!is.na(nextchoose[which(nextchoose>chosen[length(chosen)])][1])){
                  chosen <- c(chosen,nextchoose[which(nextchoose>chosen[length(chosen)])][1])
                  done <- 1
                }# exit if after updating chosen  
              }# exit if
            }#close control 4f
            
            if(done==0){
              chosen_lat <- which(sprintf("%.3f", round(route_df$lat_most,3))==sprintf("%.3f",round(path$routes[[1]]$legs[[1]]$steps[[k]]$end_location$lat,3)))
              chosen_lat
              # trovo le lon più simili
              chosen_lon <- which(sprintf("%.3f", round(route_df$lon_most,3))==sprintf("%.3f",round(path$routes[[1]]$legs[[1]]$steps[[k]]$end_location$lng,3)))
              chosen_lon
              if(length(intersect(chosen_lat, chosen_lon))!=0){
                nextchoose <- (intersect(chosen_lat, chosen_lon))
                if(!is.na(nextchoose[which(nextchoose>chosen[length(chosen)])][1])){
                  chosen <- c(chosen,nextchoose[which(nextchoose>chosen[length(chosen)])][1])
                  done <- 1
                }# exit if after updating chosen  
              }# exit if
            }#close control 3f
            
            if(done==0){
              chosen_lat <- which(sprintf("%.2f", round(route_df$lat_most,2))==sprintf("%.2f",round(path$routes[[1]]$legs[[1]]$steps[[k]]$end_location$lat,2)))
              chosen_lat
              # trovo le lon più simili
              chosen_lon <- which(sprintf("%.2f", round(route_df$lon_most,2))==sprintf("%.2f",round(path$routes[[1]]$legs[[1]]$steps[[k]]$end_location$lng,2)))
              chosen_lon
              if(length(intersect(chosen_lat, chosen_lon))!=0){
                nextchoose <- (intersect(chosen_lat, chosen_lon))
                if(!is.na(nextchoose[which(nextchoose>chosen[length(chosen)])][1])){
                  chosen <- c(chosen,nextchoose[which(nextchoose>chosen[length(chosen)])][1])
                  done <- 1
                }# exit if after updating chosen  
              }# exit if
            }#close control 2f
            
            if(done==0){
              chosen_lat <- which(sprintf("%.1f", round(route_df$lat_most,1))==sprintf("%.1f",round(path$routes[[1]]$legs[[1]]$steps[[k]]$end_location$lat,1)))
              chosen_lat
              # trovo le lon più simili
              chosen_lon <- which(sprintf("%.1f", round(route_df$lon_most,2))==sprintf("%.1f",round(path$routes[[1]]$legs[[1]]$steps[[k]]$end_location$lng,1)))
              chosen_lon
              if(length(intersect(chosen_lat, chosen_lon))!=0){
                nextchoose <- (intersect(chosen_lat, chosen_lon))
                if(!is.na(nextchoose[which(nextchoose>chosen[length(chosen)])][1])){
                  chosen <- c(chosen,nextchoose[which(nextchoose>chosen[length(chosen)])][1])
                  done <- 1
                }# exit if after updating chosen  
              }# exit if
            }#close control 1f
            
            #WE NEED TO ADD ANOTHER CONTROL: sometimes we reach the maximum befor the latest k
            # if we already reach the max dimension but not the max k just repeat
            if(lastchosen==(dim(route_df)[1]))
              chosen[k] <- dim(route_df)[1]
          }# close the else already chosen
          
          
          # trovo i tempi di arrivo secondo google
          googletimes <- c(googletimes,path$routes[[1]]$legs[[1]]$steps[[k]]$duration$value)
        }# close for loop
        
      
        # calcolo tempo totale di percorrenza secondo google
        googleTot <- sum(googletimes)
        
        # controllo la fattibilità del percorso: se uno impiega meno di 1/3 del tot ERRORE
        if( numb >= (googleTot/3)){
          # calcolo i nuovi tempi proporzionati al tempo reale in base all'istante di partenza
          realtimes <- cumsum(floor((numb*googletimes)/googleTot)) + most_frequent[i, 'connected_seconds']
          
          # inserisco i tempi in route_df
          route_df[chosen,'connected_seconds'] <- realtimes
          
          # riempio il gap iniziale
          # se non sono a due istanti adiacenti riempio il gap (DIREI!!!)
          if(chosen[1]>1){
            filler <- approx(c(most_frequent[i, 'connected_seconds'], realtimes[1]), n=chosen[1]+1)$y
            route_df[1:(chosen[1]-1),'connected_seconds'] <- round(filler[2:(length(filler)-1)])
          }
          
          if(length(chosen)>1){
            # riempio i gap seguenti
            for (j in 1:(length(path$routes[[1]]$legs[[1]]$steps)-1)){
              # controllo che non siano adiacenti
              if(chosen[j+1] - chosen[j]>1 ){
                filler <- approx(c(realtimes[j], realtimes[j+1]), n=chosen[j+1]-chosen[j]+1)$y
                route_df[(chosen[j]+1):(chosen[j+1]-1), 'connected_seconds'] <- round(filler[2:(length(filler)-1)])
              }
            }
            
            # controllo e riempio il gap finale
            if(dim(route_df)[1]!=chosen[length(path$routes[[1]]$legs[[1]]$steps)]){
              filler <- approx(c(realtimes[length(path$routes[[1]]$legs[[1]]$steps)],most_frequent[i+1, 'connected_seconds']),
                               n = dim(route_df)[1]- chosen[length(path$routes[[1]]$legs[[1]]$steps)]+1)$y
              route_df[c((chosen[length(path$routes[[1]]$legs[[1]]$steps)]+1):(dim(route_df)[1])),'connected_seconds'] <- round(filler[2:(length(filler))])
            }
          }    
          
          # aggiungo a solution
          solution <- rbind(solution, route_df, most_frequent[i+1,])
        }else{
          #  è uno spostamento non reale quindi non va considerato
          # l'elemento seguente nella lista dei most_frequent rimane quello attuale
          most_frequent[i+1,] <- most_frequent[i,]
        }
      }
    }# close initial for loop
  }
  # aggiungo i dettagli utente
  solution <- cbind(utente[1:dim(solution)[1],c('id','type','gender','age','flagcc')],solution)
  # riempio gli NA
  solution <- na.locf(solution)
}

# Create designer --------------------------------------------------------------------

setwd("~/Desktop/Telecom/Traiettorie/user_tracking_export/final_trajectories")

users_to_plot_first <- names(sort(table(as.character(milan_pos$id)))[2000:2100]) #DONE
users_to_plot_second <- names(sort(table(as.character(milan_pos$id)))[2101:2200]) #DONE
users_to_plot_third <- names(sort(table(as.character(milan_pos$id)))[2201:2300]) #DONE
users_to_plot_fourth <- names(sort(table(as.character(milan_pos$id)))[2301:2400])

# first one
prova <- SolvedSecondsTrajectory(df, users_to_plot_fourth[58])

# FIRST
# errore in 21 : "0d0165d24ccac9fd8ca877b4500cbff2"
# errore in 63 : "03f0cde01a329a19702724cbc2e5cfa4" - Error in path$routes[[1]] indice fuori limite
# errore in 96 : "0dfabe02e92db3ee145255baf8484a13" - Error in path$routes[[1]] : indice fuori limite

# SECOND
# errore in 19 

# THIRD
# errore in 64 : "076840fbda0eeed5dd86a09c40cef4fa" - Error in path$routes[[1]] : indice fuori limite

# 06/11/2016 : users_to_plot_first from 1 to 33 skipping 21

# 07/11/2016 : users_to_plot_first from 34 to 84 skipping 63

# 08/11/2016 : users_to_plot_first from 85 to 101 skipping 96 +
#              users_to_plot_second from 1 to 40  skipping 19

# 15/11/2016 : users_to_plot_second from 41 to 84  skipping 50

# 17/11/2016 : users_to_plot_second from 85 to 100 +
#              users_to_plot_third from 1 to 29

# 21/11/2016 : users_to_plot_third from 30 to 80  skipping 64

# 22/11/2016 : users_to_plot_third from 81 to 100 +
#              users_to_plot_fourth from 1 to 18

# 23/11/2016 : users_to_plot_fourth from 19 to 57

# 24/11/2016 : users_to_plot_fourth from 58 to 84

for ( i in 59:length(users_to_plot_fourth)){
  print(i)
  prova <- rbind(prova,SolvedSecondsTrajectory(df, users_to_plot_fourth[i]) )
}


save(prova,file='24112016.Rda')

