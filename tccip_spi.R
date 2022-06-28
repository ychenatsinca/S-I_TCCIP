######################################################################
#
# Load WRF dynamic downscaling climatic data for Taiwan at 5km by 5km  
# Using monthly pricipitation and temperature data to calculate SPI   
#  
# First Date: 2021-06-22 
#
# Author: Yi-Ying Chen, Email:yiyingchen@gate.sinica.edu.tw
#######################################################################

# load SEPI library for SPI calculation
library("SPEI")
# Chinese text
library("showtext")
showtext_auto()
library("ggplot2")
library("gridExtra")
library("zoo")
library("tidyverse")
library("rgdal")

# load source file for loading nc file
source("/lfs/home/ychen/scripts/R/function/src_function_ychen.R")

ssp_run <- c("ssp585")
# import the rain fall data
#file.path = c("/archive/TCCIP_grid_data/rain/monthly-v4/5km/rain.1960-2018.monthly.5km-grid-v4.nc")
#file.path = c("/lfs/home/ychen/TCCIP/rain.1960-2018.monthly.5km-grid-v4.nc")
file.path=paste("./rain.1960-2018.monthly.5km-grid-v4_pr_BCSD_TaiESM1_",ssp_run,".nc",sep="")

rain.arr <- fun_read_nc(arg1=file.path) 


#output x,y point txt file for land
grid.id <- 1

xy.data <- data.frame()
for (ix in 1:60) {
 for (iy in 1:81) {
 tmp.lon <- formatC(rain.arr$longitude[ix],width=6, digits=2, format="f")
 tmp.lat <- formatC(rain.arr$latitude[iy],width=6, digits=2, format="f")
 tmp.data <- data.frame( grid.id= formatC(grid.id,width=4,flag="0",format="d"), longitude=as.numeric(tmp.lon),latitude=as.numeric(tmp.lat) )
 if (  is.na(rain.arr$pr[ix,iy,1]) != TRUE ) {
   grid.id <- grid.id +1 
   xy.data <- rbind( tmp.data, xy.data)
  }# end if

 }#iy
}#ix


#convert the lon/lat to TM2 N/E
d <- data.frame(lon=xy.data$longitude, lat=xy.data$latitude)
coordinates(d) <- c("lon", "lat")
proj4string(d) <- CRS("+init=epsg:4326") # WGS 84
#CRS.new <- CRS("+proj=somerc +lat_0=46.9524056 +lon_0=7.43958333 +ellps=bessel +x_0=2600000 +y_0=1200000 +towgs84=674.374,15.056,405.346 +units=m +k_0=1 +no_defs")
CRS.new <- CRS("+init=epsg:3826")
d.twd97 <- spTransform(d, CRS.new)
#
d.twd97.x <- coordinates(d.twd97)[,1]
d.twd97.y <- coordinates(d.twd97)[,2]
# combine column
xy.data$twd97.tm2.x <- d.twd97.x
xy.data$twd97.tm2.y <- d.twd97.y

#=== write out thhe table ===
write.csv( xy.data, "Grid.id_lonlat.csv", row.names=FALSE, quote=FALSE)

# import the mask of reserviors  
file.path= c("/lfs/home/ychen/TCCIP/Main_Reservoirs_TCCIP.nc")
mask.arr <- fun_read_nc(arg1=file.path)
# read reservior name and id 
res.name <- read.csv("/lfs/home/ychen/TCCIP/Main_Reservoirs_TCCIP.txt", sep=",", stringsAsFactors = FALSE)

# set 0 as NA
mask.arr$reservoir[mask.arr$reservoir==0] <- NA 

# here we create phase of SPI over the past 12 months 　　 
spi.phase.arr <- array(dim=c(60,81,12)) 

# create monthly time stamp
#time.stamp <- seq(from = as.Date("1960-01-01"), to = as.Date("2018-12-31"), by = "month")
time.stamp <- seq(from = as.Date("1960-01-01"), to = as.Date("2100-12-31"), by = "month")


ld_go <- T
if (ld_go){
# average rainfall for 17 major reserviors + all island
res.rain <- array(NA, dim=c(18,length(time.stamp)))
tmp.arr <- array(NA, dim=c(60,81,length(time.stamp)))

#pdf("spi_plot_1990_2018.pdf")
xlim=as.Date( c("1990-01-01","2018-12-31"))
# go for catchment level 
for (ires in 1:18) {
     # set all island as whole
     if (ires == 18) {
           for (it in 1: length(time.stamp)){
              res.rain[ires,it]  <- mean(rain.arr$pr[,,it], na.rm=T)
     
           }
     }else{
      # set generate spatail average from masked location  
          for (it in 1: length(time.stamp)){
             res.rain[ires,it]  <- mean(rain.arr$pr[,,it]*mask.arr$reservoir[ires,,], na.rm=T)  
          }
 
     } #end if 

    # make a table for a reservior longterm monthly table
    tmp.table <-  data.frame()
    res.table <-  tmp.table
    id = 0 
       for ( iyr in 1960:2018 ) { 
       for (imon in 1:12) {
             id = id + 1
              # assign grid point rainfall to res.rain
              tmp.res.rain <- res.rain[ires,id]
              # show rainfall 
              #print(paste("Reservior No:",ires, "month:",imon,"rain:",tmp.res.rain,sep=" "))
              # write the monthly data to tmp.table
              tmp.table <- data.frame(YEAR=as.integer(iyr), MONTH=as.integer(imon), PRCIP=as.numeric(tmp.res.rain) )
              # combine the tmp.table and pot.table  by monthly record 
              res.table <- rbind(res.table, tmp.table)
        }  
        }

     # for each catachment  we do SPI analysis 
     res.SPI_3 <- (spi(ts(res.table$PRCIP,freq=12,start=c(1960,1)), scale=3, distribution="Gamma"))
     res.SPI_12 <- (spi(ts(res.table$PRCIP,freq=12,start=c(1960,1)), scale=12, distribution="Gamma"))
 

     # plot SPI grapgh
         ld_go <- F
         if (ld_go) {
         # par setting 
           par(mfcol=c(2,1))
 
           DF1 <- zoo::fortify.zoo(res.SPI_3$fitted)         
           DF1 <- DF1 %>% 
           dplyr::select(-Index) %>% 
           dplyr::mutate(Period = zoo::as.yearmon(paste(time.stamp), "%Y-%m-%d")) %>% 
           dplyr::mutate(sign = ifelse( .data[['Series 1']] >= 0, "pos", "neg")) #%>%
           #na.omit()
           

         p1  <- ggplot(DF1) +
            geom_bar(aes(x = Period, y =  .data[['Series 1']], col = sign, fill = sign),
            show.legend = F, stat = "identity") +
            scale_color_manual(values = c("pos" = "darkblue", "neg" = "red")) +
            scale_fill_manual(values = c("pos"  = "darkblue", "neg" = "red")) +
            scale_y_continuous(limits = c(-3, 3), 
            breaks = -3:3) +
            scale_x_continuous(limits = c(1990,2020),breaks = seq(1990,2020,2) ) +
            ylab("SPI") + ggtitle(paste("3-Month SPI, ",res.name$name[ires],sep="")) +
            theme_bw() + theme(plot.title = element_text(hjust = 0.5), text = element_text(family = "CNS1"))
       
            DF2 <- zoo::fortify.zoo(res.SPI_12$fitted)         
            DF2 <- DF2 %>% 
            dplyr::select(-Index) %>% 
            dplyr::mutate(Period = zoo::as.yearmon(paste(time.stamp), "%Y-%m-%d")) %>% 
            dplyr::mutate(sign = ifelse( .data[['Series 1']] >= 0, "pos", "neg")) #%>%
            #na.omit()
        
          p2 <-  ggplot(DF2) +
            geom_bar(aes(x = Period, y =  .data[['Series 1']], col = sign, fill = sign),
            show.legend = F, stat = "identity") +
            scale_color_manual(values = c("pos" = "darkblue", "neg" = "red")) +
            scale_fill_manual(values = c("pos"  = "darkblue", "neg" = "red")) +
            scale_y_continuous(limits = c(-3, 3), 
            breaks = -3:3) +
            scale_x_continuous(limits = c(1990,2020),breaks = seq(1990,2020,2) ) +
            ylab("SPI") + ggtitle("12-Month SPI") +
            theme_bw() + theme(plot.title = element_text(hjust = 0.5), text = element_text(family = "CNS1")) 
            
            g <- arrangeGrob(p1, p2, ncol=1)
           
            ggsave(paste("SPI_",trimws(res.name$name[ires]),".pdf",sep=""), width = 8, height = 6, g )
#          plot(res.SPI_3,  main=paste(res.name$name[ires], "SPI-3",sep=" "), ylim=c(-3,3), xlim=c(1,100)  )
           #plot(res.SPI_6,  main="SPI-6",ylim=c(-3,3),xlim=x.lim )
         } #ld_go
   

}

#dev.off()


# go for pixel level 
ld_go <- T

if (ld_go) { 

grid.id = 1
for (ix in 1:60) { #1:60
for (iy in 1:80) { #1:81
    print(paste("x:",ix,", y:",iy, sep=" "))
  # ix=40; iy=30
  # make a table for a signal point longterm monthly table
    tmp.table <-  data.frame()
    pot.table <-  data.frame()
    tmp.lon <- formatC(rain.arr$longitude[ix],width=6, digits=2, format="f",flag="0")
    tmp.lat <- formatC(rain.arr$latitude[iy],width=6, digits=2, format="f",flag="0")
              
    # only for valid grid points  
    if ( is.na(rain.arr$pr[ix,iy,1]) == FALSE) {  
            id=0
             for ( iyr in 1960:2100 ) { 
             for (imon in 1:12) {
             id = id + 1
              # assign grid point rainfall to pot.rain
              pot.rain <- rain.arr$pr[ix,iy,id]
              # show rainfall 
              #print(paste("x:",ix,", y:",iy, "month:",imon,"rain:",pot.rain,sep=" "))
              # write the monthly data to tmp.table
              tmp.table <- data.frame(YEAR=as.integer(iyr), MONTH=as.integer(imon), PRCIP=as.numeric(pot.rain) )
              # combine the tmp.table and pot.table  by monthly record 
              pot.table <- rbind(pot.table, tmp.table)
             }  
             }
         # for each point we do SPI analysis 
         pot.SPI_3  <- (spi(ts(pot.table$PRCIP,freq=12,start=c(1960,1)), scale=3, distribution="Gamma"))
         pot.SPI_12 <- (spi(ts(pot.table$PRCIP,freq=12,start=c(1960,1)), scale=12, distribution="Gamma"))
    
 

        # out_put spi data to the csv file for each grid  
        tmp.date.yy <- substr(as.POSIXct(as.yearmon(time(pot.SPI_3$fitted))), start=1,stop=4) 
        tmp.date.mm <- substr(as.POSIXct(as.yearmon(time(pot.SPI_3$fitted))), start=6,stop=7) 
        tmp.grid.txt  <- formatC(grid.id,flag="0", width=4, format="d")
        tmp.table <- data.frame( date_yy = tmp.date.yy, date_mm = tmp.date.mm,
                                    SPI3 = formatC( as.matrix(pot.SPI_3$fitted),  width=6, digits=2, format="f" ),
                                   SPI12 = formatC( as.matrix(pot.SPI_12$fitted), width=6, digits=2, format="f" ) )
        colnames(tmp.table) <- c("yy","mm","spi.3","spi.12")
        write.csv(tmp.table, file=paste("./grid_spi_",ssp_run,"/", "ID_",tmp.grid.txt,"_",tmp.lon,"_",tmp.lat,"_spi.csv",sep=""), row.names=FALSE, quote=FALSE)  


            # asign final year of SPI phase  to array
        # for (ii in 1:12) {
        #     spi.phase.arr[ix,iy,ii] <- as.numeric(pot.SPI_12$fitted[709-ii,1])
        # }
         # plot SPI grapgh
         ld_go <- F
         if (ld_go) {
         # par setting 
           par(mfcol=c(2,1))

          plot((pot.SPI_12),   main="SPI-12" )
         } #ld_go
   
     grid.id <-  grid.id + 1
    }#end of if valid grid point 
} #iy
} #ix 

} #end if 


} # ld_go


 # output data


