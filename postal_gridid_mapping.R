# load source file for loading nc file
source("/lfs/home/ychen/scripts/R/function/src_function_ychen.R")

# load rain data 
file.path=paste("./rain.1960-2018.monthly.5km-grid-v4_pr_BCSD_TaiESM1_ssp126.nc",sep="")

rain.arr <- fun_read_nc(arg1=file.path)



# load the geographic information of postal code in Taiwan
post_inf <- read.csv(file="postal_code_lat_lon.csv")
n_pcode <- length(post_inf$admin.name) 

#load the grid id infomration 
grid_inf <- read.csv(file="Grid.id_lonlat.csv")
n_grid <- length(grid_inf$grid.id) 


# find the minimum distance for each grid point and postal address

post_grid_map <- data.frame()
tmp_table <- data.frame()

for (ind in 1:n_grid) { 
  
     
    # calculate the distance of grid point to all postal codes
    tmp_dis <- sqrt( (post_inf$longitude - grid_inf$longitude[ind])**2. + (post_inf$latitude - grid_inf$latitude[ind])**2. )  
   
    # find the index of  minimum value in the tmp_dis
    tmp_id <- which( tmp_dis == min(tmp_dis)) 

    #add the mapping information into the table 
    tmp_table <- data.frame(grid.id=grid_inf$grid.id[ind], 
                            postal.code=post_inf$postal.code[tmp_id],
                            postal.name=post_inf$admin.name[tmp_id],
                            p.lon=post_inf$longitude[tmp_id], p.lat=post_inf$latitude[tmp_id])#,
     #                       g.lon=grid_inf$longitude[ind], g.lat=grid_inf$latitude[ind]) 
   
    #commbine by adding the information into the dataframe 
    post_grid_map <- rbind(tmp_table, post_grid_map)
       
}
  
# retrun and assign the grid id bcak to the Grid_inf 
# sorting 
 grid_inf <- grid_inf[order(grid_inf$grid.id),]
 post_grid_map <- post_grid_map[ order(post_grid_map$grid.id),]

# combine table by column 
join.table.1 <- merge( grid_inf, post_grid_map, by.x = "grid.id" )

# add reservoir mask information 
# import the mask of reserviors  
file.path= c("/lfs/home/ychen/TCCIP/Main_Reservoirs_TCCIP.nc")
mask.arr <- fun_read_nc(arg1=file.path)

# read reservior name and id 
res.name <- read.csv("/lfs/home/ychen/TCCIP/Main_Reservoirs_TCCIP.txt", sep=",", stringsAsFactors = FALSE)

# set 0 as NA
mask.arr$reservoir[mask.arr$reservoir==0] <- 0

res.mask  <- mask.arr$reservoir 

nav_lat <- array(NA, dim=c(60,81))
nav_lon <- array(NA, dim=c(60,81))
all_res <- array(NA, dim=c(60,81))


for (ix in 1:60) {
   for (iy in 1:81){
       nav_lat[ix,iy] <- rain.arr$latitude[iy]
       nav_lon[ix,iy] <- rain.arr$longitude[ix]  
  }
}

      for ( iz in 1:17) { 
            all_res[ which(mask.arr$reservoir[iz,,]==1, arr.ind=TRUE)] <- iz
       } 
  
#set 0 as 19 非水庫區分

all_res[is.na(all_res)] <- 19

res_table  <- data.frame()
tmp_table <- data.frame()


grid_lon <- formatC(rain.arr$longitude,digits=2,format="f",width=6) 
grid_lat <- formatC(rain.arr$latitude,digits=2,format="f",width=6)
 
# finding the reservior id for each grid 
for ( ind in 1:n_grid)  {

    tmp_lon <- formatC(grid_inf$longitude[ind],digits=2,format="f",width=6)
    tmp_lat <- formatC(grid_inf$latitude[ind],digits=2, format="f",width=6)

  
    # find the xid & yid  res_mask 
   tmp_ix <- as.integer( which ( grid_lon == tmp_lon ))  
   tmp_iy <- as.integer( which ( grid_lat == tmp_lat ))
   print(paste("ix:",tmp_ix, "iy:",tmp_iy,sep=" "))

   #combine_table 
   tmp_table <- data.frame(grid.id=grid_inf$grid.id[ind], res.id= all_res[tmp_ix,tmp_iy], res.name=res.name$name[ all_res[tmp_ix,tmp_iy] ],
                           arr.ix=tmp_ix, arr.iy=tmp_iy  )

   res_table <- rbind(tmp_table, res_table)
}



# sorting 
 res_table <- res_table[order(res_table$grid.id),]

# combine table by column 
join.table.2 <- merge( res_table, join.table.1, by.x = "grid.id") 



#write out the referenca table
write.csv(join.table.2, file="reference_table_pcode_gridid_lonlat.csv",row.names=FALSE, quote=FALSE)

# done   
