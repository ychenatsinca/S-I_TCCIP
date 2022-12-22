# load source file for loading nc file
source("/lfs/home/ychen/scripts/R/function/src_function_ychen.R")

# load rain data 
#file.path=paste("./rain.1960-2018.monthly.5km-grid-v4_pr_BCSD_TaiESM1_ssp126.nc",sep="")
#rain.arr <- fun_read_nc(arg1=file.path)



# load the geographic information of postal code in Taiwan
li_inf <- read.csv(file="li_lalo_name.csv")

# fields in the table
# OBJECTID,UID,PRO_ID,COUNTY_ID,TOWN_ID,VILLAGE_ID,V_Name,T_Name,C_Name,Substitute,xcoord,ycoord

n_li   <- length(li_inf$V_Name) 


#load the grid id infomration 
grid_inf <- read.csv(file="Grid.id_lonlat.csv")
n_grid <- length(grid_inf$grid.id) 


# find the minimum distance for each grid point and villiage center

li_grid_map <- data.frame()
tmp_table <- data.frame()

for (ind in 1:n_li) { 
  
    print(paste("Working on viliage no:",ind, sep=" ")) 
    # calculate the distance of grid point to all villages
    tmp_dis <- sqrt( (li_inf$xcoord[ind] - grid_inf$longitude)**2. + (li_inf$ycoord[ind] - grid_inf$latitude)**2. )  
   
    # find the index of  minimum value in the tmp_dis
    tmp_id <- which( tmp_dis == min(tmp_dis)) 

    #add the mapping information into the table 
    tmp_table <- data.frame(v.name=li_inf$V_Name[ind],
                            t.name=li_inf$T_Name[ind],  
                            c.name=li_inf$C_Name[ind],
                            v.lon=li_inf$xcoord[ind], 
                            v.lat=li_inf$ycoord[ind],
                            grid.id=grid_inf$grid.id[tmp_id],
                            g.lon=grid_inf$longitude[tmp_id], 
                            g.lat=grid_inf$latitude[tmp_id]) 
   
    #commbine by adding the information into the dataframe 
    li_grid_map <- rbind(tmp_table, li_grid_map)
    print(tmp_table)
}
  
# retrun and assign the grid id bcak to the Grid_inf 
# sorting 
 grid_inf <- grid_inf[order(grid_inf$grid.id),]
 li_grid_map <- li_grid_map[ order(li_grid_map$grid.id),]

#write out the referenca table
write.csv(li_grid_map, file="reference_table_village_gridid_lonlat.csv",row.names=FALSE, quote=FALSE)

# done   
