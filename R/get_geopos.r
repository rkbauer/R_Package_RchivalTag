get_geopos <- function(x, xlim, ylim, date_format, lang_format="en", tz="UTC", add=FALSE, prob_lim=.75){
  file <- x
  if(missing(date_format)) date_format <- "%d-%b-%Y %H:%M:%S"
  
  if(substr(file,nchar(file)-2,nchar(file)) == "csv"){
    #### check for header line:
    skip <- -1
    header_found <- F
    while(!header_found){
      skip <- skip +1
      header0 <- as.character(unlist(read.delim(file,sep=",",header=F,nrows=1,skip=skip,stringsAsFactors=F)))
      header_found <- any(grepl("Most.Likely",header0))
    }
    pos <- read.csv(file, header=T,sep=',', skip=skip)
    
    head(pos)
    names(pos) <- gsub('Most.Likely.', '', names(pos))
    names(pos) <- gsub('gitude', '', names(pos))
    names(pos) <- gsub('itude', '', names(pos))
    pos$datetime <- .fact2datetime(pos$Date, date_format = date_format, 
                                   lang_format = lang_format, tz = tz)
    pos$date <- as.Date(pos$datetime)
    pos$Date <- c()
    out <- pos
  }else{
    
    # source("~/Dropbox/my_R-packages/RchivalTag.build/RchivalTag/R/hidden_functions.r")
    # library(oceanmap)
    
    nc <- ncdf4::nc_open(file)
    #     print(nc)
    datetime <- .date2datetime("1970-01-01",tz="UTC",midday=F)+ncdf4::ncvar_get(nc,"twelve_hour_timestamps") # seconds since 1970-1-1
    lons <- ncdf4::ncvar_get(nc, "longitude")
    lats <- ncdf4::ncvar_get(nc, "latitude")
    
    if(add) {
      xlim <- par()$usr[1:2]
      ylim <- par()$usr[3:4]
    }else{
      #         if(!missing(v_area)){
      #           r <- regions(v_area)
      #           xlim <- r$xlim
      #           ylim <- r$ylim
      #         }else{
      if(missing(xlim)) xlim <- range(lons)
      if(missing(ylim)) ylim <- range(lats)
      #         }
    }
    Boundaries <- raster::extent(c(xlim, ylim)) #creates a bounding box to include all of the different gridded area sizes
    #       if(!add) oceanmap::plotmap(Boundaries) 
    
    #### load polygons:
    i <- j <- 1
    pols <- c()
    for(i in 1:length(datetime)){
      print(datetime[i])
      Raster.LR0 <- raster::raster(file,varname = "twelve_hour_likelihoods",band = i)
      Raster.LR <- raster::extend(Raster.LR0, Boundaries) #then extends any of your surfaces with the set boundaries
      #You can then use stack() to stack multiple tags, and overlay() to merge them together into a single probability surface.
      #To interpolate a surface (resample it at a higher resolution):
      Raster.big <- raster::raster(ncol=1200,nrow=1200,ext=Boundaries) #creates the higher resolution grid
      Raster.HR <- raster::resample(x=Raster.LR,y=Raster.big,method="bilinear") #will resample the data object onto the higher resolution grid
      Raster.HR@data@values <- Raster.HR@data@values/sum(Raster.HR@data@values,na.rm = T) #normalize the grid values so they sum to 1
      
      RasterVals <- sort(Raster.HR@data@values) #sort the probability values
      Raster.breaks <- c(RasterVals[max(which(cumsum(RasterVals)<=prob_lim))])
      cl <- try(rasterToContour(Raster.HR,levels = Raster.breaks),silent = T)
      cl
      if(class(cl) != "try-error"){
        p <- maptools::SpatialLines2PolySet(cl)
        spolys <- maptools::PolySet2SpatialPolygons(p)
        spolys@polygons[[1]]@ID <- as.character(datetime[i])
        
        if(is.null(pols)){
          pols <- spolys
        }else{
          pols@polygons[[j]] <- spolys@polygons[[1]]
        }
        
        j <- j +1
        # save(spolys,file="~/Desktop/test.rd")
        # pols[[datetime[i]]] <- spolys
      }
    }

    pols_joined <- pols
    # pols_joined <- SpatialPolygons(lapply(pols, function(x){x@polygons[[1]]}))
    out <- pols_df <- SpatialPolygonsDataFrame(Sr=pols_joined, data=data.frame(file=file,prob_lim=prob_lim,datetime=datetime,
                                                                              xmin=xlim[1],xmax=xlim[2],ymin=ylim[1],ymax=ylim[2]),FALSE)
  }
  return(out)
}