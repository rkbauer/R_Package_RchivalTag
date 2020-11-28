get_geopos <- function(x, xlim, ylim, date_format, lang_format="en", tz="UTC", proj4string, prob_lim=.5){
  file <- x
  if(missing(date_format)) date_format <- "%d-%b-%Y %H:%M:%S"
  if (missing(proj4string)) proj4string <- sp::CRS(as.character(NA))
  
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
  }
  
  if(substr(file,nchar(file)-1,nchar(file)) == "nc"){
    
    nc <- ncdf4::nc_open(file)
    #     print(nc)
    datetime <- .date2datetime("1970-01-01",tz="UTC",midday=F)+ncdf4::ncvar_get(nc,"twelve_hour_timestamps") # seconds since 1970-1-1
    lons <- ncdf4::ncvar_get(nc, "longitude")
    lats <- ncdf4::ncvar_get(nc, "latitude")
    
    # if(add) {
    #   xlim <- par()$usr[1:2]
    #   ylim <- par()$usr[3:4]
    # }else{
    #   #         if(!missing(v_area)){
    #   #           r <- regions(v_area)
    #   #           xlim <- r$xlim
    #   #           ylim <- r$ylim
    #   #         }else{
    #   if(missing(xlim)) xlim <- range(lons)
    #   if(missing(ylim)) ylim <- range(lats)
    #   #         }
    # }
    if(missing(xlim)) xlim <- range(lons)
    if(missing(ylim)) ylim <- range(lats)
    
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
      Raster.breaks <- c(RasterVals[max(which(cumsum(RasterVals)<=(1-prob_lim)))])
      cl <- try(rasterToContour(Raster.HR,levels = Raster.breaks),silent = T)
      cl

      
      cl0 <- cl
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
    projection(pols) <- proj4string
    pols_joined <- pols
    # pols_joined <- SpatialPolygons(lapply(pols, function(x){x@polygons[[1]]}))
    out <- pols_df <- SpatialPolygonsDataFrame(Sr=pols_joined, data=data.frame(file=file,prob_lim=prob_lim,datetime=datetime,
                                                                               xmin=xlim[1],xmax=xlim[2],ymin=ylim[1],ymax=ylim[2]),match.ID = FALSE)
  }
  
  if(substr(file,nchar(file)-2,nchar(file)) %in% c("kml","kmz")){
    pl <- .getKMLpols(kmlfile=file)
    LikelihoodArea <- prob_lim*100
    if(!(prob_lim %in% c(.99, .95, .5))) stop("Invalid 'porb_lim' value. Please select one of the following values: 0.99, 0.95, 0.50")
    out <- .merge_pols(pl, LikelihoodArea=LikelihoodArea, date_format=date_format, lang_format=lang_format, tz=tz, proj4string = proj4string, xlim=xlim, ylim=ylim)
    
  }
  return(out)
}



.getKMLpols <- function(kmlfile, ignoreAltitude=TRUE){
  if (missing(kmlfile))  stop("kmlfile is missing")
  
  
  if(substr(start = (nchar(kmlfile)-3),stop = nchar(kmlfile),kmlfile) ==".kmz"){
    kmzfile <- kmlfile
    if (grepl(" ", kmzfile) & Sys.info()['sysname'] != "Windows") {
      kmzfile <- gsub(" ","\\\\ ",kmlfile)
    }
    
    kmzfile <- kmlfile
    zipfile <- gsub(".kmz",".zip",kmzfile)
    exdir <- gsub(".kmz","",kmzfile)
    
    file.copy(kmzfile,zipfile)
    unzip(zipfile,exdir=exdir)
    tmpfile <- Sys.glob(paste0(exdir,"/*.kml"))
    kmlfile <- gsub(".kmz",".kml",kmzfile)
    
    file.copy(tmpfile, kmlfile)
    unlink(exdir, recursive = TRUE)
    file.remove(zipfile)
    # system(paste("mv",gsub(" ","\\\\ ",tmpfile),gsub(" ","\\\\ ",kmlfile)))
    # system(paste("rm -r", gsub(" ","\\\\ ",exdir)))
    # system(paste("rm -r", gsub(" ","\\\\ ",zipfile)))
    # 
    cat("extracted kml-file from provided kmz-file\n")
  }
  
  kml0 <- readLines(kmlfile,warn=F)
  istart <- grep("Time Lapse",kml0)
  iend <- grep("Maximum Likelihood",kml0)
  iend <- iend[which(iend > istart)][1]
  kml <- kml0[istart:iend]
  idates <- grep('Data name="time"',kml)
  
  n <- length(idates)
  idates <- c(idates,length(kml))
  out <- list()
  for(ii in 1:n){
    ptype <- as.numeric(strsplit(gsub("</styleUrl>","",kml[(idates[ii]-2)]),"#contour-")[[1]][[2]])+1
    ltype <- paste(c("99%","95%","50%")[ptype], "Likelihood Areas")
    
    dd <- strsplit(gsub("</value>","",kml[idates[ii]+1]),"<value>")[[1]][2]
    sub <- kml[idates[ii]:(idates[ii+1]-1)]
    pols_start <- grep("<coordinates>",sub)
    pols_end <- grep("</coordinates>",sub)
    
    
    nj <- length(pols_start); j <- 1
    while(j <= nj){
      poltype <- c("inner","outer")[grepl("outer",sub[(pols_start[j]-2)])+1]
      
      sub2 <- sub[pols_start[j]:(pols_end[j]-1)]
      sub2[1] <- strsplit(sub2[1],"<coordinates>")[[1]][2]
      coords <- read.table(textConnection(sub2),sep = ",")
      if(ignoreAltitude) coords[[3]] <- c()
      add <- coords
      out[[ltype]][[dd]][[poltype]] <- add
      j <- j+1
    }
  }
  out$file <- kmlfile
  return(out)
}

.merge_pols <- function(pl, LikelihoodArea=95, date_format = "%d-%b-%Y %H:%M:%S",lang_format="en",tz="UTC", proj4string, xlim, ylim){
  out <- pl
  file <- out$file; out$file <- c()
  ltype <- paste0(LikelihoodArea,"% Likelihood Areas")
  valid_ltypes <- gsub("% Likelihood Areas","",names(out))
  if(!ltype %in% names(out)) stop("Please select one of the following valid Likelihood Areas: ",paste(valid_ltypes,collapse=", "))
  n <- length(out[[ltype]])
  
  if (missing(proj4string)) proj4string <- sp::CRS(as.character(NA))
  pols <- c()
  if(missing(xlim)) xlim <- c()
  if(missing(ylim)) ylim <- c()
  
  for(i in 1:n){
    dd <- names(out[[ltype]])[i]
    coords <- out[[ltype]][[dd]][["outer"]]
    spolys <- SpatialPolygons(list(Polygons(list(Polygon(coords, hole=as.logical(NA))), ID=paste("outer",dd))), proj4string=proj4string)
    
    if(is.null(pols)){
      pols <- spolys
    }else{
      pols@polygons[[i]] <- spolys@polygons[[1]]
    }
    
    xlim <- range(c(xlim,coords[,1]))
    ylim <- range(c(ylim,coords[,2]))
  }
  pols_joined <- pols
  datetime <- .fact2datetime(names(out[[ltype]]),date_format = date_format, lang_format = lang_format, tz = tz)
  
  out2 <- pols_df <- SpatialPolygonsDataFrame(Sr=pols_joined, data=data.frame(file=file,prob_lim=LikelihoodArea/100,datetime=datetime,
                                                                              xmin=xlim[1],xmax=xlim[2],ymin=ylim[1],ymax=ylim[2]),FALSE)
  return(out2)
}
