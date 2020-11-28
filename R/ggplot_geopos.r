ggplot_geopos <- plot_geopos <- function(x, ggobj, xlim, ylim, zlim, standard_year=FALSE, date_format, lang_format="en", tz="UTC", cb.date_format, cbpos, cb.height = 10, cb.xlab = "",
                          prob_lim=.75, pal="jet", alpha=70, type="p", main="", lwd=1, size=2, shape=19, ...){
  
  if(missing(cbpos)) cbpos <- "r"
  cbpos_long <- c("left","right","top","bottom")
  cbpos <- cbpos_long[which(substr(cbpos_long,1,1) == cbpos)]
  if(alpha > 100){
    alpha <- 100
    warning("user specified alpha-value > 100%, resetting to 100%!")
  }
  
  if(is.character(x)) {
    if(substr(x,nchar(x)-3,nchar(x)) == ".nc") date_format <- "%Y-%m-%d %H:%M:%S"
    x <- get_geopos(x)
  }
  
  if(missing(date_format)) date_format <- "%d-%b-%Y %H:%M:%S"
  if(missing(cb.date_format) & !standard_year) cb.date_format <- "%Y-%m-%d"
  
  cmap <- NULL
  data(cmap, package='oceanmap', envir = environment())
  if(missing(pal)) pal <- "jet"
  if(length(pal) == 1 & pal[1] %in% names(cmap)) pal <- cmap[[pal]]
  
  if(is.data.frame(x)){
    pos <- x
    
    if(missing(xlim)) xlim <- range(pos$Lon+c(.5,-.5))
    if(missing(ylim)) ylim <- range(pos$Lat+c(.5,-.5))
    if(missing(ggobj)) ggobj <- oceanmap::ggplotmap(xlim=xlim, ylim=ylim, ...)
    if(standard_year) {
      pos$date <- as.Date(paste0("0",substr(as.Date(pos$date),5,10)))
      if(missing(cb.date_format)) cb.date_format <- "%b-%d"
      if(!missing(zlim)) zlim <- as.Date(paste0("0",substr(as.Date(zlim),5,10)))
    }
    
    ### set common colors
    if(missing(zlim)) {
      zlim <- as.Date(range(pos$date))
    }else{
      zlim <- as.Date(zlim)
    }
    
    dates <- zlim[1]:zlim[2]
    cols <- colorRampPalette(pal)(length(dates))
    Raster.cols <- .makeTransparent(cols,alpha = 255*alpha/100)#[1:100] #creates a color scale, add as many values as you want or use an existing scale
    df.col <- data.frame(datenm=dates, color = as.character(Raster.cols)) # merge colors and time steps
    df.col$color_full <- as.character(colorRampPalette(cmap$jet)(length(cols)))
    
    pos$datenm <- as.numeric(pos$date)
    cmb <- merge(pos,df.col,by="datenm",all=T,sort=F)

    if(type %in% c("p","b","pl")) ggobj <- ggobj + geom_point(cmb,mapping = aes_(x=~Lon,y=~Lat,color=~datenm),size=size,shape=shape) 
    if(type %in% c("l","b","pl")) ggobj <- ggobj + geom_path(cmb,mapping = aes_(x=~Lon,y=~Lat,color=~datenm,group=~Ptt),size=lwd)
    b <- ggobj
    
  }else{
    if(class(x) == "SpatialPolygonsDataFrame"){
      pos <- x
      
      pos@data$fill=colorRampPalette(pal)(nrow(pos))
      pos$id <- 1:nrow(pos)
      pos$bg <- 1:nrow(pos)
      pols_df <- fortify(pos)
      
      cmb <- pols_df
      
      tmp <- gsub("outer ","",cmb$id)
      if(any(nchar(tmp) == 10)){
        tmp[which(nchar(tmp) == 10)] <- paste(tmp[which(nchar(tmp) == 10)],"00:00:00")
      }
      
      cmb$date <- as.Date(strptime(tmp,format = date_format,tz = tz))
      if(is.na(cmb$date[1])){
        cmb$date <- as.Date(strptime(tmp,format = "%Y-%m-%d %H:%M:%S",tz = tz))
      }
      if(is.na(cmb$date[1])){
        cmb$date <- as.Date(.fact2datetime(tmp,date_format = date_format,tz = tz))
      }
      
      if(standard_year) {
        cmb$date <- as.Date(paste0("0",substr(as.Date(cmb$date),5,10)))
        if(missing(cb.date_format)) cb.date_format <- "%b-%d"
        if(!missing(zlim)) zlim <- as.Date(paste0("0",substr(as.Date(zlim),5,10)))
      }
      cmb$datenm <- as.numeric(cmb$date)
      
      if(missing(zlim)){
        date_range_nm <- range(cmb$datenm)
        zlim <- range(cmb$date)
      }else{
        date_range_nm <- as.numeric(as.Date(zlim)) 
      }
      
      ### set common colors
      tsteps <- min(date_range_nm):max(date_range_nm)
      cols <- colorRampPalette(pal)(length(tsteps))
      Raster.cols <- .makeTransparent(cols,alpha = 255*alpha/100)#[1:100] #creates a color scale, add as many values as you want or use an existing scale
      df.col <- data.frame(datenm=tsteps) # merge colors and time steps
      df.col$color <- as.character(Raster.cols)
      
      cmb <- merge(cmb, df.col, by="datenm", all=T, sort=F)

      if(missing(xlim)) xlim <- c(trunc(min(pols_df$long)),ceiling(max(pols_df$long)))
      if(missing(ylim)) ylim <- c(trunc(min(pols_df$lat)),ceiling(max(pols_df$lat)))
      if(missing(ggobj)) ggobj <- oceanmap::ggplotmap(xlim=xlim, ylim=ylim, ...)
      
      b <- ggobj + geom_polygon(data = cmb, aes_(x=~long,y=~lat,group = ~id,colour=~datenm),fill= cmb$color) 
    }
  }
  Breaks <- as.numeric(pretty(.as.Date_origin(cmb$datenm)))
  zlim <- as.numeric(as.Date(zlim))
  if(all(format(.as.Date_origin(zlim),"%d") %in% c("01","15"))) {
    Breaks <- .as.Date_origin(zlim[1]:zlim[2])
    Breaks <- as.numeric(Breaks[which(format(Breaks,"%d") %in% c("01","15"))])
  }
  if(!all(range(cmb$datenm) %in% Breaks)) Breaks <- c(median(unique(cmb$datenm)),range(cmb$datenm))
  if(length(cmb$datenm) == 1) Breaks <- cmb$datenm
  out <- b + scale_colour_gradientn(name = 'Date', colours=Raster.cols, labels=format(.as.Date_origin(Breaks),format=cb.date_format), breaks=Breaks, limits=range(cmb$datenm)) +
    theme(legend.position = cbpos) + guides(color=guide_colourbar(barheight = cb.height))
  return(out)
}


.as.Date_origin <- function(x){
  as.Date(x, origin = '1970-01-01')
}

