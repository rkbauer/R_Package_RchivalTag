
plot_geopos <- function(x, xlim, ylim, date_format, lang_format="en", tz="UTC", cb.date_format, cbpos,cbline=0,cb.xlab = "",
                        prob_lim=.75, pal="jet", alpha=70, type="p", pch=19, cex=1, lwd=1, add=FALSE, Return=FALSE, main="", ...){
  if(missing(cbpos)) cbpos <- "d"
  if(alpha > 100){
    alpha <- 100
    warning("user specified alpha-value > 100%, resetting to 100%!")
  }
  
  if(is.character(x)){
    out <- plot_geopos(get_geopos(x),xlim=xlim, ylim=ylim, date_format, lang_format=lang_format, tz=tz, cb.date_format=cb.date_format,
                       prob_lim=prob_lim, pal=pal, alpha=alpha, type=type, pch=pch, cex=cex, lwd=lwd, add=add, Return=Return, ...)
  }else{
    
    if(missing(date_format)) date_format <- "%d-%b-%Y %H:%M:%S"
    if(missing(cb.date_format)) cb.date_format <- "%Y-%m-%d"
    
    cmap <- NULL
    
    if(is.data.frame(x)){
      out <- pos <- x
      #     if(!missing(v_area)){
      #       r <- regions(v_area)
      #       xlim <- r$xlim
      #       ylim <- r$ylim
      #     }else{
      if(missing(xlim)) xlim <- range(pos$Lon+c(.5,-.5))
      if(missing(ylim)) ylim <- range(pos$Lat+c(.5,-.5))
      #     }
      if(!add) oceanmap::plotmap(xlim=xlim,ylim=ylim,main="",bwd=0,grid=F,axes=F,fill.land = F)
      
      ## plot positions as line
      if(type %in% c("l","b")) lines(pos$Lon, pos$Lat,lwd=lwd)
      
      if(type %in% c("p","b")) { ## plot positions as dots
        data(cmap, package='oceanmap', envir = environment())
        if(length(pal) == 1 & pal[1] %in% names(cmap)) pal <- cmap[[pal]]
        tsteps <- 1:nrow(pos)
        pos$col <- .makeTransparent(colorRampPalette(pal)(length(tsteps)),alpha = 255*alpha/100)#[1:100] #creates a color scale, add as many values as you want or use an existing scale
        points(pos$Lon, pos$Lat, col=pos$col, pch=pch,cex=cex)
        if(!add) oceanmap::plotmap(raster::extent(c(xlim,ylim)),add=T,...) # oceanmap::empty.plot(xlim=xlim,ylim=ylim,axes=T) #
        mtext(side=3,main,font=2)
        
        
        dates <- unique(pos$date)
        # print(range(dates))
        alphac <- 2.2*alpha; if(alphac > 100) alphac <- 100
        cmapcol <- .makeTransparent(colorRampPalette(pal)(length(dates)*3),alpha = 255*alphac/100)
        opar <- par()
        usr <- par()$usr
        # if(abs(diff(usr[3:4])) > abs(diff(usr[1:2])) | cbpos == "b"){
        #   set.colorbar(cbx = c(14,91),cby=c(12,13)+cbline,pal = cmapcol, total.reg = F, ticks = format(seq(min(dates),max(dates),length.out = 5),format=cb.date_format),cb.xlab = cb.xlab)
        # }else{
        #   set.colorbar(cbx = usr[1]+(usr[2]-usr[1])*c(10,82.5)/100+cbline,cby=usr[3:4],pal = cmapcol, total.reg = F, ticks = format(seq(min(dates),max(dates),length.out = 5),format=cb.date_format),cb.xlab = cb.xlab)
        # }
        
        if(abs(diff(usr[3:4])) > abs(diff(usr[1:2])) | cbpos == "b"){
          set.colorbarp(cbxp = c(14,91),cbyp=c(12,13)+cbline,pal = cmapcol, total.reg = F, ticks = format(seq(min(dates),max(dates),length.out = 5),format=cb.date_format),cb.xlab = cb.xlab)
        }else{
          set.colorbarp(cbxp = c(80,82.5)+cbline,cbyp=c(21,83),pal = cmapcol, total.reg = F, ticks = format(seq(min(dates),max(dates),length.out = 5),format=cb.date_format),cb.xlab = cb.xlab)
        }
        # Return <- T
      }
      # 
      # # 
      # # 
      # # file <- "/home/robert/Dropbox/my_R-packages/RchivalTag.build/RchivalTag/inst/example_files/15P1019-104659-1-GPE3.nc"
      # # 
      # # #### original script
      # # StackedObject<-stack(file,varname="twelve_hour_likelihoods") #creates a raster object that contains
      # # MergedObject<-overlay(StackedObject,fun=mean) #merges all of the stacked likelihood surfaces using the function of your choice (can do mean, sum, etc, or make your own).
      # # MergedObject[is.na(MergedObject)]<-0 #Remove NA values
      # # plot(MergedObject)
      # # extent(MergedObject)
      # # 
      # # Raster.big<-raster(ncol=1200,nrow=900,ext=Boundaries) #creates the higher resolution grid
      # # Raster.HR<-resample(x=MergedObject,y=Raster.big,method="bilinear") #will resample the data object onto the higher resolution grid
      # # Raster.HR@data@values<-Raster.HR@data@values/sum(Raster.HR@data@values) #normalize the grid values so they sum to 1
      # # plot(Raster.HR)
      # # 
      # # RasterVals<-sort(Raster.HR@data@values) #sort the probability values
      # # Raster.breaks <- c(RasterVals[max(which(cumsum(RasterVals)<=0.05))],
      # #                    RasterVals[max(which(cumsum(RasterVals)<=0.25))],
      # #                    RasterVals[max(which(cumsum(RasterVals)<=0.5))],1) #sets breaks at the cumulative probabilities
      # # Raster.cols<-colorRampPalette(c("darkblue","blue","lightblue")) #creates a color scale, add as many values as you want or use an existing scale
      # # RasterCols<- c(Raster.cols(3)) #create the colors that you will use (there must be 1 fewer colors than
      # # plot(Raster.HR,col=RasterCols,breaks=Raster.breaks) #plot the polygons
      # # 
      # 
      # ### altered script
      # file <- "/home/robert/Dropbox/my_R-packages/RchivalTag.build/RchivalTag/inst/example_files/15P1019-104659-1-GPE3.nc"
      # # file <- system.file("example_files/15P1019-104659-1-GPE3.nc",package="RchivalTag")
      # 
      # ### extract all 0.5-polygons from all tags:
      # prob_lim <- .9
      # inst.pkg(oceanmap)
      # inst.pkg(ncdf4)
      # library(maptools)
      # library(rgeos)
      # alpha <- 70
      # pal <- 'jet'
      # par(opar)
      
      if(Return) return(pos)
    }else{
      if(class(x) == "SpatialPolygonsDataFrame"){
        out <- pols_df <- x
        
        data(cmap, package='oceanmap', envir = environment())
        
        if(prob_lim >= 1) stop('"prob_lim" needs to be smaller than 1. Please revise!')
        datetime <- pols_df$datetime 
        
        df0 <- c()
        df0 <- data.frame(ncfile=pols_df$file[1],tstart=datetime[1],tend=tail(datetime,1))
        df0$date.start <- as.Date(df0$tstart)
        df0$date.end <- as.Date(df0$tend)
        
        if(length(pal) == 1 & pal[1] %in% names(cmap)) pal <- cmap[[pal]]
        ### set common colors
        tt <- diff(as.numeric((datetime[1:2])))
        tsteps <- .num2datetime(seq(as.numeric(min(df0$tstart)),as.numeric(max(df0$tend)),by=tt),tz = "UTC",hours.offset = 0)
        Raster.cols <- .makeTransparent(colorRampPalette(pal)(length(tsteps)),alpha = 255*alpha/100)#[1:100] #creates a color scale, add as many values as you want or use an existing scale
        df.col <- data.frame(tstep=tsteps) # merge colors and time steps
        df.col$color <- as.character(Raster.cols)
        df.col$color_full <- as.character(colorRampPalette(cmap$jet)(length(tsteps)))
        df.col$date.num <- as.numeric(df.col$tstep)
        
        pos <- tsub <- df.col[which(df.col$tstep %in% datetime),]
        Return <- T
        ### plot basic map:
        
        if(missing(xlim)) xlim <- c(pols_df$xmin[1],pols_df$xmax[1])
        if(missing(ylim)) ylim <- c(pols_df$ymin[1],pols_df$ymax[1])
        
        ### plot polygons:
        if(!add) oceanmap::plotmap(raster::extent(c(xlim,ylim)),add=F,...) # oceanmap::empty.plot(xlim=xlim,ylim=ylim,axes=T) #
        plot(pols_df[,1], col=tsub$color, border=tsub$color, add=T)
        # 
        # list_pols <- list()
        # for(i in 1:length(time)){
        #   plot(pols[[i]], add = T, col = tsub$color[i], border = tsub$color[i])
        #   add <- pols[[i]]@polygons
        #   add[[1]]@ID <- as.character(i)
        #   list_pols[i] <- add
        # }
        # all <- sp::SpatialPolygons(list_pols)
        all <- sp::SpatialPolygons(pols_df@polygons)
        # merged <- maptools::unionSpatialPolygons(all,ID=1:length(all))
        merged <- try(maptools::unionSpatialPolygons(all,IDs=rep(1,length(all))),silent = T)
        if(!is(merged,"try-error")) plot(merged,xlim=xlim,ylim=ylim,add=T)
        if(!add) oceanmap::plotmap(raster::extent(c(xlim,ylim)),add=T,...) # oceanmap::empty.plot(xlim=xlim,ylim=ylim,axes=T) #
        mtext(side=3,main,font=2)
        
        dates <- unique(as.Date(pols_df$datetime))
        alphac <- 2.2*alpha; if(alphac > 100) alphac <- 100
        cmapcol <- .makeTransparent(colorRampPalette(pal)(length(dates)*3),alpha = 255*alphac/100)
        usr <- par()$usr
        if(abs(diff(usr[3:4])) > abs(diff(usr[1:2])) | cbpos == "b"){
          oceanmap::set.colorbar(cbxp = c(14,91),cby=c(12,13)+cbline,pal = cmapcol, total.reg = F, ticks = format(seq(min(dates),max(dates),length.out = 5),format=cb.date_format),cb.xlab = cb.xlab)
        }else{
          oceanmap::set.colorbar(cbxp = c(97,99)+cbline,cby=c(14,88),pal = cmapcol, total.reg = F, ticks = format(seq(min(dates),max(dates),length.out = 5),format=cb.date_format),cb.xlab = cb.xlab)
        }
      }else{
        stop("unsported input format of 'x'! Please revise!")
      }
    }
    
  }

  if(Return) return(out)
}
    
    
    