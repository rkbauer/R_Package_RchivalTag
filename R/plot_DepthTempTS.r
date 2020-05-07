

plot_DepthTempTS <- function(ts_df, y="Depth", z="Temperature", xlim, ylim, zlim, pal="jet", 
                             cb.xlab, cb.xlab.line=0, pt.lwd, do_interp=TRUE, Return=FALSE, mars, ...){
  z0 <- z
  y0 <- y
  if(missing(mars)) mars <- c(5,4,4,10)
  par(mar=mars)
  a <- plot_DepthTS(ts_df, xlim = xlim, ylim = ylim, Return = TRUE, ...)
  cmap <- NULL
  # usethis::use_data("cmap", package='oceanmap', overwrite = TRUE)
  data("cmap", package='oceanmap', envir = environment())
  cpal <- cmap[[pal]]
  z <- a[[z0]]
  zstep <- 0.1
  if(missing(cb.xlab)) cb.xlab <- expression("Temperature ("*degree*C*")")
  if(missing(zlim)) zlim <- range(pretty(z),na.rm=TRUE)
  add <- data.frame(z=seq(zlim[1],zlim[2],by=zstep))
  add$z <- round(add$z,1)
  add <- unique(add)
  add$col <- colorRampPalette(cpal)(nrow(add))
  
  if(!do_interp){
    if(missing(pt.lwd)) pt.lwd <- .7
    out <- merge(a,add,by.x=z0,by.y="z",all.x=TRUE)
    points(out$datetime,out[[y0]],col=out$col,pch=19,cex=pt.lwd)
  }else{
    tstep <- min(diff(as.numeric(ts_df$datetime)))
    if(tstep > 100) warning("Consider running plot_DepthTempTS_resampled or plot_DepthTempTS_resampled_PDT, which is more accurate for low resolution and transmitted time series data!")
    if(missing(pt.lwd)) pt.lwd <- .1
    x <- a$datetime
    y <- a[[y0]]
    z <- a[[z0]]
    input <- data.frame(x=x,y=y,z=z)
    ii <- which(!is.na(input$z))
    for(j in 2:length(ii)){
      if(ii[j-1] == ii[j]-1){
        # j <- 2
        yin <- c(input$y[ii[j-1]],input$y[ii[j]])
        zin <- c(input$z[ii[j-1]],input$z[ii[j]])
        if(yin[1]>yin[2]) ystep <- -.5
        if(yin[1]<=yin[2]) ystep <- .5
        yout <- seq(yin[1],yin[2],by = ystep)
        # if(any(diff(yout)>0.5)) stop()
        if(length(yout) >1) {
          intp <- approx(x = yin, xout = yout, y = zin)
          out <- merge(data.frame(x=seq(x[ii[j-1]],x[ii[j]],length.out = length(yout)),y=intp$x,z=round(intp$y,1)),add,by="z",all.x=T)
          head(out)
        }else{
          out <- merge(data.frame(x=c(x[ii[j-1]],x[ii[j]]),y=yin,z=zin),add,by="z",all.x=T)
        }
        points(as.numeric(out$x),out$y,col=out$col,pch=19,cex=pt.lwd)
        # u <- readline()
        # if(u == "s") stop()
        # lines(as.numeric(out$x),out$y,col=out$col)
        head(out)
      }
    }
  }
  # par(new=T)
  # oceanmap::empty.plot()
  # oceanmap::set.colorbar(cbpos = "r",pal = pal,zlim = zlim, cb.xlab = cb.xlab,cb.xlab.line=cb.xlab.line)
  oceanmap::set.colorbarp(cbxp = c(90,91),cbyp = c(20,84),total.reg = F,pal = pal,zlim = zlim, cb.xlab=cb.xlab,cb.xlab.line=cb.xlab.line)
  
  if(Return) return(a)
}






plot_DepthTempTS_resampled_PDT <- function(ts_df, PDT, y="Depth", z="Temperature", xlim, ylim, zlim, pal="jet", 
                                           cb.xlab, cb.xlab.line=0, pt.lwd, do_interp=TRUE, Return=FALSE, mars, ...){
  
  if(!do_interp) {
    ts_df <- resample_PDT(ts_df, PDT)
    if(missing(pt.lwd)) pt.lwd <- .7
    a <- plot_DepthTempTS(ts_df,do_interp=F,y=y, z=z, xlim=xlim, ylim=ylim, zlim=zlim, pal=pal, 
                     cb.xlab=cb.xlab, cb.xlab.line=cb.xlab.line, pt.lwd=pt.lwd, Return=Return,...)
  }else{
    m <- interpolate_PDTs(PDT)
    M <- m$station.1$Temperature_matrix
    if(missing(pt.lwd)) pt.lwd <- .1
    
    y0 <- y
    if(missing(mars)) mars <- c(5,4,4,10)
    par(mar=mars)
    a <- plot_DepthTS(ts_df, xlim = xlim, ylim = ylim, Return = TRUE, ...)
    cmap <- NULL
    # usethis::use_data("cmap", package='oceanmap', overwrite = TRUE)
    data("cmap", package='oceanmap', envir = environment())
    cpal <- cmap[[pal]]
    if(missing(cb.xlab)) cb.xlab <- expression("Temperature ("*degree*C*")")
    
    if(missing(zlim)) zlim <- range(pretty(as.vector(M)),na.rm=TRUE)
    zstep <- 0.1
    add <- data.frame(z=seq(zlim[1],zlim[2],by=zstep))
    add$z <- round(add$z,1)
    add <- unique(add)
    add$col <- colorRampPalette(cpal)(nrow(add))
    
    if(missing(pt.lwd)) pt.lwd <- .1
    x <- a$datetime
    y <- a[[y0]]
    # z <- a[[z0]]
    input <- data.frame(x=x,y=y)
    ii <- which(!is.na(input$y))
    for(j in 2:length(ii)){ ## go through all records of y
      if(ii[j-1] == ii[j]-1){
        # j <- 2
        yin <- c(input$y[ii[j-1]],input$y[ii[j]])
        if(yin[1]>yin[2]) ystep <- -.5
        if(yin[1]<=yin[2]) ystep <- .5
        yout <- seq(yin[1],yin[2],by = ystep)
        xout <- seq(x[ii[j-1]],x[ii[j]],length.out=length(yout))
        # if(any(diff(yout)>0.5)) stop()
        
        zout <- rep(NA,length(yout))
        for(n in 1:length(yout)){
          A <- which(m$station.1$Date == a$date[j])
          B <- which(m$station.1$Depth == yout[n])
          zout[n] <- round(M[B,A],1)
        }
        out <- merge(data.frame(x=xout,y=yout,z=zout),add,by="z",all.x=T)
        
        points(as.numeric(out$x),out$y,col=out$col,pch=19,cex=pt.lwd)
        # u <- readline()
        # if(u == "s") stop()
        # lines(as.numeric(out$x),out$y,col=out$col)
        head(out)
      }
    }
    # par(new=T)
    # oceanmap::empty.plot()
    # oceanmap::set.colorbar(cbpos = "r",pal = pal,zlim = zlim, cb.xlab = cb.xlab,cb.xlab.line=cb.xlab.line)
    oceanmap::set.colorbarp(cbxp = c(90,91),cbyp = c(20,84),total.reg = F,pal = pal,zlim = zlim, cb.xlab=cb.xlab,cb.xlab.line=cb.xlab.line)
    
  }
  if(Return) return(a)
}



plot_DepthTempTS_resampled <- function(ts_df, y="Depth", z="Temperature", bin_res=10, xlim, ylim, zlim, pal="jet", 
                                           cb.xlab, cb.xlab.line=0, pt.lwd, do_interp=TRUE, Return=FALSE, mars, ...){
  
  if(!do_interp) {
    ts_df <- resample_DepthTempTS(ts_df)
    if(missing(pt.lwd)) pt.lwd <- .7
    a <- plot_DepthTempTS(ts_df,do_interp=F,y=y, z=z, xlim=xlim, ylim=ylim, zlim=zlim, pal=pal, 
                          cb.xlab=cb.xlab, cb.xlab.line=cb.xlab.line, pt.lwd=pt.lwd, Return=Return,...)
  }else{
    DepthTempTS_binned <- bin_TempTS(ts_df,res=bin_res)
    m <- interpolate_PDTs(DepthTempTS_binned,show_info = F)
    M <- m$station.1$Temperature_matrix
    if(missing(pt.lwd)) pt.lwd <- .1
    
    y0 <- y
    if(missing(mars)) mars <- c(5,4,4,10)
    par(mar=mars)
    a <- plot_DepthTS(ts_df, xlim = xlim, ylim = ylim, Return = TRUE, ...)
    cmap <- NULL
    # usethis::use_data("cmap", package='oceanmap', overwrite = TRUE)
    data("cmap", package='oceanmap', envir = environment())
    cpal <- cmap[[pal]]
    if(missing(cb.xlab)) cb.xlab <- expression("Temperature ("*degree*C*")")
    
    if(missing(zlim)) zlim <- range(pretty(as.vector(M)),na.rm=TRUE)
    zstep <- 0.1
    add <- data.frame(z=seq(zlim[1],zlim[2],by=zstep))
    add$z <- round(add$z,1)
    add <- unique(add)
    add$col <- colorRampPalette(cpal)(nrow(add))
    
    if(missing(pt.lwd)) pt.lwd <- .1
    x <- a$datetime
    y <- a[[y0]]
    # z <- a[[z0]]
    input <- data.frame(x=x,y=y)
    ii <- which(!is.na(input$y))
    for(j in 2:length(ii)){ ## go through all records of y
      if(ii[j-1] == ii[j]-1){
        # j <- 2
        yin <- c(input$y[ii[j-1]],input$y[ii[j]])
        if(yin[1]>yin[2]) ystep <- -.5
        if(yin[1]<=yin[2]) ystep <- .5
        yout <- seq(yin[1],yin[2],by = ystep)
        xout <- seq(x[ii[j-1]],x[ii[j]],length.out=length(yout))
        # if(any(diff(yout)>0.5)) stop()
        
        zout <- rep(NA,length(yout))
        for(n in 1:length(yout)){
          A <- which(m$station.1$Date == a$date[j])
          B <- which(m$station.1$Depth == yout[n])
          
          if(length(A) > 0 & length(B) > 0 ) zout[n] <- round(M[B,A],1)
        }
        out <- merge(data.frame(x=xout,y=yout,z=zout),add,by="z",all.x=T)
        
        points(as.numeric(out$x),out$y,col=out$col,pch=19,cex=pt.lwd)
        # u <- readline()
        # if(u == "s") stop()
        # lines(as.numeric(out$x),out$y,col=out$col)
        head(out)
      }
    }
    # par(new=T)
    # oceanmap::empty.plot()
    # oceanmap::set.colorbar(cbpos = "r",pal = pal,zlim = zlim, cb.xlab = cb.xlab,cb.xlab.line=cb.xlab.line)
    oceanmap::set.colorbarp(cbxp = c(93,94),cbyp = c(20,84),total.reg = F,pal = pal,zlim = zlim, cb.xlab=cb.xlab,cb.xlab.line=cb.xlab.line)
    
  }
  if(Return) return(a)
}
