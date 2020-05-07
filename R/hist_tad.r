
hist_tat <- function(df, 
                     bin_breaks=NULL, bin_prefix="Bin", 
                     main, xlab="Time at Temperature (%)", ylab=expression(paste("Temperature (",degree,"C)")), labeling=TRUE,
                     Type="TAT", ...) {
  hist_tad(df=df, bin_breaks=bin_breaks, bin_prefix=bin_prefix, 
           main=main, xlab=xlab, ylab=ylab, labeling=labeling, Type=Type, ...)
}

hist_tad <- function(df, 
                     bin_breaks=NULL, bin_prefix="Bin", 
                     select_id, select_from='Ptt', aggregate_by='Ptt',
                     date, min_perc,
                     main, xlab='Time at Depth (%)', ylab="Depth (m)", labeling=TRUE,
                     xlim=c(0, 100), adaptive.xlim=FALSE, 
                     split_by=NULL, split_levels, xlab2=split_levels, 
                     ylab.side=2, ylab.line, ylab.font=1,
                     xlab.side=3, xlab.line=2.5, xlab.font=1,
                     xlab2.side=1, xlab2.line=1, xlab2.font=2,
                     main.side=3, main.line=3.8, main.font=1,
                     col=c("darkgrey", "white"), 
                     do_mid.ticks=TRUE, yaxis.pos=0, 
                     mars,
                     #                      do.last.bin=FALSE, do.greater.than.sign=TRUE, 
                     plot_sd=TRUE, plot_nrec=TRUE, plot_ntags=TRUE,
                     cex=1.2, cex.main=cex, cex.lab=cex, cex.inf=cex, 
                     return.sm=FALSE, 
                     subplot=FALSE, inside=FALSE,Type="TAD"){
  
  if(!inside & !subplot){
    if(missing(mars)) mars <- c(2.1, 5.1, 6.1, 2.1)
    if(missing(ylab.line)) ylab.line <- 3
    if(do_mid.ticks)  {
      if(mars[2] == 4.1) mars[2] <- 8
      if(ylab.line <= 5) ylab.line <- 5
    }
    par(xaxs="i", yaxs="i",mar=mars)#, ) # c(bottom, left, top, right)
    
  }else{
    par(xaxs="i", yaxs="i")
    if(inside) ylab=''
  }
  
  if(class(df) == "list"){
    hist_list <- list()
    if(!is.null(df[[Type]]))  hist_list[[Type]] <- df[[Type]]
    if(missing(split_by) & "split_by" %in% names(hist_list[[Type]]$merged)) {
      split_by <- hist_list[[Type]]$merged$split_by
      warning(paste0("histogram data is split by '", split_by, "'. Running back2back-histogram!"))
    }
    if(!is.null(df$df))  {
      hist_list[[Type]]$merged$df <- df$df
      hist_list[[Type]]$merged$bin_breaks <- df$bin_breaks
    }
    
    if(!is.null(bin_breaks)) {
      if(Type == 'TAD') hist_list_new <- rebin_histos(hist_list, tad_breaks=bin_breaks)
      if(Type == 'TAT') hist_list_new <- rebin_histos(hist_list, tat_breaks=bin_breaks)
      hist_list <- hist_list_new
    }
    IDs <- names(hist_list[[Type]])
    if(length(IDs) > 1) cat('data from several IDs found (that will be plotted seperately):',paste(IDs,sep=", "),"\n")
    for(ID in IDs){
      df <- hist_list[[Type]][[ID]]$df
      bin_breaks <- hist_list[[Type]][[ID]]$bin_breaks
      hist_tad(df=df, 
               bin_breaks=bin_breaks, bin_prefix=bin_prefix, 
               select_id=select_id, select_from=select_from, aggregate_by=aggregate_by,
               date=date, min_perc = min_perc,
               main=main, xlab=xlab, ylab=ylab, labeling=labeling,
               xlim=xlim, adaptive.xlim=adaptive.xlim, 
               split_by=split_by, split_levels=split_levels, xlab2=xlab2, 
               ylab.side=ylab.side, ylab.line=ylab.line, ylab.font=ylab.font,
               xlab.side=xlab.side, xlab.line=xlab.line, xlab.font=xlab.font,
               xlab2.side=xlab2.side, xlab2.line=xlab2.line, xlab2.font=xlab2.font,
               main.side=main.side, main.line=main.line, main.font=main.font,
               col=col, 
               do_mid.ticks=do_mid.ticks, yaxis.pos=yaxis.pos, 
               mars=mars,
               #                      do.last.bin=FALSE, do.greater.than.sign=TRUE, 
               plot_sd=plot_sd, plot_nrec=plot_nrec, plot_ntags=plot_ntags,
               cex=cex, cex.main=cex.main, cex.lab=cex.lab, cex.inf=cex.inf, 
               return.sm=return.sm, 
               subplot=subplot, inside=inside,Type=Type)
    }
  }else{
    
    if(is.null(nrow(df))) { # plot empty plot
      stop("no data found! please revise!")
    }else{
      
      if(is.null(bin_breaks)) stop('bin_breaks missing! please revise!')
      if(!missing(date)) { # select dates
        cat('running hist_tad in day mode\n')
        df <- df[which(df$date == date), ]
        if(missing(main)) main <- as.character(as.Date(date, origin="1970-01-01"))
      }
      
      #     if("dtime" %in% names(df) & length(grep('Night', names(df))) == 0) df$Night <- df$dtime
      
      if(length(split_by) > 0){
        if(missing(split_levels) & split_by == "daytime") split_levels <- c("Night", "Day")
        if(missing(split_levels)) split_levels <- unique(df[[split_by]])
        if(length(split_by) > 1) stop("argument split_by providing the vector name to split TaD is > 1. please revise")
        if(length(grep(split_by, names(df))) == 0) stop("vector name to split TaD data does not exist. please revise!")
        if(length(split_levels) > 2) stop("more than two levels found for 'split_by'-vector:\n", split_by,"\nplease revise!")
        
        if(missing(xlab2)) xlab2 <- split_levels
      }
      
      if("Depth" %in% names(df) & Type == "TAD") {
        histos <- .ts2tad(df, tad_breaks=bin_breaks, split_by=split_by, aggregate_by=aggregate_by) # convert data if needed      
        tad.df <- histos$df
        bin_breaks <- histos$bin_breaks
      }else{
        if("Temperature" %in% names(df) & Type == "TAT") {
          histos <- .ts2tat(df, tat_breaks=bin_breaks, split_by=split_by, aggregate_by=aggregate_by) # convert data if needed
          tad.df <- histos$df
          bin_breaks <- histos$bin_breaks
        }else{
          tad.df <- df
        }
      }
      
      if(!missing(min_perc)) {
        h <- ddply(tad.df[,c("DeployID","Ptt","nperc_24h")],c("DeployID","Ptt"),function(x){
          ii <- x$nperc_24h < min_perc
          c(kept=nrow(x[!ii,]),omitted=nrow(x[ii,]))})
        warning(paste(message(paste0("Omitted the following number of entries based on min_perc=",min_perc," argument!\n",paste0(capture.output(h), collapse = "\n"))),
                      "\nOriginal data untouched! (rerun 'ts2histos' to delete the data)"))
        tad.df <- tad.df[which(tad.df$nperc_24h >= min_perc),]
        
      }
      
      h <- ddply(tad.df[,c("DeployID","Ptt","nperc_24h")],c("DeployID","Ptt"),function(x){n=nrow(x); c(p0_25=round(100*nrow(x[which(x$nperc_24h <= 25),])/n,1),
                                                                                               p0_50=round(100*nrow(x[which(x$nperc_24h <= 50),])/n,1),
                                                                                               p0_75=round(100*nrow(x[which(x$nperc_24h <= 75),])/n,1),
                                                                                               p0_90=round(100*nrow(x[which(x$nperc_24h <= 90),])/n,1))})
      if(any(h$p0_50 > 50)){
        stop(paste("High percentage of missing data in at least one individual. please revise (e.g. filter with 'min_perc' argument)!\n", 
                   message(paste0(capture.output(h), collapse = "\n"))))
      }
      
      
      
      if(!missing(select_id)){
        if(!any(grepl(select_from, names(tad.df)))) 
          stop("column 'select_from' (", select_from, ") to subset input dataframe could not be found. Please revise!")
        tad.df <- tad.df[which(tad.df[[select_from]] %in% select_id), ]
      }
      
      if(missing(main) & length(unique(tad.df$DeployID)) > 1) main <- ""
      if(missing(main)) main <- paste("Tag ID", tad.df$DeployID[1])
      
      tdb <- paste0(bin_prefix, 1:length(bin_breaks))
      if(length(tad.df) < length(bin_prefix)) tad.df[[tdb[which(!(tdb %in% names(tad.df)))[1]]]] <- NA # add additional column if required
      tad.sm <- .tad_summary(tad.df, vars=split_by, bin_prefix=bin_prefix)
      #     head(tad.sm)
      
      raw <- tad.sm[, which(names(tad.sm) %in% tdb)]
      raw.sd <- plyr::ddply(tad.sm, split_by, function(x) apply(x[, which(names(tad.sm) %in% tdb)], 2, sum, na.rm=T))
      raw.sd <- raw.sd[, which(names(raw.sd) %in% tdb)]
      names(raw) <- c()
      
      if(adaptive.xlim) {
        xlim <- range(c(0, raw[tad.sm$info == 'mean', ]), na.rm=T)
        if(plot_sd) xlim <- range(c(0, raw.sd))
      }
      
      #################### merged plot:
      if(length(split_by) == 0){
        
        if(length(grep('Night', names(tad.df))) == 1) stop('day-night data provided, but not selected for plotting! please revise!')
        bbins <- barplot(rev(unlist(raw[1, ])), horiz=T, space=F, main="", ylab="", xlab="", axes=F, col=col[1], xlim=xlim)
        
        if(plot_sd){
          .plot_errorbars(k=unlist(raw[1, ]), k.sd=unlist(raw[2, ]), bbins)
        }
        if(!inside){
          axis(3)
          tick.labels <- bin_breaks
        }
        xticks <- seq(par()$xaxp[1], par()$xaxp[2], length.out=par()$xaxp[3]+1)
        ntags <- nrow(unique(tad.df[,which(names(tad.df) %in% c('NumBins','Serial','Ptt','DeployID'))]))
        if(plot_nrec) text(xticks[par()$xaxp[3]], 1.5, paste0("n = ", nrow(tad.df), " days of data"), xpd=T, pos=1, cex=cex.inf)
        if(plot_ntags) text(xticks[par()$xaxp[3]], .5, paste0("(", ntags, .switch_if(ntags == 1, " tag)"," tags)")), xpd=T, pos=1, cex=cex.inf)
        
        if(do_mid.ticks){
          axis(2, pos=0, at=1:(length(bin_breaks)), labels=rep("", length(tick.labels)), las=1)        
          ylabels <- rev(tick.labels)

          # old code: ylabels2 <- cbind(c(intToUtf8(8805), ylabels[2:length(ylabels)]+1), c(ylabels[1], ylabels[1:length(ylabels)-1]))
          ylabels2 <- cbind(c(ylabels[2:length(ylabels)]+1), c(ylabels[1:length(ylabels)-1]))
          ylabels2[nrow(ylabels2),1] <- tail(ylabels,1)
          ylabels2a <- apply(ylabels2, 1, function(x) paste(x[1], x[2], sep=""))
          ylabels2a <- apply(ylabels2, 1, function(x) paste(x[1], x[2], sep="-"))
          # axis(2, pos=xlim[1]+yaxis.pos, at=(0.5:(ncol(raw)-0.5)), lwd="", labels=ylabels2a, las=1)
          
          ylabels2a <- ylabels2a
          ylabels2a
          axis(2, pos=xlim[1]+yaxis.pos, at=(1.5:(ncol(raw)-.5)), lwd="", labels=ylabels2a, las=1)

        }else{
          axis(2, pos=0, at=1:(length(bin_breaks)), labels=rev(tick.labels), las=1)
        }
        
        ################# day-night plot:
      }else{
        left <- which(tad.sm[[split_by]] == split_levels[1])
        right <- which(tad.sm[[split_by]] == split_levels[2])
        xlim0 <- xlim
        xlim[1] <- -xlim[2]
        par(xaxs="i", yaxs="i") # c(bottom, left, top, right)
        raw.left <- raw[left, ]
        raw.right <- raw[right, ]
        
        bbins <- barplot(-rev(unlist(raw.left[1, ])), horiz=T, xlim=xlim, space=0, axes=F, col=col[1])
        barplot(rev(unlist(raw.right[1, ])), horiz=T, add=T, space=0, axes=F, col=col[2], main="")
        
        if(plot_sd){
          .plot_errorbars(k=-unlist(raw.left[1, ]), k.sd=-unlist(raw.left[2, ]), bbins)
          .plot_errorbars(k=unlist(raw.right[1, ]), k.sd=unlist(raw.right[2, ]), bbins)
        }
        
        xticks <- pretty(xlim)
        xlabels <- abs(xticks)
        ylabels <- rev(bin_breaks)
        #       if(!labeling){
        #         xlabels <- rep('', length(xlabels))
        #         ylabels <- rep('', length(ylabels))
        #       }
        axis(3, at=xticks, labels=xlabels)
        if(do_mid.ticks){
          
          axis(2, pos=xlim[1]+yaxis.pos, at=(1:ncol(raw)), labels=rep("", ncol(raw)), las=1)
          
          # ylabels2 <- cbind(c(intToUtf8(8805), ylabels[2:length(ylabels)]), c(ylabels[1], ylabels[1:length(ylabels)-1]-1))
          # ylabels2a <- apply(ylabels2, 1, function(x) paste(x[1], x[2], sep=""))
          # ylabels2a[2:length(ylabels)] <- apply(ylabels2, 1, function(x) paste(x[1], x[2], sep="-"))[2:length(ylabels)]
          # 
          
          # ylabels2 <- cbind(c(ylabels[2:length(ylabels)]+1), c(ylabels[1:length(ylabels)-1]))
          # ylabels2[nrow(ylabels2),1] <- tail(ylabels,1)
          # ylabels2a <- apply(ylabels2, 1, function(x) paste(x[1], x[2], sep=""))
          # ylabels2a <- apply(ylabels2, 1, function(x) paste(x[1], x[2], sep="-"))
          # 
          ylabels2 <- cbind(c(ylabels[2:length(ylabels)]+1), c(ylabels[1:length(ylabels)-1]))
          ylabels2[nrow(ylabels2),1] <- tail(ylabels,1)
          ylabels2a <- apply(ylabels2, 1, function(x) paste(x[1], x[2], sep=""))
          ylabels2a <- apply(ylabels2, 1, function(x) paste(x[1], x[2], sep="-"))
          

          axis(2, pos=xlim[1]+yaxis.pos, at=(1.5:(length(ylabels2a)+0.5)), lwd="", labels=ylabels2a, las=1)
          
        }else{
          axis(2, pos=xlim[1]+yaxis.pos, at=(1:length(ylabels)), c(ylabels), las=1)
          
          # axis(2, pos=xlim[1]+yaxis.pos, at=(0:length(ylabels)), c("", ylabels), las=1)
        }      
        
        if(xlab2.side == 1){
          xdiff <- par()$usr[2]/2
          text(-xdiff, 1.2, xlab2[1], xpd=T, font=2, cex=cex.inf)
          text(xdiff, 1.2, xlab2[2], xpd=T, font=2, cex=cex.inf)
          if(plot_nrec){
            text(-xdiff, .5, paste0("n(periods) = ", tad.sm$nrec[which(tad.sm[[split_by]] == split_levels[1])]), xpd=T, cex=cex.inf-.1)
            text(xdiff, .5, paste0("n(periods) = ", tad.sm$nrec[which(tad.sm[[split_by]] == split_levels[2])]), xpd=T, cex=cex.inf-.1)
          }
          #           if(plot_ntags){
          #             text(-xdiff, .5, paste0("n(tags) = ", tad.sm$nrec[which(tad.sm[[split_by]] == split_levels[1])]), xpd=T, cex=cex.inf-.1)
          #             text(xdiff, .5, paste0("n(tags) = ", tad.sm$nrec[which(tad.sm[[split_by]] == split_levels[2])]), xpd=T, cex=cex.inf-.1)
          #           }
        }else{
          axis(3, at=-mean(xlim0), line=xlab2.line, labels = xlab2[1], cex=cex.inf, lwd=0, font=xlab2.font)
          axis(3, at=mean(xlim0), line=xlab2.line, labels = xlab2[2], cex=cex.inf, lwd=0, font=xlab2.font)
        }
      }
      
      if(labeling & !inside){
        mtext(side=ylab.side, ylab, line=ylab.line, font=ylab.font, las=0, cex=cex.lab)
        mtext(side=xlab.side, xlab, line=xlab.line, font=xlab.font, cex=cex.lab)
        mtext(side=main.side, main, line=main.line, font=main.font, cex=cex.main)
      }
      
      if(return.sm) return(tad.sm)
    }
  }
}    
