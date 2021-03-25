ggboxplot_DepthTS_by_hour <- function(ts_df, ylim, min_perc=75,
                                      main, submain, 
                                      ID, ID_label="Serial", 
                                      plot_DayTimePeriods=TRUE, twilight.set="ast",
                                      box=TRUE, jitter=FALSE, 
                                      color_by=ID_label, cb.title=color_by,
                                      pal, opacity=0.1){
  df <- ts_df
  if(missing(ID) & is.null(ts_df[[ID_label]])) {
    k <- c("DeployID","Ptt") %in% names(ts_df)
    if(any(k)) {
      ID_label <- c("DeployID","Ptt")[which(k)[1]]
      warning(paste0("ID_label 'Serial' not found! Resetting ID_label to '",ID_label,"'!"))
    }
    if(!all(k)) warning(paste0("no column corresponding to ID_label '",ID_label,"' found!"))
  }else{
    if(!missing(ID)) df <- df[which(df[[ID_label]] %in% ID),]
  }
  
  dawn.set <- paste0('dawn.', twilight.set)
  dusk.set <- paste0('dusk.', twilight.set)
  if(!all(c("sunrise","sunset",dawn.set,dusk.set) %in% names(df))){
    warning("daytime period information not found (sunrise, sunset, dawn, dusk).\n Consider joining positioning data and run 'get_DayTimeLimits' to illustrate twilight and night shadings.")
    plot_DayTimePeriods <- FALSE 
  }
  
  
  if(nrow(df) == 0){
    ggobj <-  ggplot() + theme_minimal() 
  }else{
    # sm <- df %>% group_by_(ID_label) %>% summarize(tstep=head(diff(as.numeric(datetime)),1))
    sm <- ddply(df,c(ID_label),function(x) c(tstep=head(diff(as.numeric(x$datetime)),1)))
    if(length(unique(sm$tstep)) > 1) {
      warning(message(paste("DepthTS data from several individuals contains different temporal resolutions.\nTrying to resample DepthTS data with resample_TS-function to common resolution\n", 
                            message(paste0(capture.output(sm), collapse = "\n")))))
      tstep <- max(sm$tstep)
      ## combine tagging data:
      df_bkp <- df
      df <- c()
      ids <- unique(df_bkp[[ID_label]])
      colnames <- names(df_bkp)
      for(id in ids){
        add <- df_bkp[which(df_bkp[[ID_label]] == id),]
        res <- diff(as.numeric(add$datetime))[1]
        if(res != tstep) add  <- resample_TS(add,tstep = tstep)[[1]][,colnames]
        df <- rbind(df,add)
      }
    }
    
    if(is.null(df[["date"]])) df$date <- as.Date(df$datetime)
    df$hour <- factor(.datetime2hour(df$datetime),levels=0:23)
    if(any(df$Depth < 0,na.rm = T)) df$Depth[which(df$Depth < 0)] <- 0
    df$DeployID <- df$Ptt <- df[[ID_label]]
    h <- ts2histos(df,tad_breaks = c(0,100,5000),min_perc = min_perc,aggregate_by = ID_label)
    h2 <- h$TAD$merged$df
    df$key <- paste(df[[ID_label]],df$date)
    df <- df[which(df$key %in% paste(h2[["DeployID"]],h2$date) & !is.na(df$Depth)),]
    # df <- df %>% filter(key %in% paste(h2[["DeployID"]],h2$date) & !is.na(Depth))
    dates <- df[which(!is.na(df$Depth)),]$date
    nrec <- length(unique(dates)) # days of data
    ids <- unique(df[[ID_label]])
    prefix <- "Tag ID"
    if(length(ids) > 1) prefix <- "Tag IDs"
    if(missing(main)) main <- paste(prefix, paste(ids,collapse=", "), "-", paste(range(dates),collapse=" : "))
    if(missing(submain)) submain <- paste(nrec,"days of data")
    
    if(jitter & opacity != 0) {
      outlier.shape <- NA
    }else{
      if(!missing(color_by)) warning("color_by argument ignored since jitter = FALSE")
      if(!missing(pal)) warning("color_by argument ignored since jitter = FALSE")
      outlier.shape <- 19
    }
    xlim <- c(0,24.5)
    
    if(missing(ylim)) ylim <- pretty(df$Depth)
    ylim <- rev(range(ylim))
    ylab <- "Depth (m)"
    xlab <- "Time (UTC)"

    ggobj <- ggplot(df) + 
      coord_cartesian(xlim =xlim, ylim =ylim) +
      scale_x_discrete(expand = c(0,0)) +
      scale_y_continuous(expand = c(0,0)) +
      geom_boxplot(aes_string(y="Depth",x="hour"),outlier.shape=NA)
    # ggobj <- ggobj + geom_rect(xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf, fill="grey", color=NA, alpha=.5, size=0)
    ggobj <- ggobj + 
      ylab(ylab) +
      xlab(xlab) +
      theme_classic(base_size=14) 
    
    if(plot_DayTimePeriods){
      ### calculate sunset/sunrise hours
      sunrise <- mean(.datetime2hour.dc(df$sunrise))
      sunset <- mean(.datetime2hour.dc(df$sunset))
      dawn <- mean(.datetime2hour.dc(df$dawn.ast))
      dusk <- mean(.datetime2hour.dc(df$dusk.ast))
      ggobj <- ggobj  +annotate("rect", xmin=xlim[1], xmax=xlim[2], ymin=ylim[2], ymax=ylim[1], fill="grey")
      
      if(sunset[1] < sunrise[1]){
        ggobj <- ggobj +
          annotate("rect", xmin = sunrise, xmax = xlim[2], ymin=ylim[2], ymax=ylim[1], fill="white") + 
          annotate("rect", xmin = xlim[1], xmax = sunset, ymin=ylim[2], ymax=ylim[1], fill="white") 
      }else{
        ggobj <- ggobj +
          annotate("rect", xmin = sunrise, xmax = sunset, ymin=ylim[2], ymax=ylim[1], fill="white")
      }
      ggobj <- ggobj +
        annotate("rect", xmin = dawn, xmax = sunrise, ymin=ylim[2], ymax=ylim[1], fill="grey90") + 
        annotate("rect", xmin = sunset, xmax = dusk, ymin=ylim[2], ymax=ylim[1], fill="grey90")
    }
    
    ggobj <- ggobj + geom_boxplot(aes_string(y="Depth",x="hour"),outlier.shape=outlier.shape)
    
    ggobj <- ggobj + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                           axis.ticks.length=unit(.25, "cm"))
    if(box) ggobj <- ggobj + theme(panel.border = element_rect(colour = "black", fill=NA, size=.5))
    
    ggobj <- ggobj + labs(title = main, subtitle = submain)
    
    if(jitter & opacity != 0) {
      cmap <- NULL
      data(cmap, package='oceanmap', envir = environment())
      if(missing(pal)) pal <- "jet"
      if(length(pal) == 1) pal <- cmap[[pal]]
      pal <- colorRampPalette(cmap$jet)(length(unique(df[[color_by]])))
      ggobj <- ggobj + 
        geom_jitter(aes_string(y="Depth",x="hour",colour = color_by)) + #!!ensym(color_by))) +
        scale_color_manual(name=cb.title, values=alpha(pal,opacity))
    }
  }
  return(ggobj)
}