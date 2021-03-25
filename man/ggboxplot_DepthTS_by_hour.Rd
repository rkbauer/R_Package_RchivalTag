\name{ggboxplot_DepthTS_by_hour}
\alias{ggboxplot_DepthTS_by_hour}

\title{
Boxplot on hourly aggregated DepthTS records
}
\description{
This function creates a boxplot via the ggplot library from hourly aggregated DepthTS records
}
\usage{
ggboxplot_DepthTS_by_hour(ts_df, ylim, min_perc=75,
                          main="", submain, 
                          ID, ID_label="Serial",
                          plot_DayTimePeriods=TRUE, twilight.set="ast",
                          box=TRUE, jitter=FALSE, 
                          color_by=ID_label, cb.title=color_by,
                          pal, opacity=0.1)
}
\arguments{
  \item{ts_df}{
\code{data.frame} with DepthTS data, 
}

  \item{ylim}{
limits of the y-axis.  
}
  \item{min_perc}{
the minimum data coverage (in percent) of daily DepthTS recoords (by default 75\%).
}
  \item{main, submain}{
The title and subtitle of the figure.
}

  \item{ID, ID_label}{
Tag ID and its label (column name; by default "Serial") to be selected (e.g. if input data frame holds tagging data from several tags).
}

  \item{plot_DayTimePeriods, twilight.set}{
whether day-time periods ('Night', 'Dawn', 'Day', 'Dusk') should be plotted as shaded areas. In case that \code{plot_DayTimePeriods} is set \code{TRUE}), the limits of each time period are required (columns \code{sunrise}, \code{sunset}, \code{dawn.ast},/\code{dawn.naut} and \code{dawn.ast}/\code{dawn.naut} in \code{POSIXct}-format. In case of the twilight events, the additional argument \code{twilight.set} defines the suffix of the twlight-set to be selected ( \code{"ast"} for astronomical dawn and dusks vs \code{"naut"} for nautical twilight events).
}

  \item{box}{
whether to draw a box around the figure (by default \code{TRUE})
}
  \item{jitter, color_by, cb.title, pal, opacity}{
whether to draw all Depth records on top of the boxplot (by default \code{FALSE}).
If selected, records will be colored via the different groups in the column specified in \code{color_by} with colors specified by the color palette \code{pal} and the opacity value between 0 (transparent) and 1 (solid). 

}

}

\seealso{\link{hist_tad}, \link{plot_DepthTS}, \link{dy_DepthTS}} 


\author{
Robert K. Bauer
}

\examples{

# ts_file <- system.file("example_files/104659-Series.csv",package="RchivalTag")
# ts_df <- read_TS(ts_file)
# ggboxplot_DepthTS_by_hour(ts_df)
# 
# ## Let's add position data to obtain twilight and nighttime shadings:
# 
# ts_df$Lon <- 5; ts_df$Lat <- 43
# ts_df2 <- get_DayTimeLimits(ts_df)
# ggboxplot_DepthTS_by_hour(ts_df2,ylim=c(0,100))
# 
# ### Let's add the actual depth records on top of the boxplot
# ###(only meaningful in case of few amounts of data):
# 
# ggboxplot_DepthTS_by_hour(ts_df2,jitter = T,opacity = 0.1)
}

