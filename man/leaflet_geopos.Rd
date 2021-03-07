\name{leaflet_geopos}
\alias{leaflet_geopos}

\title{
reads and plots geolocation estimates derived from archival tagging data
}
\description{
This function creates a Leaflet map widget using htmlwidgets from spatial tagging data. The widget can be rendered on HTML pages generated from R Markdown, Shiny, or other applications.
}

\usage{
leaflet_geopos(data, key, add_label=NULL, except=NULL, collapsedLayers=TRUE,
               radius=1000, pal, cb.title="Date",cbpos="bottomright",
               showScaleBar=TRUE, showSlideBar=FALSE)
}

\arguments{
\item{data}{spatial data such as \link{data.frame} or \link{SpatialPolygonsDataFrame}.
}

\item{key}{
Vector in spatial data that defines the ID of the tracks to be plotted.
}

\item{add_label}{
}

\item{except}{
}

\item{collapsedLayers}{
}

\item{radius}{
}

\item{pal}{
  color map to be plotted in case of polygon (.nc-files) or scatter plots (default is the 'jet'-colormap, and 'year.jet' in case \code{standard_year} & \code{full_year} are set \code{TRUE}). See \link{cmap} for pre-installed color maps. Note that tracking data with constant time steps is being assumed in the color assignment. To verify this, a \link{data.frame} containing the colors at each time steps will be returned for polygon and scatter plots.
}

\item{cb.title}{
character string indicating the title of the colorbar (by default \code{'Date'}.)
}

\item{cbpos}{
position of the colorbar (by default \code{'bottomright'}.)
}

\item{showScaleBar, showSlideBar}{
whether to show the scale bar or the slide bar.
}


}


\seealso{\link{ggplotly_geopos}, \link{ggplot_geopos}} 


\author{
Robert K. Bauer
}

\examples{

# csv_file <- system.file("example_files/15P1019-104659-1-GPE3.csv",package="RchivalTag")
# s0 <- get_geopos(csv_file)
# ggplot_geopos(s0)
# leaflet_geopos(s0,key="DeployID")
# leaflet_geopos(s0,key="DeployID",showSlideBar = T)
#
# kmz_file <- system.file("example_files/15P1019-104659-1-GPE3.kmz",package="RchivalTag")
# k1 <- get_geopos(kmz_file)
# kmz_file2 <- system.file("example_files/15P0986-15P0986-2-GPE3.kmz",package="RchivalTag")
# k2 <- get_geopos(kmz_file2)
# k0 <- k3 <- rbind(k1,k2)
# ggplot_geopos(k0,ggobj = ggplotmap("lion"))
# 
# # ggobj <- ggplot_geopos(k1)
# # ggplot_geopos(ggobj = ggobj,k2)
# leaflet_geopos(k0,key="DeployID",collapsedLayers = F) %>% addMiniMap()
# leaflet_geopos(k0,key="DeployID",showSlideBar = T)
}

