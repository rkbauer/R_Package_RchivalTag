\name{RchivalTag}
\alias{RchivalTag}
\docType{package}
\title{
RchivalTag - Analyzing Archival Tagging Data
}
\description{
\code{RchivalTag} provides a set of functions to analyze and visualize different data products from Archival Tags (Supported Models include amongst others: MiniPAT, sPAT, mk10, mk9 from \href{http://wildlifecomputers.com/}{Wildlife Computers} as well as LOTEK PSAT Models \href{https://www.lotek.com/products/psat-series/}{LOTEK}. Models from other Manufactorers might be supported as well.

\itemize{
  \item{"(Depth) time series data"} (See \link{empty.plot_TS}, \link{plot_TS} & \link{plot_DepthTS})
  \item{"Time-at-Depth (TaD) and Time-at-Temperature (TaT) fequencies"} (See \link{ts2histos}, \link{merge_histos}, \link{hist_tad} & \link{hist_tat})
  \item{"Depth Temperature profiles (time series data)"} (See \link{plot_DepthTempTS}, \link{plot_DepthTempTS_resampled}, \link{plot_DepthTempTS_resampled_PDT}, \link{interpolate_TempDepthProfiles}, \link{get_thermalstrat} & \link{image_TempDepthProfiles})
  \item{"PDT (PAT-style Depth Temperature profiles) data"} (See \link{read_PDT}, \link{interpolate_TempDepthProfiles}, \link{get_thermalstrat} & \link{image_TempDepthProfiles})
  \item{"visualization of geolocation estimates"} (See: \link{plot_geopos})
  }
  }

\details{
\bold{TaD-/TaT-histogram data}\cr
- The package allows to read and calculate standard summary data products (TaD-/TaT-profiles, see above) from recovered or transmitted time series data sets as well as to merge and visualize such summary data products from different tag setups/tagging programs. For more information on these data products, please see: Wildlife Computers (2016).

\bold{Depth time series data}\cr
- data visualization, optionally highlighting daytime differences (dawn, day, dusk, night).

\bold{Depth-temperature time series data}\cr
- data visualization and examination of the thermal stratification of the water column (i.e. thermocline depth, gradient and stratification index), based on previously interpolated. The paper by Bauer et al. (2015) is highly recommended in this context. 

\bold{Compatibility}\cr
So far, the package is mainly adapted for archival tagging data from \href{http://wildlifecomputers.com/}{Wildlife Computers}, but can also be applied to data from other tag manufacturers (e.g. see \link{ts2histos} in order to calculate TaD & TaT-frequencies from time series data). Function examples are based on the transmitted data sets of a miniPAT-tag from the BLUEMED-project \url{http://bluemed-project.com/}, funded by the French National Research Agency (ANR; \url{http://www.agence-nationale-recherche.fr}).
}

%\bold{Getting Started}\cr
%}


\references{
Bauer, R., F. Forget and JM. Fromentin (2015) Optimizing PAT data transmission: assessing the accuracy of temperature summary data to estimate environmental conditions. Fisheries Oceanography, 24(6): 533-539, 
\href{http://dx.doi.org/10.1111/fog.12127}{doi: 10.1111/fog.12127}

Wildlife Computers (2016) MiniPAT-User-Guide, 4 April 2016, 26 pp. \url{http://wildlifecomputers.com/wp-content/uploads/manuals/MiniPAT-User-Guide.pdf}
}

\author{
Robert K. Bauer
}

