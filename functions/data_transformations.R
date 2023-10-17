
transform_metadata_to_df <- function(data) {
  traffic_points <- data$trafficRegistrationPoints
  
  df_list <- lapply(traffic_points, function(point) {
    id <- point$id
    name <- ifelse(is.null(point$name), NA, point$name)
    
    if (is.null(point$latestData$volumeByHour)) {
      latest_data <- NA
    } else {
      latest_data <- as.POSIXct(strptime(point$latestData$volumeByHour, format="%Y-%m-%dT%H:%M:%S+02:00", tz="UTC"))
    }
    
    lat <- ifelse(is.null(point$location$coordinates$latLon$lat), NA, point$location$coordinates$latLon$lat)
    lon <- ifelse(is.null(point$location$coordinates$latLon$lon), NA, point$location$coordinates$latLon$lon)
    
    return(data.frame(id = id, name = name, latestData = latest_data, lat = lat, lon = lon, stringsAsFactors = FALSE))
  })
  
  df <- do.call(rbind, df_list)
  
  return(df)
}

# -------------------------------------------------------------------------

library(anytime)
library(lubridate)

to_iso8601 <- function(datetime, offset) {
  datetime <- as_datetime(datetime)
  offset_datetime <- datetime + days(offset)
  return(paste0(anytime::iso8601(offset_datetime), "Z"))
}


to_iso8601(as_datetime("2016-09-01 10:11:12"),0)
to_iso8601(as_datetime("2016-09-01 10:11:12"),-4)

# -------------------------------------------------------------------------

vol_qry <- function(id, from, to) {
  query <- sprintf('{
    trafficData(trafficRegistrationPointId: "%s") {
      volume {
        byHour(from: "%s", to: "%s") {
          edges {
            node {
              from
              to
              total {
                volumeNumbers {
                  volume
                }
              }
            }
          }
        }
      }
    }
  }', id, from, to)
  
  return(query)
}

GQL(
  vol_qry(
    id=stations_metadata_df$id[1], 
    from=to_iso8601(stations_metadata_df$latestData[1],-4),
    to=to_iso8601(stations_metadata_df$latestData[1],0)
  ),
  .url = configs$vegvesen_url
)

# -------------------------------------------------------------------------

transform_volumes <- function(data) {
  edges <- data$trafficData$volume$byHour$edges
  
  data_list <- lapply(edges, function(edge) {
    from <- as.POSIXct(strptime(edge$node$from, format="%Y-%m-%dT%H:%M:%S+02:00", tz="UTC"))
    to <- as.POSIXct(strptime(edge$node$to, format="%Y-%m-%dT%H:%M:%S+02:00", tz="UTC"))
    volume <- edge$node$total$volumeNumbers$volume
    
    return(list(from = from, to = to, volume = volume))
  })
  
  # Convert the list into a dataframe
  df <- do.call(rbind.data.frame, data_list)
  
  return(df)
}


