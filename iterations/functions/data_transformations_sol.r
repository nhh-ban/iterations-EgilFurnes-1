

transform_metadata_to_df <- 
  function(.){
    # Function for transforming list-data from Vegvesenet 
    # to a dataframe, and recasting columns as required. 
    (.)[[1]] %>% 
      map_dfr(~as_tibble(.)) %>% 
      mutate(latestData = map_chr(latestData,1,.default="")) %>% 
      mutate(latestData = lubridate::as_datetime(latestData,tz="UTC")) %>% 
      unnest_wider(location) %>% 
      unnest_wider(latLon)
  }


transform_volumes <- 
  function(data){
    data$trafficData$volume %>% 
      map_dfr(~as_tibble(.)) %>% 
      unnest_wider(edges) %>% 
      unnest_wider(node) %>% 
      unnest_wider(total) %>% 
      unnest_wider(volumeNumbers) %>% 
      mutate(
        from=lubridate::as_datetime(from,tz="Europe/Berlin"),
        to=lubridate::as_datetime(to,tz="Europe/Berlin")
      )
  }


to_iso8601 <- 
  function(time,days_to_add){
    (time + days(days_to_add)) %>%  
      iso8601(.) %>% 
      glue::glue(.,"Z")
  }