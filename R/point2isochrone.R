point2isochrone = function(
  latitude,
  longitude,
  time_limit = 2.479,
  mode = "foot",
  orsm = "http://localhost:1234/",
  fallback_radius = 160
) {
  options(osrm.server = orsm)
  options(osrm.profile = mode)
  
  # Attempt to get an isochrone
  isochrone <- tryCatch({
    osrmIsochrone(loc = c(longitude, latitude),
                  breaks = seq(0, time_limit, time_limit))
  }, error = function(e) {
    # If OSRM fails or times out
    NULL
  })
  
  if (is.null(isochrone) || nrow(isochrone) == 0) {
    # Fallback to a simple buffer of 'fallback_radius' meters
    return(
      st_point(c(longitude, latitude)) %>%
        st_sfc(crs = 4326) %>%
        st_transform(3857) %>%
        st_buffer(dist = fallback_radius) %>%
        st_transform(4326)
    )
  } else {
    # else use the successful isochrone
    return(isochrone)
  }
}
