point2isochrone = function(
  latitude,
  longitude,
  time_limit = 1.92*60,
  mode = "foot",
  orsm = "http://localhost:1234/",
  fallback_radius = 160
) {
  options(osrm.server = orsm)
  options(osrm.profile = mode)
  
  # Attempt to get an isochrone
  isochrone <- tryCatch({
    osrmIsochrone(loc = c(longitude, latitude),
                  breaks = seq(0, 5, 5))
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
