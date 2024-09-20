# helper function
# Function to move a point along a given direction
move_point <- function(point, angle, mydistance) {
  # Adjust the angle calculation
  angle <- 90 - angle
  
  # Calculate new coordinates
  new_lon <- st_coordinates(point)[1] + (mydistance * cos(angle * pi / 180))
  new_lat <- st_coordinates(point)[2] + (mydistance * sin(angle * pi / 180))
  
  return(data.frame(x = new_lon, y = new_lat))
}

# main function to compute the fetch for a given angle
fetch <- function(point, angle, polygon) {
  # initialize fetch as zero and define standard step size
  fetch_distance <- NA
  
  # cut shoreline with Site, segmentize and proceed with the 15 points
  # point <- st_transform(st_snap(st_transform(point, crs = "epsg:3035"), st_transform(polygon, crs = "epsg:3035"), tolerance = 0), crs = "epsg:4326")
  clipped_shoreline <- st_intersection(x = point, y = st_cast(polygon, to = "LINESTRING"))
  clipped_shoreline <- clipped_shoreline[which.min(as.numeric(st_distance(x = clipped_shoreline, y = st_centroid(point))))]
  # plot(clipped_shoreline, add = T, col = "purple", lwd = 2.5)
  tmp_points <- lapply(1:nrow(st_coordinates(x = clipped_shoreline)[, 1:2]), function(i) {
    st_sf(geometry = st_sfc(st_point(st_coordinates(x = clipped_shoreline)[, 1:2][i, 1:2])), crs = "epsg:4326")
  })
  tmp_fdists <- replicate(n = length(tmp_points), expr = NA)
  for (i in 1:length(tmp_points)) {
    current_location <- tmp_points[[i]]
    
    test_location <- st_as_sf(x = move_point(point = current_location, angle = angle, mydistance = 1 / 111320),
                              coords = c("x", "y"), crs = "epsg:4326")
    tmp_fdists[i] <- ifelse(test = as.character(st_contains(x = polygon, y = test_location)) == "integer(0)",
                            yes = 0,
                            no = NA)
    if (is.na(tmp_fdists[i]) == T) {
      test_location <- st_as_sf(x = move_point(point = current_location, angle = angle, mydistance = 20000 / 111320), coords = c("x", "y"),
                                crs = "epsg:4326")
      line <- st_sfc(st_linestring(x = matrix(data = c(st_coordinates(current_location)[1], st_coordinates(current_location)[2],
                                                       st_coordinates(test_location)[1], st_coordinates(test_location)[2]),
                                              ncol = 2, byrow = T)), crs = "epsg:4326")
      # Use tryCatch for error handling specifically for st_cast
      tryCatch({
        clipped_line <- st_cast(st_sfc(st_intersection(x = line, y = polygon), crs = "epsg:4326"), "LINESTRING")
        clipped_line <- clipped_line[which.min(as.numeric(st_distance(x = clipped_line, y = current_location)))]
        tmp_fdists[i] <- as.numeric(st_length(x = clipped_line))
        # plot(current_location, add = T, col = "blue", cex = 0.725, pch = 15)
        # plot(clipped_line, add = T, col = "red", lwd = 2)
      }, error = function(e) {
        # In case of an error, set tmp_fdists[i] to 0 and proceed to the next iteration
        tmp_fdists[i] <- 0
      })
    }
  }
  fetch_distance <- median(tmp_fdists, na.rm = T)
  return(
    round(as.numeric(fetch_distance)/10)*10
  )
}
