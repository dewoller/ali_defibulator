r = sqrt(2)/10
pt1 = st_point(c(.1,.1))
pt2 = st_point(c(.9,.9))
pt3 = st_point(c(.9,.1))
b1 = st_buffer(pt1, r)
b2 = st_buffer(pt2, r)
b3 = st_buffer(pt3, r)
(ls0 = st_nearest_points(b1, b2)) # sfg

(ls = st_nearest_points(st_sfc(b1), st_sfc(b2, b3))) # sfc
plot(b1, xlim = c(-.2,1.2), ylim = c(-.2,1.2), col = NA, border = 'green')
plot(st_sfc(b2, b3), add = TRUE, col = NA, border = 'blue')
plot(ls, add = TRUE, col = 'red')
nc = st_read(system.file("gpkg/nc.gpkg", package="sf"))

plot(st_geometry(nc))
ls = st_nearest_points(nc[33,], nc)

plot(ls, col = 'red', add = TRUE)
pts = st_cast(ls, "POINT") # gives all start & end points


plot(pts[seq(1, 200, 2)], add = TRUE, col = 'blue')
# ending, "to" points, corresponding to y:
plot(pts[seq(2, 200, 2)], add = TRUE, col = 'green')



# Load sf package
library(sf)


X =  vacar_sf
Y=victoria_defib_cleaned_sf
# Assuming X and Y are your sf objects with point geometries
# Calculate distance matrix


find_closest_osrm_points_closest_n( vacar_sf, victoria_defib_cleaned_sf, n = 10)