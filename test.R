import::from(magrittr, `%>%`, `%$%`, extract)
import::from(ggplot2, 
             ggplot, geom_polygon, geom_text, aes, coord_map, 
             map_data)
import::from(foreach, foreach, `%do%`)
import::from(broom, tidy)
dp <- loadNamespace('dplyr')
rg <- loadNamespace('rgeos')

states.df <- map_data('state') %>% 
  dp$filter(region != 'district of columbia') %>% 
  dp$mutate(region = gsub(' ', '\n', region))

states.plot <- ggplot(states.df) + 
  coord_map() + 
  geom_polygon(aes(x = long, y = lat, group = group), 
               colour = 'black', fill = 'white')
states.plot

states <- unique(states.df$region)

labels.df <- foreach(state = states, .combine = dp$bind_rows) %do% {
  state.df <- dp$filter(states.df, region == state)
  pieces <- unique(state.df$group)
  
  vertices.sp <- foreach(piece = pieces, .combine = c) %do% {
    state.df %>% 
      dp$filter(group == piece) %>% 
      dp$mutate(longlat = paste(long, lat)) %$%
      longlat %>% 
      paste(collapse = ', ') %>% 
      paste0('((', ., '))')
  } %>% 
    paste(collapse = ', ') %>% 
    paste0('MULTIPOLYGON(', ., ')') %>% 
    rg$readWKT()
  
  vertices.df <- vertices.sp %>% 
    rg$gConvexHull() %>% 
    tidy() %>% 
    dp$select(long, lat)
  
  principal.angle <- vertices.df %>% 
    cov() %>% 
    eigen() %$%
    vectors %>% 
    extract(1:2) %>% 
    rev() %>% 
    as.list() %>% 
    do.call(atan2, .) %>% 
    {. * 180 / pi + 180}
  if (principal.angle > 180 & principal.angle < 270) {
    principal.angle <- principal.angle + 180
  }
  
  centroid <- vertices.sp %>% 
    rg$gCentroid()
  
  dplyr::data_frame(state, principal.angle, 
                    long = centroid$x, lat = centroid$y)
}

states.plot + 
  geom_text(data = labels.df, 
            aes(x = long, y = lat, label = state, angle = principal.angle))

#' @title Assign angles and centroids for polygon labels
#' @description Given a tidy data frame of polygons or multipolygons, find the 
#'   centroid and approximate orientation using principal component analysis to 
#'   automatically come up with a nice way to label the polygons on a map.
#' @param polygon.df (data frame) A tidy data frame containing (multi)poygon 
#'   vertices and labels
#' @param x.col (character)
#' @param y.col (character)
#' @param id.col (character)
#' @param subid.col (character)
#' @return (data frame)
polygon.labels <- function(polygon.df, 
                           x.col = 'long', y.col = 'lat', 
                           id.col, subid.col) {
  # find vector of (multi)polygon ids
  id.vec <- unique(polygon.df[[id.col]])
  
  foreach::foreach(id = id.vec, .combine = dplyr::bind_rows) %do% {
    # subset to just this polygon
    multipol.df <- polygon.df %>%
      dplyr::filter_(paste0(label.col, ' == "', id, '"'))
    
    # vector of sub-ids (for each piece)
    subid.vec <- unique(multipol.df[[subid.col]])
    
    # construct a spatial object for the id
    vertices.sp <- foreach::foreach(subid = subid.vec, .combine = c) %do% {
      pol.df <- multipol.df %>% 
        dplyr::filter_(paste0(subid.col, ' =="', subid, '"'))
      paste(pol.df[[x.col]], pol.df[[y.col]]) %>% 
        paste(collapse = ', ') %>% 
        paste0('((', ., '))')
    } %>% 
      paste(collapse = ', ') %>% 
      paste0('MULTIPOLYGON(', ., ')') %>% 
      rgeos::readWKT()
    
    # PCA on vertices to determine the centroid
    # note that we're starting with vertices.sp instead of multipol.df
    # so we can do some simplification first
    # then find the angle of the first component
    principal.angle <- vertices.sp %>% 
      # simplify a bit (WIP)
      rgeos::gConvexHull() %>% 
      # turn into long data frame
      broom::tidy() %>% 
      # only want spatial coordinates
      dplyr::select(long, lat) %>% 
      # remove repeats (e.g., opening and ending of polygon)
      dplyr::distinct() %>% 
      cov() %>% 
      eigen() %$%
      vectors %>% 
      extract(1:2) %>% 
      rev() %>% 
      as.list() %>% 
      do.call(atan2, .) %>% 
      {. * 180 / pi + 180}
    
    # adjust the angle for better readability
    # it should go from pi / 4 to 9 pi / 4
    if (principal.angle > 180 & principal.angle < 270) {
      principal.angle <- principal.angle + 180
    }
    
    # also need the centroid
    centroid <- vertices.sp %>% 
      rg$gCentroid()
    
    dplyr::data_frame(id, principal.angle, 
                      x = centroid$x, y = centroid$y)
  }
}

states.plot + 
  geom_text(data = labels.df, 
            aes(x = x, y = y, label = id, angle = principal.angle))

midwest.states <- c('indiana', 'ohio', 'illinois', 'wisconsin', 'michigan', 
                    'minnesota', 'iowa', 'missouri', 'kansas', 'nebraska', 
                    'north\ndakota', 'south\ndakota')

midwest.df <- dp$filter(states.df, region %in% midwest.states)
midwest.labels.df <- polygon.labels(midwest.df, 
                                    id.col = 'region', subid.col = 'group')

midwest.plot <- ggplot() + 
  geom_polygon(data = midwest.df, 
               aes(x = long, y = lat, group = group), 
               colour = 'black', fill = 'white') + 
  geom_text(data = midwest.labels.df, 
            aes(x = x, y = y, label = id, angle = principal.angle)) + 
  coord_map()
midwest.plot
