library(tidyverse)
library(glue)
library(fs)
library(imager)
library(deldir)

vid_src_dir <- "D:/R/video/test-vid" # replace with portable here()-based dir defn
vid_filename <- "IMG_4440.MOV"


# select simplified representative frame for testing ----------------------

input_fps <- 1
input_vid <- load.video(path(vid_src_dir, vid_filename), fps = input_fps, maxSize = 4) # ~15s on laptop
# input_vid %>% play(loop = TRUE, delay = 500)

sample_frames <- imsplit(input_vid, "z")
# sample_frames[1:12] %>% plot()
sample_frame <- sample_frames[[7]] %>% grayscale() %>% resize_halfXY()
sample_frame %>% plot()

# blob detection ----------------------------------------------------------

# adapted from examples at https://dahtah.github.io/imager/gettingstarted.html


hessdet <- function(im, scale = 1) {
  im %>% 
    isoblur(scale) %>%
    imhessian() %$%
    {scale^2 * (xx * yy - xy^2)}
}


#' Find the coordinates of each blob in the image.
#'
#' @param im A grayscale image of type cimg
#' @param this_thresh Quantile for the threshold stage (default 0.01 selects the top 1%)
#' @param scale Standard deviation of the blur to apply before blob detection
#'
#' @return A tibble with a row for each blob, and columns named `value` (the numerical label for the blob),
#'   `mx` (x value for the centre of the blob) and `my` (y value for the centre of the blob).
#'
#' @examples
#' imname <- system.file('extdata/parrots.png', package = 'imager')
#' points <- load.image(imname) %>% grayscale %>% get_centres(scale = 2)
get_centres <- function(im, threshold = 0.01, scale = 1) {
  im %>% 
    hessdet(scale) %>% 
    threshold(glue::glue("{100 * (1 - threshold)}%")) %>%
    label() %>%
    as.data.frame() %>%
    as_tibble() %>%
    filter(value > 0) %>% 
    group_by(value) %>% 
    summarise(mx = mean(x), my = mean(y))
}

plot(sample_frame)
with(get_centres(sample_frame), points(mx, my, col = "yellow"))
with(get_centres(sample_frame, 0.001), points(mx, my, col = "red"))
with(get_centres(sample_frame, 0.001, scale = 2), points(mx, my, col = "white"))

plot(sample_frame)
with(get_centres(sample_frame, 0.001, scale = 2), points(mx, my, col = "yellow"))
with(get_centres(sample_frame, 0.001, scale = 4), points(mx, my, col = "red"))


# multi-level blob detection ----------------------------------------------

max_hessdet <- function( im, scales = seq(from = max(dim(im)[1:2]) %/% 200,
                                          to = max(dim(im)[1:2]) %/% 8,
                                          by = 1)
                         ) {
  scales %>%
    map(~ hessdet(im, .)) %>%
    which.parmax()
}

i_max <- sample_frame %>% max_hessdet(seq(12, 100, by = 1))
i_max %>% plot(main = "Index of point-wise max\nacross scales")


# add Delaunay triangulation ----------------------------------------------

centres_df <- get_centres(sample_frame, threshold = 0.001, scale = 4)

# would be nice to add a wrapper so that deldir() takes a tibble as an argument!
delaunay <- deldir::deldir(centres_df$mx, centres_df$my)
# View(delaunay)

sample_frame %>%
  as.data.frame() %>% 
  ggplot(aes(x, y)) +
  geom_raster(aes(fill = value)) +
  geom_segment(data = delaunay$delsgs, aes(x = x1, y = y1, xend = x2, yend = y2),
               colour = "yellow", alpha = 0.5) +
  geom_point(data = centres_df,
             aes(mx, my), colour = "red") +
  scale_fill_gradient(low = "black", high = "white") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), trans = scales::reverse_trans())

