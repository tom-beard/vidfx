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

#' Find the coordinates of each blob in the image.
#'
#' @param im A grayscale image of type cimg
#' @param this_thresh Quantile for the threshold stage (default 0.01 selects the top 1%)
#' @param sigma Standard deviation of the blur to apply before blob detection
#'
#' @return A tibble with a row for each blob, and columns named `value` (the numerical label for the blob),
#'   `mx` (x value for the centre of the blob) and `my` (y value for the centre of the blob).
#'
#' @examples
#' imname <- system.file('extdata/parrots.png', package = 'imager')
#' points <- load.image(imname) %>% grayscale %>% get_centres(sigma = 2)
get_centres <- function(im, this_thresh = 0.01, sigma = 0) {
  im %>% 
    isoblur(sigma) %>% 
    imhessian() %$%
    { xx * yy - xy^2 } %>%
    threshold(glue::glue("{100 * (1 - this_thresh)}%")) %>%
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
with(get_centres(sample_frame, 0.001, sigma = 2), points(mx, my, col = "white"))

plot(sample_frame)
with(get_centres(sample_frame, 0.001, sigma = 2), points(mx, my, col = "yellow"))
with(get_centres(sample_frame, 0.001, sigma = 4), points(mx, my, col = "red"))

