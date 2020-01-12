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
#' @param threshold Quantile for the threshold stage (default 0.01 selects the top 1%)
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

find_multiscale_blobs <- function(im,
                        scales = seq(from = max(dim(im)[1:2]) %/% 200,
                                     to = max(dim(im)[1:2]) %/% 8,
                                     by = 1)) {
  multiscale_hessdet <- scales %>% map(~ hessdet(im, .))
  list(i_max = which.parmax(multiscale_hessdet), d_max = parmax(multiscale_hessdet))
}

blobs <- sample_frame %>% find_multiscale_blobs(seq(12, 100, by = 1))
blobs$i_max %>% plot(main = "Index of point-wise max\nacross scales")
blobs$d_max %>% plot()

#' Find the coordinates of each blob in the image over multiple scale.
#'
#' @param im A grayscale image of type cimg
#' @param threshold Quantile for the threshold stage (default 0.01 selects the top 1%)
#' @param scales Standard deviation of the blur to apply before blob detection
#'
#' @return A tibble with a row for each blob, and columns named `scale_index` (the scale of the blob),
#'   `mx` (x value for the centre of the blob) and `my` (y value for the centre of the blob).
#'
#' @examples
#' imname <- system.file('extdata/parrots.png', package = 'imager')
#' points <- load.image(imname) %>% grayscale %>% get_multiscale_scales(scales = 4:10)
get_multiscale_blobs <- function(im, threshold = 0.01, scales) {
  blobs <- im %>% find_multiscale_blobs(seq(12, 100, by = 1))
  blobs$d_max %>% 
    threshold(glue::glue("{100 * (1 - threshold)}%")) %>%
    label() %>%
    as.data.frame() %>%
    as_tibble() %>%
    mutate(index = as.data.frame(blobs$i_max)$value) %>% 
    # filter(value > 0) %>% 
    group_by(value) %>% 
    summarise(mx = mean(x), my = mean(y), scale_index = mean(index))
}


# add Delaunay triangulation ----------------------------------------------


# a wrapper so that deldir() takes a tibble as an argument
# next steps: add options to return one of the main objects as a tibble
deldir_tidy <- function(df) {
  deldir::deldir(df$mx, df$my)
}

centres_df <- get_centres(sample_frame, threshold = 0.001, scale = 4)
delaunay <- centres_df %>% deldir_tidy()

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

centres_multiscale_df <- get_multiscale_blobs(sample_frame, threshold = 0.04, scales = seq(12, 100, by = 1))
delaunay_ms <- centres_multiscale_df %>% deldir_tidy()

sample_frame %>%
  as.data.frame() %>% 
  ggplot(aes(x, y)) +
  geom_raster(aes(fill = value)) +
  geom_segment(data = delaunay_ms$delsgs, aes(x = x1, y = y1, xend = x2, yend = y2),
               colour = "yellow", alpha = 0.5) +
  geom_point(data = centres_multiscale_df,
             aes(mx, my), colour = "red") +
  scale_fill_gradient(low = "black", high = "white") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), trans = scales::reverse_trans())
