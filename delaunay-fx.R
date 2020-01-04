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

Hdet <- with(imhessian(sample_frame), (xx * yy - xy^2))
Hdet %>% plot(main = "Determinant of Hessian")

this_thresh <- 0.001
thresh_text <- glue("{100 * (1 - this_thresh)}%")
threshold(Hdet, thresh_text) %>%
  plot(main = glue("Determinant: {100 * this_thresh}% highest values"))

lab <- threshold(Hdet, thresh_text) %>% label()
plot(lab)

df <- lab %>% as.data.frame() %>% as_tibble() %>% filter(value > 0)
# unique(df$value)
centres <- df %>% 
  group_by(value) %>% 
  summarise(mx = mean(x), my = mean(y))
plot(sample_frame)
with(centres, points(mx, my, col = "red"))


