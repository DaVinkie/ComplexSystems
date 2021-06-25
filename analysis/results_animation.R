# this code merges images from cromosim and the computed throughput
# (we convert it into gif using a web tool later)
library(png)

img_files  <- list.files("analysis/progress_animation", full.names = TRUE)
time_stamp <- seq(0, 25, 0.10)
times      <- read.csv("analysis/sensor_animation.csv", header = TRUE)
throughput <- table(c(0:25, ceiling(unlist(times)))) - 1
get_ms        <- function(x, k = 15){
  sapply(1:length(x), function(i){
    temp_start <- ifelse(i <= k, 1, i - (k-1))
    sum(x[i:temp_start])
  })
}
throughput <- get_ms(throughput)

for(i in seq_along(img_files)){
  
  temp_img <- readPNG(img_files[i])
    
  png(paste0("analysis/parsed_animation/img_", i, ".png"), width = 1000, height = 500)  
  layout(matrix(c(1, 2), nrow = 1), widths = c(0.6, 0.4))
  
  par(mar = c(0, 0, 0, 0))
  plot(NA, type='n', axes = FALSE, xlim = c(0,697), ylim = c(0, 236), xlab = "", ylab = "", asp = 1)
  rasterImage(temp_img, 0, 0, 697, 236)
  
  par(mar = c(4, 4.5, 1, 1))
  plot(NA, xlim = c(0, 25) , ylim = c(0, 40), lwd = 2, type = "n", xlab = "Time (sec)", ylab = "Throughput (15 sec)", las = 1, cex.lab = 1.5, cex.main = 2, cex.axis = 1.5, main = "")
  lines(0:floor(time_stamp[i]), throughput[0:25 <= floor(time_stamp[i])], type = "b", cex = 1.5, pch = 16)
  dev.off()
}
