output_dir <- "results"
conditions <- paste0("b/", 0:3)
max_time   <- 99
settings   <- read.csv("soc_dynamics/settings_b.csv")

# read files
conditions_data <- list()

for(con in conditions){
  
  temp_files <- list.files(file.path(output_dir, con))
  temp_data  <- list()
  for(f in temp_files){
    temp_file       <- read.csv(file.path(output_dir, con, f), header = TRUE)
    if(max(temp_file[,1]) <= max_time - 5){
      next
    }else{
      temp_data[[f]]  <- temp_file[temp_file$Time < max_time + .5,] 
    }
  }
 
  conditions_data[[con]] <-  temp_data
}


# calculate throuputs 
conditions_throuputs <- list()

for(con in conditions){
  conditions_throuputs[[con]] <- do.call(cbind, lapply(conditions_data[[con]], function(x){
    table(c(0:max_time, round(unlist(x)))) - 1
  }))
}


# take the moving average
conditions_ma <- list()
get_ms        <- function(x, k = 15){
  sapply(1:length(x), function(i){
    temp_start <- ifelse(i <= k, 1, i - (k-1))
    sum(x[i:temp_start])
  })
}

for(con in conditions){
  conditions_ma[[con]] <- apply(conditions_throuputs[[con]], 2, get_ms)
}


# plot the throuputs / condition, aggregating across heterogeneities
throuput_means <- lapply(conditions_ma, apply, 1, mean)
throuput_ses   <- lapply(conditions_ma, function(df){
  apply(df, 1, sd) / sqrt(ncol(df))
})

cols           <- RColorBrewer::brewer.pal(length(throuput_means), "Set3")
plot(NA, xlim = c(0, max_time) , ylim = c(0, 150), lwd = 2, type = "n", xlab = "Time (sec)", ylab = "Throuput (15 sec)", las = 1)
sapply(seq_along(throuput_means), function(i){
  polygon(
    c(0:max_time, rev(0:max_time)),
    c(throuput_means[[i]] + qnorm(0.025) * throuput_ses[[i]], rev(throuput_means[[i]] + qnorm(0.975) * throuput_ses[[i]])),
    col = ggplot2::alpha(cols[i], .8), border = NA)
  lines(0:max_time, throuput_means[[i]], col = cols[i], lwd = 3)
})
legend("bottomright", gsub("/", "", names(conditions_ma)), lwd = 3, col = cols, bty = "n")


# plot the throuputs based on heterogeneity
merged_conditions <- do.call(cbind, conditions_ma)
merged_ids        <- substr(colnames(merged_conditions), 1, regexpr("_", colnames(merged_conditions)) - 1)
merged_ids        <- as.numeric(merged_ids) + 1

par(mar = c(4, 4.5, 1, 1))
plot(NA, xlim = c(0, max_time) , ylim = c(0, 150), lwd = 2, type = "n", xlab = "Time (sec)", ylab = "Throuput (15 sec)", las = 1, cex.lab = 1.5, cex.main = 2, cex.axis = 1.5)
for(con in 0:5){
  for(std in c(0.00, 0.15, 0.30, 0.45, 0.60)){
    
    temp_mean <- apply(merged_conditions[,merged_ids %in% settings$seed[settings$condition == con & settings$std == std]], 1, mean)
    temp_se   <- apply(merged_conditions[,merged_ids %in% settings$seed[settings$condition == con & settings$std == std]], 1, sd) / sqrt(sum(merged_ids %in% settings$seed[settings$condition == con & settings$std == std]))
    
    polygon(
      c(0:max_time, rev(0:max_time)),
      c(temp_mean + qnorm(0.025) * temp_se, rev(temp_mean + qnorm(0.975) * temp_se)),
      col = ggplot2::alpha(cols[con + 1], 1 - std), border = NA)
    lines(0:max_time, temp_mean, col = cols[con + 1], lwd = 3)
    
  }
}
legend("bottomright", gsub("/", "", names(conditions_ma)), lwd = 3, col = cols, bty = "n", cex = 2)


# plot the throuputs based on heterogeneity
cols2 <- RColorBrewer::brewer.pal(5, "OrRd")
par(mfrow = c(2, 3), mar = c(4, 4.5, 4, 2))
for(con in 0:5){
  
  plot(NA, xlim = c(0, max_time) , ylim = c(0, 40), lwd = 2, type = "n", xlab = "Time (sec)", ylab = "Throuput (15 sec)", las = 1, main = paste0("a", con), cex.lab = 1.5, cex.main = 2, cex.axis = 1.5)
  std_i <- 0
  
  for(std in rev(c(0.00, 0.15, 0.30, 0.45, 0.60))){
    std_i <- std_i + 1
    temp_mean <- apply(merged_conditions[,merged_ids %in% settings$seed[settings$condition == con & settings$std == std]], 1, mean)
    temp_se   <- apply(merged_conditions[,merged_ids %in% settings$seed[settings$condition == con & settings$std == std]], 1, sd) / sqrt(sum(merged_ids %in% settings$seed[settings$condition == con & settings$std == std]))
    
    polygon(
      c(0:max_time, rev(0:max_time)),
      c(temp_mean + qnorm(0.025) * temp_se, rev(temp_mean + qnorm(0.975) * temp_se)),
      col = cols2[std_i], border = NA)
    lines(0:max_time, temp_mean, col = cols2[std_i], lwd = 3)
  }
  
  if(con == 0){
    legend("bottomright", paste0("sigma = ", format(rev(c(0.00, 0.15, 0.30, 0.45, 0.60)), nsmall = 2)), lwd = 3, col = cols2, bty = "n", cex = 2)
  }
}
dev.off()


# plot end throuputs
merged_settings   <- do.call(rbind, lapply(merged_ids, function(id) settings[settings$seed == id, ]))
merged_aggregated <- cbind(throuputs = merged_conditions[nrow(merged_conditions),], merged_settings)
merged_aggregated$condition <- factor(merged_aggregated$condition)
merged_aggregated$std       <- factor(merged_aggregated$std, ordered = TRUE)

par(mar = c(4, 4.5, 1, 1))
plot(NA, xlim = c(0, .70), xaxt = "n" , ylim = c(.5, 1), lwd = 2, type = "n", xlab = "Standard Deviation of Desired Velocity", ylab = "Relative Throuput (at 100 sec)", las = 1, cex.lab = 1.5, cex.main = 2, cex.axis = 1.5)
axis(1, c(0.00, 0.15, 0.30, 0.45, 0.60) + 0.04, labels = c(0.00, 0.15, 0.30, 0.45, 0.60), cex.lab = 1.5, cex.main = 2, cex.axis = 1.5)
abline(h = 1, lty = 3)
for(con in 0:5){
  
  temp_merged <- merged_aggregated[merged_aggregated$condition == con,]
  temp_merged$rel_throuputs <- temp_merged$throuputs / mean(temp_merged$throuputs[temp_merged$std == 0])
  temp_mean   <- with(temp_merged, by(rel_throuputs, std, mean))
  temp_se     <- with(temp_merged, by(rel_throuputs, std, sd)) / sqrt(with(temp_merged, by(rel_throuputs, std, length)))
  
  adj <- con / 60
  points(as.numeric(names(temp_mean)) + adj, temp_mean, pch = 16, cex = 3, col = cols[con + 1])
  arrows(
    x0 = as.numeric(names(temp_mean)) + adj, 
    x1 = as.numeric(names(temp_mean)) + adj, 
    y0 = temp_mean + qnorm(0.025) * temp_se,
    y1 = temp_mean + qnorm(0.975) * temp_se,
    code = 3, angle = 90, length = 0.05, col = cols[con + 1], lwd = 2)
  
  if(con == 0){

  }
}
legend("bottomleft", gsub("/", "", names(conditions_ma)), pch = 16, col = cols, bty = "n", lwd = 0, cex = 2)
dev.off()





par(mar = c(4, 4.5, 1, 1))
plot(NA, xlim = c(0, .70), xaxt = "n" , ylim = c(-20, 0), lwd = 2, type = "n", xlab = "Standard Deviation of Desired Velocity", ylab = "Relative Throuput (at 100 sec)", las = 1, cex.lab = 1.5, cex.main = 2, cex.axis = 1.5)
axis(1, c(0.00, 0.15, 0.30, 0.45, 0.60) + 0.04, labels = c(0.00, 0.15, 0.30, 0.45, 0.60), cex.lab = 1.5, cex.main = 2, cex.axis = 1.5)
abline(h = 0, lty = 3)
for(con in 0:5){
  
  temp_merged <- merged_aggregated[merged_aggregated$condition == con,]
  temp_merged$rel_throuputs <- temp_merged$throuputs - mean(temp_merged$throuputs[temp_merged$std == 0])
  temp_mean   <- with(temp_merged, by(rel_throuputs, std, mean))
  temp_se     <- with(temp_merged, by(rel_throuputs, std, sd)) / sqrt(with(temp_merged, by(rel_throuputs, std, length)))
  
  adj <- con / 60
  points(as.numeric(names(temp_mean)) + adj, temp_mean, pch = 16, cex = 3, col = cols[con + 1])
  arrows(
    x0 = as.numeric(names(temp_mean)) + adj, 
    x1 = as.numeric(names(temp_mean)) + adj, 
    y0 = temp_mean + qnorm(0.025) * temp_se,
    y1 = temp_mean + qnorm(0.975) * temp_se,
    code = 3, angle = 90, length = 0.05, col = cols[con + 1], lwd = 2)
  
  if(con == 0){
    
  }
}
legend("bottomleft", gsub("/", "", names(conditions_ma)), pch = 16, col = cols, bty = "n", lwd = 0, cex = 2)
dev.off()




