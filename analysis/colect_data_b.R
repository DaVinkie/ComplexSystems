# this script analyzes throughput times exported from the python model (biidirectional throughput)
# requires ggplot2 package for changing the color saturation

output_dir <- "results"
max_time   <- 99
settings   <- read.csv("soc_dynamics/settings_b.csv")

###  read files
conditions_data <- list()

stuck <- 0
conditions <- c(paste0("b/", 0:3),  paste0("b_stop/", 0:3))
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


# number of conditions
length(conditions_data)
# number of replications per conditions
sapply(conditions_data, length)


### calculate throuputs 
conditions_throuputs <- list()

for(con in conditions){
  conditions_throuputs[[con]] <- do.call(cbind, lapply(conditions_data[[con]], function(x){
    table(c(0:max_time, round(unlist(x)))) - 1
  }))
}


###  take the moving average
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


###  plot the throuputs / condition, aggregating across heterogeneities
throuput_means <- lapply(conditions_ma[c("b/0", "b/1", "b/2", "b/3")], apply, 1, mean)
throuput_ses   <- lapply(conditions_ma[c("b/0", "b/1", "b/2", "b/3")], function(df){
  apply(df, 1, sd) / sqrt(ncol(df))
})

cols           <- RColorBrewer::brewer.pal(length(throuput_means), "Set3")
plot(NA, xlim = c(0, max_time) , ylim = c(0, 150), lwd = 2, type = "n", xlab = "Time (sec)", ylab = "Throughput (15 sec)", las = 1)
sapply(seq_along(throuput_means), function(i){
  polygon(
    c(0:max_time, rev(0:max_time)),
    c(throuput_means[[i]] + qnorm(0.025) * throuput_ses[[i]], rev(throuput_means[[i]] + qnorm(0.975) * throuput_ses[[i]])),
    col = ggplot2::alpha(cols[i], .8), border = NA)
  lines(0:max_time, throuput_means[[i]], col = cols[i], lwd = 3)
})
legend("bottomright", gsub("/", "", names(conditions_ma[c("b/0", "b/1", "b/2", "b/3")])), lwd = 3, col = cols, bty = "n")



###  plot the throuputs based on heterogeneity
# without stopping
merged_conditions  <- do.call(cbind, conditions_ma[c("b/0", "b/1", "b/2", "b/3")]) 
merged_ids         <- substr(colnames(merged_conditions), 1, regexpr("_", colnames(merged_conditions)) - 1)
merged_ids         <- as.numeric(merged_ids) + 1
# with stopping
merged_conditions2 <- do.call(cbind, conditions_ma[c("b_stop/0", "b_stop/1", "b_stop/2", "b_stop/3")]) 
merged_ids2        <- substr(colnames(merged_conditions2), 1, regexpr("_", colnames(merged_conditions2)) - 1)
merged_ids2        <- as.numeric(merged_ids2) + 1

par(mfcol = c(1,2))
par(mar = c(4, 4.5, 3, 1))
plot(NA, xlim = c(0, max_time) , ylim = c(0, 150), lwd = 2, type = "n", xlab = "Time (sec)", ylab = "Throughput (15 sec)", las = 1, cex.lab = 1.5, cex.main = 2, cex.axis = 1.5, main = "No Stopping")
for(con in 0:3){
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
#legend("bottomright", gsub("/", "", c("b/0", "b/1", "b/2", "b/3")), lwd = 3, col = cols, bty = "n", cex = 2)

plot(NA, xlim = c(0, max_time) , ylim = c(0, 150), lwd = 2, type = "n", xlab = "Time (sec)", ylab = "Throughput (15 sec)", las = 1, cex.lab = 1.5, cex.main = 2, cex.axis = 1.5, main = "Random Stopping")
for(con in 0:3){
  for(std in c(0.00, 0.15, 0.30, 0.45, 0.60)){
    
    temp_mean <- apply(merged_conditions2[,merged_ids2 %in% settings$seed[settings$condition == con & settings$std == std]], 1, mean)
    temp_se   <- apply(merged_conditions2[,merged_ids2 %in% settings$seed[settings$condition == con & settings$std == std]], 1, sd) / sqrt(sum(merged_ids %in% settings$seed[settings$condition == con & settings$std == std]))
    
    polygon(
      c(0:max_time, rev(0:max_time)),
      c(temp_mean + qnorm(0.025) * temp_se, rev(temp_mean + qnorm(0.975) * temp_se)),
      col = ggplot2::alpha(cols[con + 1], 1 - std), border = NA)
    lines(0:max_time, temp_mean, col = cols[con + 1], lwd = 3)
    
  }
}
legend("bottomright", gsub("/", "", c("b/0", "b/1", "b/2", "b/3")), lwd = 3, col = cols, bty = "n", cex = 2)


###  plot the throuputs based on heterogeneity - no stopping
cols2  <- RColorBrewer::brewer.pal(5, "OrRd")
par(mfrow = c(2, 2), mar = c(4, 4.5, 4, 2))
for(con in 0:3){
  
  plot(NA, xlim = c(0, max_time) , ylim = c(0, 150), lwd = 2, type = "n", xlab = "Time (sec)", ylab = "Throughput (15 sec)", las = 1, main = paste0("a", con), cex.lab = 1.5, cex.main = 2, cex.axis = 1.5)
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


cols2  <- RColorBrewer::brewer.pal(5, "OrRd")
par(mar = c(4, 4.5, 4, 2))
con = 3
plot(NA, xlim = c(0, max_time) , ylim = c(0, 150), lwd = 2, type = "n", xlab = "Time (sec)", ylab = "Throughput (15 sec)", las = 1, main = paste0("a", con), cex.lab = 1.5, cex.main = 2, cex.axis = 1.5)
std_i <- 0

for(std in rev(c(0.00, 0.15, 0.30, 0.45, 0.60))){
  std_i <- std_i + 1
  temp_mean <- apply(merged_conditions[,merged_ids %in% settings$seed[settings$condition == con & settings$std == std]], 1, mean)
  temp_se   <- apply(merged_conditions[,merged_ids %in% settings$seed[settings$condition == con & settings$std == std]], 1, sd) / sqrt(sum(merged_ids %in% settings$seed[settings$condition == con & settings$std == std]))
  
  polygon(
    c(0:max_time, rev(0:max_time)),
    c(temp_mean + qnorm(0.025) * temp_se, rev(temp_mean + qnorm(0.975) * temp_se)),
    col = ggplot2::alpha(cols2[std_i], .50), border = NA)
  lines(0:max_time, temp_mean, col = cols2[std_i], lwd = 3)
}
legend("bottomright", paste0("sigma = ", format(c(0.00, 0.15, 0.30, 0.45, 0.60), nsmall = 2)), lwd = 3, col = rev(cols2), bty = "n", cex = 1.5)

dev.off()


###  plot the throuputs based on heterogeneity - with stopping
par(mfrow = c(2, 2), mar = c(4, 4.5, 4, 2))
for(con in 0:3){
  
  plot(NA, xlim = c(0, max_time) , ylim = c(0, 150), lwd = 2, type = "n", xlab = "Time (sec)", ylab = "Throughput (15 sec)", las = 1, main = paste0("a", con), cex.lab = 1.5, cex.main = 2, cex.axis = 1.5)
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

###  plot end throuputs
merged_settings   <- do.call(rbind, lapply(merged_ids, function(id) settings[settings$seed == id, ]))
merged_aggregated <- cbind(throuputs = merged_conditions[nrow(merged_conditions),], merged_settings)
merged_aggregated$condition <- factor(merged_aggregated$condition)
merged_aggregated$std       <- factor(merged_aggregated$std, ordered = TRUE)

merged_settings2  <- do.call(rbind, lapply(merged_ids2, function(id) settings[settings$seed == id, ]))
merged_aggregated2<- cbind(throuputs = merged_conditions2[nrow(merged_conditions2),], merged_settings2)
merged_aggregated2$condition<- factor(merged_aggregated2$condition)
merged_aggregated2$std      <- factor(merged_aggregated2$std, ordered = TRUE)

par(mar = c(4, 4.5, 1, 1))
plot(NA, xlim = c(0, .70), xaxt = "n" , ylim = c(.5, 1), lwd = 2, type = "n", xlab = "Standard Deviation of Desired Velocity", ylab = "Relative Throughput (at 100 sec)", las = 1, cex.lab = 1.5, cex.main = 2, cex.axis = 1.5)
axis(1, c(0.00, 0.15, 0.30, 0.45, 0.60) + 0.04, labels = c(0.00, 0.15, 0.30, 0.45, 0.60), cex.lab = 1.5, cex.main = 2, cex.axis = 1.5)
abline(h = 1, lty = 3)
for(con in 0:3){
  
  temp_merged <- merged_aggregated[merged_aggregated$condition == con,]
  temp_merged$rel_throuputs <- temp_merged$throuputs / mean(temp_merged$throuputs[temp_merged$std == 0])
  temp_mean   <- with(temp_merged, by(rel_throuputs, std, mean))
  temp_se     <- with(temp_merged, by(rel_throuputs, std, sd)) / sqrt(with(temp_merged, by(rel_throuputs, std, length)))
  
  temp_merged2<- merged_aggregated2[merged_aggregated2$condition == con,]
  temp_merged2$rel_throuputs <- temp_merged2$throuputs / mean(temp_merged$throuputs[temp_merged$std == 0])
  temp_mean2   <- with(temp_merged2, by(rel_throuputs, std, mean))
  temp_se2     <- with(temp_merged2, by(rel_throuputs, std, sd)) / sqrt(with(temp_merged2, by(rel_throuputs, std, length)))
  
  adj <- con / 60
  points(as.numeric(names(temp_mean)) + adj, temp_mean, pch = 16, cex = 3, col = cols[con + 1])
  arrows(
    x0 = as.numeric(names(temp_mean)) + adj, 
    x1 = as.numeric(names(temp_mean)) + adj, 
    y0 = temp_mean + qnorm(0.025) * temp_se,
    y1 = temp_mean + qnorm(0.975) * temp_se,
    code = 3, angle = 90, length = 0.05, col = cols[con + 1], lwd = 2)

  points(as.numeric(names(temp_mean2)) + adj, temp_mean2, pch = 1, cex = 3, col = cols[con + 1])
  arrows(
    x0 = as.numeric(names(temp_mean2)) + adj,
    x1 = as.numeric(names(temp_mean2)) + adj,
    y0 = temp_mean2 + qnorm(0.025) * temp_se2,
    y1 = temp_mean2 + qnorm(0.975) * temp_se2,
    code = 3, angle = 90, length = 0.05, col = cols[con + 1], lwd = 2)
  
  
}
legend("bottomleft", gsub("/", "", names(conditions_ma)[1:4]), pch = 16, col = cols, bty = "n", lwd = 0, cex = 2, y.intersp=.75)
dev.off()





par(mar = c(4, 4.5, 1, 1))
plot(NA, xlim = c(0, .70), xaxt = "n" , ylim = c(-30, 0), lwd = 2, type = "n", xlab = "Standard Deviation of Desired Velocity", ylab = "Absolute Decrease in Throughput (at 100 sec)", las = 1, cex.lab = 1.5, cex.main = 2, cex.axis = 1.5)
axis(1, c(0.00, 0.15, 0.30, 0.45, 0.60) + 0.04, labels = c(0.00, 0.15, 0.30, 0.45, 0.60), cex.lab = 1.5, cex.main = 2, cex.axis = 1.5)
abline(h = 1, lty = 3)
for(con in 0:3){
  
  temp_merged <- merged_aggregated[merged_aggregated$condition == con,]
  temp_merged$rel_throuputs <- temp_merged$throuputs - mean(temp_merged$throuputs[temp_merged$std == 0])
  temp_mean   <- with(temp_merged, by(rel_throuputs, std, mean))
  temp_se     <- with(temp_merged, by(rel_throuputs, std, sd)) / sqrt(with(temp_merged, by(rel_throuputs, std, length)))
  
  temp_merged2<- merged_aggregated2[merged_aggregated2$condition == con,]
  temp_merged2$rel_throuputs <- temp_merged2$throuputs - mean(temp_merged$throuputs[temp_merged$std == 0])
  temp_mean2   <- with(temp_merged2, by(rel_throuputs, std, mean))
  temp_se2     <- with(temp_merged2, by(rel_throuputs, std, sd)) / sqrt(with(temp_merged2, by(rel_throuputs, std, length)))
  
  
  adj <- con / 60
  points(as.numeric(names(temp_mean)) + adj, temp_mean, pch = 16, cex = 3, col = cols[con + 1])
  arrows(
    x0 = as.numeric(names(temp_mean)) + adj, 
    x1 = as.numeric(names(temp_mean)) + adj, 
    y0 = temp_mean + qnorm(0.025) * temp_se,
    y1 = temp_mean + qnorm(0.975) * temp_se,
    code = 3, angle = 90, length = 0.05, col = cols[con + 1], lwd = 2)
  
  points(as.numeric(names(temp_mean2)) + adj, temp_mean2, pch = 1, cex = 3, col = cols[con + 1])
  arrows(
    x0 = as.numeric(names(temp_mean2)) + adj, 
    x1 = as.numeric(names(temp_mean2)) + adj, 
    y0 = temp_mean2 + qnorm(0.025) * temp_se2,
    y1 = temp_mean2 + qnorm(0.975) * temp_se2,
    code = 3, angle = 90, length = 0.05, col = cols[con + 1], lwd = 2)
}
legend("bottomleft", gsub("/", "", names(conditions_ma)[1:4]), pch = 16, col = cols, bty = "n", lwd = 0, cex = 2, y.intersp=.75)
dev.off()




