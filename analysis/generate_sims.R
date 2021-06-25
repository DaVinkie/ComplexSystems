sims <- expand.grid(
  condition   = 0:5,
  std         = c(0.00, 0.15, 0.30, 0.45, 0.60),
  replication = 1:50
)
sims$seed <- 1:nrow(sims)
write.csv(sims, file = "../settings_a.csv", row.names = FALSE)


sims <- expand.grid(
  condition   = 0:3,
  std         = c(0.00, 0.15, 0.30, 0.45, 0.60),
  replication = 1:50
)
sims$seed <- 1:nrow(sims)
write.csv(sims, file = "../settings_b.csv", row.names = FALSE)
