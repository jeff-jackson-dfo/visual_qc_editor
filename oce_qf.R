library(oce)

options(eos="gsw")
data(ctd)
qc <- ctd
qc <- initializeFlagScheme(qc, "WHP CTD")
qc <- initializeFlags(qc, "salinity", 2)
Sspan <- diff(range(qc[["SA"]]))
Tspan <- diff(range(qc[["CT"]]))
n <- length(qc[["SA"]])
par(mfrow=c(1, 1))
plotTS(qc, type="o")
message("Click on bad points; quit by clicking to right of plot")
for (i in seq_len(n)) {
  xy <- locator(1)
  cat(str(xy))
  cat(str(par("usr")))
  if ((xy$x < par("usr")[1]) | (xy$x > par("usr")[2])) {
    break
  }
  if ((xy$y < par("usr")[3]) | (xy$y > par("usr")[4])) {
    break
  }
  i <- which.min(abs(qc[["SA"]] - xy$x)/Sspan + abs(qc[["CT"]] - xy$y)/Tspan)
  qc <- setFlags(qc, "salinity", i=i, value="bad")
  # qc <- handleFlags(qc, "bad")
  plotTS(qc, type="o")
}