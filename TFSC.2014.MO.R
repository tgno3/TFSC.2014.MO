## load library
library(DJL)


## Fighter jet - load dataset / parameters
data.fj <- read.table("https://dl.dropboxusercontent.com/u/12900679/datasets/fighterjet.csv", sep=",", header = T)
n.fj    <- subset(data.fj, select = 1)
d.fj    <- subset(data.fj, select = 2)
x.fj    <- data.frame(Flew = rep(1, nrow(data.fj)))
y.fj    <- subset(data.fj, select = c(8, 10, 11, 18))
t.fj    <- 1960


## Commercial Airplan - load dataset / parameters
data.ca <- read.table("https://dl.dropboxusercontent.com/u/12900679/datasets/commercialairplane.csv", sep=",", header = T)
n.ca    <- subset(data.ca, select = 1)
d.ca    <- subset(data.ca, select = 2)
x.ca    <- data.frame(Flew = rep(1, nrow(data.ca)))
y.ca    <- subset(data.ca, select = c(3, 5, 7 : 9))
t.ca    <- 2007


## Table 1. Fighter jet results
fj.all     <- roc.dea (x.fj, y.fj, d.fj, max(d.fj), "vrs", "o")
fj.pre.max <- roc.dea (x.fj, y.fj, d.fj, t.fj, "vrs", "o", "max")
fj.pre.min <- roc.dea (x.fj, y.fj, d.fj, t.fj, "vrs", "o", "min")
fj.pos.max <- target.arrival.dea (x.fj, y.fj, d.fj, t.fj, "vrs", "o", "max")
fj.pos.min <- target.arrival.dea (x.fj, y.fj, d.fj, t.fj, "vrs", "o", "min")

fj.pre.1960 <- data.frame(k       = which(d.fj <= t.fj),
                          Model   = n.fj[which(d.fj <= t.fj), ],
                          DoR     = d.fj[which(d.fj <= t.fj), ],
                          Eff_r   = fj.all$eff_r[which(d.fj <= t.fj), ],
                          Eff_c   = fj.pre.max$eff_t[which(d.fj <= t.fj), ],
                          Max.EFD = fj.pre.max$eft_date[which(d.fj <= t.fj), ],
                          Max.RoC = fj.pre.max$roc_past[which(d.fj <= t.fj), ],
                          Max.FRD = rep(NA, sum(d.fj <= t.fj)),
                          Min.EFD = fj.pre.min$eft_date[which(d.fj <= t.fj), ],
                          Min.RoC = fj.pre.min$roc_past[which(d.fj <= t.fj), ],
                          Min.FRD = rep(NA, sum(d.fj <= t.fj)))

fj.pos.1960 <- data.frame(k       = which(d.fj > t.fj),
                          Model   = n.fj[which(d.fj > t.fj), ],
                          DoR     = d.fj[which(d.fj > t.fj), ],
                          Eff_r   = fj.all$eff_r[which(d.fj > t.fj), ],
                          Eff_c   = fj.pos.max$eff_t[which(d.fj > t.fj), ],
                          Max.EFD = fj.pos.max$eft_date[which(d.fj > t.fj), ],
                          Max.RoC = rep(NA, sum(d.fj > t.fj)),
                          Max.FRD = fj.pos.max$arrival_avg[which(d.fj > t.fj), ],
                          Min.EFD = fj.pos.min$eft_date[which(d.fj > t.fj), ],
                          Min.RoC = rep(NA, sum(d.fj > t.fj)),
                          Min.FRD = fj.pos.min$arrival_avg[which(d.fj > t.fj), ])

table.1 <- rbind(fj.pre.1960, fj.pos.1960)
table.1[["MO"]] <- factor(table.1$Max.EFD == table.1$Min.EFD, labels=c("*", ""))
print(table.1[, c(1, 12, 2 : 11)], row.names = F)


## Table 2. MAD comparison - results from Xpress-MP & GLPK are omitted
table.2 <- data.frame(LP_engine = "lpSolveAPI",
                      SG_max    = sum(abs(fj.pos.max$arrival_avg - d.fj), na.rm = T) / sum(d.fj > t.fj),
                      SG_min    = sum(abs(fj.pos.min$arrival_avg - d.fj), na.rm = T) / sum(d.fj > t.fj))
print(table.2)


## Table 3. Commercial airplane results
ca.all     <- roc.dea (x.ca, y.ca, d.ca, max(d.ca), "vrs", "o")
ca.pre.max <- roc.dea (x.ca, y.ca, d.ca, t.ca, "vrs", "o", "max")
ca.pre.min <- roc.dea (x.ca, y.ca, d.ca, t.ca, "vrs", "o", "min")
ca.pos.max <- target.arrival.dea (x.ca, y.ca, d.ca, t.ca, "vrs", "o", "max")
ca.pos.min <- target.arrival.dea (x.ca, y.ca, d.ca, t.ca, "vrs", "o", "min")

ca.pre.2007 <- data.frame(k       = which(d.ca <= t.ca),
                          Model   = n.ca[which(d.ca <= t.ca), ],
                          DoR     = d.ca[which(d.ca <= t.ca), ],
                          Eff_r   = ca.all$eff_r[which(d.ca <= t.ca), ],
                          Eff_c   = ca.pre.max$eff_t[which(d.ca <= t.ca), ],
                          Max.EFD = ca.pre.max$eft_date[which(d.ca <= t.ca), ],
                          Max.RoC = ca.pre.max$roc_past[which(d.ca <= t.ca), ],
                          Max.FRD = rep(NA, sum(d.ca <= t.ca)),
                          Min.EFD = ca.pre.min$eft_date[which(d.ca <= t.ca), ],
                          Min.RoC = ca.pre.min$roc_past[which(d.ca <= t.ca), ],
                          Min.FRD = rep(NA, sum(d.ca <= t.ca)))

ca.pos.2007 <- data.frame(k       = which(d.ca > t.ca),
                          Model   = n.ca[which(d.ca > t.ca), ],
                          DoR     = d.ca[which(d.ca > t.ca), ],
                          Eff_r   = ca.all$eff_r[which(d.ca > t.ca), ],
                          Eff_c   = ca.pos.max$eff_t[which(d.ca > t.ca), ],
                          Max.EFD = ca.pos.max$eft_date[which(d.ca > t.ca), ],
                          Max.RoC = rep(NA, sum(d.ca > t.ca)),
                          Max.FRD = ca.pos.max$arrival_avg[which(d.ca > t.ca), ],
                          Min.EFD = ca.pos.min$eft_date[which(d.ca > t.ca), ],
                          Min.RoC = rep(NA, sum(d.ca > t.ca)),
                          Min.FRD = ca.pos.min$arrival_avg[which(d.ca > t.ca), ])

table.3         <- rbind(ca.pre.2007, ca.pos.2007)
table.3[["MO"]] <- factor(table.3$Max.EFD == table.3$Min.EFD, labels=c("*", ""))
print(table.3[, c(1, 12, 2 : 11)], row.names = F)


## Table 4. MAD comparison - results from Xpress-MP & GLPK are omitted
table.4 <- data.frame(LP_engine = "lpSolveAPI",
                      SG_max    = sum(abs(ca.pos.max$arrival_avg - d.ca), na.rm = T) / sum(d.ca > t.ca),
                      SG_min    = sum(abs(ca.pos.min$arrival_avg - d.ca), na.rm = T) / sum(d.ca > t.ca))
print(table.4)
