data_reedfrogs <- function() {
	data(reedfrogs)
	DT <- data.table(reedfrogs)
}

data_reedfrogs_list <- function() {
	DT <- data_reedfrogs()

	c(
		as.list(DT[, .(
			survival = surv,
			density,
			predation = as.integer(pred),
			size = as.integer(size),
			tank = seq.int(.N)
		)]),
		N = DT[, .N]
	)
}
