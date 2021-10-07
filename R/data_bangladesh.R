data_bangladesh <- function() {
	data(bangladesh)
	DT <- data.table(bangladesh)

	DT[, id := as.integer(woman)]
	DT[, district := as.integer(as.factor(district))]
	DT[, contraception := use.contraception]

	DT[, scale_age := as.numeric(scale(age.centered))]

}
