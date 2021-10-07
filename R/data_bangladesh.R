data_bangladesh <- function() {
	data(bangladesh)
	DT <- data.table(bangladesh)

	DT[, id := as.integer(woman)]
	DT[, district := as.integer(as.factor(district))]
	DT[, contraception := use.contraception]

	DT[, scale_age := as.numeric(scale(age.centered))]

}

data_bangladesh_list <- function() {
	DT <- data_bangladesh()

	c(as.list(DT[, .(contraception, district, urban, scale_age,
									 n_children = living.children)]),
		N = DT[, .N],
		K = K,
		N_district = DT[, uniqueN(district)],
		alpha_k = list(rep(2L, K))
	)
}
