data_trolley <- function() {
	data(Trolley)
	DT <- data.table(Trolley)
	edu_levels <- c(6, 1, 8, 4, 7, 2, 5, 3)
	DT[, education := as.integer(edu_levels[edu])]
	DT[, individual := as.integer(factor(id))]
}

data_trolley_list <- function() {
	DT <- data_trolley()

	c(
		as.list(DT[, .(response, action, intention, contact, education,
									 age = as.numeric(scale(age)),
									 gender = ifelse(male == 1, 0, 1))]),
		N = DT[, .N],
		K = DT[, uniqueN(response) - 1],
		N_edu = DT[, uniqueN(education)],
		alpha = list(rep(2, 7))
	)
}
