data_trolley <- function() {
	data(Trolley)
	DT <- data.table(Trolley)
	edu_levels <- c(6, 1, 8, 4, 7, 2, 5, 3)
	DT[, education := as.integer(edu_levels[edu])]
	DT[, individual := as.integer(factor(id))]
}
