data_trolley <- function() {
	data(Trolley)
	DT <- data.table(Trolley)

	DT[, education := as.integer(edu_levels[edu])]
}
