data_wines <- function() {
	data("Wines2012")

	DT <- data.table(Wines2012)

	DT[, scale_score := scale(score)]
	DT[, index_judge := .GRP, judge]
	DT[, index_wine := .GRP, wine]
	DT[, index_flight := .GRP, flight]
	DT[, index_wine_american := .GRP, wine.amer]
	DT[, index_judge_american := .GRP, judge.amer]
	DT[, index_interactions := .GRP, .(judge.amer, wine.amer, flight)]
}


data_wines_list <- function() {
	DT <- data_wines()

	n_index_judge <- DT[, uniqueN(index_judge)]
	n_index_wine <- DT[, uniqueN(index_wine)]

	n_rows <- DT[, .N]

	n_index_flight <- DT[, uniqueN(flight)]
	n_index_wine_american <- DT[, uniqueN(index_wine_american)]
	n_index_judge_american <- DT[, uniqueN(index_judge_american)]

	n_index_interactions <- DT[, uniqueN(index_interactions)]

	c(
		as.list(DT[, .(scale_score = as.numeric(scale_score),
									 index_flight,
									 index_judge,
									 index_wine,
									 index_wine_american,
									 index_judge_american,
									 index_interactions)]),
		N_flights = n_index_flight,
		N_judges = n_index_judge,
		N_wines = n_index_wine,
		N_wine_american = n_index_wine_american,
		N_judge_american = n_index_judge_american,
		N_interactions = n_index_interactions,
		N = n_rows
	)
}
