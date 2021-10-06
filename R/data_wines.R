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
