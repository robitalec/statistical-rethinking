plot_link <- function(DT, n) {
	data.table(DT)[sample(.N, n),
								 plot(data.table(x = rep(c(-2, 2), .N),
								 								y = c(V1, V2)),
								 		 type = 'l')]
}
