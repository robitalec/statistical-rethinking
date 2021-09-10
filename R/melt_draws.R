samples <- model_frogs_1_sample
chain <- 1
regex_vars <- 'alpha\\['
melt_vars <- 'alpha'

#' Melt draws from CmdStanMCMC
#'
#' @param samples CmdStanMCMC object
#' @param chains which chains? list of integers eg. 1 or c(1, 2)
#' @param regex_vars regex for  selecting column names in draws
#' @param melt_vars character passed to data.table::melt's measure.vars, used
#'                  to return a long instead of wide data.table
#'
#' @return
#' `data.table`
#' @export
#'
#' @examples
melt_draws <- function(samples, chains, regex_vars, melt_vars = NULL) {
	draws <- samples$draws(format = 'draws_list')
	draws_selected <- lapply(chains, function(chain) {
		draws_chain <- draws[[chain]]
		draws_names <- names(draws)
		select_names <- grep(regex_vars, nm, value = TRUE)
		draws_chain[select_names]
	})
	names(draws_selected) <- chains

	DT <- rbindlist(draws_selected, idcol = 'chain')

	if (!is.null(melt_var)) {
		draws_melt <- melt(DT, measure.vars = patterns(melt_vars), variable.factor = FALSE)
		draws_melt[, variable := tstrsplit(variable, '\\[|\\]', keep = 2, type.convert = TRUE)]
		setnames(draws_melt, 'variable', melt_vars)

		return(draws_melt)
	} else {
		return(DT)
	}

}

draws <- melt_draws(model_frogs_1_sample, 1, '\\[alpha', 'alpha')

draws_names <- names(draws)
select_names <- grep(regex_vars, nm, value = TRUE)

draws_melt <- melt(as.data.table(draws[select_names]), measure.vars = patterns(melt_vars),
									 variable.factor = FALSE)

draws_melt[, variable := tstrsplit(variable, '\\[|\\]', keep = 2, type.convert = TRUE)]

setnames(draws_melt, 'variable', melt_vars)

ggplot(draws_melt, aes(alpha, value)) +
	geom_density_2d()
