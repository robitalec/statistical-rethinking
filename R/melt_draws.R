samples <- model_frogs_1_sample
chain <- 1
regex_vars <- 'alpha\\['
melt_vars <- 'alpha'

# TODO: flex
draws <- samples$draws(format = 'draws_list')[[chain]]
draws_names <- names(draws)

select_names <- grep(regex_vars, nm, value = TRUE)

draws_melt <- melt(as.data.table(draws[select_names]), measure.vars = patterns(melt_vars),
									 variable.factor = FALSE)

draws_melt[, variable := tstrsplit(variable, '\\[|\\]', keep = 2, type.convert = TRUE)]

setnames(draws_melt, 'variable', melt_vars)

ggplot(draws_melt, aes(alpha, value)) +
	geom_density_2d()
