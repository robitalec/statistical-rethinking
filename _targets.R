# === Targets: socprod model ----------------------------------------------
# Alec Robitaille


# Source ------------------------------------------------------------------
lapply(dir('R', '*.R', full.names = TRUE), source)



# Options -----------------------------------------------------------------
# Targets
tar_option_set(format = 'qs',
							 workspace_on_error = TRUE)

# Stan
options(mc.cores = 2,
				scipen = 999,
				digits = 2,
				cmdstanr_draws_format = "draws_df")


compiled_dir <- file.path('stan', 'compiled')
output_stan_dir <- file.path('output', 'stan')

# Data --------------------------------------------------------------------
# w05 wines
# w06 grants
# w07 trolley
# w08 frogs (1), bangladesh (2), trolley (3)
# w09 bangladesh





# Targets: data -----------------------------------------------------------
targets_data <- c(
	# data.tables
	tar_target(
		DT_wines,
		data_wines()
	),
	tar_target(
		DT_grants,
		data_grants()
	),
	tar_target(
		DT_trolley,
		data_trolley()
	),
	tar_target(
		DT_reedfrogs,
		data_reedfrogs()
	),
	tar_target(
		DT_bangladesh,
		data_bangladesh()
	),

	# model data as lists
	tar_target(
		model_data_wines,
		data_wines_list()
	),
	tar_target(
		model_data_grants,
		data_grants_list()
	),
	tar_target(
		model_data_trolley,
		data_trolley_list()
	),
	tar_target(
		model_data_reedfrogs,
		data_reedfrogs_list()
	),
	tar_target(
		model_data_bangladesh,
		data_bangladesh_list()
	)
)



targets_stan <- c(
	tar_stan_mcmc(
		stan,
		dir('stan', 'w05', full.names = TRUE),
		data = model_data_wines,
		cpp_options = list(stan_threads = TRUE),
		chains = 4,
		quiet = FALSE,
		parallel_chains = 4,
		threads_per_chain = 4,
		dir = compiled_dir,
		output_dir = output_stan_dir
	),
	tar_stan_mcmc(
		stan_b,
		dir('stan', 'w06', full.names = TRUE),
		data = model_data_grants,
		cpp_options = list(stan_threads = TRUE),
		chains = 4,
		quiet = FALSE,
		parallel_chains = 4,
		threads_per_chain = 4,
		dir = compiled_dir,
		output_dir = output_stan_dir
	),
	tar_stan_mcmc(
		stan_c,
		dir('stan', 'w07|w08_model_trolley', full.names = TRUE),
		data = model_data_trolley,
		cpp_options = list(stan_threads = TRUE),
		chains = 4,
		quiet = FALSE,
		parallel_chains = 4,
		threads_per_chain = 4,
		dir = compiled_dir,
		output_dir = output_stan_dir
	),
	tar_stan_mcmc(
		stan_d,
		dir('stan', 'w08_model_frogs', full.names = TRUE),
		data = model_data_reedfrogs,
		cpp_options = list(stan_threads = TRUE),
		chains = 4,
		quiet = FALSE,
		parallel_chains = 4,
		threads_per_chain = 4,
		dir = compiled_dir,
		output_dir = output_stan_dir
	),
	tar_stan_mcmc(
		stan_e,
		dir('stan', 'w09|w08_model_bang', full.names = TRUE),
		data = model_data_bangladesh,
		cpp_options = list(stan_threads = TRUE),
		chains = 4,
		quiet = FALSE,
		parallel_chains = 4,
		threads_per_chain = 4,
		dir = compiled_dir,
		output_dir = output_stan_dir
	)
)



# Targets: render ---------------------------------------------------------
targets_render <- c(
	tar_render(
		render_w06,
		'rmd/homework/Week-06/Week-06_RobitailleAlec.Rmd'
	)
)


# Targets: all ------------------------------------------------------------
# Automatically grab all the "targets_*" lists above
lapply(grep('targets', ls(), value = TRUE), get)
