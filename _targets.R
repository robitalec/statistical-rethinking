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
				digits = 2)



# Data --------------------------------------------------------------------





# Targets -----------------------------------------------------------------
targets_stan <- c(
	tar_stan_mcmc(
		stan,
		stan_files,
		data = model_data_list,
		cpp_options = list(stan_threads = TRUE),
		chains = 4,
		quiet = FALSE,
		parallel_chains = 4,
		threads_per_chain = 4,
		dir = compiled_dir,
		output_dir = output_stan_dir
	)
)
