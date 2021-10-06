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
