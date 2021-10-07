data_grants <- function() {
	data(NWOGrants)
	DT <- data.table(NWOGrants)

	DT[, index_gender := .GRP, gender]
	DT[, index_discipline := .GRP, discipline]

}

data_grants_list <- function() {
	DT <- data_grants()

	c(
		as.list(DT[, .(awards, applications, index_gender, index_discipline)]),
		N = DT[, .N],
		N_gender = DT[, uniqueN(index_gender)],
		N_discipline = DT[, uniqueN(index_discipline)]
	)
}
