devtools::load_all()

ndi_m <- dep_sample_data(index = "ndi_m")

ndi_m <- dep_calc_index(ndi_m, geography = "county", index = "ndi_m", year = 2022,
                        return_percentiles = TRUE)

ndi_m <- dep_quantiles(ndi_m, source_var = NDI_M, new_var = ndi_m_quartiles,
                       n = 6L, return = "label")
