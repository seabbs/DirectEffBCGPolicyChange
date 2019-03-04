default: all
RDIR = .

all: StudyCasesSum.csv cases_demo_incidence.csv

StudyCasesSum.csv: extract_not_sum_data.R
			Rscript extract_not_sum_data.R
			
## Update incidence rate data
cases_demo_incidence.csv: get_incidence_rates.R
			Rscript get_incidence_rates.R


