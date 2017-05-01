# Set additional directories 'make' must search for dependencies in
VPATH = data scripts outputs

# Create dummy targets to ensure all intermediate targets are 'made'
.PHONY: all clean

all: data_cleaning.R \
	rds_to_csv.R \


clean:
	cd data; rm cleaned_data.*

# Generate cleaned dataset (rds format)
cleaned_data.rds: data_cleaning.R
	Rscript $<

# Generate cleaned dataset (csv format)
cleaned_data.csv: rds_to_csv.R
	Rscript $<

# Demographics
#demographics.md: demographics.Rmd
#	Rscript -e "ezknitr::ezknit(file = '$<', \
#	out_dir = 'outputs/demographics', \
#	fig_dir = 'figures', \
#	chunk_opts = list(cache = TRUE, cache.path = './outputs/demographics/cache/'), \
#	keep_html = FALSE)"
