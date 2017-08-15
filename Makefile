# Create output and data directories if needed
$(shell mkdir -p data outputs)

# Set additional directories 'make' must search for dependencies in
VPATH = data scripts outputs

# Create dummy targets to ensure all intermediate targets are 'made'
.PHONY: all clean

all: *.rds *.csv

clean:
	rm -r data
	rm -r outputs


# Generate cleaned datasets
*.rds *.csv: clean_data.R
	Rscript $<

# Generate the cleaned dataset's codebook
#clean_data_codebook.csv: clean_data_codebook.R clean_data.rds
#	Rscript $<

# Demographics
#demographics.md: demographics.Rmd
#	Rscript -e "ezknitr::ezknit(file = '$<', \
#	out_dir = 'outputs/demographics', \
#	fig_dir = 'figures', \
#	chunk_opts = list(cache = TRUE, cache.path = './outputs/demographics/cache/'), \
#	keep_html = FALSE)"
