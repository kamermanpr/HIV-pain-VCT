# Set additional directories 'make' must search for dependencies in
VPATH = data scripts outputs

# Create dummy targets to ensure all intermediate targets are 'made'
.PHONY: all clean

all:

clean:
	cd data; rm cleaned_data.csv
	cd outputs; rm -r demographics
	cd outputs; rm -r neuropathy-descriptive
	cd outputs; rm -r neuropathy-risk
	cd outputs; rm -r motor-descriptive
	cd outputs; rm -r motor-risk

# Generate cleaned dataset
cleaned_data.csv: data_cleaning.R
	Rscript $<

# Demographics
demographics.md: demographics.Rmd
	Rscript -e "ezknitr::ezknit(file = '$<', \
	out_dir = 'outputs/demographics', \
	fig_dir = 'figures', \
	chunk_opts = list(cache = TRUE, cache.path = './outputs/demographics/cache/'), \
	keep_html = FALSE)"

# Neuropathy - descriptive
neuropathy-descriptive.md: neuropathy-descriptive.Rmd
	Rscript -e "ezknitr::ezknit(file = '$<', \
	out_dir = 'outputs/neuropathy-descriptive', \
	fig_dir = 'figures', \
	chunk_opts = list(cache = TRUE, cache.path = './outputs/neuropathy-descriptive/cache/'), \
	keep_html = FALSE)"

# Neuropathy - risk
neuropathy-risk.md: neuropathy-risk.Rmd
	Rscript -e "ezknitr::ezknit(file = '$<', \
	out_dir = 'outputs/neuropathy-risk', \
	fig_dir = 'figures', \
	chunk_opts = list(cache = TRUE, cache.path = './outputs/neuropathy-risk/cache/'), \
	keep_html = FALSE)"

# Motor - descriptive
motor-descriptive.md: motor-descriptive.Rmd
	Rscript -e "ezknitr::ezknit(file = '$<', \
	out_dir = 'outputs/motor-descriptive', \
	fig_dir = 'figures', \
	chunk_opts = list(cache = TRUE, cache.path = './outputs/motor-descriptive/cache/'), \
	keep_html = FALSE)"

# Motor - risk
motor-risk.md: motor-risk.Rmd
	Rscript -e "ezknitr::ezknit(file = '$<', \
	out_dir = 'outputs/motor-risk', \
	fig_dir = 'figures', \
	chunk_opts = list(cache = TRUE, cache.path = './outputs/motor-risk/cache/'), \
	keep_html = FALSE)"

