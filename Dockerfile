FROM rocker/r-ver:4.2.2

RUN mkdir /home/outputs

RUN R -e "install.packages('remotes'); \
  remotes::install_version('assertthat', '0.2.1'); \
  remotes::install_version('haven', '2.5.1'); \
  remotes::install_version('bench', '1.1.2'); \
  remotes::install_version('dplyr', '1.0.10'); \
  remotes::install_version('Hmisc', '4.7-2'); \
  remotes::install_version('dineq', '0.1.0'); \
  remotes::install_version('purrr', '1.0.1'); \
  remotes::install_version('wINEQ', '1.1.1'); \
  remotes::install_version('ineq', '0.2-13')"

COPY data/ /home/data/
COPY benchmarks.R /home/benchmarks.R
COPY additional_ineq_functions.R /home/additional_ineq_functions.R


CMD  R -e "source('/home/benchmarks.R')"

# Windows: docker run -v ${PWD}/outputs/:/home/outputs/ r_image