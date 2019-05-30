FROM chandryou/cohortmethod:3.0.2

LABEL org.label-schema.license="Apache 2.0" \
      org.label-schema.description="Net Adverse Clinical Event between Ticagrelor and Clopidogrel in Patients with Acute Coronary Syndrome based on CohortMethod v3.0.2 environment and Rocker/verse:3.5.1" \
      org.label-schema.vcs-url="https://github.com/chandryou/TicagrelorVsClopidogrel" \
      org.label-schema.vendor="OHDSI Population-Level Estimation Working Group" \
      maintainer="Seng Chan You <applegna@gmail.com>"

RUN R -q -e 'devtools::install_github("chandryou/TicagrelorVsClopidogrel", args = "--no-multiarch")'
