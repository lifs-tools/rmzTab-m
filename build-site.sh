#!/bin/bash
R -e "library('pkgdown')" -e "pkgdown::build_site()"
