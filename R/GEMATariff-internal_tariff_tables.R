# Copyright 2016 Meik Michalke <meik.michalke@c3s.cc>
#
# This file is part of the R package GEMATariff.
#
# GEMATariff is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# GEMATariff is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public License
# along with GEMATariff.  If not, see <http://www.gnu.org/licenses/>.

# matrix with the base fees
# see https://www.gema.de/fileadmin/user_upload/Musiknutzer/Tarife/Tarife_ad/tarif_m_cd.pdf
tariff_matrix_m_cd <- matrix(
  c( # day1    day2     day3  eachday
      92.20, 129.08,  165.96,   55.32,  # min
      53.36,  74.70,   96.05,   32.02   # each2
  ),
  byrow=TRUE,
  nrow=2,
  dimnames=list(c("min","each2"),c("days1","days2","days3","eachday"))
)
# old tariff, see http://tbw.de/service/gema2012.pdf
tariff_matrix_m_u_iii_1_c_2012 <- matrix(
  c( #  le16    gt16
      247.66, 276.18, # first100
      124.98, 142.62  # each100
  ),
  byrow=TRUE,
  nrow=2,
  dimnames=list(c("first100","each100"),c("le16","gt16"))
)
# Für Musikaufführungen mit Musikern in Tanzlokalen
tariff_matrix_u_t <- list(
  le16=matrix(
    c(  # le250    le500   le1000   gt1000
         248.68,  373.00,  559.52,  839.28, # first100
         125.50,  188.25,  282.35,  423.55  # each100
    ),
    byrow=TRUE,
    nrow=2,
    dimnames=list(c("first100","each100"),c("le250","le500","le1000","gt1000"))
  ),
  gt16=matrix(
    c(  # le250    le500   le1000   gt1000
         277.29,  415.95,  623.94,  935.88, # first100
         143.21,  214.80,  322.22,  483.33  # each100
    ),
    byrow=TRUE,
    nrow=2,
    dimnames=list(c("first100","each100"),c("le250","le500","le1000","gt1000"))
  )
)

tariff_matrix_u_k_2013 <- matrix(
    c(  # min
        21.80,
        34.80,
        72.50,
        145.10,
        217.60,
        290.20,
        362.70,
        544.10,
        725.40,
        906.80,
        1088.10,
        1269.50,
        1450.80,
        1632.20,
        1813.50,
        181.40
    ),
  byrow=TRUE,
  ncol=1,
  dimnames=list(c(
    "le150",
    "le300",
    "le600",
    "le1200",
    "le1800",
    "le2400",
    "le3000",
    "le4500",
    "le6000",
    "le7500",
    "le9000",
    "le10500",
    "le12000",
    "le13500",
    "le15000",
    "each1500"),
    c("min"))
)
