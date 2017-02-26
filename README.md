# GEMATariff

[![Flattr this git repo](https://api.flattr.com/button/flattr-badge-large.png)](https://flattr.com/submit/auto?user_id=m.eik&url=https://github.com/unDocUMeantIt/GEMATariff&title=GEMATariff&language=en_GB&tags=github&category=software)

This R package was developed to get a better understanding of the tariff system of the
German collective rights society GEMA. It is intended to be useful as a research tool, and
not for any practical application beyond that. Some of the tariffs are quite complex.
Allthough we implemented everything to the best of our abilities, we do not warranty the
correctness of these calculations. Also, the tariffs are constantly subject to change, and while
you read this, some of these changes might not have been implemented yet. If you need
reliable feed back on these tariffs, you should try the official GEMA online calculator
(https://online.gema.de/aidaos/) instead.

Check out the [live demo of GEMATariff](https://calc.c3s.cc/GEMATariff/).

## Installation

### Installation via GitHub

To install the package directly from GitHub, you can use `install_github()` from the [devtools](https://github.com/hadley/devtools) package:

```
library(devtools)
install_github("unDocUMeantIt/GEMATariff") # stable release
install_github("unDocUMeantIt/GEMATariff", ref="develop") # development release
```

## Contributing

To ask for help, report bugs, suggest feature improvements, or discuss the global
development of the package, please use the issue tracker on GitHub.

### Branches

Please note that all development happens in the `develop` branch. Pull requests against the `master`
branch will be rejected, as it is reserved for the current stable release.

## License

GEMATariff Copyright (C) 2016-2017 m.eik michalke, released under the
GNU Affero General Public License version 3 or (at your option) any later version.

This software is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
or FITNESS FOR A PARTICULAR PURPOSE.

You should have received a copy of the license with the
source package as the file COPYING or LICENSE.
