# BACPAC Minimum Data Set Validator

This project is a data validation package for the BACPAC clinical trial. It validates the minimum data set required for this trial and checks for completeness and correctness.




## Getting Started

To use this package, you will need to have Docker installed on your system. If you don't have Docker installed, please follow the instructions on the [official Docker website](https://docs.docker.com/get-docker/).

To start the environment and build the validation package, follow these steps:

1.  Clone this repository to your local system.
2.  Navigate to the directory containing the repository.

    cd /path/to/bacpac-validator

1.  Run the start.sh script to build and start the Docker environment. This script will also start either an Emacs or RStudio environment based on your preference.

    ./start.sh [emacs|rstudio]

Replace [emacs|rstudio] with your choice of environment.

1.  After the Docker container is running and you have access to the RStudio or Emacs environment, navigate to the reval directory:

    cd reval

1.  Build the package using devtools::build(). If you do not have devtools installed, you can install it using install.packages("devtools").




## Features

The BACPAC Minimum Data Set Validator provides a comprehensive set of checks for the minimum data set of the BACPAC clinical trial. It checks for data completeness, correct data types, compliance with specified formats, and more.




## Support

If you encounter any problems or have any questions about this package, please create an issue in the GitHub issue tracker.




## Contributions

Contributions to this project are welcome. If you wish to contribute, please create a pull request and ensure your changes pass the existing tests and any new tests you add.




## License

This project is licensed under the MIT license. See the LICENSE file for more details.

