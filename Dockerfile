# Using the rocker/verse image as a base
FROM rocker/verse
LABEL maintainer="Vincent Toups <toups@email.unc.edu>"

# Setting up Linux user and password
ARG linux_user_pwd
SHELL ["/bin/bash", "-o", "pipefail", "-c"]
RUN echo "rstudio:$linux_user_pwd" | chpasswd
RUN adduser rstudio sudo

# Adding Emacs repository and updating repositories list
RUN apt update && apt install -y software-properties-common
RUN add-apt-repository ppa:kelleyk/emacs
RUN DEBIAN_FRONTEND=noninteractive apt update

# Installing Debian packages
RUN DEBIAN_FRONTEND=noninteractive apt install -y \
  build-essential \
  emacs28 \
  wget \
  xfce4-terminal

# Installing cling C++ interpreter
# This makes testing C++ much more interactive.
RUN wget https://root.cern.ch/download/cling/cling_2020-11-05_ROOT-ubuntu2004.tar.bz2
RUN tar xvf /cling_2020-11-05_ROOT-ubuntu2004.tar.bz2 -C /usr --strip-components=1

# Installing CRAN packages
RUN R -e "install.packages(c('gtools', 'janitor', 'readODS', 'rjson', 'xlsx', 'haven'))"
