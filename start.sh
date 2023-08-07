#!/bin/bash

# Default values
APP="rstudio"
PORT=8787
CONTAINER_NAME="reval"

# Generate a random password if the password file doesn't exist
if [[ ! -f .password ]]; then
    echo "The .password file doesn't exist. Generating a random password..."
    openssl rand -base64 12 > .password
fi

PASSWORD=$(cat .password)

# Print help message
print_help() {
  echo "Usage: $0 [options] [application]"
  echo ""
  echo "Options:"
  echo "  -h, --help              Show this help message and exit"
  echo "  -p, --port PORT         Set the port number (default: 8787)"
  echo "  -c, --container NAME    Set the container name (default: reval)"
  echo "application can be either 'rstudio' or 'emacs'"
}

# Parse command line arguments
while (( "$#" )); do
  case "$1" in
    -h|--help)
      print_help
      exit 0
      ;;
    -p|--port)
      if [ -n "$2" ] && [ ${2:0:1} != "-" ]; then
        PORT=$2
        shift 2
      else
        echo "Error: Argument for $1 is missing" >&2
        exit 1
      fi
      ;;
    -c|--container)
      if [ -n "$2" ] && [ ${2:0:1} != "-" ]; then
        CONTAINER_NAME=$2
        shift 2
      else
        echo "Error: Argument for $1 is missing" >&2
        exit 1
      fi
      ;;
    --) # end argument parsing
      shift
      break
      ;;
    -*|--*=) # unsupported flags
      echo "Error: Unsupported flag $1" >&2
      exit 1
      ;;
    *) # preserve positional arguments
      PARAMS="$PARAMS $1"
      shift
      ;;
  esac
done
echo "Building Docker container $CONTAINER_NAME..."
docker build . --build-arg linux_user_pwd="$PASSWORD" -t $CONTAINER_NAME

# Allow local connections to the X server for Emacs
xhost +SI:localuser:$(whoami)

echo "Launching $APP in container $CONTAINER_NAME on port $PORT..."

# Set APP from the trailing command line argument if present
if [[ -n "$PARAMS" ]]; then
    APP="$PARAMS"
fi

echo "Launching $APP in container $CONTAINER_NAME on port $PORT..."

if [[ "$APP" == "rstudio" ]]; then
    # When APP is "rstudio", don't pass any command
    docker run  \
           -p $PORT:8787 \
           -v $HOME/.emacs.d:/home/rstudio/.emacs.d \
           -v $HOME/.emacs-trash:/home/rstudio/.emacs-trash \
           -v $(pwd):/home/rstudio/work \
           --user rstudio \
           --workdir /home/rstudio/work\
           --hostname val-eng\
           -e DISPLAY=:0\
           -v /tmp/.X11-unix/X0:/tmp/.X11-unix/X0\
           -e PASSWORD="$PASSWORD" \
           -it $CONTAINER_NAME
else
    # When APP is "emacs", pass the command to run
    docker run \
           -p $PORT:8787 \
           -v $HOME/.emacs.d:/home/rstudio/.emacs.d \
           -v $HOME/.emacs-trash:/home/rstudio/.emacs-trash \
	   -v $HOME/emacs-local:/home/rstudio/emacs-local \
           -v $(pwd):/home/rstudio/work \
           --user rstudio \
           --workdir /home/rstudio/work\
           --hostname val-eng\
           -e DISPLAY=:0\
           -v /tmp/.X11-unix/X0:/tmp/.X11-unix/X0\
           -e PASSWORD="$PASSWORD" \
           -it $CONTAINER_NAME\
           emacs
fi

# If we're running Rstudio, print the password
if [[ "$APP" == "rstudio" ]]; then
    echo "The password for Rstudio is: $PASSWORD"
fi


