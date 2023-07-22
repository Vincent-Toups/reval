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

# ... rest of your script ...
