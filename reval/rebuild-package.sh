#!/bin/bash
set -e  # Stop on any error

clean=0  # Default is not to clean

# Function to display help message
print_help() {
  echo "Usage: $0 [options]"
  echo ""
  echo "Options:"
  echo "  -h, --help     Show this help message and exit"
  echo "  --clean        Move old package versions to /tmp/"
  echo "  --clean=no     Do not move old package versions"
}

# Parse command line arguments
while (( "$#" )); do
  case "$1" in
    -h|--help)
      print_help
      exit 0
      ;;
    --clean)
      if [ "$2" == "no" ]; then
        clean=0
        shift 2
      else
        clean=1
        shift
      fi
      ;;
    *) # preserve positional arguments
      shift
      ;;
  esac
done

# Check Git status
git_status=$(git status --porcelain)
if [ -n "$git_status" ]; then
  echo "Warning: Your Git working directory is not clean."
fi

# Run R scripts to build the package data
Rscript ./data-raw/DATASET.R

# Extract package name and version from DESCRIPTION
package_name=$(grep '^Package:' DESCRIPTION | awk '{print $2}')
package_version=$(grep '^Version:' DESCRIPTION | awk '{print $2}')

# Check if we should clean old versions
if [[ $clean -eq 1 ]]; then
  echo "Cleaning old package versions..."
  if ls ../${package_name}_*.tar.gz 1> /dev/null 2>&1; then
    mv ../${package_name}_*.tar.gz /tmp/
  else
    echo "No old package versions found."
  fi
fi

# Run package documentation and build
Rscript -e "devtools::document()"
Rscript -e "devtools::test()"
Rscript -e "devtools::build()"


# Get the first 5 digits of the git commit ID
commit_id=$(git rev-parse --short=5 HEAD)

# Generate a copy with a tag
if [ -n "$git_status" ]; then
  cp ../${package_name}_${package_version}.tar.gz ../${package_name}_${package_version}_${commit_id}_dirty.tar.gz
else
  cp ../${package_name}_${package_version}.tar.gz ../${package_name}_${package_version}_${commit_id}.tar.gz
fi

echo "Package successfully built and tagged."
