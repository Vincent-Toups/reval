#include <Rcpp.h>
using namespace Rcpp;

#include <Rcpp.h>
using namespace Rcpp;

#include"./pure-cpp.h"

//' Check if a vector of strings are valid ISO8601 durations
//' 
//' This function takes a vector of strings as input, and returns a LogicalVector
//' indicating whether each string is a valid ISO8601 duration. 
//' For each string in the input vector, the function checks if it is a valid ISO8601 duration.
//' If the string is a valid duration, it returns TRUE; otherwise, it returns FALSE.
//' If the string is NA, the function returns NA.
//' 
//' @param putative_duration_strings A vector of strings that may represent ISO8601 durations.
//' 
//' @return A LogicalVector of the same length as the input vector. 
//'         Each element in the returned vector indicates whether the corresponding string in 
//'         the input vector is a valid ISO8601 duration (TRUE) or not (FALSE).
//'         NAs in the input vector are preserved in the returned vector.
//' 
//' @export
//' @examples
//' # Assume `reval` is the name of your package
//' library(reval)
//' 
//' durations = c("P1Y2M", "INVALID", "P10D", NA)
//' check_ISO8601_durations_cpp(durations)
// [[Rcpp::export]]
LogicalVector check_ISO8601_durations_cpp(StringVector putative_duration_strings){
    
  LogicalVector out(putative_duration_strings.length());
  const int l = putative_duration_strings.length();
  for(int i = 0; i < l; i++){
    if(StringVector::is_na(putative_duration_strings[i])){
      out[i] = NA_LOGICAL;
    } else {
      out[i] =  parse_iso8601_duration(String(putative_duration_strings[i]).get_cstring()).ok;
    }
  }
  return out;
}