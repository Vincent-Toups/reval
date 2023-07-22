#ifndef PURE_CPP_H
#define PURE_CPP_H

#include <cstdlib>
#include <cstdio>
#include <cstring>
#include <cmath>

// NumReadResult struct represents the result of partially parsing an ISO 8601 Duration.
struct NumReadResult {
  int count;      // Number of characters processed
  double number;  // Numeric value extracted
  char tag;       // Tag character associated with the number (e.g., 'Y', 'M', 'D', 'H', 'M', 'S')
};

// ISO8601DurationParsed struct represents a fully parsed ISO 8601 duration.
struct ISO8601DurationParsed {
  double y;   // Years
  double mo;  // Months
  double d;   // Days
  double h;   // Hours
  double mi;  // Minutes
  double s;   // Seconds
  bool ok;    // Parsing status: true if parsing was successful, false otherwise
};

// print_parsed_ISO8601DurationParsed function prints the components of a parsed ISO 8601 duration.
void print_parsed_ISO8601DurationParsed(ISO8601DurationParsed p);

// read_typed_number function reads and interprets numeric components from a given ISO 8601 duration string.
NumReadResult read_typed_number(const char* s, const char* tags);

// read_character function reads a specific character from the ISO 8601 duration string.
int read_character(const char* s, const char* e, char* c);

// parse_iso8601_duration function fully parses an ISO 8601 duration string into its components.
ISO8601DurationParsed parse_iso8601_duration(const char* s);

#endif // PURE_CPP_H
