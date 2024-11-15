#include <Rcpp.h>

Rcpp::List get_xdf(Rcpp::String filename_);
Rcpp::DataFrame get_channels(const std::vector<std::map<std::string, std::string>> data);
Rcpp::CharacterVector make_clean_names(Rcpp::CharacterVector names, Rcpp::CharacterVector units);
Rcpp::CharacterVector make_clean_names(Rcpp::CharacterVector label);
Rcpp::DataFrame get_event_mapping(const std::vector<std::pair<std::pair<std::string, double>, int>> &vec);
Rcpp::DataFrame get_timeseries(const std::vector<std::vector<std::variant<int, float, double, int64_t, std::string>>>& data);