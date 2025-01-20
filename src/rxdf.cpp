/*
 *  \file rxdf.h
 * The header file for the rXdf class
 */

#include <Rcpp.h>
#include <string>
#include <regex>
#include "xdf.h"
#include "rxdf.h"

using namespace Rcpp;

// [[Rcpp::export]]
List load_xdf(Rcpp::String filename_, Rcpp::Nullable<Rcpp::NumericVector> stream_ids = R_NilValue) {
  
  std::string filename = filename_.get_cstring();
  // capture the Xdf data object
  Xdf xdf_data;
  
  xdf_data.load_xdf(filename);
  // xdf_data.createLabels();  // this information has better formatting by working through the channels procedure
  
  Rcpp::NumericVector indices;
  if(stream_ids.isNotNull()) {
    indices = Rcpp::as<Rcpp::NumericVector>(stream_ids);
  } else {
    indices = Rcpp::seq(1, xdf_data.streams.size());
  }
  
  
  // Extract information from the xdf_data
  
  List streams(xdf_data.streams.size());
  CharacterVector stream_names(xdf_data.streams.size());
  int i;
  
  for (size_t j = 0; j < indices.size(); ++j) {
    i = indices[j] - 1;
    //*****************************************************************************
    // 
    // Apply methods special methods where needed
    // 
    //*****************************************************************************
    
    // Channels
    DataFrame channels = get_channels(xdf_data.streams[i].info.channels);
    
    DataFrame time_series = get_timeseries(xdf_data.streams[i].time_series);
    CharacterVector clean_names(xdf_data.streams[i].info.channel_count);
    
    if(time_series.ncol() > 0) {
      if(channels.containsElementNamed("label") && (time_series.ncol() == channels.nrow())) {
        if(channels.containsElementNamed("unit")){
          clean_names = make_clean_names(channels["label"], channels["unit"]);
        } else {
          clean_names = make_clean_names(channels["label"]);
        }
        
      } else {
        for(int i = 0; i < time_series.ncol(); ++i) {
          clean_names[i] = "V" + std::to_string(i + 1);
        }
      } 
      time_series.names() = clean_names;
      time_series["time_stamp"] = xdf_data.streams[i].time_stamps;
    }
    
    
    // Clock Offset
    NumericVector clock_offsets(xdf_data.streams[i].info.clock_offsets.size());
    CharacterVector clock_offset_names(xdf_data.streams[i].info.clock_offsets.size());
    for(size_t i = 0; i < xdf_data.streams[i].info.clock_offsets.size(); ++i) {
      clock_offsets[i] = xdf_data.streams[i].info.clock_offsets[i].second;
      clock_offset_names[i] = xdf_data.streams[i].info.clock_offsets[i].first;
    }
    clock_offsets.names() = clock_offset_names;
    
    List info = List::create(
      Named("channel_count") = xdf_data.streams[i].info.channel_count,
      Named("nominal_srate") = xdf_data.streams[i].info.nominal_srate,
      Named("type") = xdf_data.streams[i].info.type,
      Named("channel_format") = xdf_data.streams[i].info.channel_format,
      Named("channels") = get_channels(xdf_data.streams[i].info.channels),
      Named("clock_offsets") = clock_offsets,
      Named("first_timestamp") = xdf_data.streams[i].info.first_timestamp,  // This part of the xdf.cpp code is not working (crashes R)
      Named("last_timestamp") = xdf_data.streams[i].info.last_timestamp, // This part of the xdf.cpp code is not working (crashes R)
      Named("sample_count") = xdf_data.streams[i].info.sample_count,
      Named("measured_srate") = xdf_data.streams[i].info.measured_srate,
      Named("effective_sample_rate") = xdf_data.streams[i].info.effective_sample_rate
    );
    
    streams[i] = List::create(
      // Named("stream") = stream_cols,
      Named("time_series") = time_series,
      Named("time_stamps") = wrap(xdf_data.streams[i].time_stamps),
      Named("info") = info,
      Named("stream_header") = xdf_data.streams[i].streamHeader,
      Named("stream_footer") = xdf_data.streams[i].streamFooter,
      Named("last_timestamp") = xdf_data.streams[i].last_timestamp,
      Named("sampling_interval") = xdf_data.streams[i].sampling_interval,
      Named("clock_times") = wrap(xdf_data.streams[i].clock_times),
      Named("clock_values") = wrap(xdf_data.streams[i].clock_values)
    );
    
    stream_names[i] = xdf_data.streams[i].info.name + "_" + std::to_string(i + 1);
  }
  streams.names() = stream_names;
  
  DataFrame event_map = get_event_mapping(xdf_data.eventMap);
  
  return List::create(
    Named("version") = xdf_data.version,
    Named("streams") = streams,
    Named("total_length") = wrap(xdf_data.totalLen),
    Named("min_timestamp") = wrap(xdf_data.minTS),
    Named("max_timestamp") = wrap(xdf_data.maxTS),
    Named("total_channels") = wrap(xdf_data.totalCh),
    Named("mode_sample_rate") = wrap(xdf_data.majSR),
    Named("max_sample_rate") = wrap(xdf_data.maxSR),
    Named("effective_sample_rates") = wrap(xdf_data.effectiveSampleRateVector),
    Named("common_sample_rate") = wrap(xdf_data.fileEffectiveSampleRate),
    Named("stream_map") = wrap(xdf_data.streamMap),
    Named("file_header") = wrap(xdf_data.fileHeader),
    // Named("dictionary") = wrap(xdf_data.dictionary),
    // Named("labels") = wrap(xdf_data.labels),
    Named("event_type") = wrap(xdf_data.eventType),
    Named("event_map") = event_map,
    Named("offsets") = wrap(xdf_data.offsets)
    // Named("user_created_events") = wrap(xdf_data.userCreatedEvents)
  );
  
  
  
  // return streams;
  
}

DataFrame get_channels(const std::vector<std::map<std::string, std::string>> data) {
  if(data.empty()) {
    return DataFrame::create();
  }
  // Channels
  std::vector<std::string> channel_names;
  for (const auto& map : data) {
    for(const auto& pair : map) {
      if(std::find(channel_names.begin(), channel_names.end(), pair.first) == channel_names.end()) {
        channel_names.push_back(pair.first);
      }
    }
  }
  std::vector<Rcpp::CharacterVector> channel_contents(channel_names.size());
  for (const auto& map : data) {
    for(size_t i = 0; i < channel_names.size(); ++i) {
      auto it = map.find(channel_names[i]);
      if(it != map.end()) {
        channel_contents[i].push_back(it->second);
      } else {
        channel_contents[i].push_back(NA_STRING);
      } 
    }
  }
  DataFrame channels = wrap(channel_contents);
  channels.names() = Rcpp::CharacterVector(channel_names.begin(), channel_names.end());
  
  return channels;
}

CharacterVector make_clean_names(CharacterVector label, CharacterVector units) {
  int n = label.size();
  std::string undef_word = "NotDefined";
  
  std::regex pattern("[^a-zA-Z0-9_]+");
  std::string replacement = "_";
  
  CharacterVector clean_names(n);
  
  for(int i = 0; i < n; ++i) {
    std::string label_str = as<std::string>(label[i]);
    std::string units_str = as<std::string>(units[i]);
    
    label_str = std::regex_replace(label_str, pattern, replacement);
    
    // Convert to lowercase
    std::transform(label_str.begin(), label_str.end(), label_str.begin(), ::tolower);
    
    // Remove "not defined" units
    size_t pos = units_str.find(undef_word);
    while(pos != std::string::npos) {
      units_str.replace(pos, undef_word.size(), "");
      
      pos = units_str.find(undef_word, pos);
    }
    
    if(units_str.empty()) {
      clean_names[i] = label_str;
    } else {
      clean_names[i] = label_str + "_" + units_str;
    }
  }
  return clean_names;
}
CharacterVector make_clean_names(CharacterVector label) {
  int n = label.size();
  std::string undef_word = "NotDefined";
  
  std::regex pattern("[^a-zA-Z0-9_]+");
  std::string replacement = "_";
  
  CharacterVector clean_names(n);
  
  for(int i = 0; i < n; ++i) {
    std::string label_str = as<std::string>(label[i]);
    
    label_str = std::regex_replace(label_str, pattern, replacement);
    
    // Convert to lowercase
    std::transform(label_str.begin(), label_str.end(), label_str.begin(), ::tolower);
  
  clean_names[i] = label_str;
  }
  return clean_names;
}

DataFrame get_event_mapping(const std::vector<std::pair<std::pair<std::string, double>, int>> &data) {
  
  std::vector<std::string> event_name;
  std::vector<double> event_timestamp;
  std::vector<int> value;
  
  for (const auto& item : data) {
    event_name.push_back(item.first.first);
    event_timestamp.push_back(item.first.second);
    value.push_back(item.second);
  }
  return DataFrame::create(
    Named("event_name") = event_name,
    Named("event_timestamp") = event_timestamp,
    Named("value") = value
  );
}

DataFrame get_timeseries(const std::vector<std::vector<std::variant<int, float, double, int64_t, std::string>>>& data) {
  if(data.empty() || data[0].empty()) {
    return Rcpp::DataFrame();
  }
  
  size_t num_cols = data.size();
  size_t num_rows = data[0].size();
  
  Rcpp::List columns(num_cols);
  
  for(size_t j = 0; j < num_cols; ++j) {
    // Check type using first element but don't reuse its value
    const std::variant<int, float, double, int64_t, std::string>& first_value = data[j][0];
    
    if(std::holds_alternative<int>(first_value)) {
      IntegerVector column(num_rows);
      for(size_t i = 0; i < num_rows; ++i){
        column[i] = std::get<int>(data[j][i]);  // Get value from correct row
      }
      columns[j] = column;
    } else if(std::holds_alternative<int64_t>(first_value)) {
      IntegerVector column(num_rows);
      for(size_t i = 0; i < num_rows; ++i){
        column[i] = std::get<int64_t>(data[j][i]);  // Get value from correct row
      }
      columns[j] = column;
    } else if(std::holds_alternative<float>(first_value)) {
      NumericVector column(num_rows);
      for(size_t i = 0; i < num_rows; ++i){
        column[i] = std::get<float>(data[j][i]);  // Get value from correct row
      }
      columns[j] = column;
    } else if(std::holds_alternative<double>(first_value)) {
      NumericVector column(num_rows);
      for(size_t i = 0; i < num_rows; ++i){
        column[i] = std::get<double>(data[j][i]);  // Get value from correct row
      }
      columns[j] = column;
    } else if(std::holds_alternative<std::string>(first_value)) {
      CharacterVector column(num_rows);
      for(size_t i = 0; i < num_rows; ++i){
        column[i] = std::get<std::string>(data[j][i]);  // Get value from correct row
      }
      columns[j] = column;
    }
  }
  
  return Rcpp::DataFrame(columns);
}
