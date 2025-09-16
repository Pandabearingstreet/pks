#include <Rcpp.h>
#include <unordered_map>
#include <unordered_set>
#include <regex>
using namespace Rcpp;

// [[Rcpp::export]]
DataFrame collapse_states_cpp(const DataFrame& bloated_states) {
  CharacterVector colnames_vec = bloated_states.attr("names");
  int nrows = bloated_states.nrows();
  int ncols = bloated_states.size();
  
  // Step 1: Remove numeric prefixes from column names
  std::vector<std::string> cleaned_names(ncols);
  std::unordered_map<std::string, std::vector<int>> group_map;
  
  std::regex prefix_regex("^\\d+_");
  for (int j = 0; j < ncols; ++j) {
    std::string colname = as<std::string>(colnames_vec[j]);
    std::string clean_name = std::regex_replace(colname, prefix_regex, "");
    cleaned_names[j] = clean_name;
    group_map[clean_name].push_back(j);
  }
  
  // Step 2: Collapse columns by group
  std::vector<std::vector<int>> collapsed_cols;
  std::vector<std::string> final_names;
  
  for (const auto& kv : group_map) {
    const std::vector<int>& cols = kv.second;
    final_names.push_back(kv.first);
    std::vector<int> col(nrows, 0);
    
    for (int idx : cols) {
      IntegerVector v = bloated_states[idx];
      for (int i = 0; i < nrows; ++i) {
        col[i] = col[i] || (v[i] != 0);
      }
    }
    collapsed_cols.push_back(col);
  }
  
  // Step 3: Remove duplicate rows using a set
  std::vector<std::vector<int>> unique_rows;
  std::unordered_set<std::string> seen;
  
  for (int i = 0; i < nrows; ++i) {
    std::string key;
    std::vector<int> row;
    
    for (const auto& col : collapsed_cols) {
      row.push_back(col[i]);
      key += std::to_string(col[i]) + ",";
    }
    
    if (seen.find(key) == seen.end()) {
      seen.insert(key);
      unique_rows.push_back(row);
    }
  }
  
  // Step 4: Order rows by row sum
  std::sort(unique_rows.begin(), unique_rows.end(), [](const std::vector<int>& a, const std::vector<int>& b) {
    int sum_a = std::accumulate(a.begin(), a.end(), 0);
    int sum_b = std::accumulate(b.begin(), b.end(), 0);
    return sum_a < sum_b;
  });
  
  // Convert to DataFrame
  List out;
  for (size_t j = 0; j < collapsed_cols.size(); ++j) {
    IntegerVector col(unique_rows.size());
    for (size_t i = 0; i < unique_rows.size(); ++i) {
      col[i] = unique_rows[i][j];
    }
    out.push_back(col);
  }
  out.attr("names") = final_names;
  out.attr("class") = "data.frame";
  out.attr("row.names") = seq(1, unique_rows.size());
  
  return out;
}
