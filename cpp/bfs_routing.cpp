#include <Rcpp.h>
#include <vector>

using namespace Rcpp;

// [[Rcpp::export]]
List bfs_pruned(
    int start_vertex_index,       // 0-indexed (R subtracts 1 before calling)
    int n_vertices,
    int n_stops,
    NumericVector vertex_time_remaining,  // 0-indexed
    IntegerVector vertex_stop_numeric,    // 0-indexed
    double max_time,
    IntegerVector adj_offsets,    // 0-indexed
    IntegerVector adj_dest,       // 0-indexed
    NumericVector adj_margin
) {

  // tracking arrays
  std::vector<bool> visited(n_vertices, false);
  std::vector<double> best_time_per_stop(n_stops, 0);

  // FIFO queue — O(1) push/pop, faster than priority queue for this graph
  // (temporal dominance pruning via best_time_per_stop handles correctness)
  std::vector<int> queue(n_vertices);
  queue[0] = start_vertex_index;
  int head = 0;
  int tail = 0;
  std::vector<bool> queued(n_vertices, false);
  queued[start_vertex_index] = true;

  while(head <= tail) {

    int current_index = queue[head];
    head = head + 1;

    if(visited[current_index]) {
      continue;
    }

    double current_time_remaining = vertex_time_remaining[current_index];
    double current_elapsed_time = max_time - current_time_remaining;

    int current_stop_numeric = vertex_stop_numeric[current_index];

    // temporal dominance: skip if this stop was already reached with more time
    if(best_time_per_stop[current_stop_numeric] >= current_time_remaining) {
      continue;
    }

    best_time_per_stop[current_stop_numeric] = current_time_remaining;
    visited[current_index] = true;

    // iterate edges via CSR
    for(int i = adj_offsets[current_stop_numeric]; i < adj_offsets[current_stop_numeric + 1]; i++) {

      double edge_margin = adj_margin[i];
      int dest_vertex = adj_dest[i];

      if(edge_margin >= current_elapsed_time) {

        // queued array prevents duplicate entries — each vertex pushed at most once
        if(!queued[dest_vertex]) {
          tail = tail + 1;
          queue[tail] = dest_vertex;
          queued[dest_vertex] = true;
        }
      }
    }
  }

  // collect results: one (stop, time_remaining) pair per reachable stop
  std::vector<int> stop_indices;
  std::vector<double> stop_times;

  for(int i = 0; i < n_stops; i++) {

    if(best_time_per_stop[i] > 0) {
      stop_indices.push_back(i + 1);  // 1-indexed for R
      stop_times.push_back(best_time_per_stop[i]);
    }

  }

  return List::create(
    Named("stop_numeric") = stop_indices,
    Named("time_remaining") = stop_times
  );

}

// [[Rcpp::export]]
List bfs_with_post_processing(
    int start_vertex_index,       // 0-indexed
    int n_vertices,
    int n_stops,
    NumericVector vertex_time_remaining,
    IntegerVector vertex_stop_numeric,    // 0-indexed
    double max_time,
    IntegerVector adj_offsets,    // 0-indexed
    IntegerVector adj_dest,       // 0-indexed
    NumericVector adj_margin,
    IntegerVector walk_offsets,   // 0-indexed CSR for walking access per stop
    IntegerVector walk_mb_numeric, // 0-indexed MB indices
    NumericVector walk_time,       // walking times
    NumericMatrix amenity_matrix,  // n_mbs x n_amenity_cols
    int n_mbs
) {

  int n_amenity_cols = amenity_matrix.ncol();

  // === BFS (identical to bfs_pruned) ===
  std::vector<bool> visited(n_vertices, false);
  std::vector<double> best_time_per_stop(n_stops, 0);

  std::vector<int> queue(n_vertices);
  queue[0] = start_vertex_index;
  int head = 0, tail = 0;
  std::vector<bool> queued(n_vertices, false);
  queued[start_vertex_index] = true;

  while(head <= tail) {
    int current_index = queue[head++];
    if(visited[current_index]) continue;

    double current_time_remaining = vertex_time_remaining[current_index];
    double current_elapsed_time = max_time - current_time_remaining;
    int current_stop_numeric = vertex_stop_numeric[current_index];

    if(best_time_per_stop[current_stop_numeric] >= current_time_remaining) continue;

    best_time_per_stop[current_stop_numeric] = current_time_remaining;
    visited[current_index] = true;

    for(int i = adj_offsets[current_stop_numeric]; i < adj_offsets[current_stop_numeric + 1]; i++) {
      if(adj_margin[i] >= current_elapsed_time && !queued[adj_dest[i]]) {
        queue[++tail] = adj_dest[i];
        queued[adj_dest[i]] = true;
      }
    }
  }

  // === Walking join + MB dedup ===
  std::vector<double> best_time_per_mb(n_mbs, -1.0);

  for(int s = 0; s < n_stops; s++) {
    if(best_time_per_stop[s] <= 0) continue;
    double stop_time = best_time_per_stop[s];

    for(int w = walk_offsets[s]; w < walk_offsets[s + 1]; w++) {
      double wt = walk_time[w];
      if(wt <= stop_time) {
        double time_incl_walking = stop_time - wt;
        int mb = walk_mb_numeric[w];
        if(time_incl_walking > best_time_per_mb[mb]) {
          best_time_per_mb[mb] = time_incl_walking;
        }
      }
    }
  }

  // === Amenity summation + collect results ===
  std::vector<double> amenity_sums(n_amenity_cols, 0.0);
  std::vector<int> reached_mbs;
  std::vector<double> reached_times;

  for(int m = 0; m < n_mbs; m++) {
    if(best_time_per_mb[m] >= 0) {
      reached_mbs.push_back(m + 1);  // 1-indexed for R
      reached_times.push_back(best_time_per_mb[m]);
      for(int a = 0; a < n_amenity_cols; a++) {
        amenity_sums[a] += amenity_matrix(m, a);
      }
    }
  }

  return List::create(
    Named("amenity_sums") = amenity_sums,
    Named("mb_numeric") = reached_mbs,
    Named("travel_times") = reached_times
  );

}
