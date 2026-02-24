#include <Rcpp.h>
#include <vector>

using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector bfs_pruned(
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
  // this is what we will return in the end
  std::vector<int> reachable_stops;

  // define tracking arrays
  std::vector<bool> visited(n_vertices, false); // if we have visited something
  std::vector<double> best_time_per_stop(n_stops, 0); //track the best time for all stops
  std::vector<int> queue(n_vertices); //build the queue
  queue[0] = start_vertex_index;
  int head = 0;
  int tail = 0;
  std::vector<bool> queued(n_vertices, false); //track what is in the queue T/F
  queued[start_vertex_index] = true;

  while(head <= tail) {

    // get current index
    int current_index = queue[head];
    head = head + 1; //iterate

    if(visited[current_index]) {
      continue;
    }
    // check current time information
    double current_time_remaining = vertex_time_remaining[current_index];
    double current_elapsed_time = max_time - current_time_remaining;

    int current_stop_numeric = vertex_stop_numeric[current_index];

    if(best_time_per_stop[current_stop_numeric] >= current_time_remaining) {
      continue;
    }

    best_time_per_stop[current_stop_numeric] = current_time_remaining;

    visited[current_index] = true;

    // now need to get the neighbours.

    for(int i = adj_offsets[current_stop_numeric]; i < adj_offsets[current_stop_numeric + 1]; i++) {

      double edge_margin = adj_margin[i];
      int dest_vertex = adj_dest[i];

      if(edge_margin >= current_elapsed_time) {

        if(!queued[dest_vertex]) {

          tail = tail + 1;
          queue[tail] = dest_vertex;
          queued[dest_vertex] = true;

        }
      }
    }
  }

  // collect visited vertices and convert back to 1-indexed for R
  for(int i = 0; i < n_vertices; i++) {

    if(visited[i]) {
      reachable_stops.push_back(i + 1);  // +1 to return 1-indexed to R
    }

  }

  return(Rcpp::wrap(reachable_stops));

}
