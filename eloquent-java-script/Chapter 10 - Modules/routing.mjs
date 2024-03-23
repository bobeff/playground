export function findRoute(graph, from, to) {
  let work = [{at: from, route: []}];
  for (let i = 0; i < work.length; i++) {
    let {at, route} = work[i];
    for (let place of graph[at]) {
      if (place == to) return route.concat(place);
      if (!work.some(w => w.at == place)) {
        work.push({at: place, route: route.concat(place)});
      }
    }
  }
}

export function findShortestPaths(graph) {
  let m = Object.create(null);

  for (let v1 in graph) {
    m[v1] = Object.create(null);
    for (let v2 in graph) {
      if (v1 === v2) {
        m[v1][v2] = {dist: 0, prev: v1};
      }
      else if (graph[v1].includes(v2)) {
        m[v1][v2] = {dist: 1, prev: v1};
      }
      else {
        m[v1][v2] = {dist: Infinity};
      }
    }
  }

  for (let v1 in graph) {
    for (let v2 in graph) {
      for (let v3 in graph) {
        if (m[v2][v3].dist > m[v2][v1].dist + m[v1][v3].dist) {
          m[v2][v3].dist = m[v2][v1].dist + m[v1][v3].dist;
          m[v2][v3].prev = m[v1][v3].prev;
        }
      }
    }
  }

  return m;
}

export function reconstructPath(v1, v2, shortestPaths) {
  let path = [v2];
  while (v1 !== v2) {
    v2 = shortestPaths[v1][v2].prev;
    path.push(v2);
  }
  path.reverse();
  return path.slice(1);
}
