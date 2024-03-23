/*
Exercise 1: Measuring a robot
-----------------------------

It’s hard to objectively compare robots by just letting them solve a few
scenarios. Maybe one robot just happened to get easier tasks or the kind of
tasks that it is good at, whereas the other didn’t.

Write a function compareRobots that takes two robots (and their starting
memory). It generates 100 tasks and lets each of the robots solve each of these
tasks. When done, it outputs the average number of steps each robot took per
task.

For the sake of fairness, make sure you give each task to both robots, rather
than generating different tasks per robot.

Exercise 2: Robot efficiency
----------------------------

Can you write a robot that finishes the delivery task faster than
goalOrientedRobot? If you observe that robot’s behavior, what obviously stupid
things does it do? How could those be improved?

If you solved the previous exercise, you might want to use your compareRobots
function to verify whether you improved the robot.
*/

var roads = [
  "Alice's House-Bob's House",   "Alice's House-Cabin",
  "Alice's House-Post Office",   "Bob's House-Town Hall",
  "Daria's House-Ernie's House", "Daria's House-Town Hall",
  "Ernie's House-Grete's House", "Grete's House-Farm",
  "Grete's House-Shop",          "Marketplace-Farm",
  "Marketplace-Post Office",     "Marketplace-Shop",
  "Marketplace-Town Hall",       "Shop-Town Hall"
];

function buildGraph(edges) {
  let graph = Object.create(null);
  function addEdge(from, to) {
    if (from in graph) {
      graph[from].push(to);
    } else {
      graph[from] = [to];
    }
  }
  for (let [from, to] of edges.map(r => r.split("-"))) {
    addEdge(from, to);
    addEdge(to, from);
  }
  return graph;
}

var roadGraph = buildGraph(roads);
console.log("Road graph");
console.log(roadGraph);
console.log();

class VillageState {
  constructor(place, parcels) {
    this.place = place;
    this.parcels = parcels;
  }

  move(destination) {
    if (!roadGraph[this.place].includes(destination)) {
      return this;
    } else {
      let parcels = this.parcels.map(p => {
        if (p.place != this.place) return p;
        return {place: destination, address: p.address};
      }).filter(p => p.place != p.address);
      return new VillageState(destination, parcels);
    }
  }
}

function runRobot(state, robot, memory, log = true) {
  for (let turn = 0;; turn++) {
    if (state.parcels.length == 0) {
      if (log) {
        console.log(`Done in ${turn} turns`);
      }
      return turn;
    }
    let action = robot(state, memory);
    state = state.move(action.direction);
    memory = action.memory;
    if (log) {
      console.log(`Moved to ${action.direction}`);
    }
  }
}

function randomPick(array) {
  let choice = Math.floor(Math.random() * array.length);
  return array[choice];
}

function randomRobot(state) {
  return {direction: randomPick(roadGraph[state.place])};
}

VillageState.random = function(parcelCount = 5) {
  let parcels = [];
  for (let i = 0; i < parcelCount; i++) {
    let address = randomPick(Object.keys(roadGraph));
    let place;
    do {
      place = randomPick(Object.keys(roadGraph));
    } while (place == address);
    parcels.push({place, address});
  }
  return new VillageState("Post Office", parcels);
};

let villageState = VillageState.random();
console.log("Village state:");
console.log(villageState);
console.log();

console.log("Random robot:")
runRobot(villageState, randomRobot);
console.log();

var mailRoute = [
  "Alice's House", "Cabin", "Alice's House", "Bob's House",
  "Town Hall", "Daria's House", "Ernie's House",
  "Grete's House", "Shop", "Grete's House", "Farm",
  "Marketplace", "Post Office"
];

function routeRobot(state, memory) {
  if (memory.length == 0) {
    memory = mailRoute;
  }
  return {direction: memory[0], memory: memory.slice(1)};
}

console.log("Route robot:");
runRobot(villageState, routeRobot, []);
console.log();

function findRoute(graph, from, to) {
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

function goalOrientedRobot({place, parcels}, route) {
  if (route.length == 0) {
    let parcel = parcels[0];
    if (parcel.place != place) {
      route = findRoute(roadGraph, place, parcel.place);
    } else {
      route = findRoute(roadGraph, place, parcel.address);
    }
  }
  return {direction: route[0], memory: route.slice(1)};
}

console.log("Goal oriented robot:");
runRobot(villageState, goalOrientedRobot, []);
console.log();

function findShortestPaths(graph) {
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

let shortestPaths = findShortestPaths(roadGraph);

function reconstructPath(v1, v2, shortestPaths) {
  let path = [v2];
  while (v1 !== v2) {
    v2 = shortestPaths[v1][v2].prev;
    path.push(v2);
  }
  path.reverse();
  return path.slice(1);
}

function chooseNextDestination(place, parcels) {
  let dest = place;
  let dist = Infinity;
  for (let parcel of parcels) {
    if (parcel.place !== place &&
        shortestPaths[place][parcel.place].dist < dist) {
      dest = parcel.place;
      dist = shortestPaths[place][dest].dist;
    }
    if (parcel.place === place &&
        shortestPaths[place][parcel.address].dist < dist) {
      dest = parcel.address;
      dist = shortestPaths[place][dest].dist;
    }
  }

  return dest;
}

function shortestPathsRobot({place, parcels}, route) {
  if (route.length === 0) {
    let dest = chooseNextDestination(place, parcels);
    route = reconstructPath(place, dest, shortestPaths);
  }
  return {direction: route[0], memory: route.slice(1)};
}

console.log("Shortest paths robot:");
runRobot(villageState, shortestPathsRobot, []);
console.log();

function compareRobots(...robots) {
  for (let robot of robots) {
    robot.steps = 0;
  }

  const TASKS_COUNT = 1000;
  for (let task = 0; task < TASKS_COUNT; ++task) {
    let state = VillageState.random();
    for (let robot of robots) {
      robot.steps += runRobot(state, robot.run, [], false);
    }
  }

  console.log("Average steps per task:");
  for (let robot of robots) {
    console.log(`${robot.name} -> ${robot.steps / TASKS_COUNT}`);
  }
}

compareRobots({name: "Random robot", run: randomRobot},
              {name: "Route robot", run: routeRobot},
              {name: "Goal oriented robot", run: goalOrientedRobot},
              {name: "Shortest paths robot", run: shortestPathsRobot});
