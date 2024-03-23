import { randomPick } from "./random.mjs";
import { findRoute, reconstructPath } from "./routing.mjs";

export function runRobot(state, robot, roadGraph, memory, shortestPaths, log = true) {
  for (let turn = 0;; turn++) {
    if (state.parcels.length == 0) {
      if (log) {
        console.log(`Done in ${turn} turns`);
      }
      return turn;
    }
    let action = robot(state, roadGraph, memory, shortestPaths);
    state = state.move(action.direction);
    memory = action.memory;
    if (log) {
      console.log(`Moved to ${action.direction}`);
    }
  }
}

export function randomRobot(state, roadGraph) {
  return {direction: randomPick(roadGraph[state.place])};
}

let mailRoute = [
  "Alice's House", "Cabin", "Alice's House", "Bob's House",
  "Town Hall", "Daria's House", "Ernie's House",
  "Grete's House", "Shop", "Grete's House", "Farm",
  "Marketplace", "Post Office"
];

export function routeRobot(state, roadGraph, memory) {
  if (memory.length == 0) {
    memory = mailRoute;
  }
  return {direction: memory[0], roadGraph, memory: memory.slice(1)};
}

export function goalOrientedRobot({place, parcels}, roadGraph, route) {
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

function chooseNextDestination(place, parcels, shortestPaths) {
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

export function shortestPathsRobot({place, parcels}, roadGraph, route,
                                   shortestPaths) {
  if (route.length === 0) {
    let dest = chooseNextDestination(place, parcels, shortestPaths);
    route = reconstructPath(place, dest, shortestPaths);
  }
  return {direction: route[0], roadGraph, memory: route.slice(1)};
}
