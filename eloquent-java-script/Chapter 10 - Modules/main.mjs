import roads from "./roads.mjs";
import { buildGraph } from "./graph.mjs";
import { VillageState } from "./state.mjs";
import { runRobot, randomRobot, routeRobot, goalOrientedRobot,
         shortestPathsRobot } from "./robots.mjs";
import { findShortestPaths } from "./routing.mjs";
import { compareRobots } from "./benchmark.mjs";

const roadGraph = buildGraph(roads);

const villageState = VillageState.random(roadGraph);
console.log("Village state:");
console.log(villageState);
console.log();

console.log("Random robot:")
runRobot(villageState, randomRobot, roadGraph);
console.log();

console.log("Route robot:");
runRobot(villageState, routeRobot, roadGraph, []);
console.log();

console.log("Goal oriented robot:");
runRobot(villageState, goalOrientedRobot, roadGraph, []);
console.log();

const shortestPaths = findShortestPaths(roadGraph);

console.log("Shortest paths robot:");
runRobot(villageState, shortestPathsRobot, roadGraph, [], shortestPaths);
console.log();

compareRobots(roadGraph, shortestPaths,
              {name: "Random robot", run: randomRobot},
              {name: "Route robot", run: routeRobot},
              {name: "Goal oriented robot", run: goalOrientedRobot},
              {name: "Shortest paths robot", run: shortestPathsRobot});
