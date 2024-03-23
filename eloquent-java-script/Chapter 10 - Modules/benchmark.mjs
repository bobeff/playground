import { VillageState } from "./state.mjs";
import { runRobot } from "./robots.mjs";

export function compareRobots(roadGraph, shortestPaths, ...robots) {
  for (let robot of robots) {
    robot.steps = 0;
  }

  const TASKS_COUNT = 1000;
  for (let task = 0; task < TASKS_COUNT; ++task) {
    let state = VillageState.random(roadGraph);
    for (let robot of robots) {
      robot.steps += runRobot(state, robot.run, roadGraph, [], shortestPaths, false);
    }
  }

  console.log("Average steps per task:");
  for (let robot of robots) {
    console.log(`${robot.name} -> ${robot.steps / TASKS_COUNT}`);
  }
}
