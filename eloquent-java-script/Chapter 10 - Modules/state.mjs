import { randomPick } from "./random.mjs";

export class VillageState {
  constructor(place, parcels, graph) {
    this.place = place;
    this.parcels = parcels;
    this.roadGraph = graph;
  }

  move(destination) {
    if (!this.roadGraph[this.place].includes(destination)) {
      return this;
    } else {
      let parcels = this.parcels.map(p => {
        if (p.place != this.place) return p;
        return {place: destination, address: p.address};
      }).filter(p => p.place != p.address);
      return new VillageState(destination, parcels, this.roadGraph);
    }
  }

  static random(roadGraph, parcelCount = 5) {
    let parcels = [];
    for (let i = 0; i < parcelCount; i++) {
      let address = randomPick(Object.keys(roadGraph));
      let place;
      do {
        place = randomPick(Object.keys(roadGraph));
      } while (place == address);
      parcels.push({place, address});
    }
    return new VillageState("Post Office", parcels, roadGraph);
  };
}
