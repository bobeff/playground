require("./journal");;

// console.log(JOURNAL);

function phi([n00, n01, n10, n11]) {
  return (n11 * n00 - n10 * n01) /
         Math.sqrt((n10 + n11) * (n00 + n01) *
                   (n01 + n11) * (n00 + n10));
}


// console.log(phi([76, 9, 4, 1]));

function tableFor(event, journal) {
  let table = [0, 0, 0, 0];
  for (let entry of journal) {
    let index = 0;
    if (entry.squirrel) index += 2;
    if (entry.events.includes(event)) ++index;
    ++table[index];
  }
  return table;
}

// console.log(tableFor("pizza", JOURNAL));

function journalEvents(journal) {
  let events = new Set();
  for (let entry of journal) {
    for (let event of entry.events) {
      if (!events.has(event)) {
        events.add(event);
      }
    }
  }
  return events;
}

// console.log(journalEvents(JOURNAL));

for (let event of journalEvents(JOURNAL)) {
  let correlation = phi(tableFor(event, JOURNAL));
  if (correlation > 0.1 || correlation < -0.1) {
    console.log(event + ":", correlation);
  }
}

for (let entry of JOURNAL) {
  if (entry.events.includes("peanuts") &&
     !entry.events.includes("brushed teeth")) {
    entry.events.push("peanut teeth");
  }
}

console.log(phi(tableFor("peanut teeth", JOURNAL)));
