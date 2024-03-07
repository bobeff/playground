/*
Dominant writing direction

Write a function that computes the dominant writing direction in a string of
text. Remember that each script object has a direction property that can be
"ltr" (left to right), "rtl" (right to left), or "ttb" (top to bottom).

The dominant direction is the direction of a majority of the characters that
have a script associated with them. The characterScript and countBy functions
defined earlier in the chapter are probably useful here.
*/

require("./scripts.js");
const {characterScript, countBy} = require("./text_scripts.js")

function dominantDirection(text) {
  let scripts = countBy(text, char => {
    let script = characterScript(char.codePointAt(0));
    return script ? script.direction : "none";
  }).filter(({name}) => name != "none");

  if (scripts.length === 0) return "none";

  return scripts.reduce((s1, s2) => s1.count < s2.count ? s2 : s1).name;
}

console.log(dominantDirection(""));
console.log(dominantDirection(" ,, ,, "));
console.log(dominantDirection("Hello!"));
console.log(dominantDirection("Hey, مساء الخير"));
