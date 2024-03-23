/*
Exercise 3: Chessboard
----------------------

Write a program that creates a string that represents an 8×8 grid, using newline
characters to separate lines. At each position of the grid there is either a
space or a "#" character. The characters should form a chessboard.

Passing this string to console.log should show something like this:

 # # # #
# # # #
 # # # #
# # # #
 # # # #
# # # #
 # # # #
# # # #

When you have a program that generates this pattern, define a binding size = 8
and change the program so that it works for any size, outputting a grid of the
given width and height.
*/

function generateChessboard(size) {
  let chessboard = "";
  for (let r = 0; r < size; ++r) {
    for (let c = 0; c < size; ++c) {
      chessboard += (r + c) % 2 == 0 ? "." : "#";
    }
    chessboard += "\n";
  }
  return chessboard;
}

for (let size = 1; size <= 10; ++size) {
  console.log("Size =", size);
  console.log(generateChessboard(size));
  console.log("\n");
}
