# Dots and Boxes with Weighted Scoring

## Overview

Dots and Boxes is a classic pencil-and-paper game. This project introduces a modified version with weighted scoring _(to be implemented)_, implemented in **Scala** to provide an engaging experience.

## Scope

- **Weighted Scoring**: Each box is randomly assigned a value between 1 and 5, and a player's score is the sum of claimed box values.
  
- **AI with MINIMAX Algorithm**: The AI opponent employs the MINIMAX algorithm for strategic moves. Human players can set parameters such as the number of plies the AI will search and the board's size.

## Implementation in Scala

- **Functionality**: The Scala implementation ensures the game works seamlessly with a user-friendly interface.
  
- **Algorithm Understanding**: The code demonstrates a solid grasp of the MINIMAX algorithm, balancing efficiency and clarity without resorting to spaghetti code.

## Pitfalls and Considerations

- **Performance Challenges**: Experimentation with different plies and board sizes may reveal performance challenges, especially for larger boards and higher plies. The analysis document delves into these challenges.

## Analysis

For a detailed analysis of the implementation, including insights into gameplay experimentation, performance considerations, and reflections on AI behavior, refer to the [Analysis Document]([link-to-analysis-document](https://github.com/areeb-can-code/dots-and-boxes-game-scala/blob/main/%5BCSC-380%5D%20Dots-n-Boxes.docx)).

---

*Note: This project is an attempt to utilize the MINIMAX function, due to the math complications, the alpha and beta pruning may be inaccurate.*
