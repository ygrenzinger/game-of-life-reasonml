type cellState =
  | Alive
  | Dead;

let nextCellState = (state: cellState, nbOfAliveNeighbours: int): cellState =>
  switch (state, nbOfAliveNeighbours) {
  | (_, 3) => Alive
  | (Dead, _) => Dead
  | (Alive, 2) => Alive
  | (Alive, _) => Dead
  };