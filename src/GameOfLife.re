open Array;

type cellState =
  | Dead
  | Alive;

let nextCellState = (state: cellState, nbOfAliveNeighbours: int): cellState =>
  switch (state, nbOfAliveNeighbours) {
  | (_, 3) => Alive
  | (Dead, _) => Dead
  | (Alive, 2) => Alive
  | (Alive, _) => Dead
  };

type position = (int, int);

type cell = {
  position,
  cellState,
};

type row = array(cellState);

let makeAlive = (row: row, columnIndex: int): row => {
  let row = copy(row);
  set(row, columnIndex, Alive);
  row;
};

let cells = (row: row, rowIndex: int): list(cell) =>
  row
  |> Array.mapi((columnIndex, cellState) =>
       {position: (rowIndex, columnIndex), cellState}
     )
  |> to_list;

type board = {
  size: int,
  grid: array(row),
};

let createBoard = (size: int) => {
  size,
  grid: make(size, make(size, Dead)),
};

let makeAlive = (board: board, rowIndex: int, columnIndex: int): board => {
  let board = {size: board.size, grid: copy(board.grid)};
  let row = makeAlive(get(board.grid, rowIndex), columnIndex);
  set(board.grid, rowIndex, row);
  board;
};

let cellAt = (board: board, rowIndex: int, columnIndex: int): cellState =>
  get(get(board.grid, rowIndex), columnIndex);

let cells = (board: board): list(cell) =>
  board.grid
  |> Array.mapi((rowIndex, row) => cells(row, rowIndex))
  |> to_list
  |> List.flatten;

let aliveNeighboursForCellAt = (board: board, rowIndex: int, columnIndex: int) =>
  Belt.Array.range(rowIndex - 1, rowIndex + 1)
  |> map(i =>
       Belt.Array.range(columnIndex - 1, columnIndex + 1)
       |> to_list
       |> List.filter(j =>
            if (!(i >= 0 && i < board.size && j >= 0 && j < board.size)) {
              false;
            } else if (rowIndex == i && columnIndex == j) {
              false;
            } else {
              cellAt(board, i, j) == Alive;
            }
          )
       |> List.length
     )
  |> fold_left((a, b) => a + b, 0);

let nextGeneration = (previousBoard: board) =>
  List.fold_left(
    (nextBoard, cell) => {
      let (i, j) = cell.position;
      let aliveNeighbours = aliveNeighboursForCellAt(previousBoard, i, j);
      let nextCellState = nextCellState(cell.cellState, aliveNeighbours);
      if (nextCellState == Alive) {
        makeAlive(nextBoard, i, j);
      } else {
        nextBoard;
      };
    },
    createBoard(previousBoard.size),
    cells(previousBoard),
  );