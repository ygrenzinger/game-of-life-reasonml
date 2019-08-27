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

type board = array(row);

let createBoard = (size: int) => make(size, make(size, Dead));

let makeAlive = (board: board, rowIndex: int, columnIndex: int): board => {
  let board = copy(board);
  let row = makeAlive(get(board, rowIndex), columnIndex);
  set(board, rowIndex, row);
  board;
};

let cellAt = (board: board, rowIndex: int, columnIndex: int): cellState =>
  get(get(board, rowIndex), columnIndex);

let cells = (board: board): list(cell) =>
  board
  |> Array.mapi((rowIndex, row) => cells(row, rowIndex))
  |> to_list
  |> List.flatten;

let aliveNeighboursForCellAt = (board: board, rowIndex: int, columnIndex: int) =>
  Belt.Array.range(rowIndex - 1, rowIndex + 1)
  |> map(i =>
       Belt.Array.range(columnIndex - 1, columnIndex + 1)
       |> to_list
       |> List.filter(j => {
            let isCurrentCell = (rowIndex == i && columnIndex == j);
            let isAlive = cellAt(board, i, j) == Alive;
            !isCurrentCell && isAlive;
          })
       |> List.length
     )
  |> fold_left((a, b) => a + b, 0);