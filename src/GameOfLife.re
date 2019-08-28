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

let cells = (rowIndex: int, row: row): list(cell) =>
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

let makeAlive = (board: board, position: position): board => {
  let board = {size: board.size, grid: copy(board.grid)};
  let (rowIndex, columnIndex) = position;
  let row = makeAlive(get(board.grid, rowIndex), columnIndex);
  set(board.grid, rowIndex, row);
  board;
};

let cellAt = (board: board, position: position): cellState => {
  let (rowIndex, columnIndex) = position;
  get(get(board.grid, rowIndex), columnIndex);
};
  

let cells = (board: board): list(cell) =>
  board.grid |> Array.mapi(cells) |> to_list |> List.flatten;

let aliveNeighboursForCellAt = (board: board, position: position) => {
  let (rowIndex, columnIndex) = position;
  let isCurrentPos = ((i, j)) => position == (i,j);
  let isInsideGrid = ((i, j)) =>
    i >= 0 && i < board.size && j >= 0 && j < board.size;
  let isCellAlive = (position) => cellAt(board, position) == Alive;

  let isAliveNeighbor = position =>
    !isCurrentPos(position)
    && isInsideGrid(position)
    && isCellAlive(position);

  Belt.Array.range(rowIndex - 1, rowIndex + 1)
  |> map(i =>
       Belt.Array.range(columnIndex - 1, columnIndex + 1)
       |> to_list
       |> List.filter(j => isAliveNeighbor((i, j)))
       |> List.length
     )
  |> fold_left((a, b) => a + b, 0);
};

let nextGeneration = (previousBoard: board) => {
  let processingCell = (nextBoard, cell) => {
    let aliveNeighbours =
      aliveNeighboursForCellAt(previousBoard, cell.position);
    let nextCellState = nextCellState(cell.cellState, aliveNeighbours);
    if (nextCellState == Alive) {
      makeAlive(nextBoard, cell.position);
    } else {
      nextBoard;
    };
  };
  List.fold_left(
    processingCell,
    createBoard(previousBoard.size),
    cells(previousBoard),
  );
};