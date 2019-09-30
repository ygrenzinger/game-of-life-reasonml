open Array;

type cellState =
  | Dead
  | Alive;

type position = (int, int);

type cell = { position, cellState };

type row = array(cell);

type board = {
  size: int,
  grid: array(row),
};

let nextCellState = (state: cellState, nbOfAliveNeighbours: int): cellState =>
  switch (state, nbOfAliveNeighbours) {
  | (_, 3) => Alive
  | (Alive, 2) => Alive
  | _ => Dead
  };

let createRow = (rowIndex: int, size: int, state: cellState): row => {
  make(size, state) |> mapi((columnIndex, cellState) => {
    {position: (rowIndex, columnIndex), cellState}
  }) 
}

let createBoard = (size: int) => {
  size,
  grid: Belt.Array.range(0, size - 1) |> mapi((i, _) => {
    createRow(i, size, Dead)
  }),
};

let cellAt = (board: board, position: position): cell => {
  let (rowIndex, columnIndex) = position;
  get(get(board.grid, rowIndex), columnIndex);
};

let isCellAlive = (board: board, position: position): bool => {
  cellAt(board, position).cellState == Alive
}

let makeAliveCell = ({position: position, _}): cell => {
  {position, cellState: Alive}
}

let makeAlive = (row: row, columnIndex: int): row => {
  let row = copy(row);
  let cell = makeAliveCell(get(row, columnIndex));
  set(row, columnIndex, cell);
  row;
};

let makeAlive = (board: board, (rowIndex, columnIndex)): board => {
  let board = {size: board.size, grid: copy(board.grid)};
  let row = makeAlive(get(board.grid, rowIndex), columnIndex);
  set(board.grid, rowIndex, row);
  board;
};

let cells = (board: board): list(cell) =>
  board.grid |> Array.map(to_list) |> to_list |> List.flatten;

let generatePositions = ((ia: int, ib: int), (ja: int, jb: int)) : list(position) => {
  Belt.Array.range(ia, ib)
  |> map(i =>
       Belt.Array.range(ja, jb)
       |> map(j => (i,j))
       |> to_list
     )
  |> to_list
  |> List.flatten
}

let aliveNeighboursForCellAt = (board: board, position: position) => {
  let isCurrentPos = (pos) => position == pos;
  let isInsideGrid = ((i, j)) =>
    i >= 0 && i < board.size && j >= 0 && j < board.size;
  let isCellAlive = (position) => isCellAlive(board, position);

  let isAliveNeighbor = position =>
    !isCurrentPos(position)
    && isInsideGrid(position)
    && isCellAlive(position);

    let (rowIndex, columnIndex) = position;
    generatePositions((rowIndex - 1, rowIndex + 1), (columnIndex - 1, columnIndex + 1))
    |> List.filter(isAliveNeighbor)
    |> List.length
};

let nextGeneration = (previousBoard: board) => {
  let processingCell = (nextBoard, cell) => {
    let nextCellState = aliveNeighboursForCellAt(previousBoard, cell.position)
      |> nextCellState(cell.cellState);
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