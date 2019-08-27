open Jest;
open Expect;
open GameOfLife;
open Belt.Array;

describe("Game of Life", () => {
  testAll(
    "Any live cell with fewer than two live neighbours dies, as if by underpopulation.",
    [0,1],
    x =>
    expect(nextCellState(Alive, x)) |> toBe(Dead)
  );
  testAll(
    "Any live cell with two or three live neighbours lives on to the next generation.",
    [2,3],
    x =>
    expect(nextCellState(Alive, x)) |> toBe(Alive)
  );
  testAll(
    "Any live cell with more than three live neighbours dies, as if by overpopulation.",
    Array.to_list(range(4,8)),
    x =>
    expect(nextCellState(Alive, x)) |> toBe(Dead)
  );
  test(
    "Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction.",
    () =>
    expect(nextCellState(Dead, 3)) |> toBe(Alive)
  );
  testAll(
    "Any dead cell stays dead if not three live neighbours",
    List.filter(x => x != 3, Array.to_list(range(0,8))),
    x =>
    expect(nextCellState(Dead, x)) |> toBe(Dead)
  );
  testAll(
    "All cell are dead when creating a board",
    createBoard(3) |> cells |> List.map((cell) => cell.state) ,
    x =>
    expect(x) |> toBe(Dead)
  );
  test(
    "Can make alive a cell",
    () => {
      let cellState = createBoard(3) -> makeAlive(1,1) -> cellAt(1,1);
      expect(cellState) |> toBe(Alive)
    }
  );
  test(
    "Count number of alive neighbours",
    () => {
      let board = createBoard(3)
      let fullBoard = cells(board)
        |> List.map((cell) => cell.position)
        |> List.fold_left((board, position) => {
          let (i, j) = position
          makeAlive(board, i, j)
        }, board)
      expect(aliveNeighboursForCellAt(fullBoard, 1, 1)) |> toBe(8)
    }
  );
});