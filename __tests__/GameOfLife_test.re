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
  test(
    "yo",
    () =>
    expect(nextCellState(Dead, 3)) |> toBe(Alive)
  );
  test(
    "yu",
    () =>
    expect(nextCellState(Dead, 2)) |> toBe(Dead)
  );
  testAll(
    "Any dead cell stays dead if not three live neighbours",
    List.filter(x => x != 3, Array.to_list(range(0,8))),
    x =>
    expect(nextCellState(Dead, x)) |> toBe(Dead)
  );
});