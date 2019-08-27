open Jest;
open Expect;
open Utils;

describe("Utils function", () => {
  test("range exclusive", () =>
    expect(rangeExclusive(0, 3)) |> toEqual([0, 1, 2])
  );

  test("range exclusive", () =>
    expect(rangeInclusive(0, 3)) |> toEqual([0, 1, 2, 3])
  );
});