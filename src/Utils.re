
[@bs.val] external requireCSS: string => unit = "require";

let rec _range = (f: (int, int) => bool, start: int, end_: int) =>
  if (f(start, end_)) {
    [];
  } else {
    [start, ..._range(f, start + 1, end_)];
  };

let rangeExclusive = _range((a, b) => a >= b);
let rangeInclusive = _range((a, b) => a > b);