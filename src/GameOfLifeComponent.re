open GameOfLife;

Utils.requireCSS("./GameOfLifeComponent.css");

let defaultBoardSize = 30;

/* State declaration */
type state = {
  board,
  running: bool,
};

/* Action declaration */
type action =
  | Start
  | Stop
  | Reset
  | Tick
  | Activating(position);

let nextBoardState = (state: state): board =>
  state.running ? nextGeneration(state.board) : state.board;

[@react.component]
let make = () => {
  let (state, dispatch) =
    React.useReducer(
      (state, action) =>
        switch (action) {
        | Start => {...state, running: true}
        | Stop => {...state, running: false}
        | Reset => {running: false, board: createBoard(defaultBoardSize)}
        | Tick => {...state, board: nextBoardState(state)}
        | Activating(pos) => {...state, board: makeAlive(state.board, pos)}
        },
      {board: createBoard(defaultBoardSize), running: false},
    );

  React.useEffect0(() => {
    let timerId = Js.Global.setInterval(() => dispatch(Tick), 1000);
    Some(() => Js.Global.clearInterval(timerId));
  });

  let renderCell = (cell: cell) => {
    let className = "cell " ++ (cell.cellState == Alive ? "alive" : "dead");
    <span className onClick={_ => dispatch(Activating(cell.position))} />;
  };

  let renderRow = (row: row) => {
    let cells = Array.map(renderCell, row);
    <div className="row"> {React.array(cells)} </div>;
  };

  let renderGrid = (board: board) => {
    let rows =
      Array.map(renderRow, board.grid);
    <div> {React.array(rows)} </div>;
  };

  <div>
    <div>
      <button onClick={_ => dispatch(Reset)}>
        {ReasonReact.string("Reset")}
      </button>
      {
        if (state.running) {
          <button onClick={_ => dispatch(Stop)}>
            {ReasonReact.string("Stop")}
          </button>;
        } else {
          <button onClick={_ => dispatch(Start)}>
            {ReasonReact.string("Start")}
          </button>;
        }
      }
    </div>
    <div> 
    {renderGrid(state.board)}
    </div>
  </div>;
};