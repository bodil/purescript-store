"use strict";

var withDevTools = typeof window !== "undefined" && window.devToolsExtension;

function checkError(devTools, result) {
  if (result.error) {
    devTools.connection.error("State deserialise error: " + result.error.join(" + "));
    return false;
  }
  return true;
}

exports.connectP = function(serialiseAction) {
  return function(deserialiseAction) {
    return function(serialiseState) {
      return function(deserialiseState) {
        return function() {
          return withDevTools ? {
            connection: window.devToolsExtension.connect(),
            serialiseAction: serialiseAction,
            deserialiseAction: deserialiseAction,
            serialiseState: serialiseState,
            deserialiseState: deserialiseState
          } : null;
        };
      };
    };
  };
};

exports.subscribe = function(devTools) {
  return function(cb) {
    return function() {
      var state, liftedState, action;

      if (devTools) {
        devTools.connection.subscribe(function(msg) {
          if (msg.type === "DISPATCH") {
            if (msg.payload.type === "JUMP_TO_STATE") {
              state = devTools.deserialiseState(JSON.parse(msg.state));
              if (checkError(devTools, state)) {
                cb.setState(state.result)();
              }
            } else if (msg.payload.type === "RESET") {
              cb.reset();
            } else if (msg.payload.type === "COMMIT") {
              cb.commit();
            } else if (msg.payload.type === "ROLLBACK") {
              state = devTools.deserialiseState(JSON.parse(msg.state));
              if (checkError(devTools, state)) {
                cb.rollback(state.result)();
              }
              // } else if (msg.payload.type === "TOGGLE_ACTION") {
              //   cb.toggleAction(msg.payload.id)(devTools.deserialiseState(msg.state))();
            }
          } else if (msg.type === "ACTION") {
            action = devTools.deserialiseAction(JSON.parse(msg.payload));
            if (checkError(devTools, action)) {
              cb.dispatch(action.result)();
            }
          } else if (msg.type === "IMPORT") {
            liftedState = JSON.parse(msg.state);
            state = liftedState.computedStates[liftedState.computedStates.length - 1].state;
            state = devTools.deserialiseState(state);
            if (checkError(devTools, state)) {
              cb.setState(state.result)();
              devTools.connection.send(null, liftedState);
            }
          // } else {
          //   console.log("UNHANDLED:", JSON.stringify(msg));
          }
        });
      }
    };
  };
};

exports.send = function(devTools) {
  return function(action) {
    return function(state) {
      return function() {
        if (devTools) {
          devTools.connection.send(Object.assign(
            {}, devTools.serialiseAction(action),
            {"type": (action.constructor && action.constructor.name)
          || action.tag || action.toString()}
          ), devTools.serialiseState(state));
        }
      };
    };
  };
};

exports.init = function(devTools) {
  return function(state) {
    return function() {
      if (devTools) {
        devTools.connection.init(devTools.serialiseState(state));
      }
    };
  };
};
