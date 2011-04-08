Craftd.events = {};

Craftd.register = function (name) {
  if (Craftd.events[name] && Craftd.events[name].constructor != Array) {
    Craftd.events[name] = [];
  }

  for (let i = 1; i < arguments.length; i++) {
    Craftd.events[name].push(arguments[i]);
  }
}

Craftd.unregister = function (name) {
  if (Craftd.events[name] && Craftd.events[name].constructor != Array) {
    return;
  }

  var callbacks = [];

  for (let i = 1; i < arguments.length; i++) {
    callbacks.push(arguments[i]);
  }

  if (callbacks.length == 0) {
    Craftd.events[name] = [];
  }
  else {
    Craftd.events[name] = Craftd.events[name].filter(function (callback) {
      for (let i = 0; i < callbacks.length; i++) {
        if (callback == callbacks[i]) {
          return false;
        }
      }

      return true;
    });
  }
}
