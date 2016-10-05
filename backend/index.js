var express = require('express');
var app = express();

var expressWs = require('express-ws')(app);

app.use(function(req, res, next) {
    res.header("Access-Control-Allow-Origin", "*");
      res.header("Access-Control-Allow-Headers", "Origin, X-Requested-With, Content-Type, Accept");
        next();
});


var state = {
  cards: [{ color: "Red", shape: "Squiggle", number: "One" }]
}

app.get('/games/1', function (req, res) {
    res.setHeader('Content-Type', 'application/json');
    res.send(JSON.stringify(state));
});

app.ws('/games/1/board_updates', function(ws) {
  setInterval(function() {
    state.cards.push(state.cards[0]);
    ws.send(JSON.stringify(state));
  }, 1000);
});

app.listen(3000, function () {
    console.log('Example app listening on port 3000!');
});
