import '../node_modules/normalize.css/normalize.css'
import '../node_modules/milligram/dist/milligram.css'
import './css/main.css';
import 'typeface-roboto';
import { Elm } from './Main.elm';
import * as serviceWorker from './serviceWorker';
import { Socket, Presence } from 'phoenix';

//var HOST = location.origin.replace(/^http/, 'ws')
var HOST = "ws://localhost:4000"

let socket = new Socket(HOST + "/socket", {})
socket.connect()

var app = Elm.Main.init({
  node: document.getElementById('root')
});

let lobby = {};

app.ports.addNewUser.subscribe(function(user_name) {
  lobby = socket.channel("lobby:lobby", {user_name: user_name})

  lobby.join()
    .receive("ok", resp => {
      app.ports.registered.send(true)
      app.ports.gotGameList.send(resp.game_list)
    })
    .receive("error", resp => { app.ports.registered.send(false) })

  let presence = new Presence(lobby)
  let presences = {}

  lobby.on("presence_diff", state => {
    presences = Presence.syncDiff(presences, state)
    app.ports.gotUserList.send(Object.keys(presences))
  })

  lobby.on("presence_state", state => {
    presences = Presence.syncState(presences, state)
    app.ports.gotUserList.send(Object.keys(presences))
  })

  lobby.on("game_list", resp => {
    app.ports.gotGameList.send(resp.game_list)
  })

})

let gameChannel = {};

app.ports.startNewGame.subscribe(function(userName) {
  lobby.push("new_game", { user_name: userName, private: false } )
    .receive("ok", (msg) => { joinGameChannel(lobby, userName, msg.game_name) })
    .receive("error", (msg) => { console.log("error", msg)})
})

app.ports.joinGame.subscribe(function(gameOpts) {
  lobby.push("join_game", { game_name: gameOpts.gameName } )
    .receive("ok", (msg) => { joinGameChannel(lobby, gameOpts.userName, gameOpts.gameName)
    })
    .receive("error", (msg) => { console.log("error", msg)})
})

function joinGameChannel(lobby, userName, gameName) {
  lobby.leave().receive("ok", () => {
    gameChannel = socket.channel("game:" + gameName, {user_name: userName})

    gameChannel.join()
      .receive("ok", resp => { app.ports.joinedGameChannel.send(true) })
      .receive("error", resp => { app.ports.joinedGameChannel.send(false) })

    gameChannel.on("game_state", gameState => {
      app.ports.gotGameState.send(gameState.game)
    })

    app.ports.pushPlayerAction.subscribe(function(action) {
      gameChannel.push(action.action, action.payload)
    })
  })
}

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();
