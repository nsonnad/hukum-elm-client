import './main.css';
import { Elm } from './Main.elm';
import * as serviceWorker from './serviceWorker';
import { Socket, Presence } from 'phoenix';

let socket = new Socket("ws://localhost:4000/socket", {})
socket.connect()

var app = Elm.Main.init({
  node: document.getElementById('root')
});

let lobby = {};

app.ports.addNewUser.subscribe(function(user_name) {
  lobby = socket.channel("lobby:lobby", {user_name: user_name})

  lobby.join()
    .receive("ok", resp => { app.ports.registered.send(true) })
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

})

let gameChannel = {};

app.ports.startNewGame.subscribe(function(user_name) {
  lobby.push("new_game")
    .receive("ok", (msg) => {joinGameChannel(user_name, msg)})
    .receive("error", (msg) => { console.log("error", msg)})
})

function joinGameChannel(user_name, msg) {
  lobby.leave().receive("ok", () => {
    gameChannel = socket.channel("game:" + msg.game_name, {user_name: user_name})
    gameChannel.join()
      .receive("ok", resp => { app.ports.joinedGameChannel.send(msg.game_name) })
      .receive("error", resp => { app.ports.joinedGameChannel.send(false) })
  })
}

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();
