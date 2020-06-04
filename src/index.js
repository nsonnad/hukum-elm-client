import './main.css';
import { Elm } from './Main.elm';
import * as serviceWorker from './serviceWorker';
import { Socket, Presence } from 'phoenix';

let socket = new Socket("ws://localhost:4000/socket", {})
socket.connect()

var app = Elm.Main.init({
  node: document.getElementById('root')
});

app.ports.addNewUser.subscribe(function(user_name) {
  let channel = socket.channel("game:lobby", {user_name: user_name})

  channel.join()
    .receive("ok", resp => { app.ports.registered.send(true) })
    .receive("error", resp => { app.ports.registered.send(false) })

  let presence = new Presence(channel)
  let presences = {}

  channel.on("presence_diff", state => {
    presences = Presence.syncDiff(presences, state)
    app.ports.gotUserList.send(Object.keys(presences))
  })

  channel.on("presence_state", state => {
    presences = Presence.syncState(presences, state)
    app.ports.gotUserList.send(Object.keys(presences))
  })

})

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();
