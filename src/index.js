import './main.css';
import { Elm } from './Main.elm';
import * as serviceWorker from './serviceWorker';
import { Socket } from 'phoenix';

let socket = new Socket("ws://localhost:4000/socket", {})
socket.connect()

let channel = socket.channel("game:lobby", {})
channel.join()
  .receive("ok", resp => {
    console.log('joined game channel', resp)
  })

Elm.Main.init({
  node: document.getElementById('root')
});

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();
