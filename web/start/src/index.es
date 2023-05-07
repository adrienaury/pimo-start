"use strict";
import { Elm } from "./Main";
var app = Elm.Main.init();

app.ports.sendMessage.subscribe(function (message) {
    console.log(message);
});
