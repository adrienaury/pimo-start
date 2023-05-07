"use strict";
import { Elm } from "./Main";
var app = Elm.Main.init();

app.ports.sendMessage.subscribe(function (message) {
    let yaml = "";

    message.forEach((element) => {
        const name = element.name;
        const type = element.type;
        let yamlElement = "";

        yamlElement += `- selector:\n    jsonpath: "${name}"\n  mask:\n`;

        switch (type) {
            case "randomChoiceInUri":
                yamlElement += `    randomChoiceInUri: "${element.uri}"\n`;
                break;
            case "template":
                yamlElement += `    template: "${element.template}"\n`;
                break;
            case "randomInt":
                yamlElement += `    randomInt:\n      min: ${element.min}\n      max: ${element.max}\n`;
                break;
            case "regex":
                yamlElement += `    regex: "${element.regex}"\n`;
                break;
            case "randomDecimal":
                yamlElement += `    randomDecimal:\n      min: ${element.min}\n      max: ${element.max}\n      precision: ${element.precision}\n`;
                break;
            case "randDate":
                yamlElement += `    randDate:\n      dateMin: "${element.min}"\n      dateMax: "${element.max}"\n`;
                break;
        }

        yaml += yamlElement;
    });

    var encodedMasking = encodeURIComponent(yaml);
    var aDownloadMasking = document.createElement("a");

    aDownloadMasking.setAttribute("href", `data:text/yaml,${encodedMasking}`);
    aDownloadMasking.setAttribute("download", "masking.yml");
    aDownloadMasking.click();
    aDownloadMasking.remove();
});
