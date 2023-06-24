import "./style.css";
import { Elm } from "./src/Main.elm";

if (process.env.NODE_ENV === "development") {
    const ElmDebugTransform = await import("elm-debug-transformer")

    ElmDebugTransform.register({
        simple_mode: true
    })
}

const root = document.querySelector("#app div");
const app = Elm.Main.init({ node: root });

//////
import { editor, Uri } from 'monaco-editor'
import EditorWorker from 'monaco-editor/esm/vs/editor/editor.worker?worker'
import { setDiagnosticsOptions } from 'monaco-yaml'
import YamlWorker from './yaml.worker.js?worker'

window.MonacoEnvironment = {
  getWorker(moduleId, label) {
    switch (label) {
      case 'editorWorkerService':
        return new EditorWorker()
      case 'yaml':
        return new YamlWorker()
      default:
        throw new Error(`Unknown label ${label}`)
    }
  }
}

// The uri is used for the schema file match.
const modelUri = Uri.parse('file://masking.yml')
let masking = 'version: "1"\nmasking:';

setDiagnosticsOptions({
    enableSchemaRequest: true,
    hover: true,
    completion: true,
    validate: true,
    format: true,
    schemas: [
        {
            // Id of the first schema
            uri: "https://raw.githubusercontent.com/CGI-FR/PIMO/main/schema/v1/pimo.schema.json",
            // Associate with our model
            fileMatch: [String(modelUri)],
          },
    ]
  })
  
  let editorYaml = editor.create(document.getElementById("editor-yaml"), {
    automaticLayout: true,
    tabSize: 2,
    scrollBeyondLastLine: false,
    minimap: { enabled: false },
    model: editor.createModel(masking, "yaml", modelUri),
  })
//////

app.ports.updateYaml.subscribe(function (yaml) {
  editorYaml.setValue(yaml)
})
