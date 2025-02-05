/* eslint-env browser */

import * as Y from "yjs";
import { WebsocketProvider } from "y-websocket";
import { QuillBinding } from "y-quill";
import Quill from "quill";
import QuillCursors from "quill-cursors";

Quill.register("modules/cursors", QuillCursors);

window.addEventListener("load", () => {
  const ydoc = new Y.Doc();
  const provider = new WebsocketProvider(
    "ws://35.215.150.250:80/ws", // use the public ws server
    // `ws${location.protocol.slice(4)}//${location.host}/ws`, // alternatively: use the local ws server (run `npm start` in root directory)
    "quill-demo-2024/06",
    ydoc
  );
  const ytext = ydoc.getText("quill");
  const editorContainer = document.createElement("div");
  editorContainer.setAttribute("id", "editor");
  document.body.insertBefore(editorContainer, null);
  editorContainer.addEventListener("keydown", (event) => {
    if (event.key === "Escape") {
      console.log("document", ydoc);
    }
  });

  const editor = new Quill(editorContainer, {
    modules: {
      cursors: true,
      toolbar: [
        [{ header: [1, 2, false] }],
        ["bold", "italic", "underline"],
        ["image", "code-block"],
      ],
      history: {
        userOnly: true,
      },
    },
    placeholder: "Start collaborating...",
    theme: "snow", // or 'bubble'
  });

  const binding = new QuillBinding(ytext, editor, provider.awareness);
  ydoc.on("update", (update) => {
    console.log(ydoc);
  });
  ydoc.on("beforeTransaction", (transaction, remote) => {
    console.log("beforeTransaction", transaction, remote);
  });
  ydoc.on("afterTransaction", (transaction, remote) => {
    console.log("afterTransaction", transaction, remote);
  });

  /*
  // Define user name and user name
  // Check the quill-cursors package on how to change the way cursors are rendered
  provider.awareness.setLocalStateField('user', {
    name: 'Typing Jimmy',
    color: 'blue'
  })
  */

  const connectBtn = document.getElementById("y-connect-btn");
  connectBtn.addEventListener("click", () => {
    if (provider.shouldConnect) {
      provider.disconnect();
      connectBtn.textContent = "Connect";
    } else {
      provider.connect();
      connectBtn.textContent = "Disconnect";
    }
  });

  // @ts-ignore
  window.example = { provider, ydoc, ytext, binding, Y };
});
