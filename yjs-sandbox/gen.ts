import * as Y from "yjs";
import * as fs from "fs";

const testsDir = __dirname + "/../tests";

const saveUpdate = (testCaseName: string, update: Uint8Array) => {
  fs.writeFileSync(testsDir + "/" + testCaseName + ".bin", update);
};

const main = () => {
  const ydoc = new Y.Doc();
  const ytext = ydoc.getText("text");
  ytext.insert(0, "あいうえお");
  ytext.insert(3, "abc");

  console.log(ytext.toString());

  var x = Y.encodeStateAsUpdate(ydoc);
  saveUpdate("test1", x);
};

main();
