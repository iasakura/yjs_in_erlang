import * as Y from "yjs";
import * as fs from "fs";

const testsDir = __dirname + "/../tests";

const saveUpdate = (testCaseName: string, update: Uint8Array) => {
  fs.writeFileSync(testsDir + "/" + testCaseName + ".bin", update);
};

const gen_test1 = () => {
  const ydoc = new Y.Doc();
  const ytext = ydoc.getText("text");
  ytext.insert(0, "あいうえお");
  ytext.insert(3, "abc");

  console.log(ytext.toString());

  var x = Y.encodeStateAsUpdate(ydoc);
  saveUpdate("test1", x);
};

const gen_test2 = () => {
  let a: Uint8Array;
  {
    const ydoc = new Y.Doc();
    ydoc.clientID = 0;
    const ytext = ydoc.getText("text");
    ytext.insert(0, "abc");
    a = Y.encodeStateAsUpdate(ydoc);
  }

  let b: Uint8Array;
  {
    const ydoc = new Y.Doc();
    ydoc.clientID = 1;
    const ytext = ydoc.getText("text");
    ytext.insert(0, "xyz");
    b = Y.encodeStateAsUpdate(ydoc);
  }

  const ydoc = new Y.Doc();
  ydoc.transact(() => {
    Y.applyUpdate(ydoc, a);
    Y.applyUpdate(ydoc, b);
  });
  console.log(ydoc.getText("text").toString());

  saveUpdate("test2-a", a);
  saveUpdate("test2-b", b);
};

gen_test1();
gen_test2();
