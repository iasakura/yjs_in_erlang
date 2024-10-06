import * as Y from "yjs";
import * as fs from "fs";

const testsDir = __dirname + "/../tests";

const saveUpdate = (testCaseName: string, update: Uint8Array) => {
  fs.writeFileSync(testsDir + "/" + testCaseName + ".bin", update);
};

const gen_test1 = () => {
  const ydoc = new Y.Doc();
  ydoc.clientID = 1;
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

const gen_test3 = () => {
  const ydoc = new Y.Doc();
  const ydoc0 = new Y.Doc();
  const ydoc1 = new Y.Doc();
  const ydoc2 = new Y.Doc();
  const ydoc3 = new Y.Doc();

  ydoc.clientID = 1000;
  ydoc0.clientID = 2;
  ydoc1.clientID = 1;
  ydoc2.clientID = 3;
  ydoc3.clientID = 4;

  ydoc.getText("text").insert(0, "b");
  const updateBaseStart = Y.encodeStateAsUpdate(ydoc);

  ydoc.getText("text").insert(1, "end");
  const updateBase = Y.encodeStateAsUpdate(ydoc);

  ydoc0.transact(() => {
    Y.applyUpdate(ydoc0, updateBaseStart);
  });
  ydoc0.getText("text").insert(1, "0");
  const update0 = Y.encodeStateAsUpdate(ydoc0);

  ydoc1.transact(() => {
    Y.applyUpdate(ydoc1, updateBase);
  });
  ydoc1.getText("text").insert(1, "1");
  const update1 = Y.encodeStateAsUpdate(ydoc1);

  ydoc2.transact(() => {
    Y.applyUpdate(ydoc2, updateBase);
  });
  ydoc2.getText("text").insert(1, "2");
  const update2 = Y.encodeStateAsUpdate(ydoc2);

  ydoc3.transact(() => {
    Y.applyUpdate(ydoc3, updateBase);
  });
  ydoc3.getText("text").insert(1, "3");
  const update3 = Y.encodeStateAsUpdate(ydoc3);

  // b123end
  ydoc2.transact(() => {
    Y.applyUpdate(ydoc2, update1);
    Y.applyUpdate(ydoc2, update3);
  });
  console.log(ydoc2.getText("text").toString());

  console.log(`HERE!`);
  ydoc2.getText("text").insert(2, "X");
  ydoc2.transact(() => {
    Y.applyUpdate(ydoc2, update0);
  });

  console.log(ydoc2.getText("text").toString());
  //   let cur = ydoc2.getText("text")._start;
  //   while (cur != undefined) {
  //     const { doc, ...node } = cur;
  //     console.log(`node:`, node);
  //     cur = cur.right;
  //   }
};

gen_test1();
gen_test2();
gen_test3();
